# This script is meant to automate repeated mesure analysis.

# Data format should be: 
# Each participant should be a different row.
# Each variable, each time point and each trial should be in different columns.
# If you have multiple time points and multiple measurements in each time point it is recommended that you use coding in the column name, in such a way that each column name is unique.
# For example if you are testing "Jump height" three times every day for 5 days, you can indicate the time point (day) as a letter of the alphabet and the trial number next to it.
# Therefore the column names would be: JumpHeightA1, JumpHeightA2, JumpHeightA3, JumpHeightB1, JumpHeightB2 ....


MyData <- read.csv("~/MyData.csv", stringsAsFactors=FALSE)
variablename <- "SquatH"                 # variable name, change the value in the quotation marks, case sensitive, write the name of the variable up until the time point
mode <- 'max'                            #Set the mode of choosing prefered values among the different trials in the same time points, 'mean', 'max or 'min' values
GroupColumnName <- "GroupOne"            # enter the name of the column that the Group variable is located
IDname<- "ID"                            # enter the name of the column that the ID variable is located

library(reshape2)  # required for melt
library(ggplot2)   # required for the graph
library(dplyr)     # required for data transformation
library(jtools)    # required to the APA figure theme
library(ez)        # required for ANOVA

if(!requireNamespace('mung')){
  devtools::install_github('MavropaliasG/R-Scripts',subdir='Scripts/mung')
  library(mung)
}

library(mung)                     # required for munging - selecting one of the trials of a variable in the same time point, based on your preferance (max, min or mean)

df <- data.frame(MyData)          # renames the data frame
df[df==""] <- NA                  # make empty cells NA
df[df==" "] <- NA                 # make empty space cells NA

myvar <- df[ , grepl(variablename, names( df ) ) ]   # creates a new variable with all the columns that start with the name you denoted in the "x" variable
maxvalue <- max(myvar, na.rm = TRUE)                 # Detects the max value in the subset to create the y axis in the graph
myvar$Group <- df[[GroupColumnName]]                 # creates a new column in the new variable containing the groups that are taken from the column of the original data frame
myvar$ID <- df[[IDname]]                             # creates a new column in the new variable containing the IDs that are taken from the column of the original data frame

splitlist <- list()
splitlist = split(myvar, f = myvar$Group)  # splits the data frame into datasets by group and places them into a list

unnamedlist <- lapply(splitlist, function(x)  {colnames(x) = gsub(variablename, '', colnames(x)); return(x)}) # remove the variable name in the list leaving only the time points

ungroupedlist <- lapply(unnamedlist, function(x) x[!(names(x) %in% c("Group"))]) # remove the "group" column from the lists, which is unused since the data frames are now already seperate elements in the list

mungedlist <- lapply(ungroupedlist, function(x) mung(x,mode)) # "mung" the data sets according the mode you selected
meansd <- lapply(mungedlist, function(x) x %>% summarise_all(funs(sd(., na.rm = TRUE)))) # calculated the standard deviation of the new munged data frames
names(meansd) <-  paste(names(meansd) ,sep = "_", "SD") # renames the standard deviation data frames
mungedlist <- append(mungedlist, meansd, 0) # puts the standard deviation data frames into the munged list with the original data frames


summarizedlist <- lapply(mungedlist, function(x) x %>% summarise_all(funs(mean(., na.rm = TRUE)))) # turnes the munged data frames to single-rows with only the mean of all the observations

noidlist <- lapply(summarizedlist, function(x) x[!(names(x) %in% c("ID"))]) # removes the "ID" columns

meltedlist <- lapply(noidlist, function(x) melt(x)) # melts the data for subsequent use in figure
