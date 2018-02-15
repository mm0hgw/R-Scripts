rm(list = ls())                          # clear your workspace
MyData <- read.csv("ExampleData.txt", stringsAsFactors=FALSE)
variablename <- "SquatH"                 # variable name, change the value in the quotation marks, case sensitive, write the name of the variable up until the time point
mungmode <- 'max'                        # Set the mode of choosing prefered values among the different trials in the same time points, 'mean', 'max or 'min' values
GroupColumnName <- "Group"               # enter the name of the column that the Group variable is located
IDname<- "ID"                            # enter the name of the column that the ID variable is located
unwant <- c("Z")                         # Declare unwanted time points so they are excluded
baseline <- c("A")                       # Declare the baseline so that percentages can be calculated
usePercentages <- T                      # declare if you want the values on the graph to be as percentages of baseline

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

mungedlist <- lapply(ungroupedlist, function(x) mung(x,mungmode)) # "mung" the data sets according the mungmode you selected
noidlist <- lapply(mungedlist, function(x) x[!(names(x) %in% c("ID"))]) # removes the "ID" columns

if(usePercentages==T){
noidlist <- lapply(noidlist, function(x) {
	out<-do.call(rbind,lapply(seq(nrow(x)),function(i){x[i,]/x[i,2]}))
	colnames(out)<-colnames(x)
	out
})}
                   
transposedlist <- lapply(noidlist, function(x) t(x)) # transposes the list elements for subsequent SD calculation
sdlist <- lapply(transposedlist, apply, 1, sd, na.rm = T) # calculates the SD of the data

summarizedlist <- lapply(noidlist, function(x) x %>% summarise_all(funs(mean(., na.rm = TRUE)))) # turnes the munged data frames to single-rows with only the mean of all the observations
transposedlist2 <- lapply(summarizedlist, function(x) t(x)) ### transpose the list again
bindedlist <- Map(cbind, transposedlist2, sdlist)  # binding the sd column in the data frames
bindedlist<-  Map(cbind, bindedlist, names(bindedlist)) # adding the names
colnam <- c("Mean","SD","Group")
bindedlist<- lapply(bindedlist, function(x) {colnames(x) = colnam; x}) # changing the column names 
combinedlist <- bindedlist

### Changing the format of the columns so they work with ggplot and anova
combinedlist <- do.call("rbind", combinedlist)
combinedlist <- as.data.frame(combinedlist)
combinedlist$Time <- rownames(combinedlist)
combinedlist$SD <- as.character(combinedlist$SD)
combinedlist$Mean <- as.character(combinedlist$Mean)
combinedlist$SD <- as.numeric(combinedlist$SD)
combinedlist$Mean <- as.numeric(combinedlist$Mean)
combinedlist$Time <- as.character(combinedlist$Time)
combinedlist$Time <- factor(combinedlist$Time, levels=unique(combinedlist$Time))
#####

myPlot <- ggplot(combinedlist, aes(x = Time, y = Mean, color = Group, group = Group)) + 
  geom_line() +
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1) 
scale_y_continuous(expand = c(0, 0), limits = c(0, maxvalue+maxvalue*0.1))

myPlot #+ theme_apa

#### Anova not currently working finished

tempmelt <- melt(mungedlist, id.vars= "ID", variable.name = "Time", value.name = variablename)

model <- ezANOVA(tempmelt, dv = SquatH , ID, within = Time , detailed = TRUE, return_aov = TRUE)

