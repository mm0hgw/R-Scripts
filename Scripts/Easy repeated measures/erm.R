library(reshape2) #required for melt
library(ggplot2) #required for the graph
library(dplyr) #required for data transformation
library(jtools) #required to the APA figure theme

# Data format should be: 
# Each participant should be a different row.
# Each variable, each time point and each trial should be in different columns.
# If you have multiple time points and multiple measurements in each time point it is recommended that you use coding in the column name, in such a way that each column name is unique.
# For example if you are testing "Jump height" three times every day for 5 days, you can indicate the time point (day) as a letter of the alphabet and the trial number next to it.
# Therefore the column names would be: JumpHeightA1, JumpHeightA2, JumpHeightA3, JumpHeightB1, JumpHeightB2 ....

mydata <- read.csv("~/Data.csv", stringsAsFactors=FALSE) #import your csv
df <- data.frame(mydata)                   #rename the data frame
df[] <- lapply(df, unlist)                 #make the data frame not a list
df[df==""]  <- NA                          #make empty cells NA
df[df==" "]  <- NA                         #make empty space cells NA

dfname <- "SquatH"             #variable name, change the value in the quotation marks, case sensitive, write the name of the variable up until the time point
group_number <- 2            #number of groups/conditions the data has
group1name <- "H"            #name of the first group
group2name <- "L"            #name of the second group
# timepoints <- 11           #the number of time points // deprecated, use length(tempgroup1) 

myvar <- df[ , grepl(dfname, names( df ) ) ]   #creates a new variable with all the columns that start with the name you denoted in the "x" variable
names <- names(myvar)                          #saves the column names of the newly created variable
maxvalue <- max(myvar, na.rm = TRUE)
myvar$Group <- df$GroupOne                     #creates a new column in the new variable containing the groups that are taken from the column of the original data frame

#assign(paste("df_",x,sep=""), myvar)          #names the new variable according to the name you gave in the "x" variable
#assign(paste("names_",x,sep=""), names)       #names the "names" variable according to the name you gave in the "x" variable

names_points <- gsub(dfname,'',names)          #removes variable name from the names, leaving only the time point indication
# names_points  <- names_points [-length(names_points)]  #removes the last element of the new variable which is the Group (NEEDS REWORK)

#assign(paste(x,group1name, sep=""), subset.data.frame(assign(paste("df_",x,sep=""), myvar), Group == "H")) #creates a new data frame with only the observations in the first group
#tempgroup1 <- assign(paste(x,group1name, sep=""), subset.data.frame(assign(paste("df_",x,sep=""), myvar), Group == "H")) #assigns the newly created group to a temp variable
#assign(paste(x,group2name, sep=""), subset.data.frame(assigsn(paste("df_",x,sep=""), myvar), Group == "L")) #creates a new data frame with only the observations in the second group
#tempgroup2 <- assign(paste(x,group2name, sep=""), subset.data.frame(assign(paste("df_",x,sep=""), myvar), Group == "L")) #assigns the newly created group to a temp variable

tempgroup1 <- subset.data.frame(myvar, Group == group1name) #assigns the newly created group to a temp variable
tempgroup2 <- subset.data.frame(myvar, Group == group2name) #assigns the newly created group to a temp variable

tempgroup1 <- subset(tempgroup1, select = -c(Group)) #removes the "Group" column from the data frame with the means of the first group
tempgroup2 <- subset(tempgroup2, select = -c(Group)) #removes the "Group" column from the data frame with the means of the second group
myvarnogroup <- subset(myvar, select = -c(Group))   #created a variable with combined groups but without the group column just to experiment

colnames(tempgroup1) <- names_points
colnames(tempgroup2) <- names_points

mung <- function(x,TESTFUN=max){  # This function can calculate the max, min and mean of the columns that have the same starting character,
  # credits for the creation go to mm0hgw  https://github.com/mm0hgw
  
  #strip numbers from colnames
  cn <- gsub('[\\.0-9]','',colnames(x))
  
  key <- cn
  out<-do.call(cbind,lapply(unique(key),function(y){
    key2 <- y==key
    sapply(seq(nrow(x)),function(z){
      TESTFUN(as.numeric(x[z,key2]))
    })
  }
  )   )
  colnames(out)<-unique(key)
  out
}

#Use the function to calculate the mean, max and min of the different trials in each time point and create a new matrix for each
temp1mean <- mung(tempgroup1,mean)
temp2mean <- mung(tempgroup2,mean)
temp1min  <- mung(tempgroup1,min)
temp2min  <- mung(tempgroup1,min)
temp1max  <- mung(tempgroup1)
temp2max  <- mung(tempgroup2)
joinedprocessed <- mung(myvarnogroup, max)  #this calculates a combined matrix to analyze later in an anova

# Turn all the newly created matrices to data frames
temp1mean <- as.data.frame(temp1mean)
temp2mean <- as.data.frame(temp2mean)
temp1min  <- as.data.frame(temp1min)
temp2min  <- as.data.frame(temp2min)
temp1max  <- as.data.frame(temp1max)
temp2max  <- as.data.frame(temp2max)
joinedprocessed <- as.data.frame(joinedprocessed)
joinedprocessed$Group <- myvar$Group    #add the groups to the combined data frame

#in the function bellow, change temp1max and temp2max according to what you want to select (max, min or mean)
temp1sd <-  temp1max %>% summarise_all(funs(sd(., na.rm = TRUE))) #gets the standard deviation of all the columns for the first data frame containing the first group
temp2sd <-  temp2max %>% summarise_all(funs(sd(., na.rm = TRUE))) #gets the standard deviation of all the columns for the first data frame containing the second group
temp1processed <-  temp1max %>% summarise_all(funs(mean(., na.rm = TRUE))) #gets the means of all the columns for the first data frame containing the first group
temp2processed <-  temp2max %>% summarise_all(funs(mean(., na.rm = TRUE))) #gets the means of all the columns for the first data frame containing the second group

# assign(paste("mean",x,group1name, sep=""), temp1processed) #creates a new variable with the means of all the columns for the first data frame containing the first group
# assign(paste("mean",x,group2name, sep=""), temp2processed) #creates a new variable with the means of all the columns for the first data frame containing the second group
# assign(paste("sd",x,group1name, sep=""), temp1sd) #creates a new variable with the standard deviation of all the columns for the first data frame containing the first group
# assign(paste("sd",x,group2name, sep=""), temp2sd) #creates a new variable with the standard deviation of all the columns for the first data frame containing the second group

# temp1sd <- subset(temp1sd, select = -c(Group)) #removes the "Group" column from the data frame with the standard deviation of the first group
# temp2sd <- subset(temp2sd, select = -c(Group)) #removes the "Group" column from the data frame with the standard deviation of the second group

temp1melt <- melt(temp1processed) #makes the columns of the first data frame of means to rows for ggplot2
temp2melt <- melt(temp2processed) #makes the columns of the second data frame of means to rows for ggplot
temp1meltsd <- melt(temp1sd) #makes the columns of the first data frame of standard deviation to rows for ggplot2
temp2meltsd <- melt(temp2sd) #makes the columns of the second data frame of standard deviation to rows for ggplot2

temp1melt$sd <- temp1meltsd$value   #add the sd values to the data frame with the means for the first data frame
temp2melt$sd <- temp2meltsd$value   #add the sd values to the data frame with the means for the second data frame

#plot

plot <- ggplot(temp1melt, aes(x=variable, group = 1)) + 
  geom_line(aes(y=temp1melt$value), linetype = "dashed") + 
  geom_point(aes(y=temp1melt$value)) +
  geom_errorbar(aes(ymin=temp1melt$value-temp1melt$sd, ymax=temp1melt$value+temp1melt$sd), width=.1) +
  geom_line(aes(y=temp2melt$value)) +
  geom_point(aes(y=temp2melt$value)) +
  geom_errorbar(aes(ymin=temp2melt$value-temp2melt$sd, ymax=temp2melt$value+temp2melt$sd), width=.1) +
  xlab("Time") +
  ylab(dfname) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, maxvalue+maxvalue*0.1)) +
  scale_x_discrete(labels=temp1melt$variable) 

plot + theme_apa()


#### anova, currently does not work

boxplot(as.matrix(joinedprocessed) ~ as.matrix(joinedprocessed$Group))

anova1 <- aov(joinedprocessed ~ joinedprocessed$Group)

summary(anova1)

