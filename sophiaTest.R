#Sophia Anderson
#Feburary 7, 2018
#Topic: Intro to Data Science

#Remember to set your working directory to the one the data is stored
# in before running the rest of the code
setwd("~/Desktop/2017-2018/Second Semester/Intro to Data Science/Labs/HW1-Exploratory-Data-Analysis/HW1_data")
##Run this line!


##Some resources
###https://www.statmethods.net/advgraphs/layout.html
#https://www.statmethods.net/graphs/bar.html
#https://www.datacamp.com/community/tutorials/make-histogram-basic-r
#http://homepage.divms.uiowa.edu/~luke/classes/STAT4580/histdens.html


datalist <- list() #For loop from Ariel T.; loads in all data from all days into one dataset
for (i in 1:31){
  datapoint <- read.csv(paste0("nyt",toString(i),".csv"))
  datalist[[i]]<-datapoint 
} 

finalset <- data.frame(matrix(ncol = 5, nrow = 77)) #the table
colnames(finalset) <- c("Age", "Clicks", "Impressions", "CpI", "Total") #col names
finalset$Age <- 14:90#all the relevant ages
finalset$Clicks <- finalset$Impressions <- finalset$Total <- 0 
#sets all other columns i need to do math in to 0 so the functions work
finalset[1,1] <- 0 # sets the first age to "0" to symbolize the unidentifies cohort

for (x in 1:31) #Goes through all the days of data we have 
{
  placeholder <- datalist[[x]] #snags one day worth of data
  placeholder <- placeholder[which(placeholder$Impressions > 0),] 
  #takes out all the people who didn't see ads in the first place
  
  for (y in 1:nrow(finalset)) #for each age we think is relevant
  {
    if (finalset[y,1] > 0) #and if the age we're working with isn't 0
    {
      test <- placeholder[which(placeholder$Age == y+13),] 
      #create a table of all data for that specific age group 
      #the plus 13 is becasue i'm working off the the row number 
      #and the lowest age I have is 15 (y begins at 2 because the 1 case is age "0")
      finalset[y,3] <- finalset[y,3] + sum(test$Impressions)
      finalset[y,2] <- finalset[y,2] + sum(test$Clicks)
      finalset[y,5] <- finalset[y,5] + nrow(test)
    }
    else #only triggers once to get all of the "unidentified" ages
    {
      test <- placeholder[which(placeholder$Age < 15),]
      test2 <- placeholder[which(placeholder$Age > 90),]

      finalset[y,3] <- finalset[y,3] + sum(test$Impressions) + sum(test2$Impressions)
      finalset[y,2] <- finalset[y,2] + sum(test$Clicks) + sum(test2$Clicks)
      finalset[y,5] <- finalset[y,5] + nrow(test) + nrow(test2)
    }
  }
}
sum(finalset$Total) # gives total number of data points collected


for(v in 1:nrow(finalset)) # doing final calculations for each row
{
  finalset[v,4] <- finalset[v,2]/finalset[v,3]
  #finalset[v,4] <- finalset[v,4]/finalset[v,5] uncomment this if you want CpI per capita
}


library(ggplot2)
ggplot(finalset, aes(y = CpI, x = Age)) +
  geom_bar(stat = "identity", color = "black", fill = "gray80", width = 0.8) 





