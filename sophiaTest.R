#Sophia Test Code
#Code taken from book and edited slightly

#Remember to set your working directory to the one the data is stored
# in before running the rest of the code

#Reading in the data
setwd("~/Desktop/2017-2018/Second Semester/Intro to Data Science/Labs/HW1-Exploratory-Data-Analysis/HW1_data")
data <- read.csv('nyt1.csv')

#catagorize
head(data)
data$agecat <- cut(data$Age, c(0,15,20,30,40,50,60,70,80,90))

#view
summary(data)

#brackets
install.packages("doBy")
library("doBy")
siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~agecat, data=data, FUN=siterange) #FUN = functions to be applied

#so only signed users have ages and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~agecat, data = data)

#plot
install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x=Clicks/Impressions, fill=agecat)) + geom_histogram(binwidth=1)
#sort of a heat map identifying the number of impressions, age group, and then the 
#number of people in that age group with that many impressions
ggplot(data, aes(x=agecat, y=Impressions, fill=agecat)) + geom_boxplot()

#create click thru rate
#we don't care about the clicks if there are no impressions
#if there are clicks with no impressions, my assumptions about this data are wrong
data$hasimps <- cut(data$Impressions, c(-Inf, 0, Inf))
summaryBy(Clicks~hasimps, data = data, FUN = siterange) 
ggplot(subset(data, Impressions>0), aes(x=Clicks/Impressions, colour = agecat)) + geom_density()
ggplot(subset(data, Clicks>0), aes(x=Clicks/Impressions, colour = agecat)) + geom_density()
ggplot(subset(data, Clicks>0), aes(x=agecat, y=Clicks, fill = agecat)) + geom_density()
ggplot(subset(data, Clicks>0), aes(x=Clicks, colour = agecat)) + geom_density()

#create catagories
data$scode[data$Impressions==0] <- "NoImps"
data$scode[data$Impressions>0] <- "Imps"
data$scode[data$Clicks>0] <- "Clicks"

#Convert the column to a factor
data$scode <- factor(data$scode)
head(data)

#look at levels
clen <- function(x){c(length(x))}
etable<- summaryBy(Impressions~scode+Gender+agecat, data=data, FUN=clen)