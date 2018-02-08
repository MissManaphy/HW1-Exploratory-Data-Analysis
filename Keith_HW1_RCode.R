#Keith Carlson 
#February 6, 2018

#Import all the Data. Be sure you are in the right folder!
file_names <- dir()
all_data <- do.call(rbind, lapply(file_names,read.csv))

#Cleaning up the Data
all_data_signed_in <- subset(all_data, Signed_In>0)  #Only taking people who are signed in.
all_data_clean <- subset(all_data_signed_in, Age>10) #Only taking people older than 10.

#This is a interesting plot because of the large dips for certain ages.
agePlot <- ggplot(all_data_clean, aes(x=Age))+geom_bar()
agePlot + labs(title="Number of People of a Certain Age", y="People")

#Number of people in each age.
all_data_clean$agecat <- cut(all_data_clean$Age,c(-1,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,Inf))
summary(all_data_clean$agecat)

#Interesting! People aged 24 and 25 drop off significantly! Lets cut out that specific group.
all_data_clean$agecat_24 <- cut(all_data_clean$Age,c(23,24))
summary(all_data_clean$agecat_24)

#Looking at Impressions
overallImpressionsPlot <- ggplot(all_data_clean, aes(x=Impressions))+geom_bar() #this is the general graph with all the data.
overallImpressionsPlot + labs(title="Number of Impressions Across the Dataset", y="People")
age24_Impressions <- ggplot(subset(all_data_clean, Age==24), aes(x=Impressions))+geom_bar() #graph with just age = 24
age24_Impressions + labs(title="Number of Impressions for People of Age 24", y="People")

#Looking at Gender
overallGenderPlot <- ggplot(all_data_clean, aes(x=Gender))+geom_bar() #this is the general graph with all the data.
overallGenderPlot + labs(title="Number of People of a Certain Gender Across the Dataset", y="People")
age24_Gender <- ggplot(subset(all_data_clean, Age==24), aes(x=Gender))+geom_bar() #graph with just age = 24
age24_Gender + labs(title="Number of People Aged 24 of a Certain Gender", y="People")
