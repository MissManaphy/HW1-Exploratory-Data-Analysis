# Dylan Critchfield

library(readxl)
nyt1 <- read_excel("C:/Users/dylan/Desktop/Classes/nyt1.xlsx")
View(nyt1)

#age_group
head(nyt1)
nyt1$age_group <- cut(nyt1$Age, c(-Inf,0,18,24,34,44,54,64,Inf))

#view
summary(nyt1)

#brackets?
install.packages("doBy")
library("doBy")
(nyt1= as.data.frame(nyt1))
func_list <- function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~age_group, data=nyt1, FUN=func_list) #FUN: a list of functions to be applied

#signed in users have age and genders
summaryBy(Gender+Signed_In+Impressions+Clicks~age_group, data= nyt1)


#plot
install.packages("ggplot2")
library(ggplot2)
ggplot(nyt1, aes(x=Impressions, fill=age_group)) + geom_histogram(binwidth = 1)
ggplot(nyt1, aes(x=age_group, y=Impressions, fill= age_group)) + geom_boxplot()

#create click thru rate
#ignore clicks with no impressions (can't click on add if no add shows up)
nyt1$has_Imps <- cut(nyt1$Impressions, c(-Inf,0,Inf))
summaryBy(Clicks~has_Imps, data=nyt1, FUN= func_list) #func_list()?

ggplot(subset(nyt1, Impressions>0), aes(x=Clicks/Impressions, color= age_group)) + geom_density()


ggplot(subset(nyt1, Clicks>0), aes(x=Clicks/Impressions, color=age_group)) + geom_density()

ggplot(subset(nyt1, Clicks>0), aes(x=age_group, y=Clicks, fill=age_group)) + geom_boxplot()

ggplot(subset(nyt1, Clicks>0), aes(x=Clicks, color=age_group)) + geom_density()



#create categories
nyt1$scode[nyt1$Impressions==0] <- "No_Imps"
nyt1$scode[nyt1$Impressions >0] <- "Imps"
nyt1$scode[nyt1$Clicks >0] <- "Clicks"

#convert the column to a factor
nyt1$scode <- factor(nyt1$scode)
head(nyt1)

#look at levels
clen <- function(x){c(length(x))}
etable <- summaryBy(Impressions~scode+Gender+age_group, data= nyt1, FUN=clen)


#Only Signed in NYT1 gender by age color.
ggplot(subset(nyt1, Signed_In>0), aes(x=Gender, color= age_group)) + geom_bar()
X <- nyt1$Gender

#Age Density for NYT1 with overlaying male female in blue/red respectively
set <- subset(nyt1, Signed_In>0)
m<- subset(set, Gender==0)
f<- subset(set, Gender==1)
md <- density(m$Age)
fd <- density(f$Age)
plot(fd, main="Age Density for NYT1")
lines(md)
polygon(fd, col="blue", border="black")
polygon(md, col="red", border="black")


