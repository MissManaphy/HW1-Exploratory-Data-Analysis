#GETTING DATA FOR 31 DAYS FOR FEMALES
#data1.frame = female
data1.frame <- data.frame(day = 0, CTR = 0)

charts <- function(temp, day1) {
  CTRfemale <- subset(temp, Gender==0)$Clicks/subset(temp, Gender==0)$Impressions
  data1.frame <- rbind(data1.frame, c(day1, mean(CTRfemale, na.rm = TRUE)))
}

for (day in c(1:31)){
  tempCSV <- paste("HW1_data/nyt",day, ".csv", sep="")
  print(tempCSV)
  nyt <- read.csv(tempCSV)
  data1.frame <- charts(nyt, day)
}

data1.frame <- data1.frame[-c(33),]
data1.frame <- data1.frame[-c(1),]
barplot(data1.frame$CTR, names.arg=days, xlab="Days", ylab="CTR", main="CTR Female", ylim=c(0,0.025))


#GETTING DATA FOR 31 DAYS FOR MALE
#data2.frame = male
data2.frame <- data.frame(day = 0, CTR = 0)
days <- c(1:31)

charts2 <- function(temp, day1) {
  CTRmale <- subset(temp, Gender==1)$Clicks/subset(temp, Gender==1)$Impressions
  data2.frame <- rbind(data2.frame, c(day1, mean(CTRmale, na.rm = TRUE)))
}

for (day in c(1:31)){
  tempCSV <- paste("HW1_data/nyt",day, ".csv", sep="")
  print(tempCSV)
  nyt <- read.csv(tempCSV)
  data2.frame <- charts2(nyt, day)
}


#ALL THE PLOTS ARE BELOW
data2.frame <- data2.frame[-c(1),]
barplot(data2.frame$CTR, names.arg=days, xlab="Days", ylab="CTR", main="CTR Male", ylim=c(0,0.025))

#female summary
summary(data1.frame$CTR)
#male summary
summary(data2.frame$CTR)

#density plot of both male and female CTR together
#plot(density(data1.frame$CTR), col="pink", main="CTR Density Male versus Female", xlim=c(0.013,0.025))
#lines(density(data2.frame$CTR), col="blue")
#legend("topright", c("Male", "Female"), cex=1, fill=c("blue", "pink"))

#male and female density plots individual
plot(density(data1.frame$CTR), col="pink", main="CTR Density Female", xlab="CTR")
sd(data1.frame$CTR)
plot(density(data2.frame$CTR), col="blue", main="CTR Density Male", xlab="CTR")
sd(data2.frame$CTR)
