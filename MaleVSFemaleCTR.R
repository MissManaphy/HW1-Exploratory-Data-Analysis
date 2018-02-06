data1.frame <- data.frame(day = 0, CTR = 0)

for (day in c(1:31)){
  tempCSV <- paste("HW1_data/nyt",day, ".csv", sep="")
  print(tempCSV)
  nyt <- read.csv(tempCSV)
  data1.frame <- charts(nyt, day)
}

charts <- function(temp, day1) {
  CTRfemale <- subset(temp, Gender==1)$Clicks/subset(temp, Gender==1)$Impressions
  data1.frame <- rbind(data1.frame, c(day1, mean(CTRfemale, na.rm = TRUE)))
  
}
#female
#print(data1.frame)
data1.frame <- data1.frame[-c(33),]
data1.frame <- data1.frame[-c(1),]
#sd(data1.frame$CTR)
#string1 <- paste("Female mean CTR:", mean(data1.frame$CTR))
#print(string1)
barplot(data1.frame$CTR, names.arg=days, xlab="Days", ylab="CTR", main="CTR Female", ylim=c(0,0.025))


#male
data2.frame <- data.frame(day = 0, CTR = 0)
days <- c(1:31)

for (day in c(1:31)){
  tempCSV <- paste("HW1_data/nyt",day, ".csv", sep="")
  print(tempCSV)
  nyt <- read.csv(tempCSV)
  data2.frame <- charts2(nyt, day)
}

charts2 <- function(temp, day1) {
  CTRmale <- subset(temp, Gender==0)$Clicks/subset(temp, Gender==0)$Impressions
  data2.frame <- rbind(data2.frame, c(day1, mean(CTRmale, na.rm = TRUE)))
  
}

#print(data2.frame)
data2.frame <- data2.frame[-c(1),]
#sd(data2.frame$CTR)
#string2 <- paste("Male mean CTR:", mean(data2.frame$CTR))
#print(string2)
barplot(data2.frame$CTR, names.arg=days, xlab="Days", ylab="CTR", main="CTR Male", ylim=c(0,0.025))

#female
summary(data1.frame$CTR)
#male
summary(data2.frame$CTR)

#density plot of both male and female CTR
plot(density(data1.frame$CTR), col="pink", main="CTR Density Male versus Female", xlim=c(0.013,0.025))
lines(density(data2.frame$CTR), col="blue")
legend("topright", c("Male", "Female"), cex=1, fill=c("blue", "pink"))

#male and female density plots individual
plot(density(data1.frame$CTR), col="pink", main="CTR Density Female", xlab="CTR")
sd(data1.frame$CTR)
plot(density(data2.frame$CTR), col="blue", main="CTR Density Male", xlab="CTR")
sd(data2.frame$CTR)
