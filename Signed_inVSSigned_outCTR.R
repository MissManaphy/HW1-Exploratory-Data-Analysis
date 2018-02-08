#SIGNED IN
Signed_in <- data.frame(day = 0, CTR = 0)

charts <- function(data1, month1) {
  CTRSigned_in <- subset(data1, Signed_In==1)$Clicks/subset(data1, Signed_In==1)$Impressions
  Signed_in <- rbind(data1.frame, c(month1, mean(CTRSigned_in, na.rm = TRUE)))
}

for (month in c(1:31)){
  temp <- paste("HW1_data/nyt",month, ".csv", sep="")
  print(temp)
  nyt2 <- read.csv(temp)
  Signed_in <- charts(nyt2, month)
}

Signed_in <- Signed_in[-c(32),]
barplot(Signed_in$CTR, names.arg=days, xlab="Days", ylab="CTR", main="CTR Signed in", ylim=c(0,0.03))



#SIGNED OUT
Signed_out <- data.frame(day = 0, CTR = 0)
days <- c(1:31)

charts2 <- function(temp, day1) {
  CTRSigned_out <- subset(temp, Signed_In==0)$Clicks/subset(temp, Signed_In==0)$Impressions
  Signed_out <- rbind(Signed_out, c(day1, mean(CTRSigned_out, na.rm = TRUE)))
}

for (day in c(1:31)){
  tempCSV <- paste("HW1_data/nyt",day, ".csv", sep="")
  print(tempCSV)
  nyt <- read.csv(tempCSV)
  Signed_out <- charts2(nyt, day)
}

Signed_out <- Signed_out[-c(1),]
barplot(Signed_out$CTR, names.arg=days, xlab="Days", ylab="CTR", main="CTR Signed out", ylim=c(0,0.03))

#signed in
#summary(Signed_in$CTR)
#signed out
#summary(Signed_out$CTR)

#PLOTS ARE BELOW

#density plot of both male and signed in CTR
plot(density(Signed_in$CTR), col="purple", main="Signed in and Female CTR", xlim=c(0.017,0.025), ylim=c(0,450), xlab="CTR")
lines(density(data1.frame$CTR), col="red")
legend("topleft", c("Signed in", "Female"), cex=0.7, fill=c("purple", "red"))

#signed in and singed out density plots
plot(density(Signed_in$CTR), col="brown", main="CTR Density Signed in", xlab="CTR")
#sd(Signed_in$CTR)
plot(density(Signed_out$CTR), col="red", main="CTR Density Signed out", xlab="CTR")
#sd(Signed_out$CTR)

#scatter plots of days vs signed in CTR and female CTR
plot(Signed_in$day, Signed_in$CTR, xlab="Day", ylab="CTR", main="Signed in CTR")
plot(data2.frame$day, data1.frame$CTR, xlab="Day", ylab="CTR", main="Female CTR")
