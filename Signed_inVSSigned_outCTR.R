Signed_in <- data.frame(day = 0, CTR = 0)

for (month in c(1:31)){
  temp <- paste("HW1_data/nyt",month, ".csv", sep="")
  print(temp)
  nyt2 <- read.csv(temp)
  Signed_in <- charts(nyt2, month)
}

charts <- function(data1, month1) {
  CTRSigned_in <- subset(data1, Signed_In==1)$Clicks/subset(data1, Signed_In==1)$Impressions
  Signed_in <- rbind(data1.frame, c(month1, mean(CTRSigned_in, na.rm = TRUE)))
  
}

#signed in
#print(Signed_in)
Signed_in <- Signed_in[-c(32),]
#Signed_in <- Signed_in[-c(1),]
#sd(Signed_in$CTR)
#string2 <- paste("Signed in mean CTR:", mean(Signed_in$CTR))
#print(string2)
barplot(Signed_in$CTR, names.arg=days, xlab="Days", ylab="CTR", main="CTR Signed in", ylim=c(0,0.03))


#signed out
Signed_out <- data.frame(day = 0, CTR = 0)
days <- c(1:31)

for (day in c(1:31)){
  tempCSV <- paste("HW1_data/nyt",day, ".csv", sep="")
  print(tempCSV)
  nyt <- read.csv(tempCSV)
  Signed_out <- charts2(nyt, day)
}

charts2 <- function(temp, day1) {
  CTRSigned_out <- subset(temp, Signed_In==0)$Clicks/subset(temp, Signed_In==0)$Impressions
  Signed_out <- rbind(Signed_out, c(day1, mean(CTRSigned_out, na.rm = TRUE)))
  
}

Signed_out <- Signed_out[-c(1),]
#print(Signed_out)
#sd(Signed_out$CTR)
#string2 <- paste("Signed out mean CTR:", mean(Signed_out$CTR))
#print(string2)
barplot(Signed_out$CTR, names.arg=days, xlab="Days", ylab="CTR", main="CTR Signed out", ylim=c(0,0.03))

#signed in
summary(Signed_in$CTR)
#signed out
summary(Signed_out$CTR)

#density plot of both male and signed in CTR
plot(density(Signed_in$CTR), col="purple", main="Signed in and Male CTR", xlim=c(0.017,0.025), ylim=c(0,450), xlab="CTR")
lines(density(data2.frame$CTR), col="red")
legend("topleft", c("Signed in", "Male"), cex=0.7, fill=c("purple", "red"))

#signed in vs singed out
plot(density(Signed_in$CTR), col="brown", main="CTR Density Signed in", xlab="CTR")
sd(Signed_in$CTR)
plot(density(Signed_out$CTR), col="red", main="CTR Density Signed out", xlab="CTR")
sd(Signed_out$CTR)

#scatter plots of days vs signed in CTR and male CTR
plot(Signed_in$day, Signed_in$CTR, xlab="Day", ylab="CTR", main="Signed in CTR")
plot(data2.frame$day, data2.frame$CTR, xlab="Day", ylab="CTR", main="Male CTR")
