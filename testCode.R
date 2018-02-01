

#age <- c(nyt1[which(nyt1$Age > 14 & nyt1$Age < 95), 1])
#clickRate <- c((nyt1[which(nyt1$Age > 14 & nyt1$Age < 95), 3])/(nyt1[which(nyt1$Age > 14 & nyt1$Age < 95), 4]))
#hist(age)
#ageplot <- hist(age)
#clickRatePlot <- hist(clickRate)

plot( ageplot, col=rgb(0,1,1,1/4))  # first histogram
plot( clickRatePlot, col=rgb(1,0,0,1/4), add=T)  # second


# Which
student.salary  <- seq(10,200, by=2)
student.salary[which(student.salary > 100 & student.salary < 150)]

#for loop: for (i in 1:1000){} or for for(item in list){}
#There are also while loop

#functions

stats <- function(x) {
  m <- mean(x)
  std <- sd(x)
  se <- std/sqrt(length(x))
  return(c(m,std,se))
}

## Vector
v <- c(7,6,4,990)
v+1
v*100
length(v)
sqrt(v)
w <- c(4,7,4)
v*w # not supposed to happen. shorter vector items gets recycled. #be careful

i <-1:10
small.ints <- i<5
small.ints

which(small.ints)

#matricies 
#as.matrix will turn a thing into a  matrix
#matrix(v, nrow = 3, ncol = 4)
v <- 1:12
matrix(v, 3, 4)
m <- matrix(v, 3, 4, byrow = T)
dim(m) # gives you dimensions of matrix
length(m) # gives you number of elements

head(m, 2) #returns a certian number of rows from the top
tail(m, 1) # returns a certian number of rows from the bottom

#subsetting
m[2,4]
m[2,]
m[,3]
m[1:2, 3]

