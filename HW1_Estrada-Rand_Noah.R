# 3
## a)
getwd()
setwd("C:/Users/noahe/Desktop/MGSC310")
## b)
id <-  seq(1:150)
id[2]
## c)
set.seed(90)
netflix <- rnorm(150,20,5)
## d)
set.seed(90)
hulu<-runif(150,0,15)
## e)
par(mfrow  = c(2,1))
hist(netflix, main = "Netflix Histogram", xlab = "Hours of Watching")
hist(hulu, main = "Hulu Histogram", xlab="Hours of Watching")
## f)
subscription <- c("Yes","No")
subscription <- factor(subscription)
is.factor(subscription)
## g)
set.seed(90)
amazon <- sample(subscription, size = 150,replace = TRUE)
amazon[0:20]
## h
amazon == "Yes"
totalSubscriptions = sum(amazon == "Yes")

## i
sum((amazon == "No") & (netflix >20))
## j
sum((hulu<12) & (netflix <12) & (amazon == "Yes"))
## k
id[amazon == "Yes"]
