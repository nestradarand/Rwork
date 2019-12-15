x <- 34
typeof(x)#tells what type of data it is

hello <-c(2,3,4,5)
 

####drawing random numbers####
#uniform distributions

y <- runif(30, 20, 21)
y
#normal distribution
x <- rnorm(100,10,2)  #(numofObs, mean, sd)
x
mean(x)
sd(x)

set.seed(310)#lets us determine starting point from which to start random number generation
p <- rnorm(100,10,2)
mean(p)
sd(p)



###histogram
hist(p, breaks = 30)

###character string of cities
city <- "Chicago"
print(city)

is.character(city)
is.numeric(city)


cities<-c("New York","Dallas","Hoboken")
print(cities)
cities[3]


####sample functionality####
sampleOfCities <-sample(cities, 100, replace = TRUE)
sampleOfCities


#categorical
gender <-c("Male","Female")
##convert to categorical
genders <-as.factor(gender)
####to find levels of factor
levels(genders)


###boolean - logical ####
passed <- TRUE
passed
###logical comparison
x <- 2
x >0
x <0
###logical operators
2 >0 || 3< 0

###conditional indexing and umbrella logic ###
day <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
rain <-c("Yes","Yes","Yes","Yes","Yes","No","Yes")
snow <-c("No","No","No","Yes","No","No","No")
rain == "Yes"
snow == "Yes"
rain == "Yes" & snow == "Yes"


day[rain == "Yes"]
day[rain =="Yes" |snow == "Yes"]

####vectors#####
x <- 1:4
y <- 10:15
z <- c(x,y)
z
length(z)
rm(x)

###data frams###
x.data <- data.frame( id = 1:5,
                      color = c ("Blue","Yello","Green","red","Yellow"),
                      year = 2000:2004)
x.data


str(x.data)
setwd("C:/Users/noahe/Desktop/MGSC310")
list.files(".")  ####same as ls



####loading data####
diamonds <-read.csv("Diamonds.csv")
str(diamonds)
diamonds
diamondsFrame <-data.frame(diamonds)
summary(diamondsFrame)


dim(diamonds)
dim(diamonds[1])
diamonds[c(5,6),3]
diamonds[5:6,3]
##second row
diamonds[1:2]
names(diamonds)
d.cut <-diamonds$cut
sum(d.cut == "Ideal")
length(d.cut)


diamonds[diamonds$cut == "Very Good",]


head(diamonds)
tail(diamonds)
diamonds[3,4] <- NA
is.na(diamonds)
sum(is.na(diamonds))
sum(complete.cases(diamonds))
sum(!complete.cases(diamonds))

list.files(".")
university <- read.csv("University.csv")
head(university)
tail(university)


dim(university)
table(university$gender)
univ.table <-table(university$gender,university$admitted)
gender.table <-table(university$gender)
gender.table/length(university$gender)
prop.table(gender.table)

gender.Admitted.Table <-table(university$gender,university$admitted)
prop.table(gender.Admitted.Table, margin = 1)

library(doBy)
summaryBy(admitted~gender, data = university)
summaryBy(GPA~department + admitted +gender, data= university)
library(ggplot2)
plot(university$GPA,university$GRE)
plot(university$department,university$GPA)

pdf("University_pairs.pdf")
pairs(university)
dev.off()

ggplot(university, aes(GRE,GPA)) + geom_point(aes(color = department)) + geom_smooth()


ggplot(university, aes(GRE,GPA,color = admitted)) + geom_point() + 
  facet_wrap(~department) + theme_minimal() + labs(title = "My First GGPLOT", subtitle = "By Noah")


library(gpairs)
##doesntwork

library(ggplot2)
ggplot(diamonds, aes(carat,price)) + geom_point() +  facet_wrap(~color)
