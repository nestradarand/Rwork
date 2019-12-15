#author Noah Estrada-Rand
#course: MGSC 310


######8#####
### a ###
setwd("C:/Users/noahe/Desktop/MGSC310")
college <-read.csv("College.csv")
dim(college)

### b ###
View(college)
rownames(college) = college[,1]
View(college)
college <- college[,-1]
View(college)
### c###
  ##i
summary(college)
  ##ii
pairs(college[,1:10])
  ##iii
aggregate(college$Outstate~college$Private,FUN = mean)
plot(college$Outstate~college$Private,
     main = "Boxplots of Out of State Cost\nFor Public and Private Colleges",
     ylab = "Out of State Tuition($)",xlab = "Whether or Not School is Private")
  ###iv
Elite <- rep("No",nrow(college))
Elite[college$Top10perc >50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college,Elite)
summary(college)
plot(college$Outstate~college$Elite, main = "Boxplot of Schools With 50%+ Students\nFrom Top 10% of High School Class",
     xlab = "Whether or not >50% of students are from top 10%",
     ylab = "Cost of Out of State Tuition ($)")
  ###v
par(mfrow = c(2,2))
hist(college$Room.Board, main = "Histogram for Room and Board",
     xlab = "Cost",ylab = "Frequency",breaks = 15)
hist(college$Top10perc,main = "Histogram for Percentage of Students\nFrom top 10%",
     xlab = "Percentages",ylab = "Frequency", breaks = 10)
hist(college$perc.alumni, main = "Histogram of % of Alumni Who Donate",
     xlab = "Percentage",ylab = "Frequency", breaks = 5)
hist(college$Grad.Rate,main = "Histogram of Graduation Rates",
     xlab = "Percentages",ylab = "Frequency",breaks = 12)
  ##vi  ####
##intitially used basic plotting functions but later decided to use ggplot for some instead

boxplot(college$Room.Board~college$Private, xlab = "Whether School is Private or Not",
        ylab = "Cost of Room and Board($)",
        main = "Boxplots for Private and Public Schools \nand The distribution of Boarding Cost")
plot(college$Top10perc,college$Expend, type = "p",
     ylab = "Instructional Expenditure per Student",
     xlab = "Percentage of Students taken from\nTop 10% of High School Class",
     main = "Scatterplot for Expenditure per Student\nvs\nProportion of Students from Top 10%")
boxplot(college$perc.alumni~college$Private, xlab = "Whether College is Private or Not",
        ylab = "Percentage of Alumni Who Donate",
        main = "Boxplots of % of Alumni Who Donate\nvs\nWhether a School is Private Or Not")

ggplot(college,aes(Top10perc,Expend)) + geom_point(aes(Top10perc,Expend,color = Private)) +
  geom_smooth() + labs(title = "Expenditure Per Student vs \nProportion of 10% of High School Class")+
  xlab("Proportion of Students Who Were Top 10%") + ylab("Instructional Expenditure Per Student")

ggplot(college,aes(Private,perc.alumni)) + geom_boxplot()

reduced.Data <- data.frame(college[,2:18])
dim(reduced.Data)
correlations <- cor(reduced.Data)



######10#####

###a)
library(MASS)
?Boston
bos <- Boston
nrow(bos)
ncol(bos)
View(bos)
####the rows represent different towns in Boston
###b)
library(ggplot2)
ggplot(bos,aes(medv,rm)) + geom_point(aes(medv,rm,color = crim)) + facet_wrap(~chas)
ggplot(bos,aes(medv,rm)) + geom_point(aes(medv,rm)) + geom_smooth() + 
  labs(title = "Number of Rooms on Average\nvs\nMedian Owner Occupied Home Value") + 
  xlab("Median Owner Occupied Home Value") + ylab("Average Number of Rooms")

str(bos)
plot(bos$medv,bos$crim,type = "p",xlab= "Median Value of Owned Homes",
     ylab = "Crime Rates(per capita)",
     main = "Scatterplot for Median Value of Owned Homes\nvs\n Crime Rates")
plot(bos$medv,bos$rm, type = "p",
     xlab = "Median Home Values", ylab = "Average Number of Rooms",
     main = "Scatterplot of Median Value of Owned Homes and \nAverage Rooms Per Dwelling")
plot(bos$rad,bos$tax, xlab = "Index of Accessibility to radial Highways",
     ylab = "Full Value property-tax rater per $10,000(%)",
     main = "Scatterplot for Accesibility to Radial Highways\nvs\n Property Tax Rates")
ggplot(bos,aes(rad,tax)) + geom_point(aes(rad,tax)) + xlab("Index of Accesibility to Radial Highways") +
  ylab("Full Value property-tax rater per $10,000(%)") + 
  labs(title = "Scatterplot for Accesibility to Radial Highways\nvs\n Property Tax Rates")
ggplot(bos,aes(medv,crim)) + geom_point(aes(medv,crim)) + xlab("Median Value of Owned Homes")+
  ylab("Crime Rates(per capita)") + geom_smooth()+
  labs(title = "Scatterplot for Median Value of Owned Homes\nvs\n Crime Rates")
###c)
cor(bos$crim,bos)

###d)
boxplot.stats(bos$crim)
boxplot.stats(bos$tax)
boxplot.stats(bos$ptratio)

###e)
sum(bos$chas ==1)
###f)
median(bos$ptratio)
###g)
min(bos$medv)
bos[bos$medv == min(bos$medv),]
range(bos$crim)
range(bos$zn)
range(bos$indus)
range(bos$rm)
range(bos$age)
range(bos$dis)
range(bos$rad)
range(bos$tax)
range(bos$ptratio)
range(bos$black)
range(bos$lstat)
###h)
length(bos[bos$rm >7,])
