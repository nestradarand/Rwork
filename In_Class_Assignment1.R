#####In class assignment1 #####


library(ISLR)
data("Auto")
auto.data <- data.frame(Auto)
str(auto.data)
View(auto.data)
summary(auto.data)
dim(auto.data)
data.names <-names(auto.data)
hist(auto.data$horsepower, main = "Horsepower Histgram",
     xlab = "Horspower")
?prop.table

originByCylinders <- table(auto.data$origin,auto.data$cylinders)

cars <- sum(auto.data$mpg >20 & auto.data$horsepower >100)

year.Boxplot <- boxplot(auto.data$year, main = "Boxplot of Year" ,ylab = "Manufacture Year")

?plot
plot(auto.data$acceleration,auto.data$weigh, type = "p", 
     main = "Scatterplot of Acclearation and Weight", xlab = "acceleration",
     ylab = "Weight")
