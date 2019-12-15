setwd("C:/Users/noahe/Desktop/MGSC310")
movies <- read.csv("movie_metadata.csv")
library(tidyverse)
library(margins)
library(forecast)


####1)
###b)
movies <- movies[!is.na(movies$budget),]
movies <- movies[!is.na(movies$gross),]
movies <- movies[(movies$content_rating != "" & movies$content_rating != "Not Rated"), ]
movies <- movies[movies$budget<4e+8,]
movies$grossM <- movies$gross/1e+6
movies$budgetM <- movies$budget/1e+6
movies$profitM <- movies$grossM-movies$budgetM
movies$rating_simple <- fct_lump(movies$content_rating, n = 4)
set.seed(2019)
train_indx <- sample(1:nrow(movies), 0.8 * nrow(movies), replace=FALSE)
movies_train <- movies[train_indx, ]
movies_test <- movies[-train_indx, ]

### c)
mod.gross.score <- lm(grossM ~ imdb_score + budgetM, data = movies_train)
summary(mod.gross.score)

###d)

###e)
mod.gross.score.budgetsquared <- lm(grossM~ imdb_score + budgetM + I(budgetM^2), data = movies_train)
summary(mod.gross.score.budgetsquared)

# f)
##looks at the marginal effects of given variables in model at certain values for budget
margins(mod.gross.score.budgetsquared, at = list(budgetM = c(25, 50, 75, 90,
                                                             100, 200,300)))

# g)
###this code tells cplot what lm to use and then what we want to look at (the effects)
##and "budgetM" informs the functiuon on what to plot for the marginal effects
cplot(mod.gross.score.budgetsquared,"budgetM", what = "effect",
      main = "The Marginal Effects of Budget Increases\nOn Net Gross",
      xlab = "Budget (millions)",ylab = "Marginal effect of Additional Million to Budget")



### 2)
######a )
mod3 <- lm(grossM ~ imdb_score + budgetM + I(budgetM^2) + 
             relevel(rating_simple,ref = "R"), data = movies_train)
summary(mod3)

##b)


## c)
trainingPredictions <- predict(mod3)
testPredictions <- predict(mod3,movies_test)

##d)

trainingResids <- movies_train$grossM - trainingPredictions
testResids <- movies_test$grossM - testPredictions

#e)

dummySet <- data.frame(trainingResids = trainingResids,
                       trainingPredictions = trainingPredictions,
                       movies_train)
ggplot(dummySet,aes(x = trainingPredictions,y = trainingResids)) + geom_point()+
   labs(title = "Residuals Against Predicted Values\nFor Train Data",
        y = "Training Resdiduals",x = "Training Predictions")
dummySet2 <- data.frame(testingResids = testResids,
                        testingPredictions = testPredictions,
                        movies_test)
ggplot(dummySet2,aes(x = testingPredictions,y = testingResids)) + geom_point()+
   labs(title = "Residuals Against Predicted Values\nFor Test Data",
        y = "Testing Resdiduals",x = "Testing Predictions")

#f)

ggplot(dummySet,aes(x = grossM,y = trainingPredictions)) + geom_point()+
   labs(title = "Training Predictions Against True Values\nFor Train Data",
        y = "Training Predictions",x = "Training True Values")
ggplot(dummySet2,aes(x = grossM,y = testingPredictions)) + geom_point()+
   labs(title = "Testing Predictions Against True Values\nFor Test Data",
        y = "Testing Predictions",x = "Testing True Values")


# g)
RMSE <- function(t, p) {
   sqrt(sum(((t - p)^2)) * (1/length(t)))
}
rmse.train <-RMSE(movies_train$grossM,trainingPredictions)
rmse.test <-RMSE(movies_test$grossM,testPredictions)
?accuracy
accuracy(trainingPredictions,movies_train$grossM)
accuracy(testPredictions,movies_test$grossM)
print(rmse.train)
print(rmse.test)
