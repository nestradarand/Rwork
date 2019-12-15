setwd("C:/Users/noahe/Desktop/MGSC310")
titanic <- read.csv("titanic.csv")

###1
titanic <- titanic[!is.na(titanic$Age),]

mod1 <- lm(Fare~factor(Pclass) + Age,data = titanic)

###2
#calc rmse
library(caret)
mod1_preds <- predict(mod1)
rmse_mod1 <- RMSE(mod1_preds,titanic$Fare)
####loocv
fare_loocv <- NULL
for(i in 1:nrow(titanic)){
  mod <- lm(Fare~factor(Pclass) + Age , data = titanic[-i,])
  fare_loocv[i] <- predict(mod, newdata = titanic[i,])
}
rmse_loocv <- RMSE(fare_loocv,titanic$Fare)

####kfold
nfolds <- 5
fare_folds <- createFolds(titanic$Fare,k = nfolds,
                          list = FALSE)
titanicPreds_df <- data.frame(titanic,)
for(i in 1:nfolds){
  
}