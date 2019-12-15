#######################################
#                                     #
# Analyzing Credit Debt Defaulting    #
#                                     #
#              10/10/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - for loops in R
#   - LOOCV
#   - K-fold hold-out
#   - Bootstrap
#######################################

#--------------------------------------
# 1. for loops
#--------------------------------------

# for loops in R
# syntax:
# for( val in sequence){
#     statement
#   }
# print 1 to 10 using for
for(i in 1:10){
  print(i)
}
# print powers of 2 from 1 to 15
for(i in 1:15){
  print(2^i)
}

#--------------------------------------
# 2. load data
#--------------------------------------
# use Auto data set from ISLR package
library(ISLR)
data(Auto)
# removing the column: name (everything else would be numeric)
Auto_subset <- Auto[,-9]

#--------------------------------------
# 3. LOOCV
#--------------------------------------
# for loop of model
preds_LOOCV <- NULL
for(i in 1:nrow(Auto_subset)){
  mod <- lm(mpg ~ ., data = Auto_subset[-i,])
  preds_LOOCV[i] <- predict(mod,newdata = Auto_subset[i,])
}
head(preds_LOOCV)
# note that . can be used to represent all the variables in the regression model

# train the insample model to compare the errors
mod_insample <- lm(mpg~.,data = Auto_subset)

# store the hold-out predictions and in_sample predictions

###storing predicted for the in sample model, the leave one out models and the true values
preds_df <- data.frame(preds_insample = predict(mod_insample),
                       preds_LOOCV = preds_LOOCV,
                       true = Auto_subset$mpg)


# compute RMSE LOOCV 
# use caret package
library(caret)
###rmse for loocv
RMSE(preds_df$preds_LOOCV,preds_df$true)
#rmse for insample
RMSE(preds_df$preds_insample,preds_df$true)
####can also use R2 in caret to find r squared metric

#--------------------------------------
# 4. k-fold Cross validation
#--------------------------------------

# creating folds (using createFolds from caret package)
#mpg is variable of interest
Auto_subset$folds <- createFolds(Auto_subset$mpg,
                                 k=10,
                                 list = FALSE)


### K-Fold Cross Validation
nfolds <- 10
# for loop for K-Fold CV
preds10FoldCV_df <- data.frame(folds = Auto_subset$folds,
                               preds10FoldCV = rep(NA,nrow(Auto_subset)))


head(preds10FoldCV_df)

for(i in 1:nfolds){
  mod <- lm( mpg ~ . ,
             data = Auto_subset[Auto_subset$folds != i,])
  preds <- predict(mod,
                   newdata = Auto_subset[Auto_subset$folds == i,])
  ###only add to the column for the rows you want and for column two which 
  # are the predicted values
  preds10FoldCV_df[Auto_subset$folds == i,2] <- preds
}

preds_df <- data.frame(
  preds10FoldCV = preds10FoldCV_df$preds10FoldCV,
  preds_df
)

RMSE(preds_df$preds10FoldCV,preds_df$true)
RMSE(preds_df$preds_LOOCV,preds_df$true)
RMSE(preds_df$preds_insample,preds_df$true)

#--------------------------------------
# 5. Boostrapping
#--------------------------------------

# create 100 bootstrap samples with size of 200
B <- 100 #number of bootstrap samples
n_boot <- 200 # size of bootstrap samples

coef_boot <- NULL

for(b in 1:B){
  indx <- sample(1:nrow(Auto),size = n_boot,replace = TRUE)
  mod <- lm(mpg~displacement, data = Auto[indx,])
  coef_boot[b] <- mod$coefficients[2]
}
coef_boot
##gives us the average coefficient based on the boostrap samples
mean(coef_boot)

# linear model to compare with bootstrap
mod1 <- lm(mpg~displacement,data = Auto)
summary(mod1)
# SE of bootstrap estimates
mean(coef_boot)
sd(coef_boot)
# bootstrap coefficients (100 of them)

# histogram of coefficeints
library(ggplot2)
coef_df <- data.frame(coef_boot= coef_boot)
ggplot(coef_df,aes(x = coef_boot)) + geom_histogram() + 
  geom_vline(xintercept = mod1$coefficients[2],color = "red") ###adds line to visually see where the coefficient is
