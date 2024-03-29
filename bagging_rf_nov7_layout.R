#######################################
#                                     #
#      Bagging and Random Forest      #
#                                     #
#              11/07/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#  - Bagging algorithm
#  - Estimating random forest models
#  - mtry and ntree parameters
#  - Variable "importance" 
#  - explain_forest() to explain random forests
#  - Variable depth plots
#  - depth versus importance
#  - Interaction plots
#  - Variable importance plots
#  - Gini impurity
#  if time permits:
#  - How to estimate a bagged model manually (optional)
#  - Tuning parameters of mtry and ntree in random forests
#  - Cross validating to select these tuning parameters
#######################################

#------------------------------------------------
### Bagging
#------------------------------------------------
# using Boston data
library(ggplot2)
library(MASS)
data(Boston)

#install.packages("randomForest")
library(randomForest)
####need to set the seed for randomization in random forest
set.seed(2019)
train_index <- sample(1:nrow(Boston),.6*nrow(Boston),replace = FALSE)
boston_train <- Boston[train_index,]
boston_test <- Boston[-train_index,]

# using random forest function with mtry = p
###m = p gives same as bagging
#mtry is p-1 since one of them is the outcome in this case
bos_bag_mod <- randomForest(medv~.,
                           data = boston_train,
                           mtry = 13,
                           ntree = 500,
                           importance = TRUE)
# performance of the model
bos_bag_mod
preds_bag_bos <- predict(bos_bag_mod,newdata = boston_test)
plot(preds_bag_bos,boston_test$medv)
abline(0,1,col = "red")

MSE <- function( pred,t) {
  mean((t-pred)^2)
}
MSE(preds_bag_bos,boston_test$medv)
#------------------------------------------------
### Random Forest
#------------------------------------------------
# random forest with mtry = 6
set.seed(2019)
randoForest_bos <- randomForest(medv~.,
                                data = boston_train,
                                ntree = 500,
                                mtry = 6,
                                importance = TRUE)
# performance of the model
randoForest_bos
randofores_preds <- predict(randoForest_bos,newdata = boston_test)
plot(randofores_preds,boston_test$medv)
abline(0,1,col = "red")
MSE(randofores_preds,boston_test$medv)
#------------------------------------------------
### Variable Importance
#------------------------------------------------
# importance
importance(randoForest_bos)
# importance plot
varImpPlot(randoForest_bos)
explain_forest(randoForest_bos)


# Note: We will explore randomForestExplainer later
#       That gives more information and insights

#------------------------------------------------
### Classification
#------------------------------------------------
library(randomForest)
# using titanic data set
setwd("C:\\Users\\noahe\\Desktop\\MGSC310")
# removing missing
titanic <- read.csv("titanic.csv")

titanic <- titanic[complete.cases(titanic),]
# outcomeL Survived
titanic$Survived <- ifelse(titanic$Survived == 1,"Survived","Dead")
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
# splitting data into training and test
train_index <- sample(1:nrow(titanic),.7*nrow(titanic),replace =FALSE)
titanic_train <- titanic[train_index,]
titanic_test <- titanic[-train_index,]
# bagging

set.seed(2019)
dim(titanic)
###classification used for categorical variables
bag_titanic <- randomForest(Survived~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked,
                            data = titanic_train,
                            type = classification,
                            mtry = 7,
                            ntree = 500,
                            importance = TRUE)

# results
bag_titanic
# random forest with mtry=3
set.seed(2019)
rf_titanic <- randomForest(Survived~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked,
                            data = titanic_train,
                            type = classification,
                            mtry = 3,
                            ntree = 500,
                            importance = TRUE)

# results

rf_titanic
#------------------------------------------------
### Random Forest Explanations
#------------------------------------------------
# imprtance

importance(rf_titanic)
varImpPlot(rf_titanic)
#install.packages("randomForestExplainer")
library(randomForestExplainer)
# plot min



# explanation file 
explain_forest(rf_titanic,interaction = TRUE,data = titanic_train)
#------------------------------------------------
### Bagging Manually
#------------------------------------------------
# bagging - bootstrapp aggregation
library(tree)
####
#single tree
tree_mod <- tree(medv~.,
                 data = boston_train)
tree_mod
plot(tree_mod)
text(tree_mod,pretty = 0)

preds_test_tree <- predict(tree_mod,newdata = boston_test)
MSE(preds_test_tree,boston_test$medv)

## examine some of the individual models


###multiple trees using boostrap
boston_train_preds <- data.frame(row.names = rownames(boston_train))
B <- 100 #num of bootstrap samples
num_b <- 250 #sample size for each boostrap

boot_mods <- list()#empty list

###boostrap for loop
for(i in 1:B){
  boot_index <- sample(1:nrow(boston_train),num_b)###dont have to use replace == true for assignments
  #fit tree to sample
  boot_tree <- tree(medv~.,
                    data = boston_train[boot_index,])
  ###store bootstrap model for later usage
  boot_mods[[i]] <- boot_tree ###use double index for list
  ###generate predictions to observe performance for each boostrap model
  #make empty data frame
  preds_boot <-  data.frame(row.names = rownames(boston_train))
  ###store the predictions at the correct observations that were included in the bootstrap
  preds_boot[boot_index,"preds_boot"] <- predict(boot_tree)
  ###labels which boostrap is responsible for which predictions
  names(preds_boot)[1] <- paste("preds_boot",i,sep = "")
  ###merge predictions to data frame that includes all boostraps
  ##just appends the new data frame to the overall data frame with the new column name
  boston_train_preds <- data.frame(boston_train_preds,preds_boot)
  
}
head(boston_train_preds)
####plot each boostrap tree

###plots the first tree
plot(boot_mods[[1]])
text(boot_mods[[1]],pretty = 0)

####tenth tree
plot(boot_mods[[10]])
text(boot_mods[[10]],pretty = 0)


###to aggregate the results
#take average for each row while avoiding NA values
preds_boot <- rowMeans(boston_train_preds,na.rm = TRUE)
MSE(preds_boot,boston_train$medv)

#------------------------------------------------
### Random Forests Tuning
#------------------------------------------------

rf_mods <- list()
oob_err <- NULL##to store out of bag error
test_err <- NULL
for(mtry in 1:13){
  rf_fit <- randomForest(medv~.,
                         data = boston_train,
                         mtry = mtry,
                         ntree = 500,)
  oob_err[mtry] <- rf_fit$mse[500]
}
results_df <- data.frame(mtry = 1:13,
                         oob_err)
ggplot(results_df,aes(x = mtry,y = oob_err)) + geom_point()





#####Boosting 
library(gbm)
set.seed(2019)
boost_mod <- gbm(medv~.,
                 data= boston_train,
                 distribution = "gaussian", ###use bernoulli for classification
                 n.trees = 5000,
                 interaction.depth = 4)
#summary of results
summary(boost_mod)
###partial dependence plot

###interpret "y" as the marginal effect on the outcome
plot(boost_mod,i = "rm")
plot(boost_mod,i = "lstat")

preds_test_boost <- predict(boost_mod,newdata = boston_test,
                            n.trees = 5000)
###calc mse
mean((preds_test_boost- boston_test$medv)^2)
MSE(preds_test_boost,boston_test$medv)

set.seed(2019)

###shrinkage used to add penalty to training to reduce amount of info to get at each step
boost_mod2 <- gbm(medv~.,
                 data= boston_train,
                 distribution = "gaussian", ###use bernoulli for classification
                 n.trees = 5000,
                 shrinkage = .2,
                 interaction.depth = 4)
preds_test_boost2 <- predict(boost_mod2,newdata = boston_test,
                             n.trees = 5000)
MSE(preds_test_boost2,boston_test$medv)

