#######################################
#                                     #
#           Decision Trees            #
#                                     #
#              11/05/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
# 1. Understanding from partitioning data to regression tree representation
# 2. Nodes, leafs, and split terminology
# 3. How a regression tree is constructed
# 4. How leaf values are constructed. 
# 5. How we predict using trees. 
# 6. Pruning trees
# 7. Estimating Regression trees in R
# 8. Cross validating tree complexity
# 9. Vizualizing regression trees
#######################################

#------------------------------------------------
### Regression Tree
#------------------------------------------------
#install.packages("tree")
library(tree)

# Load Hitters data from ISLR
library(ISLR)
data(Hitters)
# Do necessary data transformation:
#   - dropping missing
#   - log transformation of salary
Hitters <- Hitters[!is.na(Hitters$Salary),]

hist(Hitters$Salary)
Hitters$logSalary <- log(Hitters$Salary)
hist(Hitters$logSalary)
# Split data into training (75%) and test (25%)
set.seed(310)
train_index <- sample(1:nrow(Hitters),floor(.75*nrow(Hitters)),replace = FALSE)
hitter_train <- Hitters[train_index,]
hitter_test <- Hitters[-train_index,]
# Train the tree
tree_hitters <-tree(logSalary ~ AtBat + Hits + HmRun +RBI+Walks +
                      Years + Division +Assists +Errors, data = hitter_train)
# Report the results
###the stars indicate a leaf node and the numbers at the end of the rows are the predicted values
tree_hitters
summary(tree_hitters)
# Plot the tree
plot(tree_hitters)
text(tree_hitters,pretty = 0)
# Predict the test
test_preds <-  predict(tree_hitters,newdata = hitter_test,)


# Measure MSE test
MSE <- function(pred,t){
  mean((t-pred)^2)
}
MSE(test_preds,hitter_test$logSalary)

# Plot predicted vs true
plot(test_preds,hitter_test$logSalary)
abline(0,1)##gives 45 degree angle

# cross-validation to find best tree size
cv_hitter <- cv.tree(tree_hitters)
###gives lowest error
min(cv_hitter$dev)

###find best tree size by index
best_tree_index <- which.min(cv_hitter$dev)
cv_hitter$size[best_tree_index]


# report the results

# plto the error vs tree size
plot(cv_hitter$size,cv_hitter$dev,type = "b")
# prune the tree by the best size (6)
pruned_tree_hitters <- prune.tree(tree_hitters,best = 6)
# plot the pruned tree
plot(pruned_tree_hitters)
text(pruned_tree_hitters,pretty = 0)

#------------------------------------------------
### Classification Tree
#------------------------------------------------

# use titanic data set
# use steps above to predict Survived
# Remember the differences between classification tree and regression tree
setwd("C:\\Users\\noahe\\Desktop\\MGSC310")

titanic <- read.csv("titanic.csv")

#reformat outcome variable
titanic$Survived <- ifelse(titanic$Survived == 1,"Survived","Dead")
titanic$Survived <- as.factor(titanic$Survived)
head(titanic$Survived)


titanic$Pclass <- as.factor(titanic$Pclass)
###split data into training and test
set.seed(2019)
trainindex <- sample(1:nrow(titanic),floor(.75*nrow(titanic)),replace = FALSE)
train_titanic <- titanic[trainindex,]
test_titanic <- titanic[-trainindex,]


####train the model (tree)

tree_titanic <- tree(Survived~Pclass+Sex + Age + SibSp +
                       Parch + Fare+ Embarked,data = train_titanic)
plot(tree_titanic)
text(tree_titanic,pretty = 0)


preds_titanic <- predict(tree_titanic,type = "class",newdata = test_titanic)

table(preds_titanic,test_titanic$Survived)


accuracy <- (113+69)/223


###pruning the tree
cv_tree_titanic <- cv.tree(tree_titanic,FUN = prune.misclass)###makes the error based on misclassification
cv_tree_titanic
plot(cv_tree_titanic$size,cv_tree_titanic$dev,type = "b")                           

###choose the best tree size (5)
pruned_tree_titanic <- prune.misclass(tree_titanic,best = 5)
