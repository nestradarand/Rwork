setwd("C:\\Users\\noahe\\Desktop\\310_mine")
library(ISLR)
data(Auto)
#A)
set.seed(2019)

#B)
train_index <- sample(1:nrow(Auto),floor(.75*nrow(Auto)),replace = FALSE)
auto_train <- Auto[train_index,]
auto_test <- Auto[-train_index,]

#c)
library(tree)
auto_reg_tree <- tree(mpg~cylinders+ displacement + horsepower + weight +
                        acceleration + year + origin,data=  auto_train)
#d)
plot(auto_reg_tree,main = "Regression Tree for MPG")
text(auto_reg_tree,pretty = 0)

#f)
train_preds <- predict(auto_reg_tree)
test_preds <- predict(auto_reg_tree,newdata = auto_test)
head(train_preds)
head(test_preds)
train_mse <- mean((train_preds- auto_train$mpg)^2)
test_mse <- mean((test_preds- auto_test$mpg)^2)
print(train_mse)
print(test_mse)

# g)
tree_mod_cv <- cv.tree(auto_reg_tree)
best_tree_index <- which.min(tree_mod_cv$dev)
tree_mod_cv$size[best_tree_index]

# h)
pruned_tree <- prune.tree(auto_reg_tree,best = 9)
pruned_train_preds <- predict(pruned_tree)
pruned_test_preds <- predict(pruned_tree,newdata = auto_test)
mse_train <- mean((pruned_train_preds - auto_train$mpg)^2)
mse_test <- mean((pruned_test_preds - auto_test$mpg)^2)
print(mse_train)
print(mse_test)

# i)
library(randomForest)
dim(auto_train)
bag_mod <- randomForest(mpg~cylinders+ displacement + horsepower + weight +
                          acceleration + year + origin,
                        data = auto_train,
                        mtry = 7,
                        ntrees = 500,
                        importance = TRUE)
bag_train_preds <- predict(bag_mod)
bag_test_preds <- predict(bag_mod,newdata = auto_test)

mse_train <- mean((bag_train_preds - auto_train$mpg)^2)
mse_test <- mean((bag_test_preds - auto_test$mpg)^2)
print(mse_train)
print(mse_test)

# j)
random_forest_mod <- randomForest(mpg~cylinders+ displacement + horsepower + weight +
                                    acceleration + year + origin,
                                  data = auto_train,
                                  mtry = 3,
                                  ntrees = 500,
                                  importance = TRUE)

# l)
rando_preds_train <- predict(random_forest_mod)
rando_preds_test <- predict(random_forest_mod,newdata =  auto_test)
train_mse <- mean((rando_preds_train - auto_train$mpg)^2)
test_mse <- mean((rando_preds_test - auto_test$mpg)^2)
print(train_mse)
print(test_mse)

# m)
importance(random_forest_mod)
varImpPlot(random_forest_mod)
