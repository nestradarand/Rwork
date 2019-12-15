setwd("C:/Users/noahe/Desktop/MGSC310")
library(MASS)
library(doBy)
library(ggplot2)
library(gmodels)
library(plotROC)
data(Boston)


## a) creating test and training test
#tells you if the home is"pricey"
Boston$PriceyHome <- ifelse(Boston$medv > 40, 1, 0)

Boston$chas <- factor(Boston$chas)##creates a factor
set.seed(2019)
trainSize <- 0.75
train_idx <- sample(1:nrow(Boston), size = floor(nrow(Boston) * trainSize))
housing_train <- Boston[train_idx,]
housing_test <- Boston[-train_idx,]

# b)
summaryBy( . ~ PriceyHome,data = housing_train, FUN = mean)

# c)
ggplot(housing_train,aes(x = tax,y = PriceyHome,color = factor(PriceyHome))) + geom_point() +
  scale_color_manual(values = c("red","navy blue")) +
  labs(title = "Plotted Values of Pricey Homes against Taxes",
       y = "Pricey Home indication",
       x = "Tax Rates per $10,000")

ggplot(housing_train,aes(x =crim,y = PriceyHome,color = factor(PriceyHome))) +geom_point() +
  scale_color_manual(values = c("dark green","red")) + 
  labs(title = "Pricey Home Indication Plotted Against Crime Rates",
       y = "Crime Rates",
       x = "Pricey Home")

ggplot(housing_train,aes(x = zn,y = PriceyHome,color = factor(PriceyHome))) +geom_point()+
  scale_color_manual(values =c("orange","brown")) +
  labs(title = "Pricey Home Indication plotted Against \nMedian Values of Homes",
       x = "Median Values of Homes ($1000)",
       y = "Pricey Home")

#d)
mod1 <- glm(PriceyHome~chas,data = housing_train,family = binomial)
summary(mod1)
exp(mod1$coefficients)

#e)
mod2 <-glm(PriceyHome ~ chas + crim + lstat + ptratio + zn + rm + tax + rad + nox,
           family = binomial,
           data = housing_train)
summary(mod2)
exp(mod2$coefficients)

#f)
housing_train$priceyProb <- predict(mod2,type = "response")
housing_test$priceyProb <- predict(mod2,type = "response",newdata = housing_test)

housing_train$PriceyPred <- ifelse(housing_train$priceyProb > .5,1,0)
housing_test$PriceyPred <- ifelse(housing_test$priceyProb >.5,1,0)

#g)
name_of_rows <- c("Not Pricey","Pricey","Sum")
name_of_cols <- c("Predicted Not","Predicted Pricey","Sum")
train_confusion <- table(housing_train$PriceyHome,housing_train$PriceyPred)
train_confusion <- addmargins(train_confusion)
rownames(train_confusion) <- name_of_rows
colnames(train_confusion) <- name_of_cols

test_confusion <- table(housing_test$PriceyHome,housing_test$PriceyPred)
test_confusion <- addmargins(test_confusion)
rownames(test_confusion) <- name_of_rows
colnames(test_confusion) <- name_of_cols


###didnt end up using this
CrossTable(housing_train$PriceyHome,housing_train$PriceyPred,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)
CrossTable(housing_test$PriceyHome,housing_test$PriceyPred,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)
#h
train_accuracy <- (sum(housing_train$PriceyHome == 1 & housing_train$PriceyPred == 1)+
                    sum(housing_train$PriceyHome == 0 & housing_train$PriceyPred == 0))/length(housing_train$PriceyHome)

test_accuracy <- (sum(housing_test$PriceyHome == 1 & housing_test$PriceyPred == 1)+
                    sum(housing_test$PriceyHome == 0 & housing_test$PriceyPred == 0))/length(housing_test$PriceyHome)
train_sensitivity <- sum(housing_train$PriceyHome == 1 & housing_train$PriceyPred == 1)/
  (sum(housing_train$PriceyHome == 1))

test_sensitivity <-sum(housing_test$PriceyHome == 1 & housing_test$PriceyPred == 1)/
  sum(housing_test$PriceyHome == 1)

train_specificity <- sum(housing_train$PriceyHome == 0 & housing_train$PriceyPred == 0)/
  sum(housing_train$PriceyHome == 0)

test_specificity <- sum(housing_test$PriceyHome == 0 & housing_test$PriceyPred == 0)/
  sum(housing_test$PriceyHome == 0)

train_false_positive <- sum(housing_train$PriceyHome == 0 & housing_train$PriceyPred == 1)
train_false_positive/sum(housing_train$PriceyHome == 1)

test_false_positive <- sum(housing_test$PriceyHome == 0 & housing_test$PriceyPred == 1)
test_false_positive/sum(housing_test$PriceyHome == 1)

train_false_negative <- sum(housing_train$PriceyHome == 1 & housing_train$PriceyPred == 0)
train_false_negative/sum(housing_train$PriceyHome == 0)

test_false_negative <- sum(housing_test$PriceyHome == 1 & housing_test$PriceyPred == 0)
test_false_negative/sum(housing_test$PriceyHome == 0)

# i)
train_ROC <- ggplot(housing_train,aes(m = priceyProb,
                                      d = PriceyHome)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01)) +
  labs(title = "ROC Curve for Train Data",x = "False Positive Fraction",
       y= "True Positive Fraction")
test_ROC <- ggplot(housing_test,aes(m = priceyProb,
                                    d = PriceyHome)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01)) +
  labs(title = "ROC Curve for Test Data",x = "False Positive Fraction",
       y= "True Positive Fraction")

# j)
train_AUC <- calc_auc(train_ROC)
test_AUC <- calc_auc(test_ROC)
