setwd("C:/Users/noahe/Desktop/MGSC310")
donation <- read.csv("donation.csv")



###1)
summary(donation)

##2)
table(donation$Homeowner)
####how to do summary stats for a given dimension with a certain function
library(doBy)
summaryBy(.~ Homeowner,data = donation,FUN = mean)

###3)
set.seed(2019)
train_index <- sample(1:nrow(donation),size = .7*nrow(donation),replace = FALSE)
train_data <- donation[train_index,]
test_data <- donation[-train_index,]
###4)
train_lm <- lm(Donation ~ ., data = train_data)
summary(train_lm)

###5)
#significant predictors are NUMPROM, LASTGIFT, and AVG GIFT
#you can interpret them as with each increase in liftime number of promotions
#received to date, there is an increase of .04$ for donation.
#furthermore, with each dollar increase for the amount of the previous gift,
###there is a .10$ increase in donation 
###when considering avg gift, for every dollar increase in average dollar amount per gift
#there is an increase of .28$ in donation

#6)
train_preds <- data.frame(predict = predict(train_lm))
test_preds <- data.frame(predict = predict(train_lm,newdata = test_data))
##7)
train_preds$resids <- train_lm$residuals
test_preds$resids <- test_data$Donation - test_preds$predict
plot(train_preds$predict,train_preds$resids)
plot(test_preds$predict,test_preds$resids)

###train residuals appear heteroskedastic
###test residuals appear hetersoskedastic

###8)
RMSE <- function(t, p) {
  sqrt(sum(((t - p)^2)) * (1/length(t)))
}
print(RMSE(train_data$Donation,train_preds$predict))
print(RMSE(test_data$Donation,test_preds$predict))
####indicates that the average error is 10.57 and 8.39 respectively

###9)
library(leaps)
best_subset_donation <- regsubsets(Donation ~ .,data = train_data,
                                   nvmax = 17)

plot(best_subset_donation,scale = "adjr2")
max(summary(best_subset_donation)$adjr2)
summary(best_subset_donation)
###best model is model seven which has numchild,income,wealth,numprom,lastgift,totalmonths,avggift
?regsubsets

#10)
bkwd_stepwise_donation <- regsubsets(Donation~.,data  = train_data,
                                      method = "backward")
max(summary(bkwd_stepwise_donation)$adjr2)
summary(bkwd_stepwise_donation)$adjr2
plot(bkwd_stepwise_donation,scale = "adjr2")
#model six has the best r^2
bkwd_stepwise_donation2 <- regsubsets(Donation~.,data = train_data,
                                      nvmax = 17, method = "backward")
summary(bkwd_stepwise_donation2)
plot(bkwd_stepwise_donation2,scale = "adjr2")
##the selcted 6 for only 6 variables are 
#numchild, income,wealth,numprom,lastgift,avggift
dim(train_data)


#11)
library(glmnet)
library(glmnetUtils)
ridge_donation <- cv.glmnet(Donation~., data = train_data,
                            alpha = 0,)
lamb_min <- ridge_donation$lambda.min

library(coefplot)
plot(ridge_donation)
###how to use particular lambda for predictions
ridge_preds <- predict(ridge_donation,newdata = train_data, s = lamb_min)
mean(mean(ridge_preds - train_data$Donation)^2)
###to find the mse use cvm and then you can index to which mse you want
ridge_donation$cvm[ridge_donation$lambda == ridge_donation$lambda.min]

#12)
lasso_donation <- cv.glmnet(Donation~., data = train_data,
                            alpha = 1)

min(lasso_donation$cvm)
plot(lasso_donation)
lamb_min <- lasso_donation$lambda.min
as.matrix(coef(lasso_donation, s = lasso_donation$lambda.min))
lasso_preds <- predict(lasso_donation, newdata = train_data, s = lamb_min)
mean(mean(lasso_preds- train_data$Donation)^2)

#13)
#We choose lambda min when we want to minimize standard error whereas we use 
#1se when we want to find the most important coefficients

ebay <- read.csv("ebay.csv")


#1)
library(ggplot2)
###to make a straight line use method  - "lm"
str(ebay)
ggplot(ebay,aes(y = sellerRating,x = OpenPrice)) + geom_point(aes(color = factor(Competitive))) +
  geom_smooth(method = "lm") + labs(title = "sellerRating vs OpenPrice")
#2)
set.seed(2019)
train_index <- sample(1:nrow(ebay),.7*nrow(ebay),replace = FALSE)
ebay_train <- ebay[train_index,]
ebay_test <- ebay[-train_index,]


#3)
ebay_logit <- glm(Competitive ~.,data = ebay_train,
                  family = binomial)
ggplot(ebay,aes(x = OpenPrice,y = Competitive)) + geom_point()

#4)
exp(ebay_logit$coefficients)
#interpreatation of marginal effect of OpenPrice:
#when all other variables are held constant, a single unit increase in Opening PRice
#translates to an auction being 98% more likely to be categorized as a "competitve" auction

#5)
preds_train <- predict(ebay_logit,type ="response")
preds_test <-predict(ebay_logit,type = "response",newdata = ebay_test)
ebay_train <- data.frame(ebay_train, 
                         preds05 = ifelse(preds_train >.5,1,0),
                         preds06 = ifelse(preds_train >.6,1,0),
                         preds07 = ifelse(preds_train >.7,1,0),
                         preds08 = ifelse(preds_train >.8,1,0),
                         competitiveProb = preds_train)
ebay_test <- data.frame(ebay_test,
                        preds05 = ifelse(preds_test >.5,1,0),
                        preds06 = ifelse(preds_test >.6,1,0),
                        preds07 = ifelse(preds_test >.7,1,0),
                        preds08 = ifelse(preds_test >.8,1,0),
                        competitiveProb = preds_test)
library(gmodels)

CrossTable(ebay_train$Competitive,ebay_train$preds05,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)
CrossTable(ebay_test$Competitive,ebay_test$preds05,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)

###6) error rates
library(plotROC)
####training data error rates
train_sensitivity <- 471/746
train_specificity <- 378/634
#train false positives = 275/746
#train false negatives = 256/634


test_sensitivity <- 222/327
test_specificity <- 148/265
#test false positives = 105/327
# test false negatives = 117/265
###ROC plot
roc_plot_ebay_train <- ggplot(ebay_train,aes(m = competitiveProb,
                                       d = Competitive)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01)) +
  labs(title = "ROC Curve for Train Data",x = "False Positive Fraction",
       y= "True Positive Fraction")

roc_plot_ebay_test <- ggplot(ebay_test,aes(m = competitiveProb,
                                           d = Competitive)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01)) +
  labs(title = "ROC Curve for Test Data",x = "False Positive Fraction",
       y= "True Positive Fraction")


###I can say that the model at the .5 cutoff points offers the most true postiives
# and the least false positives

####how to choose nvmax: just limits how many iterations occurr