######Question 1
setwd("C:/Users/noahe/Desktop")
school <- read.csv("perf_at_school.csv")
school <- subset(school, select = -c(G1,G2))
str(school)
View(school)

#make dummy variables out of all categorical variables
#just need to keep track of what represents what
school[,1:2] <- lapply(school[,1:2],as.numeric)
school[,4:6] <- lapply(school[,4:6],as.numeric)
school[,9:12] <- lapply(school[,9:12],as.numeric)
school[,16:23] <- lapply(school[,16:23],as.numeric)
#a)
str(school)
library(corrplot)

cormat <- cor(school)
corrplot(cormat)

#b)
library(doBy)
##famsize == 1 is GT3 else LE3
summaryBy(G3~factor(famsize) + factor(Fedu),data = school,FUN = "mean")

#c)
cors <-cor(school$G3,school)

print(cors)
###use size because the last one will be correlated as 1 with g3
tail(sort(abs(cors)),6)
### top 5 correlation: failures, higher, school, Medu, studytime
mod1 <- lm(G3~factor(higher)+Medu+studytime + failures +factor(school),
           data = school)
summary(mod1)

##school1 == GP

#d)



#e)

#f)
#talk about R^2
#talk about significance of all variables
#look at MSE
preds <- predict(mod1)
school$preds <- preds
library(ggplot2)
ggplot(school,aes(y = G3, x = preds)) + geom_point() +geom_smooth() +
  labs(x = "Predicted Values", y="G3 Values",title = "Predicted versus True Plot")
library(caret)
(RMSE(school$G3,school$preds))^2 ###gives the mse

####G)
##reset variables to without dummies
school <- read.csv("perf_at_school.csv")
school <- subset(school, select = -c(G1,G2))
str(school)
library(leaps)
fwd_step <- regsubsets(G3~.,data = school,
                       nvmax = 8,
                       method = "forward")
summary(fwd_step)
plot(fwd_step)


#h)
set.seed(2019)
train_index <- sample(1:nrow(school),size = .75*nrow(school),replace = FALSE)
train_school <- school[train_index,]
test_school <- school[-train_index,]
####estimate model of c and g on train
#from c
train_lm <- lm(G3~factor(higher)+Medu+studytime + failures +factor(school),
               data = train_school)
#from g
train_fwd_lm <- lm(G3~factor(school) + sex + Medu + studytime +
                     failures + factor(higher) + schoolsup + Dalc,
                   data = train_school)

train_lm_preds <-  predict(train_lm)
train_fwd_lm_preds <- predict(train_fwd_lm)
#testpreds simple linear
preds_test_lm <- predict(train_lm,newdata = test_school)
preds_test_fwd_lm <- predict(train_fwd_lm,newdata = test_school)

####residuals
reg_lm_trainMSE <- (RMSE(train_school$G3,train_lm_preds))^2
fwd_lm_trainMSE <- (RMSE(train_school$G3,train_fwd_lm_preds))^2

reg_lm_testMSE <- (RMSE(test_school$G3,preds_test_lm))^2
fwd_lm_testMSE <- (RMSE(test_school$G3,preds_test_fwd_lm))^2

#i)


#j)
library(glmnet)
library(glmnetUtils)
lasso <- cv.glmnet(G3~.,data = train_school,
                   alpha = 1)
coef(lasso)
coef_min_matrix <- as.matrix(coef(lasso,s = lasso$lambda.min))
coef_1se_matrix <- as.matrix(coef(lasso,s = lasso$lambda.1se))
coef_df <- data.frame(Lambda_min_coefs = coef_min_matrix,
                      Lambda_1se_coefs = coef_1se_matrix)
colnames(coef_df) <- c("Lambda_Min_coefs","Lambda_1se_coefs")
print(coef_df)

#k)
plot(lasso)
print(lasso$lambda.1se)
print(lasso$lambda.min)



#Question 2
credit <- read.csv("gmsc_cs-training.csv")


dim(credit)
sum(complete.cases(credit))
credit <- credit[complete.cases(credit),]
nrow(credit)
sum(is.na(credit))
names(credit)
#a)
trainindex <- sample(1:nrow(credit),.75*nrow(credit), replace = FALSE)
credit_train <- credit[trainindex,]
credit_test <- credit[-trainindex,]


ggplot(credit,aes(x = age, y = DebtRatio)) + geom_point(aes(color = factor(SeriousDlqin2yrs)))+
  labs(x = "Age",y = "Debt Ratio",title = "Debt Ratio Plotted Against Age")
ggplot(credit,aes(x = NumberOfDependents,y = DebtRatio)) + 
  geom_point(aes(color = factor(SeriousDlqin2yrs))) + 
  labs(x = "Number Of Dependents",y = "Debt Ratio",
       title = "Debt Ratio Against Number of Dependents")

#b)
cors <- cor(credit$SeriousDlqin2yrs,credit)
##to get top 4 correlated
tail(sort(abs(cors)),5)
##4 most strongly correlated predictors
#number of times 30.59
#number of times 90 days 
#age
#njmber of time 60.89


###b) find logit model
###write the model 

logitMod_train <- glm(SeriousDlqin2yrs ~ age+
                        NumberOfTime60.89DaysPastDueNotWorse + 
                        NumberOfTimes90DaysLate + 
                        NumberOfTime30.59DaysPastDueNotWorse,
                      data = credit_train,
                      family = "binomial")
summary(logitMod_train)
#c)
exp(logitMod_train$coefficients)


#d)
train_preds_df <- predict(logitMod_train, type = "response")
test_preds_df <- predict(logitMod_train,newdata =  credit_test,type = "response")
credit_train$delinqScores <- train_preds_df
credit_test$delinqScores <- test_preds_df

credit_train$preds05 <- ifelse(credit_train$delinqScores >.5,1,0)
credit_train$preds07 <- ifelse(credit_train$delinqScores > .7,1,0)
credit_test$preds05 <- ifelse(credit_test$delinqScores >.5,1,0)
credit_test$preds07 <- ifelse(credit_test$delinqScores >.7,1,0)

library(gmodels)
###cutoff 50 train
CrossTable(credit_train$SeriousDlqin2yrs,credit_train$preds05,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)
#cutoff 70 train
CrossTable(credit_train$SeriousDlqin2yrs,credit_train$preds07,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)

###cutoff 50 test
CrossTable(credit_test$SeriousDlqin2yrs,credit_test$preds05,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)
#cutoff 70 test
CrossTable(credit_test$SeriousDlqin2yrs,credit_test$preds07,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)

##e)
##### for train we have 
#train
trainaccuracy05 <- (158+55913)/60139
trainsensitivity05 <- (158/261)
trainspecificity05 <- 55913/59878

trainaccuracy07

##for threshold of 70 we have
accuracy07 <- (8+18621)/20047
sensitivity07 <- 8/21
specificity07 <- 18621/20026



###I gave up on coding them and did them manually

#f)
library(plotROC)
roc_plot_delinq <- ggplot(credit_test,aes(m = delinqScores,
                                          d = SeriousDlqin2yrs)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.9,.7,.6,.5,.4,.3,.2,.1)) +
  labs(title = "ROC Curve for Test Data Logit Model",x = "False Positive Fraction",
       y= "True Positive Fraction")
roc_plot_delinq_train <- ggplot(credit_train,aes(m = delinqScores,
                                                 d = SeriousDlqin2yrs)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.9,.7,.6,.5,.4,.3,.2,.1)) +
  labs(title = "ROC Curve for Test Data Logit Model",x = "False Positive Fraction",
       y= "True Positive Fraction")
#g)
calc_auc(roc_plot_delinq)
calc_auc(roc_plot_delinq_train)
