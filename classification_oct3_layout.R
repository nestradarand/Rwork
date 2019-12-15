#######################################
#                                     #
# Analyzing Credit Debt Defaulting    #
#                                     #
#              10/03/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - Why is linear regression not appropriate?
#   - Estimating logit model using glm()
#   - Odds ratio interpretation
#   - Exponentiating logit coefficients to understand impact on odds ratio
#   - Generating individual predictions from estimated logit model
#   - "Scoring" dataset 
#   - Multiple logistic regression
#   - false positives, false negatives
#   - Confusion matrices
#   - Calculating confusion matrices in R
#   - Assigning predicted classes given model probabilities
#   - How to select probability threshold - guideliens
#   - Specificity and Sensitiviy
#   - ROC curve, what it is 
#   - calculating ROC curves in R
#   - AUC
#   - calculating AUC curves
#######################################

#--------------------------------------
# 1. Load data
#--------------------------------------
# load data "Default" from ISLR package
library(ISLR)
data("Default")
# we can force R to avoid scientif number formats
options(scipen = 999)
# create a binary (dummy) variable for default (outcome)
###args are as follows (testcase,what the value is if its true, value if its false)
Default$default_binary <- ifelse(Default$default == "Yes",1,0)
summary(Default)

#--------------------------------------
# 2. Linear Model
#--------------------------------------
# estimate an OLS model using the 0,1 
# variable as our dependent variable

mod1 <- lm(default_binary ~balance,data = Default)

# predicted outcome (create a dataframe with other data)

preds_DF <- data.frame("preds" = predict(mod1),Default)


# what kind of predictions do we get for this model?


# plotting actual outcome with the fitted linear line


library(ggplot2)
###how to make plot with regression line
ggplot(Default,aes(x = balance,y = default_binary)) + geom_point() +
  geom_abline(intercept = mod1$coefficients[1], 
              slope = mod1$coefficients[2],
              color = "red",linetype = "dashed")
#--------------------------------------
# 2. Logistic Model
#--------------------------------------

# logistic model default ~ balance 

logit.fit <- glm(default ~balance,
                 family = binomial,
                 data = Default)

# interpretation

summary(logit.fit)
##interpret coefficient for balance 
exp(logit.fit$coefficients)
#answer: one unit increase in balance makes individual .5% more likely to default

# display exponentiated coefficients (optional)
# install.packages("jtools")
library(jtools)
##does the same thing but uses different method and allows us to exponentiate the coefficients
summ(logit.fit,exp = TRUE)

###
#--------------------------------------
# 3. Prediction Probabilities
#--------------------------------------

# predicted probabilities (manually)
# @ balance == 1000

# recall: Pr(Y=1|X=1000) = exp(beta_0 + beta_1 * X) / (1 + exp(beta_0 + beta_1 * X))



# predicting for all the rows

###gives predicted probabilities of the given values from the model
scores <- predict(logit.fit,type = "response")



# storing scores in a dataframe

preds_DF <- data.frame(scores_mod1 = scores,Default)
head(preds_DF)
# distributions of scores (predicted probs)
# boxplot by default == Yes or No
ggplot(preds_DF,aes(x = default, y = scores_mod1)) + geom_boxplot()
# histogram of scores

hist(preds_DF$scores_mod1)

ggplot(preds_DF,aes(x = scores_mod1)) + 
  geom_histogram(data = preds_DF[preds_DF$default == "Yes",],
                 fill = "blue",alpha = .9,bins = 10)+
  geom_histogram(data = preds_DF[preds_DF$default == "No",],
                 fill = "red",alpha = .3,bins = 10)+
  labs(x = "Estimated Probabilities (scores)",y = "count")
  

#--------------------------------------
# 4. Independent Factor Variable
#--------------------------------------

# cross table of students and default

table(Default$student,Default$default)

# a nicer way to produce the above table
# use package gmodels
# install.packages("gmodels")
library(gmodels)
# function CrossTable (Check various options)
CrossTable(Default$student,Default$default,
           prop.r = FALSE,
           prop.c = FALSE,
           prop.t = FALSE,
           prop.chisq = FALSE)




# logistic model with student as independent var.

logit.fit2 <- glm(default~student,
                  family = binomial,
                  data = Default)
summary(logit.fit2)
exp(logit.fit2$coefficients)
  

# using summ from jtools to print the exp(coefficients)
summ(logit.fit2, exp =TRUE)

#--------------------------------------
# 5. Multiple Logistic Regression
#--------------------------------------

# logistic model default ~ balance + income + student
logit.fit3 <- glm(default ~ balance + income+student,
                  family = binomial,
                  data = Default)
summary(logit.fit3)
exp(logit.fit3$coefficients)
# storing scores from three different models



#--------------------------------------
# 6. TP/FP/TN/FN
#--------------------------------------

# plot predicted probabilities over balance for model 1

scores_mod1 <- predict(logit.fit,type = "response")
scores_mod2 <- predict(logit.fit2,type = "response")
scores_mod3 <- predict(logit.fit3,type = "response")
preds_DF <- data.frame(scores_mod1 = scores_mod1,
                             scores_mod2 = scores_mod2,
                             scores_mod3 = scores_mod3,
                             Default)

ggplot(preds_DF,aes(x = balance, y = scores_mod1)) +
  geom_point(aes(color = default))

# TP/TN/FP/FN

# cutoff Pr > 0.5 (There are four conditions to check to determine TP/FP/TN/FN)
preds_DF$PosNeg05 <- ifelse(preds_DF$scores_mod1 >.5 & preds_DF$default == "Yes","TP",
                            ifelse(preds_DF$scores_mod1 >.5 & preds_DF$default == "No","FP",
                                   ifelse(preds_DF$scores_mod1 <=.5 &preds_DF$default == "No","TN","FN")))


# we can create a function to do the above for an arbitary cutoff
cutoffs <- function(dataFrame,threshold) {
  preds_DF$PosNeg05 <- ifelse(dataFrame$scores_mod1 >threshold & dataFrame$default == "Yes","TP",
                              ifelse(dataFrame$scores_mod1 > threshold & dataFrame$default == "No","FP",
                                     ifelse(dataFrame$scores_mod1 <= threshold &dataFrame$default == "No","TN","FN")))
  
}
preds_DF$PosNeg06 <- cutoffs(preds_DF,.6)
preds_DF$PosNeg04 <- cutoffs(preds_DF,.4)
preds_DF$PosNeg07 <- cutoffs(preds_DF,.7)



# let's do the same for different cutoffs


# take a sample of 5% of data (for more visibility)
# use set.seed(2019)
set.seed(2019)
###pulls the indices to be sampled
preds_sample <- preds_DF[sample(1:nrow(preds_DF),.05*nrow(preds_DF),replace = FALSE),]


# plot predicted scores against balance

#####specifying the shape tells us what shape to make each data point based on specified data
####size is just size of the figures in the plot
ggplot(preds_sample,aes(x = balance, y = scores_mod1))+
  geom_point(aes(color = default,shape = default),size = 3)+
  geom_hline(yintercept = .5, color = "red", linetype = "dashed")

# add a horizontal line to the above plot to represent the cutoff at 0.50


ggplot(preds_sample,aes(x = balance, y = scores_mod1))+
  geom_point(aes(color = PosNeg05,shape = default),size = 3)+
  geom_hline(yintercept = .5, color = "red", linetype = "dashed")
# differntiate TP/FP/TN/FN


# cutoff at 0.6



# cutoff at 0.4



# check the frequency of TP/FP/TN/FN by table for different cutoffs

table(preds_sample$PosNeg05)


#--------------------------------------
# 7. Confusion Matrix
#--------------------------------------

# create the confusion matrix for different cutoffs
# first create a dataframe for predicted classes

class_preds05 <- ifelse(preds_sample$scores_mod1 >.5,1,0)
class_preds06 <- ifelse(preds_sample$scores_mod1 >.6,1,0)
class_preds04 <- ifelse(preds_sample$scores_mod1 >.4,1,0)
class_preds001 <- ifelse(preds_sample$scores_mod1 >.01,1,0)

preds_DF <- data.frame(class_preds05 = class_preds05,
                       class_preds001 = class_preds001,
                       class_preds04=  class_preds04,
                       class_preds06 = class_preds06,
                       preds_sample)


# use table for confusion matrix
table(preds_DF$class_preds05,preds_DF$default)

#--------------------------------------
# 8. ROC Curves
#--------------------------------------

# ROC plots
# use package plotROC
# install.packages("plotROC")
library(plotROC)
###scores in this case are just the predicted probabilities
ggplot(preds_DF,aes(m = scores_mod1,
                    d = default)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01))


# comparing ROC for models 2 and 3
TrainROCS <- ggplot(preds_DF,aes(m = scores_mod1,d = default))+
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01))
calc_auc(TrainROCS)

# plot ROC


# calcuate AUC (area under curve) for both models


