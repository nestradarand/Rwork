setwd("C:/Users/noahe/Desktop/MGSC310")
library(leaps)

# a)
Bikes_df <-  read.csv("day.csv")

#b)
Bikes_df[,3:9] <- lapply(Bikes_df[,3:9],factor)

# c)
sapply(Bikes_df,is.factor)

# d)
#get rid of index number column
Bikes_df <- Bikes_df[,2:ncol(Bikes_df)]
str(Bikes_df)
#plot(Bikes_df$casual,Bikes_df$cnt)
Bikes_df$casual_sq <- Bikes_df$casual^2
#plot(Bikes_df$registered,Bikes_df$cnt)
Bikes_df$registered_sq <- Bikes_df$registered^2

####don't necessarily need this
#de normalization of certain variables
Bikes_df$windspeed <-  Bikes_df$windspeed *67
Bikes_df$hum<- Bikes_df$hum *100
#max and min for the original temp measurements were -8 and + 39 respectively
tmin <- -8
tmax <- +39
Bikes_df$temp <- (Bikes_df$temp)*(tmax-tmin) + tmin
#max and in for original atemp measurements were -16 and +50
atmin <-  -16
atmax <- 50
Bikes_df$atemp <- (Bikes_df$atemp) * (atmax-atmin) + atmin

# e)
set.seed(2019)
train_index <- sample(1:nrow(Bikes_df),(.7*nrow(Bikes_df)),replace = FALSE)
train_bikes <- Bikes_df[train_index,]
test_bikes <- Bikes_df[-train_index,]

#f)
fwd_fit <- regsubsets(cnt ~ season + holiday + mnth + workingday +
                              weathersit + temp + hum +windspeed, 
                      data = train_bikes, nvmax = 8,
                      method = "forward")
summary(fwd_fit)




#g)
bkwd_fit <- regsubsets(cnt ~ season + holiday + mnth + workingday +
                         weathersit + temp + hum +windspeed, 
                       data = train_bikes, nvmax = 8,
                       method = "backward")
summary(bkwd_fit)

#h)
summary(fwd_fit)$adjr2
summary(fwd_fit)$which
summary(bkwd_fit)$adjr2
library(glmnet)
library(glmnetUtils)
##stands for cross validation glmnet
train_ridge <- cv.glmnet(cnt ~ season + holiday + mnth + workingday +
                           weathersit + temp + hum +windspeed,
                         data = train_bikes,alpha = 0)
plot(train_ridge, main = "Ridge Regression for Bikes Training Data")

#i)
train_ridge$lambda.min
train_ridge$lambda.1se

#j)
print(as.matrix(coef(train_ridge,s = train_ridge$lambda.min)))
print(as.matrix(coef(train_ridge,s = train_ridge$lambda.1se)))

#l lasso model
train_lasso <- cv.glmnet(cnt ~season + holiday + mnth + workingday +
                           weathersit + temp + hum +windspeed,
                         data = train_bikes,alpha = 1)

#m)
print(as.matrix(coef(train_lasso,s = train_lasso$lambda.min)))
print(as.matrix(coef(train_lasso,s = train_lasso$lambda.1se)))


###when using lasso, you are more worried about insight
###with regsubsets you are more worried about accuracy
