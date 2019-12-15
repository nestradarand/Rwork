setwd("C:/Users/noahe/Desktop/MGSC310")
movies <- read.csv("movie_metadata.csv")
##import corrplot
library(corrplot)

 ##b
##just following what the hw told me
movies <- movies[!is.na(movies$budget),]
movies <- movies[!is.na(movies$gross),]
movies <- movies[movies$budget<4e+8,]
movies$grossM <- movies$gross/1e+6
movies$budgetM <- movies$budget/1e+6
movies$profitM <- movies$grossM-movies$budgetM
movies$cast_total_facebook_likes000s <- movies$cast_total_facebook_likes / 1000
set.seed(2019)
train_indx <- sample(1:nrow(movies), 0.8 * nrow(movies), replace=FALSE)
movies_train <- movies[train_indx, ]
movies_test <- movies[-train_indx, ]

##c)
dim(movies_train)
dim(movies_test)

#d)
nums <- sapply(movies, is.numeric) # names of numeric variables
cormat <- cor(movies[,nums], use="complete.obs") ###only uses numerica variables
print(cormat[,"profitM"])
## e)
corrplot(cormat)
##f)
mod1 <- lm(profitM~imdb_score + cast_total_facebook_likes000s,data = movies_train)
summary(mod1)
###g)
#the estimated effect of cast facebook likes is that for every thousand like increase
#in cast facebook likes, the profit of the movie incrases by $330,000 dollars 

##h)
##pvalue for imdb_score is <2e-16
##pvalue for cast_total_facebook_likes ios <2e-16
##pvalue is the probability of observing the results we got by chance
###in this case it is the probability of observing an effect of 
##imdb scores and cast_total_facebook_likes on profitm

###i)
##the estimate pvalue in this case implies that we can reject the null hypothesis
#which states that there is no relationship between imdb scores and profit and instead
#conclude that imbd score has a significant effect on profit
##both variables are statistically significant at 95% confidence

#j)
# the R^2 is .07082 and the adjusted R^2 is .07022
##R^2 indicates how much of the variance in the outcome variable, in this case profit
# is explained by the independent variable of a linear regression model

#k) the f stat of the model is 118.1
# the fstat tells us the significance of all present coefficients
# in other words it checks if all coefficients are zero and is a higher number if not
#this helps determine the significance of a model

#l
mod1$residuals
length(mod1$residuals)
dim(movies_train)
###m)
hist(mod1$residuals, breaks = 40,
     main = "Histogram of Residuals from Linear Model",
     xlab = "Residuals")
###n)
tss <- sum((movies_train$profitM - mean(movies$profitM))^2)
rss <- sum((mod1$residuals)^2)
r.squared <- 1-(rss/tss)
