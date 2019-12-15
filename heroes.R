setwd("C:/Users/noahe/Desktop/MGSC310")
comics <-read.csv("comic_characters.csv")
library(ggplot2)
library(forcats) ##for fct_lump
library(margins)
library(car)
library(forecast)


###dimensions
dim(comics)


levels(comics$ALIGN)
first.table <- table(comics$ALIGN)
prop.table(first.table)

second.Table <- table(comics$ALIGN, comics$INC)


ggplot(comics,aes(x = ALIGN)) + geom_bar(aes(fill = INC), position = 'dodge')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #changes how text is displayed
  labs(title = "Type of Characters",x = "CharacterType", y = "Count")

ggplot(comics,aes(x = YEAR, y = APPEARANCES)) +
  geom_bar(aes(fill = INC), stat = 'identity', alpha = .8) + ##fill shows you what part of each bar is made up of the specified variables
  theme_light()


####find top ten most characters
dc <- comics[comics$INC == "dc",]
dc[order(dc$APPEARANCES, decreasing = TRUE),"name"][1:10] ##sorts by appearances in decreasing order
##only gives their names ^^^^^


marvel <-comics[comics$INC == "marvel",]
marvel[order(marvel$APPEARANCES, decreasing = TRUE),"names"][1:10]


####number of years since inception

comics$age <- 2019 - comics$YEAR
###how to check missing values for appearance
sum(is.na(comics$APPEARANCES))

####simply prints out the number of missing appearances
TotalMissing <-  function(x){
  sum(is.na(x))
}

###apply lets you use a function over columns or rows
#margin 1 for rows margin 2 for columns
###this will give use the total number of missing values across all columns
apply(comics,2,TotalMissing)

###check levels
levels(comics$EYE)
levels(comics$ALIGN)
length(levels(comics$EYE))

###given that there are 26 levels for eyes we need to reduce the number of levels for analyses
comics$ALIGN_simple <- fct_lump(comics$ALIGN,n = 2)
####takes the highest frequencies then puts all others into an other category
comics$EYE_simple <- fct_lump(comics$EYE, n =3)
table(comics$EYE_simple)
####to assign a specified value to a missing variable
comics[is.na(comics$EYE_simple),"EYE_simple"] <- "Other"


table(comics$ID)


####make train and test sets
set.seed(2019)
train_index <- sample(1:nrow(comics),.8*nrow(comics),replace = FALSE)
comic_train <- comics[train_index,]
comic_test <- comics[-train_index,]
dim(comic_train)
dim(comic_test)


###model1 regression

###for looking at the coefficient of good characters
        ###compared to bad characters(baseline) good characters have 24 more appearances compared
        #### to the bad characters
mod1 <- lm(APPEARANCES ~ age + EYE_simple + ALIGN_simple + ID, data = comic_train)
summary(mod1)

###change baseline for align to good characters
mod1 <- lm(APPEARANCES ~ age + EYE_simple + 
             relevel(ALIGN_simple, ref = "Good Characters") + ID, data = comic_train)
summary(mod1)



####
attach(comics)
plot(age,APPEARANCES)


###using the squared terms of age
mod2 <- lm(APPEARANCES ~ age + I(age^2) + EYE_simple + 
             relevel(ALIGN_simple, ref = "Good Characters") + ID, data = comic_train)
summary(mod2)


###find effects for different levels of age
margins(mod2,at = list(age = c(10,20,30,40)))


cplot(mod2, x = 'age',what = 'prediction',scatter = TRUE)


comics$logAppearances <-log(comics$APPEARANCES)
hist(comics$logAppearances)


mod3 <- lm(log(APPEARANCES) ~ age + EYE_simple + ALIGN_simple +
             relevel(ALIGN_simple, ref = "Good Characters") + ID, data = comic_train)
summary(mod3)

vif(mod3)
vif(mod2)

plot(mod3$fitted.values, mod3$residuals)

predicted_train <- predict(mod3)

predicted_test <- predict(mod3,newdata = comic_test)

####train error
accuracy(predicted_train, comic_train$APPEARANCES)
####test_error
accuracy(predicted_test, comic_train$APPEARANCES)


####predicted values vs true
plot(comic_test$APPEARANCES,predicted_test,
     xlim = c(0,4),ylim = c(0,8))
abline(0,1,col = "red")
