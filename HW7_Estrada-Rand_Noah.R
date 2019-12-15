setwd("C:/Users/noahe/Desktop/MGSC310")
library(plotROC)
####author Noah Estrada-Rand MGSC310


#a)
options(scipen = 50)
movies <- read.csv("movie_metadata.csv")
# removing missing values
movies <- movies[complete.cases(movies),]
# removing empty content rating or not rated
movies <- movies[(movies$content_rating != "" & movies$content_rating != "Not Rated"), ]
# removing movies with budget > 400M
movies <- movies[movies$budget < 400000000,]
# creating budget, gross, and profit columns in millions
movies$grossM <- movies$gross/1e+6
movies$budgetM <- movies$budget/1e+6
movies$profitM <- movies$grossM - movies$budgetM
# creating a column for main genre
movies$genre_main <- do.call('rbind',strsplit(as.character(movies$genres), '|', fixed=TRUE))[,1]
# creating a dummy for blockbuster movies
movies$blockbuster <- ifelse(movies$grossM > 200, 1, 0)
library(forcats)
movies$genre_main <- fct_lump(movies$genre_main,5)
movies$content_rating <- fct_lump(movies$content_rating,3)
movies$country <- fct_lump(movies$country,2)
movies$cast_total_facebook_likes000s <- movies$cast_total_facebook_likes / 1000
# top director
director_props <- data.frame(prop.table(table(movies$director_name)))
directors_indx <- order(director_props$Freq,decreasing = TRUE)
top_directors_indx <- directors_indx[1:floor(0.1*nrow(director_props))]
top_directors_names <- director_props[top_directors_indx, 1]
movies$top_director <- ifelse(movies$director_name %in% top_directors_names, 1, 0)
# train/test split
set.seed(1861)
train_idx <- sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train <- movies[train_idx,]
movies_test <- movies[-train_idx,]

#b)
mean(movies_train$blockbuster)
mean(movies_test$blockbuster)
t.test(movies_train$blockbuster,movies_test$blockbuster)

#c)
logit_1 <- glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s+
                 content_rating +genre_main, family = binomial,
               data = movies_train)
summary(logit_1)

#d)
exp(logit_1$coefficients)

# e)
preds_train <- data.frame(movies_train, 
                          predictions = predict(logit_1,type = "response"))
preds_test <- data.frame(movies_test, 
                         predictions = predict(logit_1,newdata = movies_test,type = "response"))

#f)
preds_LOOCV = NULL;
for(i in 1:nrow(movies_train)){
  mod <- glm(blockbuster ~ budgetM + top_director + cast_total_facebook_likes000s+
               content_rating +genre_main, family = binomial,
             data = movies_train[-i,])
  preds_LOOCV[i] <- predict(mod,newdata = movies_train[i,],type = "response")
}
head(preds_LOOCV)
preds_train <- data.frame(preds_train,loocvPreds = preds_LOOCV)

#g)

loocvROC <- ggplot(preds_train,aes(m = loocvPreds,
                                      d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01)) +
  labs(title = "ROC Curve for LOOCV Predictions",x = "False Positive Fraction",
       y= "True Positive Fraction")
inSampleROC <- ggplot(preds_train,aes(m = predictions,
                                    d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01)) +
  labs(title = "ROC Curve for In-Sample Predictions",x = "False Positive Fraction",
       y= "True Positive Fraction")
testROC <- ggplot(preds_test,aes(m = predictions,
                                    d = blockbuster)) +
  geom_roc(labelsize = 3.5,
           cutoffs.at = c(.99,.9,.7,.6,.5,.4,.1,.01)) +
  labs(title = "ROC Curve for Test Predictions",x = "False Positive Fraction",
       y= "True Positive Fraction")
# h)
calc_auc(loocvROC)
calc_auc(inSampleROC)
calc_auc(testROC)


