###Homework 3 By: Noah Estrada-Rand

setwd("C:/Users/noahe/Desktop/MGSC310")
library(ggplot2)
library(doBy)

movies <- read.csv("movie_metadata.csv")
movies
####1
###a)
dim(movies)
#b)
colnames(movies)
#c)
sum(is.na(movies$budget))
movies <- movies[is.na(movies$budget) == FALSE,]
dim(movies)
##d)
length(unique(movies$director_name))
##e)
ggplot(movies,aes(imdb_score,budget)) + geom_point(aes(imdb_score,budget)) +
  labs(title = "Scatterplot of IMDB Scores and Budget") + 
  xlab("IMDB Score") + ylab ("Budget")
#F)
movies <- movies[movies$budget<400000000,]
dim(movies)
#G)
ggplot(movies,aes(imdb_score,budget)) + geom_point(aes(imdb_score,budget)) +
  labs(title = "Scatterplot of IMDB Scores and Budget\nWith Trendline") + 
  xlab("IMDB Score") + ylab ("Budget") + geom_smooth()
#H)
ggplot(movies,aes(imdb_score,budget)) + geom_point(aes(imdb_score,budget)) +
  labs(title = "Scatterplot of IMDB Scores and Budget\nWith Trendline") + 
  xlab("IMDB Score") + ylab ("Budget") + geom_smooth() + 
  facet_wrap(~content_rating,scales = "free")
###there appears to be a strong relationship when considering pg-13 movies,R and g rated movies

#####2)
movies$grossM <- movies$gross/1e+6
movies$budgetM <- movies$budget/1e+6
movies$genre_main <- do.call('rbind',strsplit(as.character(movies$genres), 
                                              '|', fixed=TRUE))[,1] ###doesnt work
##a)
movies$profitM <- movies$gross-movies$budget
movies$ROI <- movies$profit/movies$budget
##b)
mean(movies$ROI, na.rm = TRUE)
##c)
hist(movies$ROI, breaks = 10, 
     main = "Histogram of ROI for Movies",
     ylab = "Frequency",
     xlab = "ROI")
##d)
length(movies[movies$ROI >10,])
movies <- movies[movies$ROI <10,]
hist(movies$ROI, 
     main = "Histogram of ROI for Movies",
     ylab = "Frequency",
     xlab = "ROI")
##e)
ggplot(movies,aes(ROI)) + geom_histogram(aes(fill = genre_main)) + 
  labs(title = "Histogram for ROI for Movies") + ylab("Frequency") +
  xlab("ROI")
##f)
summarys_by_genre <- summaryBy(ROI~genre_main,data = movies)
##g)
ggplot(summarys_by_genre,aes(genre_main,ROI.mean)) + geom_bar(stat = "identity",col = "blue") +
  labs(title = "Barplot of ROI Mean for Each Main Genre") + 
  xlab("ROI") + ylab("Main Genres")

###3)
#a
set.seed(42)
train_index <- sample(1:nrow(movies),.8*nrow(movies),replace = FALSE) ##returns indices
train_data <- movies[train_index,]
test_data <- movies[-train_index,]
###b
dim(train_data)
dim(test_data)
###c
mod1 <- lm(profitM~imdb_score,data = train_data)
summary(mod1)
###e
coef(mod1)
