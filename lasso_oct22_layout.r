

#######################################
#                                     #
#               Lasso                 #
#                                     #
#              10/22/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - Lasso regression equation 
#   - How Lasso acts as a variable selection mechanism
#   - Estimating Lasso models in OLS
#   - How to print Lasso coefficients for different lambdas using the coef() function 
#   - Examining shrinkage path using coefpath
#   - Graphical representation of Lasso penalty
#   - When to use Ridge versus Lasso
#######################################

#------------------------------------------------
### Lasso Regression
#------------------------------------------------
library(glmnet)
library(glmnetUtils)
setwd("C:/Users/noahe/Desktop/MGSC310")
# load the movies dataset and clean the data
options(scipen = 50)
set.seed(2019)
movies <- read.csv("movie_metadata.csv")
movies <- movies[complete.cases(movies),]
movies <- movies[movies$budget < 400000000,]
movies <- movies[(movies$content_rating != "" & movies$content_rating != "Not Rated"),  ]
movies$grossM <- movies$gross/1e+6
movies$budgetM <- movies$budget/1e+6
movies$profitM <- movies$grossM - movies$budgetM
movies$genre_main <- do.call('rbind',strsplit(as.character(movies$genres), '|', fixed=TRUE))[,1]
library(forcats)
movies$genre_main <- fct_lump(movies$genre_main,5)
movies$content_rating <- fct_lump(movies$content_rating,3)
movies$country <- fct_lump(movies$country,2)
movies$cast_total_facebook_likes000s <- movies$cast_total_facebook_likes / 1000
train_idx <- sample(1:nrow(movies),size = floor(0.75*nrow(movies)))
movies_train <- movies[train_idx,]
movies_test <- movies[-train_idx,]


# estimate Lasso mod 

movies_train_subset <- subset(movies_train,select = -c(director_name,actor_1_name,
                                              actor_2_name,actor_3_name,
                                              plot_keywords,movie_imdb_link,
                                              country,budgetM,grossM, genres,
                                              language, movie_title, budget, gross))

####alpha = 1 indicates we are using lasso
#####alpha  = 0 gives ridge
lasso_mod <- cv.glmnet(profitM~.,data = movies_train_subset,
                       alpha = 1)
lasso_mod
# lasso coefficients
#s is used to set the lambda value
coef(lasso_mod,s = lasso_mod$lambda.min)

#set to the 1se lambda

coef(lasso_mod,s = lasso_mod$lambda.1se)

# put in a matrix
coef_mat <- data.frame(
  lasso_min = as.matrix(round(coef(lasso_mod,s = lasso_mod$lambda.min),3)),
  lasso_1se = as.matrix(round(coef(lasso_mod,s = lasso_mod$lambda.1se),3)))

colnames(coef_mat) <- c("Lasso_min","Lasso_1se")

# plot lambda and erros
plot(lasso_mod)
# place in one coef

# explore how coefficients 
# change as we change lambda
library(coefplot)
coefpath(lasso_mod)


