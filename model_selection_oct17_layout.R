#######################################
#                                     #
#           Model Selection           #
#                                     #
#              10/17/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - What best subset selection is
#   - Why best subset selection is infeasible for most datasets
#   - Forward stepwise algorithm
#   - Programming a forward stepwise model using "leaps" package
#   - Backward stepwise algorithm
#   - Programming backward stepwise model 
#   - Pros and cons of forward stepwise
#   - Ridge regression equation  
#   - Lambda in ridge regression
#   - How ridge regression trades off bias for variance
#   - Estimating ridge model with glmnet and glmnetUtils
#######################################

#------------------------------------------------
### Setup
#------------------------------------------------
library(ISLR)
set.seed(2019)
data(Auto)

Auto_sub <- Auto[,-9]
head(Auto_sub)
library(leaps)

#------------------------------------------------
### Forward Stepwise Selection
#------------------------------------------------
# install.packages('leaps')
auto_fit_fwd <- regsubsets(mpg~.,
                           data  = Auto_sub,
                           nvmax = 7,
                           method = "forward")
reg_summary <- summary(auto_fit_fwd)
####gievs the scale to compare models with different predictors based on R^2 scores
plot(auto_fit_fwd,scale = "adjr2")
#says which variables are included or not
reg_summary$which
reg_summary$adjr2

plot(reg_summary$adjr2,ylab = "Adjusted Rsquare",
     type = "l")

#------------------------------------------------
### Backward Stepwise Selection
#------------------------------------------------

auto_fit_bwd <- regsubsets(mpg~.,
                           data = Auto_sub,
                           nvmax = 7,
                           method = "backward")
summary(auto_fit_bwd)
plot(auto_fit_bwd,scale = "adjr2")


#------------------------------------------------
### Ridge Regression
#------------------------------------------------
#install.packages("glmnet")
#install.packages("glmnetUtils")
library(glmnet)
library(glmnetUtils)
setwd("C:/Users/noahe/Desktop/MGSC310")
movies <- read.csv("movie_metadata.csv")


# load the movies dataset and clean the data
options(scipen = 50)
set.seed(2019)
movies <- read.csv("Datasets/movie_metadata.csv")
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

# estimate ridge mod 
# profitM ~ all variables except: director_name,actor_1_name,
#                                 actor_2_name,actor_3_name,
#                                 plot_keywords,movie_imdb_link,
#                                 country,budgetM,grossM, genres,
#                                 language, movie_title, budget, gross

mov_subset <- subset(movies_train,select = -c(director_name,actor_1_name,
                                actor_2_name,actor_3_name,
                                plot_keywords,movie_imdb_link,
                                country,budgetM,grossM, genres,
                                language, movie_title, budget, gross))
#####shrinks all coefficients by taking care of overfitting problem by introducing bias
ridge_mod <- cv.glmnet(profitM ~.,
                       data = mov_subset,
                       alpha = 0)
coef(ridge_mod)
# ridge coefficients

# explore how coefficients change as we change lambda
library(coefplot)
####shows us how coefficient changes with lambda
####as lambda goes to infinity, coefficients become zero(not helpful in predicting)
###introduces bias
coefpath(ridge_mod)
