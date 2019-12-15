#######################################
#                                     #
#    Analyzing NBA Players' Stats     #
#                                     #
#              9/19/19                #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - Explore data
#   - Check for missing values
#   - Identify and remove outliers
#   - Correlation between variables
#   - Correlation map using corrplot()
#   - Calculate and plot average measures per team
#   - Using frequency table
#   - Partition data into test and train
#   - Run linear regression using lm()
#   - Interpret the results of lm
#   - Plot the linear model
#   - Predict using the trained model
#   - Preparing R markdown report
#######################################

#######################################
# 1. Reading Data
#######################################

# We are using a dataset on NBA players' stats from 1996 to 2016
# It is from Kaggle website. You can download the data from here:
# https://www.kaggle.com/justinas/nba-players-data
# Dataset is also available from blackboard. Download it and put
#  it in your working directory. The file name is "nba_players.csv"

# First things first: set the working directory
# (Alternatively, you can use R project to set the working director)


# Now, open the data set using read.csv() and store it in 'nba' dataframe
data <- read.csv("nba_players.csv")

# Great! now, let's check what the data is about
# What is its dimension?
dim(data)
# So there are 9561 records (obs) in this data!
# Check out the variables names
colnames(data)
# Cool! Here is the description of the variables:

# 1)  X:                 index
# 2)  player_name:       Name of the player
# 3)  team_abbreviation: Abbreviated name of the team the player played for (at the end of the season)
# 4)  age:               Age of the player
# 5)  player_height:     Height of the player (in centimeters)
# 6)  player_weight:     Weight of the player (in kilograms)
# 7)  college:           Name of the college the player attended
# 8)  country:           Name of the country the player was born in (not necessarily the nationality)
# 9)  draft_year:        The year the player was drafted
# 10) draft_round:       The draft round the player was picked
# 11) draft_number:      The number at which the player was picked in his draft round
# 12) gp:                Games played throughout the season
# 13) pts:               Average number of points scored
# 14) reb:               Average number of rebounds grabbed
# 15) ast:               Average number of assists distributed
# 16) net_rating:        Team's point differential per 100 possessions while the player is on the court
# 17) oreb_pct:          Percentage of available offensive rebounds the player grabbed while he was on the floor
# 18) dreb_pct:          Percentage of available defensive rebounds the player grabbed while he was on the floor
# 19) usg_pct:           Percentage of team plays used by the player while he was on the floor (FGA + Possession Ending FTA + TO) / POSS)
# 20) ts_pct:            Measure of the player's shooting efficiency that takes into account free throws, 2 and 3 point shots (PTS / (2*(FGA + 0.44 * FTA)))
# 21) ast_pct:           Percentage of teammate field goals the player assisted while he was on the floor
# 22) season:            NBA season 

# Now, take a look at the data
View(data)
# Check the types of the variables
str(data)
# there are 12 numeric variables

# how many unique players are there?
length(unique(data$player_name))

#######################################
# 2. Data Cleaning
#######################################

# Check how many missing variables are there
sum(is.na(data))
# it seems we are in luck, and there is no missing data.

# Draft round
# make a box plot of pts over draft round
boxplot(data$pts~data$draft_round)

attach(data)##used to easily access things and make all contained vars into global vars
boxplot(pts,draft_round)
detach(data)
# anything odd? usually there are two rounds of drafting in nba
# count the draft numbers not in 1,2,undrafted
# note that draft_round is stored as factor
# use %in% operator to check whether a value is in a set

###returns true or false for each provided vector across all data in specified category
#thus if draft_round data will return true if in undrafted,1,2

sum(!data$draft_round %in% c("Undrafted","1","2"))

# 44 observations are not in that set. That might be a data anomaly.
# Remove them from the data set
data <- data[data$draft_round %in% c("Undrafted","1","2"),]

# we need to convert it again to factor to remove unused levels
data$draft_round <- factor(data$draft_round)

boxplot(data$pts~data$draft_round)
# Creating BMI variable (Body Mass Index: weight/height^2)

data$bmi <- data$player_weight/data$player_height
hist(data$bmi)
#######################################
# 3. Correlation structure
#######################################

# Let's calculate the (pairwise) correlation among the variables: 
#    player_height, player_weight, gp, pts, reb, ast, net_rating

cor(data[,c(5,6,12:16)]) ###run correlation an all rows and only specified columns

# That would be hard to read. Let's use a visualization tool.
# Use "corrplot" package. If you don't have it, install.packages("corrplot")
# Note that corrplot function takes correlation matrix as input
library(corrplot)
sapply(data,is.numeric)
corrplot(cor(data[,c(5,6,12:16)]))

#######################################
# 4. Partition data into train and test
#######################################



# Now, we want to make our train and test sets
# Split the data by 75% train and 25% test
# Simple method of testing/training split
# very important to set seed! use set.seed(310)
# get number of obs and use sample() with no replacement to pick the row index

set.seed(310)
####this samples from 1 to all rows and pulls 75% of them without replacement
train_index <- sample(1:nrow(data),.75*nrow(data),replace = FALSE) ##returns indices
train_data <- data[train_index,]
test_data <- data[!train_index,]

# check the number of rows for train and test
dim(train_data)
dim(test_data)
#######################################
# 5. Linear Regression
#######################################

# Simple linear regression (just one variable)
# Model 1: regress net_rating on pts (for train data)
model1 <- lm(net_rating~pts, data = train_data)
plot(model1)

# use summary to see the results
summary(model1)

# how do you interpret the results?

# make a scatter plot and the fitted line
plot(train_data$pts,train_data$net_rating)
abline(model1, col = "green")
library(ggplot2)
ggplot(train_data,aes(pts,net_rating)) + geom_point(aes(pts,net_rating)) + geom_smooth()

# Model 2: regress pts on weight (for train data)
model2 <- lm(pts~player_weight,data = train_data)
# use summary to see the results
summary(model2)
# how do you interpret the results?

# make a scatter plot and the fitted line
# this time use ggplot2
ggplot(train_data,aes(pts,player_weight)) + geom_point(aes(pts,player_weight,color =draft_round))+
  geom_smooth() 


# Multiple linear regression (more variables)
# regress net_rating on weight, height, pts, reb, and ast
multiModel <- lm(net_rating~player_weight + player_height + pts +reb +ast,
                 train_data)

# what variables are statistically significant at 95% level?
summary(multiModel)


# We can use coefplot package to visually see the magnitudes of the effect.
library(coefplot)
coefplot(multiModel,predictors = c("player_weight","pts","reb","ast",sort = "magnitude"))

# What happens if add bmi?

#######################################
# 6. Your Turn
#######################################

### Popular Colleges
# Let's find out what colleges are the most productive ones

# Now, we want to sort the results. First, we store that results in a dataframe

# create the sorted index using order (make the order descending)


# show the first 10 colleges


###Average performance by team
# We want to check the average net_rating by each team
# Recall we can use summaryBy() from doBy package


# store the results from summaryBy() in a dataframe and plot it


# use ggplot to produce the same plot


###Linear Regression
# Regress pts on height, weight, and bmi

# what do you find?