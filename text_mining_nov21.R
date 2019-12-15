#######################################
#                                     #
#          Sentiment Analysis         #
#                                     #
#              11/21/19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################
setwd("C:\\Users\\noahe\\Desktop\\310_mine")
#######################################
# Goals:
#   - Importing a tsv file
#   - Cleaning a text data
#   - Word Clouds
#   - xgboost: Boosting model
#   - Predicting IMDB review sentiment
#   - Using Rsentiment package
#######################################

# Terminology:
#  - term: word
#  - document: review
#  - corpus: collections of documents containing (natural language) text
#  - term_document matrix: how many times a word is appeared in each review
#  - stop word: common words in a language


# installing required packages
install.packages("xgboost")  # for boosting
install.packages("FeatureHashing")  # for feature generation
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("RSentiment")  # for pre-trained sentiment analyses

# Load
library(ggplot2)
library(plotROC)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(xgboost)
library(FeatureHashing)
library(Matrix)
library(RSentiment)


#------------------------------------------------
### Importing a .tsv file
#------------------------------------------------

# imdb review file is store as a tsv file.
# tsv file is very similar to csv except it uses tabs (\t)
# instead of comma as delimeter
imdb <- read.delim("labeledTrainData.tsv",quote="", as.is=T)

# explore the data
View(imdb)
dim(imdb)

# sample review
imdb$review[457]
strwrap(imdb$review[457], width = 80)

# ratio of positives and negatives
table(imdb$sentiment)

#------------------------------------------------
### Cleaning (Pre-processing) the text
#------------------------------------------------

# a function to
#     1. remove the punctuation,
#     2. lowercase, and
#     3. create a (hashed) features.


####has.size limits the size of the data stored in RAM
##allows us to reduce how many words we take from each thing

cleantext <- function(df){
  df$review <- tolower(gsub("[^[:alnum:] ]"," ", df$review))
  features <- hashed.model.matrix(~ split(review, delim = " ", type = "tf-idf"),
                            data = imdb, hash.size = 2^16, signed.hash = FALSE)
}

# hash.size of 2^16 limits the number of columns in the document term matrix and is
# how we convert a feature of an unknown number of categories to a binary 
# representation of known, fixed size.

data = cleantext(imdb)

dim(data)   # it resulted into more than 65000 features!

# the data file that we created features for it, is sparse matrix
# we can convert it by transforming it to dense matrix
# as.matrix(data)

as.integer(which(data[457, ] != 0))

#------------------------------------------------
### Word Cloud
#------------------------------------------------

# concatenating all the first 1000 reviews into a single string
####collapse is used to take out spaces
text <- paste(imdb$review[1:1000], collapse=" ")
# creating a corpus
docs <- Corpus(VectorSource(imdb$review[1:500]))

# more cleaning
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# Remove some punctuations
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "><br")
docs <- tm_map(docs, toSpace, ".<br")
docs <- tm_map(docs, toSpace, "<br")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("movie", "film"))
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# creating the term-document matrix:
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

# top 10 words in the corpus
head(d, 10)

# creating the word cloud
set.seed(2019)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# options:
# words : the words to be plotted
# freq : their frequencies
# min.freq : words with frequency below min.freq will not be plotted
# max.words : maximum number of words to be plotted
# random.order : plot words in random order. If false, they will be plotted in decreasing frequency
# rot.per : proportion words with 90 degree rotation (vertical text)
# colors : color words from least to most frequent. Use, for example, colors ="black" for single color.

# Remember: text preprocessing (cleaning) has enormous impact on the model performance

## Further Explorations

# find the words that appeared at least 100 times
findFreqTerms(dtm, lowfreq = 100)

# find the words that are mostly correlated with "thriller"
findAssocs(dtm, terms = "thriller", corlimit = 0.3)

# create a bar plot for top 10 frequent words
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


#------------------------------------------------
### Classification Model
#------------------------------------------------

# split the data set into training and test
# choose 20000 for train and 5000 for test
set.seed(2019)
train <- sample(1:nrow(imdb), size=20000)
# we create xgb.DMatrix for xgboost
dtrain <- xgb.DMatrix(data[train,], label = imdb$sentiment[train])
dvalid <- xgb.DMatrix(data[-train,], label = imdb$sentiment[-train])


# xgboost can be used as boosting method for logistic and trees

# boosted logistic (generalized linear)
boosted_linear <- xgboost(booster = "gblinear", data =dtrain, 
                          eta = 0.02, eval_metric = "error",
                          nrounds = 10, objective = "binary:logistic",
                          verbose = 1)
# boosted tree
boosted_tree <- xgboost(booster = "gbtree", data =dtrain, 
                        max_depth = 10, eta = 0.05, nthread = 4, 
                        colsample_bytree = 0.5, eval_metric = "error",
                        nrounds = 400, objective = "binary:logistic",
                        verbose = 1)

# Tuning Hyper-parameters
# there are so many parameteres to adjust. We can try different parameterers and
#   cross-validation to find the best ones

preds_DF <- data.frame(scores_lm = predict(boosted_linear, newdata=dvalid),
                       scores_tree = predict(boosted_tree, newdata=dvalid),
                       test_labels = imdb$sentiment[-train])

## Plot ROC curve

# boosted linear
ggplot(preds_DF, 
       aes(m = scores_lm, 
           d = test_labels)) + 
  geom_roc(cutoffs.at = c(.99,.9,.8,.7,.6,.5,.4,.3,.2,.1,0))

# boosted tree
ggplot(preds_DF, 
       aes(m = scores_tree, 
           d = test_labels)) + 
  geom_roc(cutoffs.at = c(.99,.9,.8,.7,.6,.5,.4,.3,.2,.1,0))

# accuracy
predicted_sentiment <- ifelse(preds_DF$scores_tree>0.5,"Pos","Neg")
table(predicted_sentiment,preds_DF$test_labels)


# test the model with some random text!

bad_review <- "This movie fucking sucks"
good_review <- "Solid movie, Definitely would watch it again"

test_df <- data.frame(review = c(bad_review,good_review))

# process the test
test_processed <- tolower(gsub("[^[:alnum:] ]"," ", test_df$review))
test_processed <- hashed.model.matrix(~ split(review, delim = " ", type = "tf-idf"),
                                      data = test_df, hash.size = 2^16, signed.hash = FALSE)

# predict
predict(boosted_linear, newdata=test_processed)
predict(boosted_tree, newdata=test_processed)

#------------------------------------------------
### RSentiment Package
#------------------------------------------------

sentences <- c("This is a good text", 
               "This is a bad text", 
               "This is a really bad text", 
               "This is horrible")

calculate_total_presence_sentiment(sentences)

calculate_sentiment(sentences)

calculate_score(sentences)

# Let's see if the model can detect sarcasm!
calculate_total_presence_sentiment("Really, Sherlock? No! You are clever.")
calculate_total_presence_sentiment("Nice perfume. How long did you marinate in it?")

