titanic <- read.csv("titanic.csv")

###remove missing rows
titanic <- titanic[!is.na(titanic$Age),]

logit1 <- glm(Survived ~ factor(Pclass) + Sex + Age,
              family = binomial,
              data = titanic)
summary(logit1)
exp(logit1$coefficients)


scores1 <- predict(logit1, type = "response")

preds_df <- data.frame(
  class05  = ifelse(scores1>.5,1,0),
  class06 = ifelse(scores1 >.6,1,0),
  titanic
)
#confusion matrix
table(preds_df$class05,preds_df$Survived)
sensitivity <- (207/(207+83))
specificity <- (356/(356/68))
