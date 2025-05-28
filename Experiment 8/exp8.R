
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(e1071)

titanic <- read_csv("titanic.csv")
View(titanic)


titanic <- titanic %>% select(-PassengerId, -Name, -Ticket, -Cabin)


titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)


titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
titanic$Embarked[is.na(titanic$Embarked)] <- as.character(names(sort(table(titanic$Embarked), decreasing = TRUE))[1])
titanic$Embarked <- as.factor(titanic$Embarked)


set.seed(123)
trainIndex <- createDataPartition(titanic$Survived, p = 0.8, list = FALSE)
train <- titanic[trainIndex, ]
test <- titanic[-trainIndex, ]


model <- glm(Survived ~ ., data = train, family = binomial)

summary(model)


probabilities <- predict(model, newdata = test, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
predicted_classes <- as.factor(predicted_classes)

conf_matrix <- confusionMatrix(predicted_classes, test$Survived, positive = "1")
print(conf_matrix)


cat("\nAccuracy:", conf_matrix$overall['Accuracy'], "\n")


exp(coef(model))


ggplot(titanic, aes(x = Age, fill = Survived)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~Sex) +
  ggtitle("Age Distribution by Sex and Survival")
