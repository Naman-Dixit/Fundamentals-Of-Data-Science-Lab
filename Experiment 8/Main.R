
# %%
library(tidyverse)
library(dplyr)

# %%
# Basic way to load a CSV
train <- read.csv("/home/asus/content/Notes/Semester 4/FDN Lab/Experiments/Experiment 8/titanic/train.csv")

test <- read.csv("/home/asus/content/Notes/Semester 4/FDN Lab/Experiments/Experiment 8/titanic/test.csv")



# %%
# Removing Unessacry Cols
train <- train %>% select(-one_of("Cabin", "Ticket", "Name", "Embarked"))
test <- test %>% select(-one_of("Cabin", "Ticket", "Name", "Embarked"))

# %%

train <- train %>% fill(everything(), .direction = "down")
test <- test %>% fill(everything(), .direction = "down")

# %%
X_train <- train %>% select(-Survived)
Y_train <- train %>% select(Survived)

# %%
train_df <- as_tibble(train) %>%
  mutate(Survived = train)
# %%

# Train the model
logit_model <- glm(Survived ~ .,
                   data = train,
                   family = binomial)

# %%
predictions <- predict(logit_model, newdata = train, type = "response")

# %%

predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# %%
ground_truth <- train$Survived

# %%
conf_matrix <- table(Predicted = predicted_classes, Actual = ground_truth)
# %%

print(conf_matrix)
# Actual
# Predicted   0   1
#          0 472 110
#          1  77 232

# %%
accuracy <- sum(diag(conf_matrix))/sum(conf_matrix)
precision <- conf_matrix[2,2]/sum(conf_matrix[2,])
recall <- conf_matrix[2,2]/sum(conf_matrix[,2])
f1_score <- 2 * (precision * recall) / (precision + recall)

# %%
summary(logit_model)
print(conf_matrix)
cat("\nAccuracy:", round(accuracy, 3))
cat("\nPrecision:", round(precision, 3))
cat("\nRecall/Sensitivity:", round(recall, 3))
cat("\nF1 Score:", round(f1_score, 3))
cat("\nSpecificity:", round(conf_matrix[1,1]/sum(conf_matrix[,1]), 3))
