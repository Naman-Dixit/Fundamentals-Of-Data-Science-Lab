

# Load required libraries
library(caret)
library(glmnet)
library(randomForest)
library(pROC)
library(corrplot)
library(dplyr)

# Load the Cleveland Heart Disease dataset
# Assuming the CSV is in your working directory
da <- read.csv("Heart_disease_cleveland_new.csv")

# Remove rows with missing values (if any)
da <- na.omit(da)

# Convert target to factor for classification
da$target <- as.factor(da$target)

# Basic structure of the data
str(da)

# Basic plot
plot(da)

# 2. Perform feature engineering (categorical encoding, scaling, transformations)

# 1. Normalize numerical features
num_cols <- sapply(da, is.numeric)
da_scaled <- da
da_scaled[num_cols] <- scale(da[num_cols])

# 2. Create a new feature: chol_age_ratio (cholesterol/age)
da_scaled$chol_age_ratio <- da_scaled$chol / da_scaled$age

# 3. Correlation matrix visualization
cor_matrix <- cor(da_scaled[, sapply(da_scaled, is.numeric)])
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# 3. Recursive Feature Elimination (RFE)

set.seed(42)
control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Remove outcome variable for input
x <- da_scaled %>% select(-target)
y <- da_scaled$target

results_rfe <- rfe(x, y, sizes = c(1:14), rfeControl = control)
print(results_rfe)

# 4. Model Training and Evaluation

# Add the new feature to the original dataset
da <- da %>% mutate(chol_age_ratio = chol / age)

# Train-test split
set.seed(42)
trainIndex <- createDataPartition(da$target, p = 0.8, list = FALSE)
trainData <- da[trainIndex, ]
testData <- da[-trainIndex, ]

# Model with all features
model_all <- train(target ~ ., data = trainData, method = "glm", family = "binomial")
pred_all <- predict(model_all, newdata = testData)
confusionMatrix(pred_all, testData$target)

# Model with selected features (based on RFE results, assuming top features)
# Adjust these based on actual RFE output
model_selected <- train(target ~ cp + thalach + oldpeak + chol_age_ratio,
                        data = trainData, method = "glm", family = "binomial")
pred_selected <- predict(model_selected, newdata = testData)
confusionMatrix(pred_selected, testData$target)

# 5. ROC Curve

prob_all <- predict(model_all, testData, type = "prob")[, 2]
prob_sel <- predict(model_selected, testData, type = "prob")[, 2]

roc_all <- roc(testData$target, prob_all)
roc_sel <- roc(testData$target, prob_sel)

plot(roc_all, col = "blue", main = "ROC Curve")
lines(roc_sel, col = "green")
legend("bottomright", legend = c("All Features", "Selected Features"),
       col = c("blue", "green"), lwd = 2)