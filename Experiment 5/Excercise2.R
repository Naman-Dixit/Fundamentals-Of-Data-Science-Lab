
data <- read.csv("C:/Users/naman/OneDrive/Desktop/Data science lab/New folder/Experiments/Experiment 5/house_prices.csv")


# %%
summary(data)

# %%
pairs(data, pch = 19)  # Holy shit? scatterplot matrices?

# %% md
#Fit a linear regression model using lm() where Price is the dependent variable.

# %%
lm(Age ~ Price, data=data)
models <- list()
for (col in names(data)[names(data) != "Price"]) {
  formula <- as.formula(paste("Price ~", col))
  models[[col]] <- lm(formula = formula, data = data)
}

# %%
print(names(models[["Area"]]))
# %%
library(ggplot2)

# %%
for(col in names(data)[names(data) != "Price"]){
  p <- ggplot(data, aes_string(y = "Price", x = col)) +
    geom_point(color="red", size=2) +
    geom_abline(color="blue", intercept=coef(models[[col]])[1], slope=coef(models[[col]])[2]) +
    labs(title = paste("Price vs", col), x = col, y = "Price"  )

  ggsave(paste("C:/Users/naman/OneDrive/Desktop/Data science lab/New folder/Experiments/Experiment 5", col,".jpeg"))
}
# %%

library(dplyr)

# %%

# Create a summary dataframe
model_performance <- data.frame(
  Predictor = names(models),
  RMSE = sapply(models, function(m) sqrt(mean((data$Price - predict(m))^2))),
  R_squared = sapply(models, function(m) summary(m)$r.squared)
)

# %%
# Sort by best R-squared (descending)
model_performance 
  arrange(desc(R_squared)) 
  print()
# %%
ggplot(model_performance, aes(x = reorder(Predictor, -RMSE), y = RMSE)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "RMSE by Predictor", x = "Predictor", y = "RMSE ($)")
# %%
ggplot(model_performance, aes(x = reorder(Predictor, R_squared), y = R_squared)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "R-squared by Predictor", x = "Predictor", y = "R-squared") +
  ylim(0, 1)
