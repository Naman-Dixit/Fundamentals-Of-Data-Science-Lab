# experiment 5 

# i. Loading
house_data <- read.csv("house_prices.csv")

# ii. EDA
str(house_data)
summary(house_data)

# Scatter plot
plot(house_data$Area, house_data$Price, main = "Price vs Area", xlab = "Area (sqft)", ylab = "Price", col = "blue")



# Cor matrix
cor_matrix <- cor(house_data[c("Area", "Bedrooms", "Bathrooms", "Age", "Price")], use = "complete.obs")
print(cor_matrix)

# iii.
model <- lm(Price ~ Area + Bedrooms + Bathrooms + Age, data = house_data)

# iv. Model 
summary(model)

# v. Predict house prices using the model
predicted_prices <- predict(model, newdata = house_data)


# vi. 
r_squared <- summary(model)$r.squared
cat("R-squared:", r_squared, "\n")

# RMSE
rmse <- sqrt(mean((house_data$Price - predicted_prices)^2))
cat("RMSE:", rmse, "\n")