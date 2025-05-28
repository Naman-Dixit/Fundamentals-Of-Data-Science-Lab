library(ggplot2)
library(factoextra)
library(cluster)
library(readr)

## Load the Dataset
df <- read.csv("Mall_Customers.csv")
colnames(df) <- c("CustomerID", "Gender", "Age", "Annual_Income", "Spending_Score")
data <- df[, c("Annual_Income", "Spending_Score")]

# Handle Missing Values
if (sum(is.na(data)) > 0) {
  print("Warning: Missing values detected!")
  data$Annual_Income[is.na(data$Annual_Income)] <- mean(data$Annual_Income, na.rm = TRUE)  
  data$Spending_Score[is.na(data$Spending_Score)] <- mean(data$Spending_Score, na.rm = TRUE)
}

# Elbow Method for Optimal k
wcss <- function(k) {
  kmeans(data, centers = k, nstart = 25)$tot.withinss
}

k_values <- 1:10
wcss_values <- sapply(k_values, wcss)

# Plot the Elbow Method
plot(k_values, wcss_values, type = "b", pch = 19, col = "blue",
     xlab = "Number of Clusters (k)", ylab = "WCSS",
     main = "Elbow Method for Optimal k")

# Apply KMeans clustering with the optimal k value
set.seed(42)
optimal_k <- 5
km_model <- kmeans(data, centers = optimal_k, nstart = 25)

# Add cluster labels to the dataframe
df$Cluster <- as.factor(km_model$cluster)

# Write the clustered data to a new CSV file
write_csv(df, "Mall_Customers_Clustered.csv")

# Plot the Customer Segments
ggplot(df, aes(x = Annual_Income, y = Spending_Score, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "Customer Segments", x = "Annual Income", y = "Spending Score") +
  theme_minimal()
