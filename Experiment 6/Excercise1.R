library(ggplot2)
library(dplyr)
library(cluster)

# %%
set.seed(1)

mall_data <- read.csv("C:/Users/naman/OneDrive/Desktop/Data science lab/New folder/Experiments/Experiment 6/Mall_Customers.csv")

# %%
head(mall_data)
summary(mall_data)

# %%
colnames(mall_data) <- c("CustomerID", "Gender", "Age", "AnnualIncome", "SpendingScore")

# %%
ggplot(mall_data, aes(x = AnnualIncome, y = SpendingScore)) +
  geom_point(aes(color = Gender), alpha = 0.7) +
  labs(title = "Annual Income vs Spending Score by Gender",
       x = "Annual Income (k$)",
       y = "Spending Score (1-100)") +
  theme_minimal()


# %%
ggplot(mall_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of Customers",
       x = "Age",
       y = "Count") +
  theme_minimal()


# %%
ggplot(mall_data, aes(x = Gender, y = SpendingScore, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Spending Score Distribution by Gender",
       y = "Spending Score (1-100)") +
  theme_minimal()
# ===================================
# Part 2
# ===================================
# %%
data_for_clustering <- mall_data[, c("AnnualIncome", "SpendingScore")]

# %%

wss <- function(k) {
  kmeans(data_for_clustering, k, nstart = 10)$tot.withinss
}

# %%
k_values <- 1:10
wss_values <- sapply(k_values, wss)

# %%
plot(k_values, wss_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares",
     main = "Elbow Method for Optimal K")

# %%
avg_sil <- function(k) {
  km.res <- kmeans(data_for_clustering, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data_for_clustering))
  mean(ss[, 3])
}

# %%

k_values <- 2:10
avg_sil_values <- sapply(k_values, avg_sil)

# %%
plot(k_values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Average Silhouette Scores",
     main = "Silhouette Method for Optimal K")

# %%
final_k <- 5

# %%
kmeans_result <- kmeans(data_for_clustering, centers = final_k, nstart = 25)
mall_data$Cluster <- as.factor(kmeans_result$cluster)

# %%
ggplot(mall_data, aes(x = AnnualIncome, y = SpendingScore, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Customer Segments by Annual Income and Spending Score",
       x = "Annual Income (k$)",
       y = "Spending Score (1-100)") +
  theme_minimal()


# %%
cluster_stats <- mall_data %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Avg_Age = mean(Age),
    Avg_Income = mean(AnnualIncome),
    Avg_Spending = mean(SpendingScore),
    Female_Pct = sum(Gender == "Female") / n() * 100
  )

# %%
# Printing Stats
print(cluster_stats)


# %%
ggplot(cluster_stats, aes(x = Avg_Income, y = Avg_Spending, size = Count, color = Cluster)) +
  geom_point() +
  scale_size(range = c(5, 15)) +
  labs(title = "Cluster Characteristics",
       x = "Average Annual Income (k$)",
       y = "Average Spending Score") +
  theme_minimal()

# %%
