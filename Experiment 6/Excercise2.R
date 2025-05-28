library(tidyverse)
library(cluster)


# %%
set.seed(1)
# %%

happiness <- read_csv("C:/Users/naman/OneDrive/Desktop/Data science lab/New folder/Experiments/Experiment 6/archive(11)/2019.csv")


# %%
head(happiness)
summary(happiness)

# %%
# Filtering out the text cols
features <- c("Overall rank", "Score", "GDP per capita", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Generosity", "Perceptions of corruption")

happiness <- happiness %>%
  select(all_of(features)) %>%
  na.omit()

# %%

features <- c("Overall rank", "Country", "Score", "GDP per capita", "Social support", "Healthy life expectancy", "Freedom to make life choices", "Generosity", "Perceptions of corruption")

happiness_country <- happiness %>%
  select(all_of(features))
# %%
#SCAAAAAAAAAAAAAAAALIng
happiness_scaled <- scale(happiness)

# %%
# total within-cluster sum of squares
wss <- function(k) {
  kmeans(happiness_scaled, k, nstart = 10)$tot.withinss
}


# %%
k_values <- 1:10
wss_values <- map_dbl(k_values, wss)

# %%

library(ggplot2)
ggplot(data.frame(k = k_values, wss = wss_values), aes(k, wss)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = k_values) +
  labs(title = "Elbow Method for Optimal Number of Clusters",
       x = "Number of clusters K",
       y = "Total within-cluster sum of squares") +
  theme_minimal()

# %%
avg_sil <- function(k) {
  km.res <- kmeans(happiness_scaled, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(happiness_scaled))
  mean(ss[, 3])
}

# %%
k_values <- 2:10
avg_sil_values <- map_dbl(k_values, avg_sil)

# %%

ggplot(data.frame(k = k_values, silhouette = avg_sil_values), aes(k, silhouette)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = k_values) +
  labs(title = "Silhouette Method for Optimal Number of Clusters",
       x = "Number of clusters K",
       y = "Average Silhouette Width") +
  theme_minimal()
# %%

optimal_k <- 5


# %%
kmeans_result <- kmeans(happiness_scaled, centers = optimal_k, nstart = 25)


# %%
# Add cluster assignments to original data
happiness$Cluster <- as.factor(kmeans_result$cluster)

# %%
# Perform PCA for visualization
pca_result <- prcomp(happiness_scaled, scale. = TRUE)
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df$Cluster <- happiness$Cluster

# %%
# Plot clusters in PCA space
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Country Clusters Based on Happiness Factors",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()
# %%
# Prepare data for parallel coordinates plot
cluster_means <- happiness %>%
  group_by(Cluster) %>%
  summarise(across(where(is.numeric), mean))

# %%
cluster_means_long <- cluster_means %>%
  pivot_longer(cols = -Cluster, names_to = "Feature", values_to = "Mean_Value")

# %%

ggplot(cluster_means_long, aes(x = Feature, y = Mean_Value, group = Cluster, color = Cluster)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  labs(title = "Cluster Characteristics Across Happiness Factors",
       y = "Standardized Mean Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# %%
# Calculate and display cluster characteristics
cluster_profiles <- happiness %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Avg_GDP = mean(`GDP per capita`, na.rm = TRUE),
    Avg_Social = mean(`Social support`, na.rm = TRUE),
    Avg_Health = mean(`Healthy life expectancy`, na.rm = TRUE),
    Avg_Freedom = mean(`Freedom to make life choices`, na.rm = TRUE),
    Avg_Generosity = mean(Generosity, na.rm = TRUE),
    Avg_Corruption = mean(`Perceptions of corruption`, na.rm = TRUE)
  )

# %%

# Print cluster profiles
print(cluster_profiles)
# %%
cluster_profiles_long <- cluster_profiles %>%
  select(-Count) %>%
  pivot_longer(cols = -Cluster, names_to = "Feature", values_to = "Mean_Value")

# %%
ggplot(cluster_profiles_long, aes(x = Feature, y = Mean_Value, fill = as.factor(Cluster))) +
  geom_col(position = "dodge") +
  labs(title = "Average Feature Values by Cluster",
       y = "Mean Value",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# %%
