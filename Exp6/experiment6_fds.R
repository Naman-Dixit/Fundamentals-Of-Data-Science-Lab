library(ggplot2)
library(dplyr)
 
df <- read.csv("C:\\Users\\naman\\Downloads\\Mall_Customers.csv")
 
head(df)
str(df)
summary(df)
 
colSums(is.na(df))
 
table(df$Gender)
 
ggplot(df, aes(x = Age, y = `Spending.Score..1.100.`, color = Gender)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Age vs Spending Score", x = "Age", y = "Spending Score") +
  theme_minimal()

ggplot(df, aes(x = `Annual.Income..k..`, y = `Spending.Score..1.100.`, color = Gender)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Annual Income vs Spending Score", x = "Annual Income (k$)", y = "Spending Score") +
  theme_minimal()

 
ggplot(df, aes(x = Age, y = Annual.Income..k.., color = Gender)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Age vs Annual Income", x = "Age", y = "Annual Income (k$)") +
  theme_minimal()
 
#  Exercise1(ii)
data_for_clustering <- df[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]
 
scaled_data <- scale(data_for_clustering)

#Elbow method

wss <- vector()   

for (k in 1:10) {
  kmeans_result <- kmeans(scaled_data, centers = k, nstart = 10)
  wss[k] <- kmeans_result$tot.withinss
}

# Plot the elbow curve
plot(1:10, wss, type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")




library(cluster)

avg_sil <- c()

for (k in 2:10) {
  km_res <- kmeans(scaled_data, centers = k, nstart = 10)
  ss <- silhouette(km_res$cluster, dist(scaled_data))
  avg_sil[k] <- mean(ss[, 3])
}

# Plot Silhouette scores
plot(2:10, avg_sil[2:10], type = "b", pch = 19,
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Method for Optimal k")

 
library(ggplot2)
 
data_for_cluster <- df[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")]
 
scaled_data <- scale(data_for_cluster)
 
set.seed(42)
kmeans_result <- kmeans(scaled_data, centers = 5, nstart = 25)
 
df$Cluster <- as.factor(kmeans_result$cluster)
 
ggplot(df, aes(x = `Annual.Income..k..`, y = `Spending.Score..1.100.`, color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Customer Segments using K-Means Clustering",
    x = "Annual Income (k$)",
    y = "Spending Score"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")












 
segment_summary <- aggregate(
  df[, c("Age", "Annual.Income..k..", "Spending.Score..1.100.")],
  by = list(Cluster = df$Cluster),
  FUN = mean
)
 
segment_summary$Age <- round(segment_summary$Age, 1)
segment_summary$Annual.Income..k.. <- round(segment_summary$Annual.Income..k.., 1)
segment_summary$Spending.Score..1.100. <- round(segment_summary$Spending.Score..1.100., 1)
 
for (i in 1:nrow(segment_summary)) {
  cat("\n-------------------------\n")
  cat("Segment:", segment_summary$Cluster[i], "\n")
  cat("Avg Age:", segment_summary$Age[i], "\n")
  cat("Avg Annual Income (k$):", segment_summary$Annual.Income..k..[i], "\n")
  cat("Avg Spending Score:", segment_summary$Spending.Score..1.100.[i], "\n")
   
  income <- segment_summary$Annual.Income..k..[i]
  score <- segment_summary$Spending.Score..1.100.[i]
  
  if (income > 70 && score > 60) {
    cat("Insight: High income, high spending — likely luxury shoppers.\n")
  } else if (income > 70 && score < 40) {
    cat("Insight: High income, low spending — possibly conservative or selective.\n")
  } else if (income < 40 && score > 60) {
    cat("Insight: Low income, high spending — impulsive or trend-focused shoppers.\n")
  } else if (score < 30) {
    cat("Insight: Low spenders — may shop only when necessary.\n")
  } else {
    cat("Insight: Balanced shoppers — moderate income and spending.\n")
  }
}








#exercise 2


 

 
library(readr)
library(dplyr)
library(ggplot2)
 
happiness_data <- read_csv("C:\\Users\\naman\\Downloads\\clean_combined_data_with_year.csv")
 
clustering_data <- happiness_data %>%
  select(Score, GDP, `Social support`, `Healthy life expectancy`,
         Freedom, Generosity, `Perceptions of corruption`) %>%
  na.omit()  
 
scaled_data <- scale(clustering_data)
 
set.seed(123)  
kmeans_result <- kmeans(scaled_data, centers = 3)
 
clustering_data$Cluster <- kmeans_result$cluster
 
head(clustering_data)
table(clustering_data$Cluster)

 






 
library(readr)
library(dplyr)
library(cluster)      
library(factoextra)   
 
happiness_data <- read_csv("C:\\Users\\naman\\Downloads\\clean_combined_data_with_year.csv")
 
data_for_clustering <- happiness_data %>%
  select(Score, GDP, `Social support`, `Healthy life expectancy`,
         Freedom, Generosity, `Perceptions of corruption`) %>%
  na.omit()
 
scaled_data <- scale(data_for_clustering)

#  Elbow Method  
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal K")

#   Silhouette Method
fviz_nbclust(scaled_data, kmeans, method = "silhouette") +
  labs(title = "Silhouette Score for Optimal K")










 
library(readr)
library(dplyr)
library(ggplot2)
library(factoextra)
 
happiness_data <- read_csv("C:\\Users\\naman\\Downloads\\clean_combined_data_with_year.csv")
 
clustering_data <- happiness_data %>%
  select(Score, GDP, `Social support`, `Healthy life expectancy`,
         Freedom, Generosity, `Perceptions of corruption`) %>%
  na.omit()
 
scaled_data <- scale(clustering_data)
  
set.seed(123)
k <- 3  
kmeans_result <- kmeans(scaled_data, centers = k)
  
fviz_cluster(kmeans_result, data = scaled_data,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = paste("K-Means Clustering with", k, "Clusters"))











 
clustered_data <- clustering_data
clustered_data$Cluster <- kmeans_result$cluster
 
cluster_summary <- clustered_data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Score = mean(Score),
    Avg_GDP = mean(GDP),
    Avg_Support = mean(`Social support`),
    Avg_LifeExp = mean(`Healthy life expectancy`),
    Avg_Freedom = mean(Freedom),
    Avg_Generosity = mean(Generosity),
    Avg_Corruption = mean(`Perceptions of corruption`)
  )

print(cluster_summary)








#Exercise 3



 
library(readr)
library(dplyr)
library(ggplot2)
library(factoextra)
 
happiness_data <- read_csv("C:\\Users\\naman\\Downloads\\clean_combined_data_with_year.csv")
 
data_for_clustering <- happiness_data %>%
  select(Score, GDP, `Social support`, `Healthy life expectancy`,
         Freedom, Generosity, `Perceptions of corruption`) %>%
  na.omit()
 
scaled_data <- scale(data_for_clustering)
 
for (k in 2:6) {
  set.seed(123)  
  km_result <- kmeans(scaled_data, centers = k, nstart = 25)
   
  print(fviz_cluster(km_result, data = scaled_data,
                     geom = "point",
                     main = paste("K-Means Clustering with K =", k),
                     palette = "jco",
                     ellipse.type = "convex",
                     ggtheme = theme_minimal()))
}












 
library(readr)
library(dplyr)
library(cluster)
library(factoextra)
 
happiness_data <- read_csv("C:\\Users\\naman\\Downloads\\clean_combined_data_with_year .csv")
 
data_for_clustering <- happiness_data %>%
  select(Score, GDP, `Social support`, `Healthy life expectancy`,
         Freedom, Generosity, `Perceptions of corruption`) %>%
  na.omit()

scaled_data <- scale(data_for_clustering)
 
wcss_values <- c()
silhouette_scores <- c()
 
for (k in 2:10) {
  set.seed(123)
  km_model <- kmeans(scaled_data, centers = k, nstart = 25)
   
  wcss_values[k - 1] <- km_model$tot.withinss
   
  sil <- silhouette(km_model$cluster, dist(scaled_data))
  silhouette_scores[k - 1] <- mean(sil[, 3])
}
 
plot(2:10, wcss_values, type = "b", pch = 19, col = "red",
     xlab = "Number of Clusters (k)",
     ylab = "WCSS (Within-Cluster Sum of Squares)",
     main = "Elbow Method - WCSS vs k")
 
plot(2:10, silhouette_scores, type = "b", pch = 19, col = "darkgreen",
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette Score",
     main = "Silhouette Score vs k")










 
library(readr)
library(dplyr)
library(factoextra)
 
happiness_data <- read_csv("C:\\Users\\naman\\Downloads\\clean_combined_data_with_year.csv")
 
clustering_data <- happiness_data %>%
  select(Score, GDP, `Social support`, `Healthy life expectancy`,
         Freedom, Generosity, `Perceptions of corruption`) %>%
  na.omit()
 
scaled_data <- scale(clustering_data)
 
for (k in 2:6) {
  set.seed(123)
  km <- kmeans(scaled_data, centers = k, nstart = 25)
  
  print(fviz_cluster(km, data = scaled_data,
                     geom = "point",
                     ellipse.type = "convex",
                     main = paste("Clustering with k =", k),
                     palette = "jco",
                     ggtheme = theme_minimal()))
}








