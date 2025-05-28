# Statistical Analysis and Visualization in R

# 1. Summary Statistics with mtcars Dataset
library(dplyr)
library(ggplot2)

# Summary statistics for mpg and hp
summary_stats <- mtcars %>%
  summarise(
    mpg_mean = mean(mpg),
    mpg_median = median(mpg),
    mpg_sd = sd(mpg),
    hp_mean = mean(hp),
    hp_median = median(hp),
    hp_sd = sd(hp)
  )
print("Summary Statistics:")
print(summary_stats)

# Visualization of mpg distribution
ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 10, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Miles per Gallon", 
       x = "Miles per Gallon", 
       y = "Frequency")



