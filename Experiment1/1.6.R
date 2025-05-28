mean_val <- as.numeric(readline("Enter mean: "))
sd_val <- as.numeric(readline("Enter standard deviation: "))

random_values <- rnorm(1000, mean = mean_val, sd = sd_val)
hist(random_values, main = "Histogram of Generated Normal Values", col = "blue")
