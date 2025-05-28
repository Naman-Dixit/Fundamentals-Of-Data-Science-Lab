n <- as.integer(readline("Enter the number of times to roll the die: "))
rolls <- sample(1:6, n, replace = TRUE)
probabilities <- table(rolls) / n
print(probabilities)
