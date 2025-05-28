cost <- as.numeric(readline("Enter cost of game: "))
outcomes <- as.numeric(strsplit(readline("Enter possible outcomes (comma-separated): "), ",")[[1]])
probabilities <- as.numeric(strsplit(readline("Enter their probabilities (comma-separated): "), ",")[[1]])

if (sum(probabilities) == 1) {
  expected_value <- sum(outcomes * probabilities) - cost
  cat("Expected value of the game is:", expected_value, "\n")
} else {
  cat("Error: Probabilities must sum to 1.\n")
}
