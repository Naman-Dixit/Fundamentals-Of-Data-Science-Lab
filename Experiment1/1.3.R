set1 <- as.integer(strsplit(readline("Enter first set of numbers (comma-separated, 1-6): "), ",")[[1]])
set2 <- as.integer(strsplit(readline("Enter second set of numbers (comma-separated, 1-6): "), ",")[[1]])

if (all(set1 %in% 1:6) & all(set2 %in% 1:6)) {
  union_prob <- length(unique(c(set1, set2))) / 6
  cat("Union probability:", union_prob, "\n")
} else {
  cat("Invalid input. Please enter numbers between 1 and 6.\n")
}
