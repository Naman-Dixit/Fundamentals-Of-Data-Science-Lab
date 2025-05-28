num1 <- as.integer(readline("Enter first number (1-6): "))
num2 <- as.integer(readline("Enter second number (1-6): "))
if (num1 >= 1 & num1 <= 6 & num2 >= 1 & num2 <= 6) {
  prob <- (1/6) * (1/6)
  cat("Probability of rolling", num1, "and", num2, "is", prob, "\n")
} else {
  cat("Invalid input. Please enter numbers between 1 and 6.\n")
}
