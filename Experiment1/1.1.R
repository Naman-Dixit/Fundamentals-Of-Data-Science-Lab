die_face <- as.integer(readline("Enter a die face (1-6): "))
if (die_face >= 1 & die_face <= 6) {
  prob <- 1/6
  cat("Probability of rolling", die_face, "is", prob, "\n")
} else {
  cat("Invalid input. Please enter a number between 1 and 6.\n")
}
