prior <- as.numeric(readline("Enter prior probability (as a decimal): "))
sensitivity <- as.numeric(readline("Enter sensitivity (as a decimal): "))
false_positive <- as.numeric(readline("Enter false positive rate (as a decimal): "))

posterior <- (sensitivity * prior) / ((sensitivity * prior) + ((1 - prior) * false_positive))
cat("Probability of having the disease given a positive test is", posterior, "\n")
