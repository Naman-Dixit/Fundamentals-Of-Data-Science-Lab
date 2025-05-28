lambda <- as.numeric(readline("Enter average rate (lambda): "))
k <- as.integer(readline("Enter specific number of events (k): "))

poisson_prob <- dpois(k, lambda)
cat("Poisson probability of", k, "events occurring is", poisson_prob, "\n")
