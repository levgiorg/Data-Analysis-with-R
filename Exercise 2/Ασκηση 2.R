p <- seq(0.001, 1, length = 10000)
geom_loglikelihood <- function(data, p) {
  results <- rep(NA, 10000)
  for (i in 1:10000) {
    results[i] <- sum(dgeom(data - 1, p[i], log = TRUE))
  }
  return(results)
}
x <- c(3, 2, 1, 5, 2, 4, 5, 2, 3, 10) 
results <- geom_loglikelihood(x, p)
plot(p, results, xlab = "Probability (p)", ylab = "Log-Likelihood", type = "l")

p_results <- round(p[order(results)[10000]], 4)
theoretical_p= round(1/mean(x),4)

three_dice_rolls <- function(N) {
if (!is.numeric(N) || N <= 0 || round(N) != N) {

stop("N must be a positive integer.")
}

success_count <- 0
for (i in 1:N) {

dice_rolls <- sample(1:6, size = 3, replace = TRUE)

if (dice_rolls[1] == dice_rolls[3] && dice_rolls[1] != dice_rolls[2]) {

success_count <- success_count + 1
}

}

relative_frequency <- success_count / N

return(relative_frequency)
}


