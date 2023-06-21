imitono <- function(n, x) {
  # check if n is a natural number
  if(!is.numeric(n) || n < 0 || n != round(n)) {
    stop("n must be a positive integer")
  }
  
  # calculate the series expansion
  sign <- c(1, rep(-1, n))
  terms <- sign * (x^(2 * (0:n) + 1) / factorial(2 * (0:n) + 1))
  
  # return the sum of the first n terms
  return(sum(terms[1:n]))
}


result1 <- imitono(11, 0.5)
result1

sin(0.5)



imitono2 <- function(n, x) {
  # check if n is a natural number
  if(!is.numeric(n) || n < 0 || n != round(n)) {
    stop("n must be a positive integer")
  }
  
  # compute series expansion using a recursive function
  calc_sum <- function(i) {
    if(i == 0) {
      return(x)
    } else {
      term <- (-1)^(i) * x^(2*i+1) / factorial(2*i+1)
      return(term + calc_sum(i-1))
    }
  }
  
  # call calc_sum to compute the result
  result <- calc_sum(n)
  
  # return the result
  return(result)
}

result2 <- imitono2(4, 0.5)
result2
sin(0.5)

x <- seq(0, 2*pi, by = 0.01)


# Generate values for sin(x) and imitono(x, 10)
y1 <- sin(x)
y2 <- numeric(length(x))
for (i in 1:length(x)) {
  y2[i] <- imitono(50, x[i])
}

# Create a plot and add the two sets of points
plot(x, y1, type = "l", col = "blue", xlab = "x", ylab = "y")
lines(x, y2, col = "red")



x <- seq(0, 2*pi, length.out = 1000)
y1 <- sin(x)
y2 <- imitono(10, x)

plot(x, y1, type = "l", col = "blue")
lines(x, y2, col = "red")

  