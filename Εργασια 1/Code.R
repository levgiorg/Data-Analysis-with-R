# load the data from the URL and replace "*" with NA
gifts <- read.table("http://www.math.ntua.gr/~fouskakis/Data_Analysis/Exercises/gifts.txt", header = TRUE,na.strings="*")

# check the structure of the object
str(gifts)

# remove any rows with missing values or age under 18
gifts <- na.omit(gifts)
gifts <- subset(gifts, age >= 18)

spend <- gifts$spend
age <- gifts$age
holiday <-gifts$holiday
sex <- gifts$sex  
time <- gifts$time
salary <- gifts$salary
# descriptive breakdown of the variables

class(spend)
summary(spend)
hist(spend, main="Amount Spent during Holiday Season", xlab="Euros")
boxplot(spend, main="Amount Spent during Holiday Season", xlab="Euros")
fivenum(spend)

class(age)
summary(age)
hist(age, main="Age Distribution of Survey Participants", xlab="Age")
boxplot(age, main="Age Distribution of Survey Participants", xlab="Age")
fivenum(age)

holiday_freq <- table(gifts$holiday)
pie(holiday_freq, main="Holiday Period of Purchases")
legend("topright", c("Christmas", "Easter", "Other"), fill=1:3)
summary(holiday)
fivenum(holiday)

hist(table(sex), main="Gender Distribution of Survey Participants", xlab="Gender")
table(sex)
prop.table(table(sex))


class(time)
summary(time)
hist(time, main="Daily Time Spent on Advertising Promotions", xlab="Time (minutes)")
boxplot(time, main="Daily Time Spent on Advertising Promotions", xlab="Time (minutes)")
fivenum(time)



class(salary)
summary(salary)
hist(salary, main="Monthly Income Distribution of Survey Participants", xlab="Income (Euros)")
boxplot(salary, main="Monthly Income Distribution of Survey Participants", xlab="Income (Euros)")
fivenum(salary)

# compare spend with holiday period
# compare spend with holiday period
boxplot(spend ~ holiday, 
        main = "Amount Spent by Holiday Season", 
        xlab = "Holiday Season", 
        ylab = "Amount Spent (Euros)")

plot(spend ~ age, data = gifts, main = "Amount Spent by Age",
     xlab = "Age (Years)", ylab = "Amount Spent (Euros)")


boxplot(spend ~ sex, 
        main = "Amount Spent by Gender", 
        xlab = "Gender", 
        ylab = "Amount Spent (Euros)")


plot(spend ~ time,
     main = "Amount Spent by Daily Time Spent on Advertising Promotions", 
     xlab = "Daily Time Spent on Advertising Promotions (min)", 
     ylab = "Amount Spent (Euros)")

plot(spend ~ salary,
     main = "Amount Spent by Monthly Income", 
     xlab = "Monthly Income (Euros)", 
     ylab = "Amount Spent (Euros)")

f_age <- cut(gifts$age, c(18, 25, 38, 50, Inf), right = FALSE, 
             labels = c("[18-25)", "[25-38)", "[38-50)", "[50 and over)"))

q <- quantile(gifts$time, probs = c(0, 0.25, 0.5, 0.75, 1))
f_time <- cut(gifts$time, q, include.lowest = TRUE,
              labels = c("[0,q1)", "[q1,q2)", "[q2,q3)", "[q3 and above)"))\

table(gifts$f_age)

prop.table(table(gifts$f_age))



freq_table <- table(f_age, f_time)
freq_table

prop.table(freq_table)

barplot(freq_table, col = c("red","green","blue","yellow"), 
        xlab = "Age", ylab = "Frequency", 
        main = "Frequency Correlation Table of Age and Daily Time Spent on Advertising Promotions",
        legend.text = TRUE, args.legend = list(title = "Daily Time Spent"))



imitono <- function(n, x) {
  if(!is.numeric(n) || !is.integer(n) || n <= 0) {
    stop("n must be a positive integer")
  }
  
  sign <- c(1, rep(-1, n))
  terms <- sign * (x^(2 * (0:n) + 1) / factorial(2 * (0:n) + 1))
  
  return(sum(terms[1:n]))
}


imitono2 <- function(n, x) {
  
  if(!is.numeric(n) || n < 0 || n != round(n)) {
    stop("n must be a positive integer")
  }
  
  calc_sum <- function(i) {
    if(i == 0) {
      return(x)
    } else {
      term <- (-1)^(i) * x^(2*i+1) / factorial(2*i+1)
      return(term + calc_sum(i-1))
    }
  }
  
  result <- calc_sum(n)
  
  return(result)
}

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

result2 <- imitono3(11, 0.5)
result2

x <- seq(0, 2*pi, length.out=1000)

plot(x, sin(x), type="l", col="blue", xlab="x", ylab="y")

for(n in 1:10) {
  lines(x, imitono2(n, x), col=rainbow(10)[n], lty=2)
}

legend("topright", legend=c("sin(x)", paste0("imitono2(", 1:10, ",x)")), 
       col=c("blue", rainbow(10)), lty=c(1, rep(2, 10)))


imitono3 <- function(x) {
  tol <- 0.001
  counter <- 0
  n <- 0
  imitono_old <- 0
  imitono3 <- x  
  err <- abs(sin(x) - imitono3)  
  
  while (err > tol) {
    n <- n + 1
    imitono_old <- imitono3
    imitono3 <- imitono_old + ((-1)^n * x^(2*n+1)) / factorial(2*n+1)
    err <- abs(sin(x) - imitono3)
    counter <- counter + 1
  }
  
  cat("The approximation of sin(", x, ") using the Taylor series with a maximum error tolerance of", tol, "is:", imitono3, "\n")
  cat("The approximation took", counter, "iterations to achieve the specified error tolerance of", tol, "\n")
}

legend("topright", legend=c("sin(x)", paste0("imitono2(", 1:10, ",x)")), 
       col=c("blue", rainbow(10)), lty=1)