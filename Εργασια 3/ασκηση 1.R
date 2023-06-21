gifts <-read.table("http://www.math.ntua.gr/~fouskakis/Data_Analysis/Exercises/gifts.txt", header = TRUE,na.strings="*")

str(gifts)
gifts <- na.omit(gifts)
gifts <- subset(gifts, age >= 18)

spend <- gifts$spend
age <- gifts$age
holiday <-gifts$holiday
sex <- gifts$sex  
time <- gifts$time
salary <- gifts$salary


cor_spend_age <- cor(spend, age)
cat("Correlation between spend and age:", correlation, "\n")

cor_spend_time <- cor(spend, time)
cat("Correlation between spend and time:", cor_spend_time, "\n")

cor_spend_salary <- cor(spend, salary)
cat("Correlation between spend and salary:", cor_spend_salary, "\n")

cor_age_time <- cor(age, time)
cat("Correlation between age and time:", cor_age_time, "\n")

cor_age_salary <- cor(age, salary)
cat("Correlation between age and salary:", cor_age_salary, "\n")

cor_time_salary <- cor(time, salary)
cat("Correlation between time and salary:", cor_time_salary, "\n")


# Fit the multiple linear regression model
model <- lm(spend ~ age + holiday + sex + time + salary, data = gifts)


model
# Get the 95% confidence intervals for the coefficients
conf_intervals <- confint(model, level = 0.95)

# Print the confidence intervals
print(conf_intervals)

summary(model)

levels(gifts$holiday)

# Check residuals vs fitted values (for homoscedasticity)
plot(model$residuals ~ model$fitted.values)
abline(h = 0, lty = 2)

# Check Normal Q-Q plot (for normality)
qqnorm(model$residuals)
qqline(model$residuals)

# Check Scale-Location plot (for equal variance of residuals)
plot(sqrt(abs(rstandard(model))) ~ model$fitted.values)
abline(h = 0, lty = 2)

acf(residuals(model))


# Create a new data frame for the prediction
newdata <- data.frame(
  age = 25,
  holiday = "Easter",
  sex = "Woman",
  time = 13,
  salary = 575
)

# Estimate the spending
estimate <- predict(model, newdata = newdata, interval = "confidence", level = 0.90)

# Print the estimate
print(estimate)

# Create new variables for the log transformations
log_spend <- log(gifts$spend)
log_age <- log(gifts$age)
log_time <- log(gifts$time)
log_salary <- log(gifts$salary)

# Fit the multiple linear regression model with log transformed variables
log_model <- lm(log_spend ~ log_age + holiday + sex + log_time + log_salary, data = gifts)

# Print the summary of the model
summary(log_model)


plot(log_model$fitted.values, log_model$residuals, main = "(1) Residuals vs Fitted values")
abline(h = 0, lty = 2)

qqnorm(log_model$residuals, main = "(2) Normal Q-Q Plot")
qqline(log_model$residuals)


plot(log_model$fitted.values, sqrt(abs(log_model$residuals)), main = "(3) Scale-Location Plot")

plot(hatvalues(log_model), log_model$residuals, main = "Residuals vs Leverage")


# Create a new data frame for the prediction
newdata1 <- data.frame(
  age = rep(log(25), nrow(gifts)),
  holiday = rep("Easter", nrow(gifts)),
  sex = rep("Woman", nrow(gifts)),
  time = rep(log(13), nrow(gifts)),
  salary = rep(log(575), nrow(gifts))
)

# Estimate the spending using the multiplicative model
estimate <- exp(predict(log_model, newdata = newdata1, interval = "confidence", level = 0.90))

# Print the estimate and confidence interval
print(estimate)


