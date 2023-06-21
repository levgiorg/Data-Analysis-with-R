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

boxplot(gifts$spend, ylab = "Spending", main = "Boxplot of Spending")
fivenum(gifts$spend)


hist(gifts$spend, xlab = "Spending", ylab = "Frequency", main = "Histogram of Spending")
qqnorm(gifts$spend, main = "Normal Q-Q Plot of Spending", ylab = "Sample Quantiles", xlab = "Theoretical Quantiles")
qqline(gifts$spend)


shapiro.test(gifts$spend)

t.test(gifts$spend, mu = 500, alternative = "greater", conf.level = 0.99)


gifts$Christmas <- ifelse(gifts$holiday == "Christmas", 1, 0)

non_christmas_spend <- gifts[gifts$Christmas == 0, ]$spend
summary(non_christmas_spend)

christmas_spend <- gifts[gifts$Christmas == 1, ]$spend
summary(christmas_spend)

boxplot(spend ~ Christmas, data = gifts, main = "Expenditure by festive period", xlab = "Festive period", ylab = "Expenditure", names = c("Non-Christmas", "Christmas"))

t.test(spend ~ Christmas, data = gifts, alternative = "two.sided")

t.test(spend ~ sex, data = gifts, alternative = "two.sided" , conf.level = 0.90)

gifts$spend_bin <- ifelse(gifts$spend < 673, 0, 1)

cont_table <- table(gifts$sex, gifts$spend_bin)

chi_result <- chisq.test(cont_table)

chi_result




cont_table <- table(gifts$sex, gifts$holiday)

chi_result1 <- chisq.test(cont_table)

chi_result1


boxplot(non_christmas_spend, ylab = "Spending", main = "Boxplot of Spending (Non-Christmas)")

hist(non_christmas_spend, xlab = "Spending", ylab = "Frequency", main = "Histogram of Spending (Non-Christmas)")

boxplot(christmas_spend, ylab = "Spending", main = "Boxplot of Spending (Christmas)")

hist(christmas_spend, xlab = "Spending", ylab = "Frequency", main = "Histogram of Spending (Christmas)")


fivenum(christmas_spend)

fivenum(non_christmas_spend)
