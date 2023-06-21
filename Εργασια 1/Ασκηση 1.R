# load the data from the URL and replace "*" with NA
gifts <- read.table("http://www.math.ntua.gr/~fouskakis/Data_Analysis/Exercises/gifts.txt", header = TRUE,na.strings="*")

# check the structure of the object
str(gifts)

gifts
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

holiday <-factor(holiday)
holiday_freq <- table(holiday)
barplot(holiday_freq)
pie(holiday_freq, main="Holiday Period of Purchases")
legend("topright", c("Christmas", "Easter", "Other"), fill=1:3)

barplot(table(sex), main="Gender Distribution of Survey Participants", xlab="Gender")
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

hist(time, main="Time Spent on Advertising Promotions", xlab="Minutes")
hist(salary, main="Monthly Salary", xlab="Euros")

# compare spend with holiday period
boxplot(spend ~ holiday, 
        main = "Amount Spent by Holiday Season", 
        xlab = "Holiday Season", 
        ylab = "Amount Spent (Euros)")
# compare spend with gender
boxplot(spend ~ sex, data=gifts, main="Amount Spent by Gender")
# compare spend with time spent on advertising
plot(spend ~ time, data=gifts, main="Amount Spent vs. Time Spent on Advertising")

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

# create f_age and f_time variables
gifts$f_age <- cut(gifts$age, c(18,25,38,50,max(gifts$age)+1),right = FALSE,
                   labels = c("[18-25)", "[25-38)", "[38-50)", "[50 and over)"))
gifts$f_time <- cut(gifts$time, quantile(gifts$time, c(0, 0.25, 0.5, 0.75, 1)),
                    labels = c("[0,q1)", "[q1,q2)", "[q2,q3)", "[q3 and above)"))

# frequency correlation table of f_age and f_time
table(gifts$f_age, gifts$f_time)

# stacked bar chart of f_age and f_time
barplot(table(gifts$f_age, gifts$f_time), col=c("red","green","blue","yellow"), xlab="Age Group", ylab="Frequency", main="Frequency of Age Group and Time Spent on Advertising")
legend("topright", legend=c("0-<q1", "q1-<q2", "q2-<q3", "q3->"), fill=c("red","green","blue","yellow"))

f_age <- cut(gifts$age, c(18, 25, 38, 50, Inf), right = FALSE, 
             labels = c("[18-25)", "[25-38)", "[38-50)", "[50 and over)"))

q <- quantile(gifts$time, probs = c(0, 0.25, 0.5, 0.75, 1))

f_time <- cut(gifts$time, q, include.lowest = TRUE,
              labels = c("[0,q1)", "[q1,q2)", "[q2,q3)", "[q3 and above)"))

freq_table <- table(f_age, f_time)
freq_table

prop.table(freq_table)

barplot(freq_table, col = c("red","green","blue","yellow"), 
        xlab = "Age", ylab = "Frequency", 
        main = "Frequency Correlation Table of Age and Daily Time Spent on Advertising Promotions",
        legend.text = TRUE, args.legend = list(title = "Daily Time Spent"))


table(gifts$f_age)


prop.table(table(gifts$f_age))
