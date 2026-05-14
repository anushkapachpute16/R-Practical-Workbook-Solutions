#ASSIGNMENT NO.6: Data Aggregation, Cross-Tabulation,Exploring Frequency and Contingency Tables in R

#Use customer_data.csv to solve following questions
#Load Dataset
customer_data <- read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/customer_data.csv")

# Q1) Sales Performance Analysis

# a) Total sales by region
aggregate(Amount ~ Region, customer_data, sum)

# b) Average sales per gender
aggregate(Amount ~ Gender, customer_data, mean)

# c) Frequency distribution of purchase
table(customer_data$Purchase)

# d) Cross-tab: Region vs Purchase
table(customer_data$Region, customer_data$Purchase)

# e) Chi-square test
chisq.test(table(customer_data$Region, customer_data$Purchase))

# Q2) Customer Segmentation

# a) Count customers by region
table(customer_data$Region)

# b) Average purchase by region using tapply
tapply(customer_data$Amount, customer_data$Region, mean)

# c) Frequency of gender
table(customer_data$Gender)

# d) Cross-tab: Gender vs Region
table(customer_data$Gender, customer_data$Region)

# e) Chi-square test
chisq.test(table(customer_data$Gender, customer_data$Region))

# Q3) Purchase Pattern Analysis

# a) Total purchase amount
sum(customer_data$Amount)

# b) Row-wise sum using apply
numeric_data <- data.frame(customer_data$Amount)
apply(numeric_data, 1, sum)

# c) Frequency of purchase
table(customer_data$Purchase)

# d) Cross-tab: Purchase vs Region
table(customer_data$Purchase, customer_data$Region)

# e) Chi-square test
chisq.test(table(customer_data$Purchase, customer_data$Region))

# Q4) High-Value Customer Analysis

# a) Filter customers with Amount > 5000
high_value <- customer_data[customer_data$Amount > 5000, ]
high_value

# b) Mean purchase of filtered group
mean(high_value$Amount)

# c) Frequency of region (filtered)
table(high_value$Region)

# d) Cross-tab: Gender vs Purchase (filtered)
table(high_value$Gender, high_value$Purchase)

# e) Chi-square test
chisq.test(table(high_value$Gender, high_value$Purchase))

# Q5) Business Decision Analysis

# a) Total revenue
sum(customer_data$Amount)

# b) Average revenue per region
aggregate(Amount ~ Region, customer_data, mean)

# c) Frequency distribution of regions
table(customer_data$Region)

# d) Cross-tab: Region vs Purchase
table(customer_data$Region, customer_data$Purchase)

# e) Statistical inference
chisq.test(table(customer_data$Region, customer_data$Purchase))
