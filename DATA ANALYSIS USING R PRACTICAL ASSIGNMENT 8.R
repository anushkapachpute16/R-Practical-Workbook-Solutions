#ASSIGNMENT NO.8: Working with CSV and Excel Files in R Importing Data from CSV

Questions:
  Use data.xlsx to solve following questions

#Retail Sales Analysis
Dataset: sales.xlsx (Sheet: sales)
library(readxl)
library(dplyr)

# Load dataset
sales_data <- read_excel("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/sales_data.xlsx")


#Q1) A Retail Sales Analysis (Dataset: sales.csv)
# a) Load the sales dataset and display the first 10 records


# Display first 10 records
head(sales_data, 10)

# Explanation:
# head() function displays top records from dataset.
# 10 specifies first 10 rows.

# b) Calculate total revenue for each product

library(dplyr)

# Create Revenue Column
sales_data$Revenue <- sales_data$Quantity * sales_data$Price

# Total Revenue by Product
total_revenue <- sales_data %>%
  group_by(Product) %>%
  summarise(Total_Revenue = sum(Revenue))

total_revenue

# Explanation:
# Revenue = Quantity × Price
# group_by(Product) groups product-wise data.
# summarise() calculates total revenue.

# c) Top 5 products with highest revenue

total_revenue <- aggregate(Revenue ~ Product,
                           sales_data,
                           sum)

top5_products <- total_revenue[order(-total_revenue$Revenue), ]

head(top5_products, 5)

# d) Filter sales where quantity sold is greater than 50

sales_data[sales_data$Quantity > 50, ]

# e) Find average product price by category

aggregate(Price ~ Category,
          sales_data,
          mean)

#Q2) The E-Commerce Customer Analysis (Dataset: customers.csv)
# Load Dataset
customer_data <- read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/customer_data.csv")

#a) Display first 10 records
head(customer_data, 10)

# Explanation:
# head() function displays the first rows of the dataset.
# 10 specifies first 10 records.


# b) Calculate average purchase amount

avg_purchase <- mean(customer_data$Amount)
avg_purchase

# Explanation:
# mean() calculates average purchase amount of all customers.

# c) Find customers above average purchase amount

above_avg <- subset(customer_data,
                    Amount > avg_purchase)
above_avg

# Explanation:
# subset() selects customers whose purchase amount is greater than average.

# d) Count number of male and female customers

gender_count <- table(customer_data$Gender)
gender_count

# Explanation:
# table() counts frequency of each gender category.

# e) Create age groups: Youth (<25), Adult (25–50), Senior (>50)

customer_data$AgeGroup <- ifelse(customer_data$Age < 25,
                                 "Youth",
                                 ifelse(customer_data$Age <= 50,
                                        "Adult",
                                        "Senior"))

# Display Dataset
head(customer_data)

# Explanation:
# Youth → Age below 25
# Adult → Age between 25 and 50
# Senior → Age above 50

# f) Find top 10 highest spending customers

top_customers <- head(customer_data[order(-customer_data$Amount), ], 10)
top_customers

# Explanation:
# order(-Amount) sorts customers by highest purchase amount.
# head(10) returns top 10 spending customers.

#Q3) The Banking Transaction Analysis (Dataset: transactions.csv)

#a) Load the transactions dataset and display the first 10 records. 
transactions_data<- read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/transcations.csv")

# Display First 10 Records
head(transactions_data, 10)

# Explanation:
# head() displays the top rows of the dataset.
# 10 specifies first 10 records.

# b) Calculate total deposits

library(dplyr)
total_deposits <- transactions_data %>%
  filter(TransactionType == "Deposit") %>%
  summarise(Total_Deposit = sum(Amount))

total_deposits

# Explanation:
# Filters only deposit transactions.
# sum() calculates total deposited amount.

# c) Calculate total withdrawals

total_withdrawals <- transactions_data %>%
  filter(TransactionType == "Withdrawal") %>%
  summarise(Total_Withdrawal = sum(Amount))
total_withdrawals

# Explanation:
# Filters withdrawal transactions.
# Calculates total withdrawn amount.

# d) Find accounts with transactions above 10000

high_transactions <- transactions_data %>%
  filter(Amount > 10000)

high_transactions

# Explanation:
# Displays accounts where transaction amount exceeds 10000.

# e) Calculate average transaction amount

avg_transaction <- mean(transactions_data$Amount)
avg_transaction

# Explanation:
# mean() calculates average transaction amount.

# f) Count number of transactions by type

transaction_count <- table(transactions_data$TransactionType)
transaction_count

# Explanation:
# table() counts frequency of each transaction type.
# Example: Deposit, Withdrawal, Transfer, etc.


#Q4) The Healthcare Patient Dataset (patients.csv)

# a) Load the patient’s dataset and display the first 10 records

# Load dataset
patients_data<- read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/patients.csv")

# Display First 10 Records
head(patients_data, 10)

# Explanation:
# head() displays top rows of dataset.
# 10 specifies first 10 patient records.

# b) Find patients with high blood pressure (>140)

library(dplyr)

high_bp <- patients_data %>%
  filter(BloodPressure > 140)
high_bp

# Explanation:
# filter() selects patients having blood pressure greater than 140.

# c) Detect patients with fever (>37°C)

fever_patients <- patients_data %>%
  filter(Temperature > 37)
fever_patients

# Explanation:
# Filters patients whose body temperature exceeds 37°C.

# d) Calculate average patient age

avg_age <- mean(patients_data$Age)
avg_age

# Explanation:
# mean() calculates average age of patients.

# e) Find maximum and minimum blood pressure

# Maximum Blood Pressure
max_bp <- max(patients_data$BloodPressure)

# Minimum Blood Pressure
min_bp <- min(patients_data$BloodPressure)
max_bp
min_bp

# Explanation:
# max() returns highest blood pressure.
# min() returns lowest blood pressure.

# f) Count number of patients above 60 years

senior_patients <- patients_data %>%
  filter(Age > 60)

# Count Patients
nrow(senior_patients)

# Explanation:
# filter() selects patients above 60 years.
# nrow() counts total such patients.


#Q5) The Student Performance Dataset (students.csv)

#a) Load the students dataset and display the first 10 records.

students_data<-read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/students.csv")

# Display first 10 records
head(students_data, 10)

# Explanation:
# head() function displays top records from dataset.
# 10 specifies first 10 rows.

# b) Find students who scored above 80

# Students scoring above 80
high_scorers <- students_data %>%
  filter(Marks > 80)

high_scorers

# Explanation:
# filter() function selects rows based on condition.
# Here it selects students with Marks greater than 80.

# c) Calculate average marks by subject

# Average marks by subject
avg_marks <- students_data %>%
  group_by(Subject) %>%
  summarise(Average_Marks = mean(Marks))
avg_marks

# Explanation:
# group_by() groups data based on Subject.
# mean() calculates average marks for each subject.

# d) Find top scoring student

# Top scoring student
top_student <- students_data %>%
  arrange(desc(Marks)) %>%
  head(1)
top_student

# Explanation:
# arrange(desc(Marks)) sorts data in descending order of marks.
# head(1) selects the highest scoring student.

# e) Find students who failed (Marks < 40)

# Failed students
failed_students <- students_data %>%
  filter(Marks < 40)
failed_students

# Explanation:
# filter() selects students with marks less than 40.

# f) Count number of students in each subject

# Student count by subject
subject_count <- table(students_data$Subject)
subject_count

# Explanation:
# table() counts frequency of each subject in dataset.

#Q6) The Social Media Analytics Dataset (posts.csv)


# a) Load the posts dataset and display the first 10 records

posts_data<-read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/posts.csv")

# Display First 10 Records
head(posts_data, 10)

# Explanation:
# head() displays the top rows of the dataset.
# 10 specifies first 10 records.

# b) Calculate total engagement per post

# Total Engagement
posts_data$Total_Engagement <- posts_data$Likes +
  posts_data$Comments +
  posts_data$Shares

# Display Dataset
head(posts_data)

# Explanation:
# Total Engagement = Likes + Comments + Shares

# c) Find posts with engagement > 500

# High Engagement Posts
high_engagement <- posts_data %>%
  filter(Total_Engagement > 500)
high_engagement

# Explanation:
# filter() selects posts with engagement greater than 500.

# d) Find most liked post

# Most Liked Post
most_liked <- posts_data %>%
  arrange(desc(Likes)) %>%
  head(1)

most_liked

# Explanation:
# arrange(desc(Likes)) sorts posts by Likes in descending order.
# head(1) returns the highest liked post.

# e) Calculate average engagement

# Average Engagement
avg_engagement <- mean(posts_data$Total_Engagement)
avg_engagement

# Explanation:
# mean() calculates average engagement across all posts.

# f) Find posts with low engagement (<100)

# Low Engagement Posts
low_engagement <- posts_data %>%
  filter(Total_Engagement < 100)
low_engagement

# Explanation:
# filter() selects posts with Total Engagement less than 100.

