#ASSIGNMENT NO.9: Introduction to S3 and S4 Classes and Using  R Objects and References

#Q1) Customer Object Modeling (S3 System)
#Scenario: A retail company wants to convert customer records into structured objects

#Load Dataset
oop_customer_data <- read_excel("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/oop_customer_data.xlsx")

# Q1 Customer Object Modeling (S3 System)
# Scenario: A retail company wants to convert customer records into structured objects.

# 1 Create an S3 class "Customer" using dataset rows

oop_customer_data <- data.frame(
  CustomerID = c("C101","C102","C103","C104","C105"),
  Name = c("Rahul","Ananya","Aarav","Diya","Vihaan"),
  Age = c(30,25,40,35,28),
  Income = c(55000,48000,75000,62000,45000),
  Membership = c("Gold","Silver","Platinum","Gold","Bronze"),
  SpendingScore = c(78,65,88,72,55)
)

# Create Customer Object
customer <- list(
  Name = oop_customer_data$Name[1],
  Age = oop_customer_data$Age[1],
  Income = oop_customer_data$Income[1],
  Membership = oop_customer_data$Membership[1]
)

# Assign S3 Class
class(customer) <- "Customer"
customer

# EXPLANATION:
# - list() creates object structure from dataset row
# - class() assigns S3 class named "Customer"

# 2 Assign attributes like Age, Income, Membership

customer$Age
customer$Income
customer$Membership

# EXPLANATION:
# - $ operator is used to access object attributes

# 3 Create a custom print.Customer() method

print.Customer <- function(obj)
{
  cat("Customer Details\n")
  cat("------------------\n")
  cat("Name :", obj$Name, "\n")
  cat("Age :", obj$Age, "\n")
  cat("Income :", obj$Income, "\n")
  cat("Membership :", obj$Membership, "\n")
}

print(customer)

# EXPLANATION:
# - print.Customer() overrides default print behavior
# - cat() displays formatted output

# 4 Extract and display customer details using object reference

customer$Name
customer$Age
customer$Income
customer$Membership

# EXPLANATION:
# - object reference ($) allows extraction of individual fields

# 5 Modify one attribute and observe behavior

customer$Membership <- "Platinum"

print(customer)

# EXPLANATION:
# - attribute value gets updated dynamically
# - modified object reflects new membership category


# Q2 Advanced Customer Class (S4 System)
# Scenario: A banking system requires strict validation of customer records.


# 1 Define S4 class "Customer" with slots (Age, Income, Membership)

setClass(
  "Customer",
  slots = list(
    Age = "numeric",
    Income = "numeric",
    Membership = "character"
  )
)

# EXPLANATION:
# - setClass() is used to define an S4 class
# - slots define object attributes and their data types
# - S4 system provides strict validation


# 2 Create objects from dataset rows

customer1 <- new(
  "Customer",
  Age = oop_customer_data$Age[1],
  Income = oop_customer_data$Income[1],
  Membership = oop_customer_data$Membership[1]
)

customer1

# EXPLANATION:
# - new() creates an object of S4 class
# - values must match slot data types

# 3 Write a show() method

setMethod(
  "show",
  "Customer",
  function(object)
  {
    cat("Customer Details\n")
    cat("------------------\n")
    cat("Age :", object@Age, "\n")
    cat("Income :", object@Income, "\n")
    cat("Membership :", object@Membership, "\n")
  }
)

customer1

# EXPLANATION:
# - setMethod() customizes display behavior
# - @ operator accesses S4 object slots
# - object automatically uses custom show() method when printed

# Q4 Memory Optimization Study
# Scenario: Company wants to optimize memory usage.

# 1 Create large object from dataset

large_data <- rbind(
  oop_customer_data,
  oop_customer_data,
  oop_customer_data,
  oop_customer_data,
  oop_customer_data
)

head(large_data)

# EXPLANATION:
# - rbind() combines multiple copies of dataset
# - creates a large object for memory analysis

# 2 Check memory usage using object.size()

object.size(large_data)

# EXPLANATION:
# - object.size() returns memory used by object in bytes

# 3 Remove unnecessary objects using rm()

rm(large_data)

# EXPLANATION:
# - rm() removes objects from environment
# - helps free memory space

# 4 Run garbage collection

gc()

# EXPLANATION:
# - gc() clears unused memory
# - improves performance and memory management

# Q5 List-Based Customer Object System
# Scenario: System uses lists instead of data frames.

# 1 Convert dataset into list of customers

customer_list <- split(oop_customer_data, seq(nrow(oop_customer_data)))
customer_list[[1]]

# EXPLANATION:
# - split() converts each row into separate list element
# - each customer becomes an individual list object

# 2 Assign S3 class to each list element

customer_list <- lapply(customer_list, function(x)
{
  class(x) <- "Customer"
  return(x)
})
class(customer_list[[1]])

# EXPLANATION:
# - lapply() applies function to each list element
# - assigns S3 class "Customer"

# 3 Access nested values

customer_list[[1]]$Name
customer_list[[1]]$Age

# EXPLANATION:
# - [[ ]] accesses list element
# - $ accesses attributes inside object

# 4 Modify one customer's data

customer_list[[1]]$Income <- 80000
customer_list[[1]]

# EXPLANATION:
# - updates Income value of first customer

# 5 Print structure using str()

str(customer_list)

# EXPLANATION:
# - str() shows structure of list object
# - displays nested elements and data types

library(readxl)

# Load Dataset
oop_customer_data <- data.frame(
  CustomerID = c("C101","C102","C103"),
  Name = c("Rahul","Ananya","Aarav"),
  Age = c(30,25,40),
  Income = c(55000,48000,75000),
  Membership = c("Gold","Silver","Platinum"),
  SpendingScore = c(78,65,88)
)

#Q6) Class Inspection and Documentation
#Scenario: Developer needs to inspect object structure.

# 1 Check class of dataset
class(oop_customer_data)
# Explanation:
# • class() returns the type or class of object.
# • Data loaded from Excel is generally a data frame / tibble.

# 2 Use str() to inspect structure
str(oop_customer_data)
# Explanation:
# • str() displays:
# o Number of rows and columns
# o Variable names
# o Data types
# o Sample values

# 3 Extract attributes
attributes(oop_customer_data)
# Explanation:
# • attributes() displays metadata of object.
# • Includes:
# o Column names
# o Row names
# o Class information

# 4 Use summary() for overview
summary(oop_customer_data)
# Explanation:
# • summary() provides statistical overview:
# o Minimum
# o Maximum
# o Mean
# o Median
# o Quartiles
# • For categorical data, frequency counts are displayed.

# Q7 S4 Slot Manipulation
# Scenario: Product system tracks spending behavior.

# 1 Create S4 class "SpendingProfile"
setClass(
  "SpendingProfile",
  slots = list(
    SpendingScore = "numeric",
    Income = "numeric"
  )
)

# Explanation:
# • setClass() defines an S4 class.
# • Slots define object properties and data types.

# 2 Add slots for SpendingScore and Income
getClass("SpendingProfile")

# Explanation:
# • Slots included:
# o SpendingScore
# o Income
# • Both slots are numeric type.

# 3 Create object using dataset values
profile1 <- new(
  "SpendingProfile",
  SpendingScore = 78,
  Income = 65000
)

profile1

# Explanation:
# • new() creates object of S4 class.
# • Values must match slot definitions.

# 4 Access slots using @
profile1@SpendingScore
profile1@Income

# Explanation:
# • @ operator is used to access S4 slots.

#5  Modify slot values and validate
profile1@SpendingScore <- 90
profile1@Income <- 75000

profile1

# Explanation:
# • Slot values can be updated directly using @.
# • S4 system validates datatype consistency automatically.

# Q8 Advanced Data Structure Integration
# Scenario: Company integrates multiple data sources.

# 1 Create nested list containing dataset and summary
sales_summary <- summary(oop_customer_data)

company_data <- list(
  Dataset = oop_customer_data,
  Summary = sales_summary
)

company_data

# Explanation:
# • Nested list stores multiple objects together.
# • Contains:
# o Complete dataset
# o Statistical summary

# 2 Access inner elements
company_data$Dataset
company_data$Summary

# Explanation:
# • $ operator accesses inner elements of list.

# 3 Modify nested structure
company_data$CompanyName <- "Retail Analytics Pvt Ltd"

company_data

# Explanation:
# • New elements can be dynamically added to nested lists.

# 4 Apply class to nested object
class(company_data) <- "CompanyData"

class(company_data)

# Explanation:
# • Assigns custom S3 class "CompanyData" to nested object.

# 5 Display structure using str()
str(company_data)

# Explanation:
# • str() displays:
# o Nested list structure
# o Object hierarchy
# o Variable types
# o Class information


# Q9 Object-Oriented Data Analysis
# Scenario: Analyst builds reusable object-based analysis system.


# 1 Convert dataset into S3 object
customer_object <- oop_customer_data
class(customer_object) <- "CustomerData"

class(customer_object)

# Explanation:
# • Dataset is converted into custom S3 object.
# • "CustomerData" becomes user-defined class.

# 2 Create function to calculate average income
average_income <- function(obj)
{
  mean(obj$Income)
}

# Explanation:
# • Function accepts object as input.
# • Calculates mean income using mean().

# 3 Apply function to object
average_income(customer_object)

# Explanation:
# • Function processes S3 object data.
# • Returns average income value.

# 4 Extend object with new attribute
attr(customer_object, "Source") <- "Customer Survey Data"

attributes(customer_object)

# Explanation:
# • attr() adds metadata/attributes to object.
# • New attribute "Source" is attached.

# 5 Validate object consistency
str(customer_object)
class(customer_object)
sum(is.na(customer_object))

# Explanation:
# • str() validates object structure.
# • class() confirms object type.
# • is.na() checks missing values for consistency validation.

# Q10 Enterprise-Level Object System Design
# Scenario: A company wants scalable object-oriented design.

# 1 Create both S3 and S4 versions of dataset

# S3 Object Creation
s3_customer <- oop_customer_data
class(s3_customer) <- "CustomerS3"
class(s3_customer)

# S4 Object Creation
setClass(
  "CustomerS4",
  slots = list(
    Age = "numeric",
    Income = "numeric",
    Membership = "character"
  )
)

s4_customer <- new(
  "CustomerS4",
  Age = 35,
  Income = 65000,
  Membership = "Gold"
)

s4_customer

# Explanation:
# • S3 system is simple and flexible.
# • S4 system uses strict slot definitions.
# • Data types are validated automatically.

# 2 Compare flexibility vs strictness

s3_customer$new_column <- "Flexible"

# Example:
# s4_customer@Age <- "Thirty"  (will give error)

# Explanation:
# • S3 allows dynamic changes (flexible)
# • S4 enforces strict validation (strict)

# 3 Implement method for summary statistics

#S3 Summary Method
summary.CustomerS3 <- function(object)
{
  cat("Average Income :", mean(object$Income), "\n")
  cat("Average Age :", mean(object$Age), "\n")
}

summary(s3_customer)

#S4 Summary Method

setMethod(
  "summary",
  "CustomerS4",
  function(object)
  {
    cat("Customer Summary\n")
    cat("----------------\n")
    cat("Age :", object@Age, "\n")
    cat("Income :", object@Income, "\n")
    cat("Membership :", object@Membership, "\n")
  }
)

summary(s4_customer)

# Explanation:
# • S3 uses function naming convention
# • S4 uses setMethod() for method definition

# 4 Document differences between systems

cat("Differences Between S3 and S4 Systems\n")
cat("--------------------------------------\n")
cat("S3: Simple, Flexible, Weak validation\n")
cat("S4: Complex, Strict, Strong validation\n")
cat("S3: Faster for small projects\n")
cat("S4: Better for enterprise systems\n")

# Conclusion:
# • S3 is suitable for lightweight applications
# • S4 is suitable for enterprise-level scalable systems requiring strict validation and
scalability.