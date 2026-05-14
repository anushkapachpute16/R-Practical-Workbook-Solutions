#ASSIGNMENT NO. 10: Complete Data Analysis Using R Final Project


# STEP 1: IMPORT THE DATASET INTO R

#import dataset
data <- read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/customer_data_finalproject.csv")

# Install Required Packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")


# Load Required Libraries

library(readr)
library(dplyr)
library(ggplot2)


# Import Dataset

#import dataset
data <- read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/customer_data_finalproject.csv")

# Display First Records
head(data)

# Explanation:
# read_csv() imports CSV dataset into R.
# head() displays first rows of dataset.


# STEP 2: DATA CLEANING & PREPROCESSING

# Check Structure of Dataset
str(data)

# Explanation:
# str() displays:
# • Column names
# • Data types
# • Structure of dataset
# • Number of observations

# Check Missing Values
colSums(is.na(data))

# Explanation:
# is.na() checks missing values.
# colSums() counts total missing values
# present in each column.


# Remove Missing Values
data <- na.omit(data)

# Explanation:
# na.omit() removes rows containing
# missing or NULL values.


# Remove Duplicate Records
data <- distinct(data)

# Explanation:
# distinct() removes duplicate rows
# from dataset.

# Convert Education into Factor
data$Education <- as.factor(data$Education)


# Explanation:
# as.factor() converts categorical
# variables into factor datatype.
# Education is a categorical variable.


# Display Summary Statistics
summary(data)

# Explanation:
# summary() displays:
# • Minimum value
# • Maximum value
# • Mean
# • Median
# • Quartiles
# • Frequency for categorical variables



# DATA CLEANING PERFORMED

# • Removed missing values
# • Removed duplicate records
# • Converted categorical variables into factors
# • Verified dataset structure
# • Generated summary statistics



# STEP 3: EXPLORATORY DATA ANALYSIS (EDA)


# 3.1 Distribution of Income
ggplot(data,
       aes(x = Income)) +
  
  geom_histogram(fill = "skyblue",
                 bins = 20) +
  
  ggtitle("Income Distribution")

# Interpretation:
# • Shows distribution of customer income.
# • Helps identify skewness and outliers.
# • Displays frequency of customers
#   across different income ranges.



# 3.2 Spending Pattern by Education
ggplot(data,
       aes(x = Education,
           y = MntWines)) +
  
  geom_boxplot(fill = "orange") +
  
  ggtitle("Wine Spending by Education")


# Interpretation:
# • Compares spending behavior across
#   education groups.
# • Shows minimum, maximum, median,
#   and quartile values.
# • Helps identify high-spending groups
#   and outliers.

# 3.3 Age Distribution
ggplot(data,
       aes(x = Age)) +
  
  geom_histogram(fill = "lightgreen",
                 bins = 15) +
  
  ggtitle("Customer Age Distribution")


# Interpretation:
# • Displays age frequency distribution.
# • Helps understand customer age groups.
# • Identifies concentration of customers
#   within specific age ranges.


# STEP 4: APPLY ANALYTICAL METHODS

# 4.1 Correlation Analysis
# Correlation between Income and Wine Spending
cor(data$Income,
    data$MntWines,
    method = "pearson")

# Interpretation:
# • Positive correlation indicates that
#   higher income customers spend more.
# • Negative correlation indicates inverse
#   relationship.
# • Correlation value ranges between
#   -1 and +1.


# 4.2 Correlation Test
cor.test(data$Income,
         data$MntWines)

# Interpretation:
# • Performs statistical significance test
#   for correlation.
# • p-value < 0.05 indicates significant
#   relationship between variables.
# • p-value > 0.05 indicates insignificant
#   relationship.

# 4.3 Normal Distribution
# Histogram of Income

hist(data$Income,
     probability = TRUE,
     col = "lightblue")

# Generate Normal Curve

x <- seq(min(data$Income),
         max(data$Income),
         length = 100)

y <- dnorm(x,
           mean = mean(data$Income),
           sd = sd(data$Income))

# Add Normal Curve

lines(x, y,
      col = "red",
      lwd = 2)

# Interpretation:
# • Compares actual income distribution
#   with theoretical normal distribution.
# • Helps identify skewness and variation
#   in income data.

# 4.4 Hypothesis Testing (t-test)

# Test Average Income = 50000
t.test(data$Income,
       mu = 50000)

# Interpretation:
# • Tests whether average income
#   significantly differs from ₹50,000.
# • p-value < 0.05 indicates significant
#   difference.
# • p-value > 0.05 indicates no significant
#   difference.

# 4.5 Chi-Square Test

# Education Frequency Table
education_table <- table(data$Education)

# Chi-Square Test
chisq.test(education_table)

# Interpretation:
# • Tests whether education categories
#   are equally distributed.
# • p-value < 0.05 indicates significant
#   difference in category frequencies.
# • p-value > 0.05 indicates equal
#   distribution across categories.


#Step 5: Interpretation of Results

#Key Findings:
#1. Income and spending showed positive relationship.
#2. High-income customers spent more on premium products.
#3. Majority customers belonged to middle-age category.
#4. Certain education groups had higher spending patterns.
#5. Statistical tests confirmed significant behavioral trends. 


# STEP 6: PRESENT FINDINGS USING VISUALIZATIONS

# Scatter Plot: Income vs Spending
ggplot(data,
       aes(x = Income,
           y = MntWines)) +
  
  geom_point(color = "blue") +
  
  ggtitle("Income vs Wine Spending")


# Interpretation:
# • Displays relationship between
#   customer income and wine spending.
# • Positive upward trend indicates
#   higher income customers spend more.
# • Helps identify spending patterns
#   and outliers.

# Bar Chart: Education Count
ggplot(data,
       aes(x = Education)) +
  
  geom_bar(fill = "purple") +
  
  ggtitle("Customer Education Categories")

# Interpretation:
# • Displays frequency of customers
#   in each education category.
# • Helps understand customer
#   educational distribution.


# Boxplot: Income by Marital Status
ggplot(data,
       aes(x = Marital_Status,
           y = Income)) +
  
  geom_boxplot(fill = "gold") +
  
  ggtitle("Income by Marital Status")


# Interpretation:
# • Compares income distribution across
#   different marital status groups.
# • Shows median, quartiles,
#   minimum and maximum income.
# • Helps identify income variation
#   among customer groups.



# FINAL CONCLUSION

# The analysis successfully studied customer
# demographics, income, and spending behavior using R programming.
# Visualizations and statistical tests helped
# identify important customer trends and
# business insights.

# The project demonstrated:
# • Data preprocessing
# • Exploratory data analysis
# • Statistical testing
# • Data visualization
# • Business decision support
