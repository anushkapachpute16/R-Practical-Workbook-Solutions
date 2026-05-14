#ASSIGNMENT NO.7: Correlation and Probability Distributions and Performing Univariate Analyses 

#Questions:
  #Use customer_data.csv to solve following questions 

#Load Dataset
customer_data <- read.csv("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/customer_data.csv")

#Q1) Retail Income vs Spending Strategy 
#Scenario: A retail company wants to understand whether high-income customers actually spend more.

#1. Calculate Pearson correlation between Income and Spending.Score. Interpret the result. 
# Pearson Correlation
correlation <- cor(customer_data$Income,
                   customer_data$SpendingScore,
                   method = "pearson",
                   use = "complete.obs")

correlation

#Interpretation:
#• Positive value → Higher income customers tend to spend more.
#• Negative value → Higher income customers spend less.
#• Value close to 1 or -1 indicates strong relationship.
#• Value near 0 indicates weak relationship.

#2. Test significance of the correlation. What conclusion can be drawn?

# Correlation significance test
cor_test <- cor.test(customer_data$Income,
                     customer_data$SpendingScore,
                     method = "pearson")
cor_test

#Conclusion:
#• If p-value < 0.05 → Correlation is statistically significant.
#• If p-value > 0.05 → No significant relationship exists.

#3. Generate a normal distribution using Income and compare with actual data.

# Histogram of actual Income
hist(customer_data$Income,
     probability = TRUE,
     main = "Income Distribution",
     col = "lightblue")

# Generate normal curve

x <- seq(min(customer_data$Income),
         max(customer_data$Income),
         length = 100)

y <- dnorm(x,
           mean = mean(customer_data$Income),
           sd = sd(customer_data$Income))

lines(x, y, col = "red", lwd = 2)

# 4. Perform a one-sample t-test to check if average income is ₹50,000

# One-sample t-test
t_test <- t.test(customer_data$Income,
                 mu = 50000)

t_test

# Conclusion:
# p-value < 0.05 → Mean income significantly differs from ₹50,000.
# p-value > 0.05 → Mean income is approximately ₹50,000.

# 5. Test whether purchase success rate differs from 50% using binomial test

# Convert Purchase to success count
success <- sum(customer_data$Purchase == "Yes")

total <- nrow(customer_data)

# Binomial test
binom_test <- binom.test(success,
                         total,
                         p = 0.50)

binom_test

# Conclusion:
# p-value < 0.05 → Purchase success rate significantly differs from 50%.
# p-value > 0.05 → No significant difference from 50%.

# Q2) Customer Segmentation Strategy
#Scenario: Business wants to divide customers into meaningful segments.

# 1. Compute correlation matrix of Age, Income, SpendingScore

cor_matrix <- cor(customer_data[, c("Age",
                                    "Income",
                                    "SpendingScore")],
                  use = "complete.obs")

cor_matrix

# 2. Identify strongest relationship and justify

# Display correlation matrix
print(cor_matrix)

# Justification:
# Highest absolute correlation value represents strongest relationship.
# Positive value → Direct relationship.
# Negative value → Inverse relationship.

# 3. Generate normal distribution for SpendingScore

hist(customer_data$SpendingScore,
     probability = TRUE,
     col = "lightgreen",
     main = "SpendingScore Distribution")

x <- seq(min(customer_data$SpendingScore),
         max(customer_data$SpendingScore),
         length = 100)

y <- dnorm(x,
           mean = mean(customer_data$SpendingScore),
           sd = sd(customer_data$SpendingScore))

lines(x, y, col = "red", lwd = 2)

# 4. Perform t-test for SpendingScore = 60

t_test_spending <- t.test(customer_data$SpendingScore,
                          mu = 60)

t_test_spending

# 5. Use Kruskal-Wallis test to compare SpendingScore across regions

kruskal_result <- kruskal.test(SpendingScore ~ Region,
                               data = customer_data)

kruskal_result

# Conclusion:
# p-value < 0.05 → SpendingScore differs across regions.
# p-value > 0.05 → No significant regional difference.

# Q3) Financial Risk Assessment
#Scenario: Bank wants to assess risk based on income and spending.


# 1. Analyze correlation between Income and SpendingScore

cor(customer_data$Income,
    customer_data$SpendingScore,
    method = "pearson")

# 2. Test statistical significance

cor.test(customer_data$Income,
         customer_data$SpendingScore,
         method = "pearson")

# 3. Generate Poisson distribution for number of risky customers

# Simulated risky customer counts

risky_customers <- rpois(100,
                         lambda = 5)

hist(risky_customers,
     main = "Poisson Distribution",
     col = "orange")

# 4. Perform t-test on Income = ₹40,000

t.test(customer_data$Income,
       mu = 40000)

# 5. Perform binomial test assuming 60% safe customers

safe <- sum(customer_data$Purchase == "Yes")
total <- nrow(customer_data)

binom.test(safe,
           total,
           p = 0.60)

# Q4) Product Demand Forecasting
#Scenario: Company studies demand patterns based on demographics.

# 1. Compute Spearman correlation between Age and Income

spearman_cor <- cor(customer_data$Age,
                    customer_data$Income,
                    method = "spearman")

spearman_cor

# 2. Explain monotonic relationship if any

# Explanation:
# Positive correlation → As age increases, income increases.
# Negative correlation → As age increases, income decreases.
# Near 0 → Weak monotonic relationship.

# 3. Generate binomial distribution for purchase events

# Binomial distribution

purchase_events <- rbinom(100,
                          size = 10,
                          prob = 0.5)

hist(purchase_events,
     col = "skyblue",
     main = "Binomial Distribution")

# 4. Perform t-test on Age = 35
t.test(customer_data$Age,
       mu = 35)

# 5. Use chi-square goodness-of-fit for Region distribution
region_table <- table(customer_data$Region)
chisq.test(region_table)

# Q5) Customer Satisfaction Modeling
#Scenario: SpendingScore is treated as satisfaction level.

# 1. Correlate Income and SpendingScore using Pearson

cor(customer_data$Income,
    customer_data$SpendingScore,
    method = "pearson")

# 2. Validate using Spearman correlation

cor(customer_data$Income,
    customer_data$SpendingScore,
    method = "spearman")

# 3. Generate normal distribution for SpendingScore

hist(customer_data$SpendingScore,
     probability = TRUE,
     col = "pink",
     main = "SpendingScore Distribution")

x <- seq(min(customer_data$SpendingScore),
         max(customer_data$SpendingScore),
         length = 100)

y <- dnorm(x,
           mean = mean(customer_data$SpendingScore),
           sd = sd(customer_data$SpendingScore))

lines(x, y, col = "blue", lwd = 2)

# 4. Perform t-test for mean satisfaction = 55

t.test(customer_data$SpendingScore,
       mu = 55)

# 5. Apply Wilcoxon test between Age and SpendingScore

wilcox.test(customer_data$Age,
            customer_data$SpendingScore)

# Q6) Sales Probability Analysis
#Scenario: Business wants to model probability of purchase success. 

# 1. Convert Purchase into binary and correlate with Income

# Convert Purchase to binary
customer_data$PurchaseBinary <- ifelse(customer_data$Purchase == "Yes", 1, 0)
cor(customer_data$PurchaseBinary,
    customer_data$Income)

# 2. Generate binomial distribution for purchase trials

purchase_trials <- rbinom(100,
                          size = 20,
                          prob = 0.6)

hist(purchase_trials,
     col = "lightgreen",
     main = "Purchase Trials Distribution")

# 3. Perform binomial test for observed success rate

success <- sum(customer_data$PurchaseBinary)

total <- nrow(customer_data)

binom.test(success,
           total,
           p = 0.5)

# 4. Perform t-test on Income = ₹55,000

t.test(customer_data$Income,
       mu = 55000)

# 5. Apply chi-square test for purchase distribution

purchase_table <- table(customer_data$Purchase)
chisq.test(purchase_table)

#Q7) Strategic Business Decision Model
#Scenario: Management wants a statistical summary for decision making. 

# 1. Compute correlation matrix among all numeric variables

numeric_data <- customer_data[, sapply(customer_data, is.numeric)]

cor_matrix <- cor(numeric_data,
                  use = "complete.obs")
cor_matrix


# 2. Identify strongest predictor of SpendingScore

cor_matrix

# Interpretation:
# Variable having highest absolute correlation with SpendingScore is the strongest predictor.

# 3. Generate Poisson distribution for simulated demand

demand <- rpois(100,
                lambda = 8)

hist(demand,
     col = "yellow",
     main = "Simulated Demand Distribution")

# 4. Perform t-test on Income benchmark ₹60,000

t.test(customer_data$Income,
       mu = 60000)

# 5. Perform Kruskal-Wallis test for Age across regions

kruskal.test(Age ~ Region,
             data = customer_data)
