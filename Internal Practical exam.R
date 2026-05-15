#Practical Question 2 : Data Manipulation and Aggregation

set.seed(100)
sales<-data.frame(
  Product=sample(c("Laptop","Mobile","Tablet"),50,replace = TRUE),
  City=sample(c("Pune","Mumbai","Delhi"),50,replace=TRUE),
  Quantity=sample(1:5,50,replace=TRUE),
  Price=sample(seq(10000,50000,5000),50,replace=TRUE)
)
sales$Revenue <- sales$Quantity*sales$Price
print(sales)
#Tasks:

#1.Show records where Revenue>100000.
high_revenue <- sales[sales$Revenue > 100000,]
print(high_revenue)

#2. Sort data by Revenue in descending order
sorted_sales <- sales[order(sales$Revenue,decreasing = TRUE),]
print(sorted_sales)

#3.Find total Revenue by city.
city_revenue <- aggregate(Revenue ~ City,sales,sum)
print(city_revenue)

#4.Find average price by product.
avg_price <- aggregate(Price ~ Product,sales,mean)
print(avg_price)


#5.Identify the product with the highest revenue.
highest_revenue <- sales[which.max(sales$Revenue),]
print(highest_revenue)

#Practical Question 1 : Data Cleaning and Transformation

df<- data.frame(
  name =c("Raj","Amit","Neha","raj","Amit"),
  marks= c("80","90","NA","85","90"),
  city = c("Pune","pune","Mumbai","PUNE","Mumbai")
)
print(df)
#Perform the following tasks:
#1.Remove extra spaces from the name column
df$name <- trimws(df$name)
print(df)

#2.Convert all names to lowercase

df$name <- tolower(df$name)
print(df)

#3.Convert marks column to numeric and handle missing values
df$marks <- as.numeric(df$marks)
df$marks[is.na(df$marks)] <- 0
print(df)

#4.Standardize city names(all lowercase)
df$city <- tolower(df$city)
print(df)


#5.Filter students with marks greater than 80

high_marks <- df[df$marks > 80, ]
print(high_marks)

#6.Create a new column grade:
#Marks>/ 85 -> "A"
#Marks < 85 -> "B"
df$grade <- ifelse(df$marks >= 85,"A","B")
print(df)


#2. Sort data by Revenue in descending order.
sorted_sales <- sales[order(sales$Revenue,decreasing = TRUE),]
print(sorted_sales)


#3.Find total Revenue by city.

city_revenue <- aggregate(Revenue ~ City, sales,sum)
print(city_revenue)

#4.Find average price by product.

avg_price <- aggregate(Price ~ Product,sales,mean)
print(avg_price)


#5.Identify the product with the highest revenue.

highest_revenue <- sales[which.max(sales$Revenue),]
print(highest_revenue)

