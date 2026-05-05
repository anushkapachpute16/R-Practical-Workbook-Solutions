#ASSIGNMENT NO.2: Data Structures and Control Structures in R 
#Questions: 
  #Use following data frame to solve following questions 
set.seed(123) 
sales_data <- data.frame( 
  OrderID = 1:100, 
  CustomerName = 
    sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),100,replace=TRUE), 
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),100,replace=TRUE), 
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),100,replace=TRUE), 
  Category = sample(c("Electronics","Fashion"),100,replace=TRUE), 
  Quantity = sample(1:10,100,replace=TRUE), 
  Price = sample(seq(500,50000,500),100,replace=TRUE), 
  Discount = sample(c(0,5,10,15,20),100,replace=TRUE), 
  PaymentMethod = sample(c("Cash","Card","UPI"),100,replace=TRUE) 
) 
sales_data$Revenue <- sales_data$Quantity * sales_data$Price 
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount / 100 
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount 
head(sales_data)

#Q1) Filtering Rows 
#1. Show orders where Price > 20000. 
#2. Show orders where City = Pune. 
#3. Show orders where Quantity > 5. 
#4. Show orders where Category = Electronics. 
#5. Combine two conditions.

#Price > 20000
sales_data[sales_data$Price > 20000,]
#City = Pune
sales_data[sales_data$City == "Pune",]
#Quantity > 5 
sales_data[sales_data$Quantity > 5,]
#Category = Electronics
sales_data[sales_data$Category == "Electronics",]
#combine conditions 
sales_data[sales_data$Price > 20000 & sales_data$City == "Pune,"]

#Q2) Sorting Data 
#1. Sort dataset by Price. 
#2. Sort dataset by Revenue. 
#3. Sort by City alphabetically. 
#4. Sort by Quantity descending. 
#5. Sort by multiple columns.

#sort by price
sales_data[order(sales_data$Price),]
#sort by revenue
sales_data[order(sales_data$Revenue),]
#sort by city
sales_data[order(sales_data$City),]
#sort by quantity descending 
sales_data[order(-sales_data$Quantity),]
#sort by multiple coloumns 
sales_data[order(sales_data$City ,-sales_data$Revenue),]

#Q3) Aggregation 
#1. Total revenue by city. 
#2. Average price by category. 
#3. Total quantity sold by product. 
#4. Maximum order value by city. 
#5. Minimum order value by product. 

# 1.	Total revenue by city.
aggregate(Revenue ~ City, sales_data, sum)

# 2.	Average price by category.

aggregate(Price ~ Category, sales_data, mean)

# 3.	Total quantity sold by product.
aggregate(Quantity ~ Product, sales_data, sum)

# 4.	Maximum order value by city.
aggregate(Revenue~ City, sales_data, max)

# 5.	Minimum order value by product.
aggregate(Revenue ~ Product, sales_data, min)


#Q4) Conditional Statements 
#1. If discount > 10 mark "High Discount". 
#2. Classify orders into High / Medium / Low value by using if else. 
#3. Print revenue of each order by using for loop. 
#4. Calculate cumulative revenue by using while loop 
#5. Skip fashion category. 


#1. If discount > 10 mark "High Discount". 
sales_data$DiscountFlag <- ifelse(sales_data$Discount >10, "High Discount","Normal")
sales_data$DiscountFlag

#2. Classify orders into High / Medium / Low value by using if else. 
sales_data$OrderCategory <- ifelse(sales_data$Revenue > 30000, "High", 
                                   ifelse(sales_data$Revenue > 10000, "Medium", "Low"))
sales_data$OrderCategory 

#3. Print revenue of each order by using for loop. 
for(i in 1:nrow(sales_data)){
  print(sales_data$Revenue[i])
}


#4. Calculate cumulative revenue by using while loop 
i <- 1
total <- 0

while(i <= nrow(sales_data)){
  total <- total + sales_data$Revenue[i]
  i <- i+1
}
print (total)

#5. Skip fashion category. 
sales_data[sales_data$Category != "fashion",]


# Q5) Discount Analysis
# 1.	Find Average discount.
# 2.	Orders with discount > 10%.
# 3.	Total discount amount.
# 4.	City with highest discount.
# 5.	Count discount levels.

# 1. Average discount.
mean(sales_data$Discount)

# 2. Orders with discount > 10%.
sales_data[sales_data$Discount > 10, ]

# 3. Total discount amount.
sum(sales_data$DiscountAmount)

# 4.	City with highest discount.
aggregate(DiscountAmount ~ City, sales_data, sum)

# 5.	Count discount levels.
table(sales_data$Discount)

#Q6) Multi Condition Filtering 
#1. Electronics + Price > 20000 
#2. Fashion + Quantity > 5 
#3. Pune + Discount > 10 
#4. Revenue > 50000 + Cash payment 
#5. High price + high quantity 


# 1.	Electronics + Price > 20000
sales_data[sales_data$Category == "Electronics" & sales_data$Price > 20000, ]

# 2.	Fashion + Quantity > 5
sales_data[sales_data$Category == "Fashion" & sales_data$Quantity > 5, ]

# 3.	Pune + Discount > 10
sales_data[sales_data$City == "Pune" & sales_data$Discount > 10, ]

# 4.	Revenue > 50000 + Cash payment
sales_data[sales_data$Revenue > 50000 & sales_data$PaymentMethod == "Cash", ]

# 5.	High price + high quantity
sales_data[sales_data$Price > 30000 & sales_data$Quantity > 5, ]
