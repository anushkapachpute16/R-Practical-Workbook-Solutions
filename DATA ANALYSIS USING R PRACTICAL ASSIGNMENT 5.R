#ASSIGNMENT 5: Data Visualization with ggplot2 in R

#Use sales_data.csv to solve following questions

#Load Dataset and Package
# Load dataset
sales_data <- read_excel("C:/NOTES/R AND DATA ANAYALSIS  SEM 2/assignment/sales_data.xlsx")
# Load ggplot2
library(ggplot2)

#Q1) Basic ggplot Charts

#a) Create scatter plot of Price vs Revenue
ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point()

#b) Create bar chart for City
ggplot(sales_data, aes(x = City)) +
  geom_bar()

#c) Create histogram of Price
ggplot(sales_data, aes(x = Price)) +
  geom_histogram(bins = 20)

#d) Create boxplot of Revenue by Category
ggplot(sales_data, aes(x = Category, y = Revenue)) +
  geom_boxplot()

#e) Create line chart for Revenue by OrderID
ggplot(sales_data, aes(x = OrderID, y = Revenue)) +
  geom_line()

#Q2) Customized Visualization
    
#a) Add title to chart
ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  labs(title = "Orders by City")

#b) Change axis labels
ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point() +
  labs(x = "Product Price", y = "Total Revenue")

#c) Change color of bars
ggplot(sales_data, aes(x = City)) +
  geom_bar(fill = "blue")

#d) Change theme style
ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  theme_minimal()

#e) Rotate x-axis labels
ggplot(sales_data, aes(x = Product)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45))

#Q3) Multi-Variable Visualization

#a) Price vs Revenue colored by Category
ggplot(sales_data, aes(x = Price, y = Revenue, color = Category)) +
  geom_point()

#b) Price vs Quantity colored by Category
ggplot(sales_data, aes(x = Price, y = Quantity, color = Category)) +
  geom_point()

#c) Revenue vs Discount
ggplot(sales_data, aes(x = Discount, y = Revenue)) +
  geom_point()

#d) Top cities by revenue
city_rev <- aggregate(Revenue ~ City, sales_data, sum)
ggplot(city_rev, aes(x = City, y = Revenue)) +
  geom_bar(stat = "identity")

#e) Top products by sales
product_sales <- aggregate(Revenue ~ Product, sales_data, sum)
ggplot(product_sales, aes(x = Product, y = Revenue)) +
  geom_bar(stat = "identity")

#Q4) Bar Chart, Scatter Plot, Histogram Analysis

#a) Number of orders per city (Bar Chart)
ggplot(sales_data, aes(x = City)) +
  geom_bar()

#b) Price vs Revenue (Scatter Plot)
ggplot(sales_data, aes(x = Price, y = Revenue)) +
  geom_point()

#c) Quantity vs Revenue (Scatter Plot)
ggplot(sales_data, aes(x = Quantity, y = Revenue)) +
  geom_point()

#d) Distribution of Quantity (Histogram)
ggplot(sales_data, aes(x = Quantity)) +
  geom_histogram(bins = 10)

#e) Histogram by Category
ggplot(sales_data, aes(x = Revenue, fill = Category)) +
  geom_histogram(bins = 20)

#Q5) Box Plot, Line Chart, Faceted Chart Analysis

#a) Revenue by Category (Box Plot)
ggplot(sales_data, aes(x = Category, y = Revenue)) +
  geom_boxplot()

#b) Price by Product (Box Plot)
ggplot(sales_data, aes(x = Product, y = Price)) +
  geom_boxplot()

#c) Revenue trend by OrderID (Line Chart)
ggplot(sales_data, aes(x = OrderID, y = Revenue)) +
  geom_line()

#d) Discount trend (Line Chart)
ggplot(sales_data, aes(x = OrderID, y = Discount)) +
  geom_line()

#e) City orders by category (Faceted Chart)
ggplot(sales_data, aes(x = City)) +
  geom_bar() +
  facet_wrap(~Category)
