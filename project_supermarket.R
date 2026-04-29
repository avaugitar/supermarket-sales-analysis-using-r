library(dplyr)
library(ggplot2)
data <- read.csv("C:/Users/hp Pavilion/Downloads/archive (1)/SuperMarket Analysis.csv")
str(data)
summary(data)
sum(is.na(data))

#Total Sales per Product Line
produk_sales <- data %>%
  group_by(Product.line) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))
produk_sales

ggplot(produk_sales, aes(x = Product.line, y = Total_Sales)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Total Sales per Product Line",
       x = "Product Line",
       y = "Total Sales")

#Top Branch (Cabang Terbesar)
branch_sales <- data %>%
  group_by(Branch) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))

ggplot(branch_sales, aes(x = Branch, y = Total_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sales per Branch")

#Customer Type (Tipe Pelanggan)
cust_sales <- data %>%
  group_by(Customer.type) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))

ggplot(cust_sales, aes(x = Customer.type, y = Total_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Customer Type")

#Payment Method
payment_sales <- data %>%
  group_by(Payment) %>%
  summarise(Total_Sales = sum(Sales, na.rm = TRUE))

ggplot(payment_sales, aes(x = Payment, y = Total_Sales)) +
  geom_bar(stat = "identity") +
  labs(title = "Sales by Payment Method")

#Quantity vs Sales
ggplot(data, aes(x = Quantity, y = Sales)) +
  geom_point() +
  labs(title = "Quantity vs Sales")

#Rating Distribution
ggplot(data, aes(x = Rating)) +
  geom_histogram(bins = 10) +
  labs(title = "Distribution of Customer Ratings")
