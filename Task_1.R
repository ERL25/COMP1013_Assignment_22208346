#Task 1
# Data used: order_items.csv, products.csv

library(tidyverse)

#Load the data
order_items = read_csv("C:/Users/RL/Documents/COMP1013_Assignment_22208346/order_items.csv")
products = read_csv("C:/Users/RL/Documents/COMP1013_Assignment_22208346/products.csv")

#Price distribution
#Mean and median of every item price
mean_price = mean(order_items$price)
median_price = median(order_items$price)
mean_price
median_price

#A few items cost thousands, which stretches the x-axis.
#Only plot items under 1000 so the main shape is visible.
#The mean and median above still use all the items.
order_items %>%
  filter(price < 1000) %>%
  ggplot() +
  geom_histogram(aes(x = price), binwidth = 20, fill = "steelblue", colour = "white") +
  geom_vline(xintercept = mean_price, colour = "red") + #mean
  geom_vline(xintercept = median_price, colour = "darkgreen") + #median
  labs(title = "Distribution of item prices (items under 1000)",
       x = "Price", y = "Number of items")

#Same data on a log scale, using all items
ggplot(order_items) +
  geom_histogram(aes(x = price), bins = 40, fill = "steelblue", colour = "white") +
  scale_x_log10() +
  labs(title = "Distribution of item prices (log scale)",
       x = "Price (log scale)", y = "Number of items")

#Join order_items and products
#Check that product_id is a primary key in products
nrow(products)
n_distinct(products$product_id)

#Left join keeps every order item, so the row count should not change
items_products = order_items %>%
  left_join(products, by = "product_id")
nrow(order_items)
nrow(items_products)

#Some products have a missing weight or a weight of 0, which is not possible. So, filter() with > 0 removes both cases.
freight_data = items_products %>%
  filter(product_weight_g > 0)
nrow(items_products) - nrow(freight_data) #number of rows removed

#Freight value against weight
#alpha makes the points see-through because there are so many of them
ggplot(freight_data, aes(x = product_weight_g, y = freight_value)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", formula = y ~ x) +
  labs(title = "Freight value against product weight",
       x = "Product weight (g)", y = "Freight value")

#How strong is the relationship?
cor(freight_data$product_weight_g, freight_data$freight_value) #Pearson looks at a straight-line relationship.
