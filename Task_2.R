# COMP1013 Assignment - Task 2: Orders That Were Not Delivered
# Data used: orders.csv and customers.csv

library(tidyverse)

#Load the data
orders = read_csv("orders.csv")
customers = read_csv("customers.csv")

#Orders by status
#Count the orders in each status and work out the percentage of all orders
status_counts = orders %>%
  count(order_status) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))
status_counts

#Bar chart of the percentage in each status
ggplot(status_counts) +
  geom_col(aes(x = reorder(order_status, -percent), y = percent)) +
  labs(title = "Percentage of orders by status",
       x = "Order status", y = "Percentage of orders")

#Percentage not delivered
#Not delivered means any status other than "delivered"
orders %>%
  summarise(percent_not_delivered = mean(order_status != "delivered") * 100)

#Not delivered by customer state
#customer_id is the key linking orders to customers
orders_customers = orders %>%
  left_join(customers, by = "customer_id")

state_summary = orders_customers %>%
  group_by(customer_state) %>%
  summarise(orders = n(),
            percent_not_delivered = mean(order_status != "delivered") * 100) %>%
  arrange(desc(percent_not_delivered))
state_summary

#Bar chart, states ordered from highest to lowest
ggplot(state_summary) +
  geom_col(aes(x = reorder(customer_state, -percent_not_delivered),
               y = percent_not_delivered)) +
  labs(title = "Percentage of orders not delivered by customer state",
       x = "Customer state", y = "Percentage not delivered")