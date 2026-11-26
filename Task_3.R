#Task 3
#Data used: payments.csv

library(tidyverse)

payments = read_csv("payments.csv")

#Usage frequency of each payment type
#Number of payments and percentage of all payments for each type
type_counts = payments %>%
  count(payment_type) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))
type_counts

#Bar chart of the number of payments
ggplot(type_counts) +
  geom_col(aes(x = reorder(payment_type, -n), y = n)) +
  labs(title = "Number of payments by payment type",
       x = "Payment type", y = "Number of payments")

#Payment values by payment type
#Boxplot of payment value for each type
ggplot(payments) +
  geom_boxplot(aes(x = payment_type, y = payment_value)) +
  labs(title = "Payment value by payment type",
       x = "Payment type", y = "Payment value")

#A few very large payments squash the boxes, so here is the same plot
#on a log scale. Payments of 0 cannot be shown on a log scale.
payments %>%
  filter(payment_value > 0) %>%
  ggplot() +
  geom_boxplot(aes(x = payment_type, y = payment_value)) +
  scale_y_log10() +
  labs(title = "Payment value by payment type (log scale)",
       x = "Payment type", y = "Payment value (log scale)")

#Median and mean payment value for each type
payments %>%
  group_by(payment_type) %>%
  summarise(payments = n(),
            median_value = median(payment_value),
            q75 = quantile(payment_value, 0.75),
            max_value = max(payment_value),
            mean_value = mean(payment_value)) %>%
  arrange(desc(median_value))