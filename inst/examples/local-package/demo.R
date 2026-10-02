# Demo: local package + CRAN packages

library(dplyr)
library(ggplot2)
library(demotools)

# Load data
metrics <- read.csv("data/metrics.csv")
metrics$month <- as.Date(paste0(metrics$month, "-01"))

# Analyze with demotools
describe_vector(metrics$revenue, "Revenue")
describe_vector(metrics$customers, "Customers")

# Calculate profit
metrics <- metrics |>
  mutate(profit = revenue - expenses)

# Plot
ggplot(metrics, aes(x = month)) +
  geom_line(aes(y = revenue, color = "Revenue"), linewidth = 1.2) +
  geom_line(aes(y = expenses, color = "Expenses"), linewidth = 1.2) +
  geom_ribbon(aes(ymin = expenses, ymax = revenue), alpha = 0.2, fill = "green") +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_manual(values = c("Revenue" = "steelblue", "Expenses" = "coral")) +
  labs(title = "Revenue vs Expenses", x = NULL, y = NULL, color = NULL) +
  theme_minimal()
