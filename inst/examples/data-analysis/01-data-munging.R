# Load and summarize sales data

sales <- read.csv("data/sales.csv")
sales$date <- as.Date(sales$date)

# Revenue by region
sales_by_region <- sales |>
  group_by(region) |>
  summarize(
    units = sum(units),
    revenue = sum(revenue),
    .groups = "drop"
  ) |>
  arrange(desc(revenue))

# Revenue by product
sales_by_product <- sales |>
  group_by(product) |>
  summarize(
    units = sum(units),
    revenue = sum(revenue),
    .groups = "drop"
  )

sales_by_region
sales_by_product
