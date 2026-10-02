# Plot revenue by region

ggplot(sales_by_region, aes(reorder(region, revenue), revenue, fill = region)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::dollar(revenue)), hjust = -0.1) +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar, expand = expansion(mult = c(0, 0.15))) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Revenue by Region", x = NULL, y = NULL) +
  theme_minimal()
