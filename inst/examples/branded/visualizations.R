# Visualization functions

create_bar_chart <- function(data = sample_data) {
  barplot(
    data$value,
    names.arg = data$category,
    col = rainbow(nrow(data)),
    main = "Values by Category",
    xlab = "Category",
    ylab = "Value"
  )
}

create_pie_chart <- function(data = sample_data) {
  pie(data$value, labels = data$category, col = rainbow(nrow(data)), main = "Distribution")
}

plot_growth <- function(data = sample_data) {
  colors <- ifelse(data$growth >= 0, "#4A90D9", "#E74C3C")
  barplot(
    data$growth * 100,
    names.arg = data$category,
    col = colors,
    main = "Growth Rate (%)",
    xlab = "Category",
    ylab = "Growth %"
  )
  abline(h = 0, lwd = 2)
}
