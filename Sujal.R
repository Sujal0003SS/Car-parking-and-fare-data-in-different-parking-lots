# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Sample dataset: Car parking and fare data in different parking lots
parking_data <- data.frame(
  Parking_Lot = c("Lot A", "Lot B", "Lot C", "Lot D", "Lot E"),
  Total_Spots = c(120, 150, 100, 130, 110),
  Occupied_Spots = c(95, 110, 85, 100, 90),
  Hourly_Fare = c(5.0, 6.5, 4.0, 5.5, 4.5),
  Daily_Revenue = c(750, 980, 620, 890, 710)
)

# Display parking data
display_parking <- function(data) {
  print("Car Parking and Fare Data:")
  print(data)
}

display_parking(parking_data)

# Basic Statistical Analysis
calculate_statistics <- function(data) {
  stats <- data.frame(
    Metric = colnames(data)[-1],
    Mean = sapply(data[-1], mean),
    Median = sapply(data[-1], median),
    Std_Dev = sapply(data[-1], sd)
  )
  return(stats)
}

stats <- calculate_statistics(parking_data)
print("Summary Statistics:")
print(stats)

# Display summary statistics as a table
summary_table <- function(stats) {
  print("Summary Statistics Table:")
  print(stats)
}

summary_table(stats)

# Visualization: Bar Plot for Parking Metrics
bar_plot <- function(data) {
  melted_data <- pivot_longer(data, cols = -Parking_Lot, names_to = "Metric", values_to = "Value")
  ggplot(melted_data, aes(x = Parking_Lot, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Parking Lot Metrics", x = "Parking Lot", y = "Value", fill = "Metric") +
    theme_minimal()
}

bar_plot(parking_data)

# Visualization: Box Plot for Parking Metrics
box_plot <- function(data) {
  melted_data <- pivot_longer(data, cols = -Parking_Lot, names_to = "Metric", values_to = "Value")
  ggplot(melted_data, aes(x = Metric, y = Value, fill = Metric)) +
    geom_boxplot() +
    labs(title = "Distribution of Parking Metrics", x = "Metric", y = "Value") +
    theme_minimal()
}

box_plot(parking_data)

# Visualization: Histogram of Parking Metrics
histogram_plot <- function(data) {
  melted_data <- pivot_longer(data, cols = -Parking_Lot, names_to = "Metric", values_to = "Value")
  ggplot(melted_data, aes(x = Value, fill = Metric)) +
    geom_histogram(binwidth = 20, alpha = 0.7, position = "dodge", color = "black") +
    labs(title = "Histogram of Parking Metrics", x = "Value", y = "Frequency", fill = "Metric") +
    theme_minimal()
}

histogram_plot(parking_data)

# Identify Parking Lots with Max and Min Metrics
identify_extremes <- function(data) {
  results <- lapply(names(data)[-1], function(metric) {
    highest <- data[which.max(data[[metric]]), "Parking_Lot"]
    lowest <- data[which.min(data[[metric]]), "Parking_Lot"]
    return(data.frame(Metric = metric, Highest = highest, Lowest = lowest))
  })
  return(do.call(rbind, results))
}

extremes <- identify_extremes(parking_data)
print("Highest and Lowest Values by Metric:")
print(extremes)
