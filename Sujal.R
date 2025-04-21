# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Sample dataset: Hospital patient metrics
patient_data <- data.frame(
  Patient = c("Alice", "Bob", "Charlie", "Diana", "Ethan"),
  Age = c(29, 45, 37, 52, 41),
  Blood_Pressure = c(120, 140, 130, 150, 135),
  Heart_Rate = c(72, 85, 78, 90, 76),
  Cholesterol = c(180, 220, 200, 240, 210)
)

# Display patient data
display_patients <- function(data) {
  print("Patient Medical Records:")
  print(data)
}

display_patients(patient_data)

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

stats <- calculate_statistics(patient_data)
print("Summary Statistics:")
print(stats)

# Summary Table
summary_table <- function(stats) {
  print("Summary Statistics Table:")
  print(stats)
}

summary_table(stats)

# Bar Plot for Patient Metrics
bar_plot <- function(data) {
  melted_data <- pivot_longer(data, cols = -Patient, names_to = "Metric", values_to = "Value")
  ggplot(melted_data, aes(x = Patient, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Patient Medical Metrics", x = "Patient", y = "Value", fill = "Metric") +
    theme_minimal()
}

bar_plot(patient_data)

# Box Plot
box_plot <- function(data) {
  melted_data <- pivot_longer(data, cols = -Patient, names_to = "Metric", values_to = "Value")
  ggplot(melted_data, aes(x = Metric, y = Value, fill = Metric)) +
    geom_boxplot() +
    labs(title = "Distribution of Patient Metrics", x = "Metric", y = "Value") +
    theme_minimal()
}

box_plot(patient_data)

# Histogram
histogram_plot <- function(data) {
  melted_data <- pivot_longer(data, cols = -Patient, names_to = "Metric", values_to = "Value")
  ggplot(melted_data, aes(x = Value, fill = Metric)) +
    geom_histogram(binwidth = 10, alpha = 0.6, position = "identity", color = "black") +
    labs(title = "Histogram of Patient Metrics", x = "Value", y = "Frequency", fill = "Metric") +
    theme_minimal()
}

histogram_plot(patient_data)

# Identify Extremes
identify_extremes <- function(data) {
  results <- lapply(names(data)[-1], function(metric) {
    highest <- data[which.max(data[[metric]]), "Patient"]
    lowest <- data[which.min(data[[metric]]), "Patient"]
    return(data.frame(Metric = metric, Highest = highest, Lowest = lowest))
  })
  return(do.call(rbind, results))
}

extremes <- identify_extremes(patient_data)
print("Highest and Lowest Values by Metric:")
print(extremes)
