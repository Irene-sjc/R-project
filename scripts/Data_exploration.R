# =====================
# CP02 - Data Exploration
# =====================

# =====================
# Load libraries
# =====================
library(ggplot2)

# =====================
# Load data (USE RELATIVE PATH)
# =====================
places_raw <- read.csv("data/raw/PLACES_sample.csv", stringsAsFactors = FALSE)

# Check structure
head(places_raw)

# =====================
# Data cleaning
# =====================

# Function to safely convert to numeric
to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  suppressWarnings(as.numeric(x))
}

# Convert variables
places_raw$Data_Value <- to_num(places_raw$Data_Value)
places_raw$Low_Confidence_Limit <- to_num(places_raw$Low_Confidence_Limit)
places_raw$High_Confidence_Limit <- to_num(places_raw$High_Confidence_Limit)
places_raw$TotalPopulation <- to_num(places_raw$TotalPopulation)

# =====================
# Variable table
# =====================
variable_table <- data.frame(
  Variable = c("StateAbbr","LocationName","Measure","Data_Value","Data_Value_Unit",
               "Low_Confidence_Limit","High_Confidence_Limit","TotalPopulation"),
  Type = c("Categorical","Categorical","Categorical","Continuous","Categorical",
           "Continuous","Continuous","Continuous")
)

print(variable_table)

# =====================
# Filter data
# =====================
chosen_measure <- "Obesity among adults"

obesity_data <- places_raw[
  places_raw$Measure == chosen_measure & !is.na(places_raw$Data_Value),
]

# =====================
# Visualization 1: Histogram
# =====================
hist_plot <- ggplot(obesity_data, aes(x = Data_Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Adult Obesity Prevalence",
    x = "Prevalence (%)",
    y = "Count"
  ) +
  theme_minimal()

print(hist_plot)

# =====================
# Visualization 2: Scatterplot
# =====================
scatter_plot <- ggplot(obesity_data, aes(x = TotalPopulation, y = Data_Value)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  labs(
    title = "Obesity Prevalence vs Population",
    x = "Population (log scale)",
    y = "Obesity (%)"
  ) +
  theme_classic()

print(scatter_plot)
