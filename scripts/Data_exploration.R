# =====================
# CP02 - Data Exploration
# =====================

library(ggplot2)

# Load data (USE RELATIVE PATH)
places_raw <- read.csv("data/raw/PLACES__Local_Data_for_Better_Health_County_Data_2025.csv", stringsAsFactors = FALSE)

# Check structure
head(places_raw)

# Convert numeric columns
to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  suppressWarnings(as.numeric(x))
}

places_raw$Data_Value <- to_num(places_raw$Data_Value)
places_raw$Low_Confidence_Limit <- to_num(places_raw$Low_Confidence_Limit)
places_raw$High_Confidence_Limit <- to_num(places_raw$High_Confidence_Limit)
places_raw$TotalPopulation <- to_num(places_raw$TotalPopulation)

# Variable table
variable_table <- data.frame(
  Variable = c("StateAbbr","LocationName","Measure","Data_Value","Data_Value_Unit",
               "Low_Confidence_Limit","High_Confidence_Limit","TotalPopulation"),
  Type = c("Categorical","Categorical","Categorical","Continuous","Categorical",
           "Continuous","Continuous","Continuous")
)

print(variable_table)

# Filter obesity
chosen_measure <- "Obesity among adults"

obesity_data <- places_raw[
  places_raw$Measure == chosen_measure & !is.na(places_raw$Data_Value),
]

# Histogram
ggplot(obesity_data, aes(x = Data_Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Obesity",
       x = "Prevalence (%)",
       y = "Count") +
  theme_minimal()

# Scatterplot
ggplot(obesity_data, aes(x = TotalPopulation, y = Data_Value)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  labs(title = "Obesity vs Population",
       x = "Population (log scale)",
       y = "Obesity (%)") +
  theme_classic()