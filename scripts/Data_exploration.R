# =====================
# CP02 - Data Exploration
# =====================

# Packages
library(ggplot2)

# =====================
# Load data (USE RELATIVE PATH)
# =====================
places_raw <- read.csv(
  "data/raw/PLACES_sample.csv",
  stringsAsFactors = FALSE
)

# Check structure
head(places_raw)

# =====================
# Data Cleaning
# =====================

to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  suppressWarnings(as.numeric(x))
}

places_raw$Data_Value <- to_num(places_raw$Data_Value)
places_raw$Low_Confidence_Limit <- to_num(places_raw$Low_Confidence_Limit)
places_raw$High_Confidence_Limit <- to_num(places_raw$High_Confidence_Limit)
places_raw$TotalPopulation <- to_num(places_raw$TotalPopulation)

# =====================
# Variable Overview Table
# =====================

variable_table <- data.frame(
  Variable = c("StateAbbr","LocationName","Measure","Data_Value","Data_Value_Unit",
               "Low_Confidence_Limit","High_Confidence_Limit","TotalPopulation"),
  Type = c("Categorical","Categorical","Categorical","Continuous","Categorical",
           "Continuous","Continuous","Continuous"),
  Class = c(class(places_raw$StateAbbr)[1], class(places_raw$LocationName)[1],
            class(places_raw$Measure)[1], class(places_raw$Data_Value)[1],
            class(places_raw$Data_Value_Unit)[1], class(places_raw$Low_Confidence_Limit)[1],
            class(places_raw$High_Confidence_Limit)[1], class(places_raw$TotalPopulation)[1]),
  row.names = NULL
)

print(variable_table)

# =====================
# Histogram - Obesity Distribution
# =====================

chosen_measure <- "Obesity among adults"

if (!chosen_measure %in% places_raw$Measure) {
  stop("chosen_measure not found. Check spelling.")
}

obesity_data <- places_raw[
  places_raw$Measure == chosen_measure & !is.na(places_raw$Data_Value),
]

unit_val <- unique(obesity_data$Data_Value_Unit[!is.na(obesity_data$Data_Value_Unit)])[1]
if (is.na(unit_val)) unit_val <- ""

mean_val <- mean(obesity_data$Data_Value)
median_val <- median(obesity_data$Data_Value)

ggplot(obesity_data, aes(x = Data_Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = mean_val, color = "Mean"), linewidth = 1) +
  geom_vline(aes(xintercept = median_val, color = "Median"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "red", "Median" = "black")) +
  labs(
    title = paste("Distribution of", chosen_measure, "Across U.S. Counties"),
    x = paste0("Prevalence (", unit_val, ")"),
    y = "Number of Counties",
    color = "",
    caption = "Histogram of county-level estimates from CDC PLACES."
  ) +
  theme_minimal()

ggsave("output/cp02_histogram.png", width = 6, height = 4)

# =====================
# Scatterplot - Obesity vs Population
# =====================

scatter_data <- obesity_data[
  !is.na(obesity_data$TotalPopulation) &
  !is.na(obesity_data$Data_Value),
]

ggplot(scatter_data, aes(x = TotalPopulation, y = Data_Value)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  labs(
    title = paste(chosen_measure, "vs County Total Population"),
    x = "Total Population (log10 scale)",
    y = paste0("Prevalence (", unit_val, ")"),
    caption = "Scatterplot of county-level estimates from CDC PLACES."
  ) +
  theme_classic()

ggsave("output/cp02_scatterplot.png", width = 6, height = 4)
