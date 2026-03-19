# =====================
# CP02 - Data Exploration
# =====================

library(ggplot2)

# Load data
places_raw <- read.csv("data/raw/PLACES_sample.csv", stringsAsFactors = FALSE)

# Cleaning
to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  suppressWarnings(as.numeric(x))
}

places_raw$Data_Value <- to_num(places_raw$Data_Value)
places_raw$TotalPopulation <- to_num(places_raw$TotalPopulation)

# =====================
# Histogram
# =====================

chosen_measure <- "Obesity among adults"

obesity_data <- places_raw[
  places_raw$Measure == chosen_measure &
  !is.na(places_raw$Data_Value), ]

mean_val <- mean(obesity_data$Data_Value)
median_val <- median(obesity_data$Data_Value)

p1 <- ggplot(obesity_data, aes(x = Data_Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = mean_val, color = "Mean"), linewidth = 1) +
  geom_vline(aes(xintercept = median_val, color = "Median"),
             linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "red", "Median" = "black")) +
  theme_minimal()

p1

# =====================
# Scatterplot (log scale)
# =====================

scatter_data <- obesity_data[
  !is.na(obesity_data$TotalPopulation), ]

p2 <- ggplot(scatter_data,
             aes(x = TotalPopulation, y = Data_Value)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  theme_classic()

p2
