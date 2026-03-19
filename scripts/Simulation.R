# =====================
# CP04 - Simulation
# =====================

# Clean environment (good practice)
rm(list = ls())

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load data
data <- read.csv("data/raw/PLACES_sample.csv")

# Clean data
data_clean <- data %>%
  filter(Data_Value_Type == "Crude prevalence",
         Year == 2023)

# Select and reshape variables
selected <- data_clean %>%
  filter(Measure %in% c("No leisure-time physical activity among adults",
                        "Obesity among adults")) %>%
  select(LocationName, Measure, Data_Value) %>%
  pivot_wider(
    names_from = Measure,
    values_from = Data_Value,
    values_fn = mean   # ⭐ FIX: handle duplicate values
  )

# Rename variables
selected <- selected %>%
  rename(
    County = LocationName,
    Physical_Inactivity = `No leisure-time physical activity among adults`,
    Obesity = `Obesity among adults`
  ) %>%
  drop_na()

# Scatter plot
ggplot(selected, aes(x = Physical_Inactivity, y = Obesity)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship between Physical Inactivity and Obesity (2023)",
    x = "Physical Inactivity (%)",
    y = "Obesity (%)"
  ) +
  theme_minimal()
