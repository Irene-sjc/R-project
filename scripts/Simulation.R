# =====================
# CP04 - Simulation
# =====================

library(ggplot2)
library(dplyr)
library(tidyr)

# Load data
data <- read.csv("data/raw/PLACES__Local_Data_for_Better_Health,_County_Data,_2025_release_20260203.csv")

# Clean
data_clean <- data %>%
  filter(Data_Value_Type == "Crude prevalence",
         Year == 2023)

# Select variables
selected <- data_clean %>%
  filter(Measure %in% c("No leisure-time physical activity among adults",
                        "Obesity among adults")) %>%
  select(LocationName, Measure, Data_Value) %>%
  pivot_wider(names_from = Measure,
              values_from = Data_Value)

selected <- selected %>%
  rename(
    County = LocationName,
    Physical_Inactivity = `No leisure-time physical activity among adults`,
    Obesity = `Obesity among adults`
  ) %>%
  drop_na()

# Scatter
ggplot(selected, aes(x = Physical_Inactivity, y = Obesity)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE)

# Model
model <- lm(Obesity ~ Physical_Inactivity, data = selected)

# Residual plot
ggplot(data.frame(
  Fitted = fitted(model),
  Residuals = resid(model)
), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red")

# Simple simulation
set.seed(123)

simulate_data <- function(n = 500, effect = 0.5, noise = 5) {
  X <- rnorm(n, mean(selected$Physical_Inactivity), sd(selected$Physical_Inactivity))
  Y <- effect * X + 10 + rnorm(n, 0, noise)
  data.frame(X, Y)
}

sim <- simulate_data()

ggplot(sim, aes(x = X, y = Y)) +
  geom_point(alpha = 0.4)