# =====================
# CP04 - Simulation
# =====================

library(ggplot2)
library(dplyr)
library(tidyr)

data <- read.csv("data/raw/PLACES_sample.csv")

data_clean <- data %>%
  filter(Data_Value_Type == "Crude prevalence",
         Year == 2023)

selected <- data_clean %>%
  filter(Measure %in% c("No leisure-time physical activity among adults",
                        "Obesity among adults")) %>%
  select(LocationName, Measure, Data_Value) %>%
  pivot_wider(names_from = Measure,
              values_from = Data_Value,
              values_fn = mean) %>%
  rename(
    County = LocationName,
    Physical_Inactivity = `No leisure-time physical activity among adults`,
    Obesity = `Obesity among adults`
  ) %>%
  drop_na()

# =====================
# Scatter + regression
# =====================
p1 <- ggplot(selected,
             aes(x = Physical_Inactivity, y = Obesity)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red")

p1

# =====================
# Residual plot
# =====================
model <- lm(Obesity ~ Physical_Inactivity, data = selected)

res_df <- data.frame(
  Fitted = fitted(model),
  Residuals = resid(model)
)

p2 <- ggplot(res_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red")

p2

# =====================
# Plot 3: Heatmap (simulation)
# =====================

p3 <- ggplot(combined_summary,
             aes(x = sample_size,
                 y = effect_size,
                 fill = Mean_Difference)) +
  geom_tile() +
  facet_wrap(~noise_level) +
  labs(
    title = "Mean Difference in Physical Inactivity Between Obesity Groups",
    x = "Sample Size",
    y = "Effect Size",
    fill = "Mean Difference"
  ) +
  scale_fill_gradient(low = "white", high = "darkred")

p3
