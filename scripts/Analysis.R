# =====================
# CP03 - Comparative Analysis
# =====================

library(ggplot2)
library(dplyr)

places <- read.csv("data/raw/PLACES_sample.csv")

data_filtered <- places %>%
  filter(Measure %in% c("Obesity among adults",
                        "No leisure-time physical activity among adults")) %>%
  select(StateAbbr, LocationName, Measure, Data_Value)

data_wide <- reshape(data_filtered,
                     idvar = c("StateAbbr", "LocationName"),
                     timevar = "Measure",
                     direction = "wide")

colnames(data_wide) <- c("State", "County", "Obesity", "Inactivity")

data_clean <- na.omit(data_wide)

median_inactivity <- median(data_clean$Inactivity)

data_clean <- data_clean %>%
  mutate(Inactivity_Group = ifelse(Inactivity > median_inactivity,
                                   "High Inactivity",
                                   "Low Inactivity"))

# =====================
# Boxplot
# =====================
p1 <- ggplot(data_clean, aes(x = Inactivity_Group, y = Obesity)) +
  geom_boxplot() +
  theme_minimal()

p1

# =====================
# Scatter + regression
# =====================
p2 <- ggplot(data_clean, aes(x = Inactivity, y = Obesity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

p2
