# =====================
# CP03 - Comparative Analysis
# =====================

library(ggplot2)
library(dplyr)

# Load data
places <- read.csv("data/raw/PLACES__Local_Data_for_Better_Health,_County_Data,_2025_release_20260203.csv")

# Filter variables
data_filtered <- places %>%
  filter(Measure %in% c("Obesity among adults",
                        "No leisure-time physical activity among adults")) %>%
  select(StateAbbr, LocationName, Measure, Data_Value)

# Reshape
data_wide <- reshape(data_filtered,
                     idvar = c("StateAbbr", "LocationName"),
                     timevar = "Measure",
                     direction = "wide")

colnames(data_wide) <- c("State", "County", "Obesity", "Inactivity")

# Clean
data_clean <- na.omit(data_wide)

# Group variable
median_inactivity <- median(data_clean$Inactivity)

data_clean <- data_clean %>%
  mutate(Inactivity_Group = ifelse(Inactivity > median_inactivity,
                                   "High", "Low"))

# Summary
summary_table <- data_clean %>%
  group_by(Inactivity_Group) %>%
  summarise(
    Mean_Obesity = mean(Obesity),
    SD_Obesity = sd(Obesity),
    N = n()
  )

print(summary_table)

# Boxplot
ggplot(data_clean, aes(x = Inactivity_Group, y = Obesity)) +
  geom_boxplot() +
  theme_minimal()

# Scatterplot
ggplot(data_clean, aes(x = Inactivity, y = Obesity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()