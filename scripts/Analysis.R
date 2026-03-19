# =====================
# CP03 - Analysis
# =====================

# Load libraries
library(ggplot2)
library(dplyr)

# =====================
# Load data
# =====================
places <- read.csv("data/raw/PLACES_sample.csv")

# =====================
# Filter variables
# =====================
data_filtered <- places %>%
  filter(Measure %in% c("Obesity among adults",
                        "No leisure-time physical activity among adults"),
         Data_Value_Type == "Crude prevalence",
         Year == 2023) %>%
  select(StateAbbr, LocationName, Measure, Data_Value)

# =====================
# Reshape data
# =====================
data_wide <- reshape(data_filtered,
                     idvar = c("StateAbbr", "LocationName"),
                     timevar = "Measure",
                     direction = "wide")

# Rename columns
colnames(data_wide) <- c("State", "County", "Obesity", "Inactivity")

# =====================
# Clean data
# =====================
data_clean <- na.omit(data_wide)

# =====================
# Create group variable
# =====================
median_inactivity <- median(data_clean$Inactivity, na.rm = TRUE)

data_clean <- data_clean %>%
  mutate(Inactivity_Group = ifelse(Inactivity > median_inactivity,
                                   "High Inactivity",
                                   "Low Inactivity"))

# =====================
# Summary statistics
# =====================
summary_table <- data_clean %>%
  group_by(Inactivity_Group) %>%
  summarise(
    Mean_Obesity = mean(Obesity),
    SD_Obesity = sd(Obesity),
    Median_Obesity = median(Obesity),
    N = n(),
    .groups = "drop"
  )

print(summary_table)

# =====================
# Visualization
# =====================

# Boxplot
ggplot(data_clean, aes(x = Inactivity_Group, y = Obesity)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Obesity by Physical Inactivity Group",
    x = "Inactivity Group",
    y = "Obesity (%)"
  ) +
  theme_minimal()

# Scatterplot
ggplot(data_clean, aes(x = Inactivity, y = Obesity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relationship Between Inactivity and Obesity",
    x = "Physical Inactivity (%)",
    y = "Obesity (%)"
  ) +
  theme_minimal()
