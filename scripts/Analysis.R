# =====================
# CP03 - Comparative Analysis
# =====================

# Packages
library(ggplot2)
library(dplyr)

# =====================
# Load data (USE RELATIVE PATH)
# =====================
places <- read.csv(
  "data/raw/PLACES_sample.csv",
  stringsAsFactors = FALSE
)

# Check structure
str(places)

# =====================
# Data Preparation
# =====================

# Filter for obesity and physical inactivity
data_filtered <- places %>%
  filter(Measure %in% c("Obesity among adults",
                        "No leisure-time physical activity among adults")) %>%
  select(StateAbbr, LocationName, Measure, Data_Value)

# Reshape to wide format
data_wide <- reshape(data_filtered,
                     idvar = c("StateAbbr", "LocationName"),
                     timevar = "Measure",
                     direction = "wide")

# Rename columns
colnames(data_wide) <- c("State", "County", "Obesity", "Inactivity")

# Remove missing values
data_clean <- na.omit(data_wide)

# Create categorical variable (median split)
median_inactivity <- median(data_clean$Inactivity, na.rm = TRUE)

data_clean <- data_clean %>%
  mutate(Inactivity_Group = ifelse(Inactivity > median_inactivity,
                                   "High Inactivity",
                                   "Low Inactivity"))

# =====================
# Summary Statistics
# =====================

summary_table <- data_clean %>%
  group_by(Inactivity_Group) %>%
  summarise(
    Mean_Obesity = mean(Obesity),
    SD_Obesity = sd(Obesity),
    Median_Obesity = median(Obesity),
    N = n()
  )

print(summary_table)

# =====================
# Boxplot
# =====================

ggplot(data_clean, aes(x = Inactivity_Group, y = Obesity)) +
  geom_boxplot(fill = "steelblue") +
  labs(
    title = "Obesity Prevalence by Physical Inactivity Group",
    x = "Physical Inactivity Group",
    y = "Obesity Prevalence (%)"
  ) +
  theme_minimal()

# =====================
# Scatterplot
# =====================

ggplot(data_clean, aes(x = Inactivity, y = Obesity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Association Between Physical Inactivity and Obesity",
    x = "Physical Inactivity (%)",
    y = "Obesity (%)"
  ) +
  theme_minimal()

# =====================
# Save Figures
# =====================

ggsave("output/cp03_boxplot.png", width = 6, height = 4)
ggsave("output/cp03_scatterplot.png", width = 6, height = 4)
