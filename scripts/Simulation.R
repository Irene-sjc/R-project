# =====================
# CP04 - Simulation Analysis
# =====================

# Packages
library(ggplot2)
library(dplyr)
library(tidyr)

# =====================
# Load data (USE RELATIVE PATH)
# =====================
data <- read.csv(
  "data/raw/PLACES_sample.csv",
  stringsAsFactors = FALSE
)

# =====================
# Data Preparation
# =====================

data_clean <- data %>%
  filter(Data_Value_Type == "Crude prevalence",
         Year == 2023)

selected <- data_clean %>%
  filter(Measure %in% c("No leisure-time physical activity among adults",
                        "Obesity among adults")) %>%
  select(LocationName, Measure, Data_Value) %>%
  pivot_wider(
    names_from = Measure,
    values_from = Data_Value,
    values_fn = mean
  ) %>%
  rename(
    County = LocationName,
    Physical_Inactivity = `No leisure-time physical activity among adults`,
    Obesity = `Obesity among adults`
  ) %>%
  drop_na()

# =====================
# Scatterplot
# =====================

scatter_plot <- ggplot(selected, aes(x = Physical_Inactivity, y = Obesity)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Association Between Physical Inactivity and Obesity",
    x = "Physical Inactivity (%)",
    y = "Obesity (%)"
  ) +
  theme_minimal()

ggsave(
  "output/figures/cp04_scatterplot.png",
  plot = scatter_plot,
  width = 6,
  height = 4
)

# =====================
# Residual Plot
# =====================

model <- lm(Obesity ~ Physical_Inactivity, data = selected)

residual_plot <- ggplot(data.frame(
  Fitted = fitted(model),
  Residuals = resid(model)
), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red") +
  labs(
    title = "Residual Plot",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal()

ggsave(
  "output/figures/cp04_residuals.png",
  plot = residual_plot,
  width = 6,
  height = 4
)

# =====================
# Simulation Example
# =====================

set.seed(123)

effect_example <- 0.5
noise_example <- 5
n_example <- 500

X <- rnorm(n_example,
           mean = mean(selected$Physical_Inactivity),
           sd = sd(selected$Physical_Inactivity))

epsilon <- rnorm(n_example, mean = 0, sd = noise_example)

Y <- effect_example * X + 10 + epsilon

sim_data <- data.frame(
  Physical_Inactivity = X,
  Obesity = Y
)

# =====================
# Summary Statistics (Example)
# =====================

summary_stats <- sim_data %>%
  mutate(Obesity_Group = ifelse(Obesity > median(Obesity),
                                "High_Obesity",
                                "Low_Obesity")) %>%
  group_by(Obesity_Group) %>%
  summarise(
    Q1_Inactivity = quantile(Physical_Inactivity, 0.25),
    Median_Inactivity = median(Physical_Inactivity),
    Q3_Inactivity = quantile(Physical_Inactivity, 0.75),
    Mean_Inactivity = mean(Physical_Inactivity),
    .groups = "drop"
  )

# =====================
# Simulation Function
# =====================

simulate_data <- function(effect_size, noise_sd, sample_size) {
  
  X <- rnorm(sample_size,
             mean = mean(selected$Physical_Inactivity),
             sd = sd(selected$Physical_Inactivity))
  
  epsilon <- rnorm(sample_size, mean = 0, sd = noise_sd)
  
  Y <- effect_size * X + 10 + epsilon
  
  sim_data <- data.frame(
    Physical_Inactivity = X,
    Obesity = Y
  )
  
  summary_stats <- sim_data %>%
    mutate(Obesity_Group = ifelse(Obesity > median(Obesity),
                                  "High_Obesity",
                                  "Low_Obesity")) %>%
    group_by(Obesity_Group) %>%
    summarise(
      Q1_Inactivity = quantile(Physical_Inactivity, 0.25),
      Median_Inactivity = median(Physical_Inactivity),
      Q3_Inactivity = quantile(Physical_Inactivity, 0.75),
      Mean_Inactivity = mean(Physical_Inactivity),
      .groups = "drop"
    )
  
  return(list(data = sim_data, summary = summary_stats))
}

# =====================
# Simulation Automation
# =====================

actual_n <- nrow(selected)

effect_sizes <- seq(0.1, 1, length.out = 10)
sample_sizes <- unique(round(seq(100, actual_n, length.out = 10)))
noise_levels <- c(2, 5, 10)

param_grid <- expand.grid(
  effect_size = effect_sizes,
  sample_size = sample_sizes,
  noise_level = noise_levels
)

results <- vector("list", nrow(param_grid))

for (i in 1:nrow(param_grid)) {
  
  sim <- simulate_data(
    effect_size = param_grid$effect_size[i],
    noise_sd = param_grid$noise_level[i],
    sample_size = param_grid$sample_size[i]
  )
  
  results[[i]] <- list(
    parameters = param_grid[i, ],
    simulated_data = sim$data,
    summary_stats = sim$summary
  )
}

# =====================
# Combine Results
# =====================

combined_summary <- do.call(rbind,
                            lapply(results, function(res) {
                              
                              high <- res$summary %>%
                                filter(Obesity_Group == "High_Obesity") %>%
                                pull(Mean_Inactivity)
                              
                              low <- res$summary %>%
                                filter(Obesity_Group == "Low_Obesity") %>%
                                pull(Mean_Inactivity)
                              
                              data.frame(
                                effect_size = res$parameters$effect_size,
                                sample_size = res$parameters$sample_size,
                                noise_level = res$parameters$noise_level,
                                Mean_Difference = high - low
                              )
                            })
)

# =====================
# Heatmap
# =====================

heatmap_plot <- ggplot(combined_summary,
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
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal()

ggsave(
  "output/figures/cp04_heatmap.png",
  plot = heatmap_plot,
  width = 7,
  height = 5
)