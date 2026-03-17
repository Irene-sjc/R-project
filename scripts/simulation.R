---
  title: "VTPEH 6270 - Check Point 04"
author: "Jinchan Sun"
date: "`r Sys.Date()`"
output:
  pdf_document:
  toc: true
toc_depth: 1
number_sections: true
latex_engine: xelatex
header-includes:
  - \usepackage{booktabs}
urlcolor: blue
editor_options:
  chunk_output_type: console
---
  
  ```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

# Introduction

The goal of this report is to explore data simulation in the context of power analysis. We examine the association between physical inactivity and obesity prevalence at the county level and simulate different sampling scenarios to evaluate how effect size, noise, and sample size influence statistical results.

# Objective and Approach

## Research Question

Is county-level physical inactivity prevalence positively associated with obesity prevalence?
  
  ## Data Description
  
  ```{r, results='asis'}

# Load packages
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)

# Read data
data <- read.csv(
  "data/raw/PLACES__Local_Data_for_Better_Health,_County_Data,_2025_release_20260203.csv",
  stringsAsFactors = FALSE
)

# Keep crude prevalence and year 2023
data_clean <- data %>%
  filter(Data_Value_Type == "Crude prevalence",
         Year == 2023)

# Select obesity and physical inactivity
selected <- data_clean %>%
  filter(Measure %in% c("No leisure-time physical activity among adults",
                        "Obesity among adults")) %>%
  select(LocationName, Measure, Data_Value) %>%
  pivot_wider(
    names_from = Measure,
    values_from = Data_Value,
    values_fn = mean
  )

# Rename variables
selected <- selected %>%
  rename(
    County = LocationName,
    Physical_Inactivity = `No leisure-time physical activity among adults`,
    Obesity = `Obesity among adults`
  )

# Remove missing values
selected <- selected %>%
  drop_na()

# Create variable description table
variable_table <- data.frame(
  `Variable Name` = c("Physical Inactivity (%)",
                      "Obesity (%)"),
  `Variable Type in Data Set` = c("Continuous",
                                  "Continuous"),
  `R Class` = c(class(selected$Physical_Inactivity),
                class(selected$Obesity))
)

# Print table
print(
  kable(variable_table,
        format = "latex",
        booktabs = TRUE,
        caption = "Description of Variables Included in the Analysis")
)
```

## Data Visualization

### Scatter plot

```{r, results='hide'}

# 
ggplot(selected, aes(x = Physical_Inactivity, y = Obesity)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Association Between Physical Inactivity and Obesity",
       x = "Physical Inactivity (%)",
       y = "Obesity (%)")
```

### Residual plot

```{r, results='hide'}

# 
model <- lm(Obesity ~ Physical_Inactivity, data = selected)

ggplot(data.frame(
  Fitted = fitted(model),
  Residuals = resid(model)
), aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residual Plot",
       x = "Fitted Values",
       y = "Residuals")
```

## Description of Plausible Relationships

Based on the visual examination of the scatter plot, physical inactivity and obesity appear to exhibit a positive linear relationship. Counties with higher levels of physical inactivity tend to have higher obesity prevalence. The upward-sloping regression line suggests that obesity increases approximately linearly as physical inactivity increases.

The relationship can be represented by a linear model of the form 
$Y = aX + b + \varepsilon$, where $a$ represents the effect size (slope), $b$ the intercept, and $\varepsilon$ the random error term (noise).

The residual plot shows that residuals are randomly distributed around zero without clear patterns or curvature, indicating that a linear model is appropriate. There is no strong evidence of nonlinearity or threshold effects. The spread of residuals appears relatively constant across fitted values, suggesting approximate homoscedasticity.

## Parameters of Interest

```{r, results='asis'}

# Define the key parameters
parameter_table <- data.frame(
  `Parameter` = c("Slope (effect size)", "Intercept", "Noise"),
  `Symbol` = c("$a$", "$b$", "$\\epsilon$"),
  `Description` = c(
    "Change in obesity per 1\\% increase in inactivity",
    "Baseline obesity when inactivity is zero",
    "Random variation not explained by the model"
  )
)

# Print table
print(
  kable(parameter_table,
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        caption = "Parameters Used in the Simulation Model")
)
```

# Simulation

## Simulation Basis

```{r, results='asis'}

# Set seed for reproducibility
set.seed(123)

# Choose one example setting
effect_example <- 0.5
noise_example <- 5
n_example <- 500

# Generate X values
X <- rnorm(n_example,
           mean = mean(selected$Physical_Inactivity),
           sd = sd(selected$Physical_Inactivity))

# Generate random noise
epsilon <- rnorm(n_example, mean = 0, sd = noise_example)

# Generate Y values
Y <- effect_example * X + 10 + epsilon

# Create simulated data frame
sim_data <- data.frame(
  Physical_Inactivity = X,
  Obesity = Y
)
```

## Simulation Output

```{r, results='asis'}

# Create stratified groups based on Obesity
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

# Print summary statistics table
print(
  kable(summary_stats,
        format = "latex",
        booktabs = TRUE,
        digits = 2,
        caption = "Stratified Summary Statistics of Physical Inactivity by Obesity Group")
)
```

## Simulation Function

```{r, results='hide'}

# Simulate data function
simulate_data <- function(effect_size, noise_sd, sample_size) {
  
  # Generate X values
  X <- rnorm(sample_size,
             mean = mean(selected$Physical_Inactivity),
             sd = sd(selected$Physical_Inactivity))
  
  # Generate noise
  epsilon <- rnorm(sample_size, mean = 0, sd = noise_sd)
  
  # Generate Y values
  Y <- effect_size * X + 10 + epsilon
  
  # Create simulated data set
  sim_data <- data.frame(
    Physical_Inactivity = X,
    Obesity = Y
  )
  
  # Create stratified summary statistics
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
```

## Simulation Automation

```{r, results='hide'}

# Check actual sample size
actual_n <- nrow(selected)

# Define parameter ranges
effect_sizes <- seq(0.1, 1, length.out = 10)

# Include actual sample size order of magnitude
sample_sizes <- unique(round(seq(100, actual_n, length.out = 10)))

noise_levels <- c(2, 5, 10)

# Create parameter grid
param_grid <- expand.grid(
  effect_size = effect_sizes,
  sample_size = sample_sizes,
  noise_level = noise_levels
)

# Initialize list
results <- vector("list", nrow(param_grid))

# Run simulations
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

# Show structure of one simulation result
str(results[[1]])
```

## Combine Summary Statistics

```{r, results='hide'}

# Combine simulation results into one summary data frame
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
```

# Visualization

```{r, results='hide'}

# Heatmap
ggplot(combined_summary,
       aes(x = sample_size,
           y = effect_size,
           fill = Mean_Difference)) +
  geom_tile() +
  facet_wrap(~noise_level) +
  labs(title = "Mean Difference in Physical Inactivity Between Obesity Groups",
       x = "Sample Size",
       y = "Effect Size",
       fill = "Mean Difference") +
  scale_fill_gradient(low = "white", high = "darkred")
```

# Interpretation

The simulation results show that the mean difference in physical inactivity between high and low obesity groups increases as the effect size increases. This pattern is consistent across all sample sizes and noise levels, indicating that stronger underlying associations produce clearer group differences.

Across panels, higher noise levels (e.g., noise = 10) reduce the magnitude of the mean difference compared to lower noise levels (e.g., noise = 2). This demonstrates how increased variability weakens the observable association between variables.

Sample size also influences the stability of the results. While the mean difference does not change dramatically with larger sample sizes, the pattern appears more consistent and less variable across effect sizes as sample size increases. This reflects improved precision with larger samples.

Overall, these simulations illustrate that stronger effect sizes and lower noise levels make relationships easier to detect, while higher variability reduces the clarity of the association. Larger sample sizes improve stability but cannot compensate for very small effect sizes or high noise.

# AI Use Disclosure Statement

*As part of this assignment, please indicate whether you used any AI-based tools (e.g., ChatGPT, Claude, Copilot, Gemini, etc.). Indicate:*
  
  * *Did you use AI? Yes / No*
  
  * *If yes: Write a short disclosure statement (2–3 sentences) describing:*
  
  * *Which tool(s) you used.*
  
  * *How you used it (e.g., to help decide on an analysis approach, to generate a first draft of code, to improve or debug code you had written yourself, etc.).*
  
  *Example: “This document was generated using Claude to generate original code, which was then reviewed and adjusted” or "This document was generate using ChatGPT to assist with debugging of code generated by the author".*
  
  *Please keep your statement brief and honest. The goal is transparency, not detail: we do not need exact prompts or transcripts, just a clear sense of how AI supported this work.*
  
  Yes. This document was generated with assistance from ChatGPT, which was used to help debug and improve code written by the author. The primary code and analysis were completed by the author.
