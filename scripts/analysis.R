---
  title: "VTPEH 6270 - Check Point 03"
author: "Jinchan Sun"
date: "`r Sys.Date()`"
editor_options:
  chunk_output_type: console
output:
  pdf_document:
  toc: true
toc_depth: 1
number_sections: true
bibliography: references.bib
urlcolor: blue
---
  
  ```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

# Introduction

This report explores county-level health indicators using the CDC PLACES 2025 dataset. The dataset includes health outcomes, prevention measures, and risk factors across U.S. counties. The goal of this report is to explore potential associations between selected health indicators and demographic or behavioral characteristics at the county level.

# Research Question and Background

## Research Question

Is adult obesity prevalence associated with physical inactivity at the county level?
  
  ## Scientific Plausibility and Context
  
  Physical inactivity is widely recognized as a major public health concern. Global health authorities identify insufficient physical activity as an important risk factor for non-communicable diseases, including obesity [@WHO2024PhysicalActivity]. A recent systematic review reported consistent evidence that sedentary behavior and low physical activity are associated with increased abdominal obesity and overall adiposity in adults [@Silveira2022Sedentary]. Earlier research has further suggested a possible vicious cycle, in which physical inactivity contributes to weight gain, and increased body weight may in turn reduce physical activity levels over time [@Pietilainen2007PhysicalInactivity].

# Data Preparation

## Import Data

```{r, results='hide'}

# Packages
library(ggplot2)
library(dplyr)
# Load data
places <- read.csv(
  "data/raw/PLACES__Local_Data_for_Better_Health,_County_Data,_2025_release_20260203.csv",
  stringsAsFactors = FALSE
)
# Check structure
str(places)
```

## Data Cleaning and Rename

```{r, results='hide'}

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
# Check column names after reshape
colnames(data_wide)
# Rename columns safely
colnames(data_wide) <- c("State", "County",
                         "Obesity", "Inactivity")
# Remove missing values
data_clean <- na.omit(data_wide)
# Create categorical variable (median split)
median_inactivity <- median(data_clean$Inactivity, na.rm = TRUE)

data_clean <- data_clean %>%
  mutate(Inactivity_Group = ifelse(Inactivity > median_inactivity,
                                   "High Inactivity",
                                   "Low Inactivity"))
```
The dataset was cleaned by selecting county-level obesity and physical inactivity measures. Missing values were removed. Physical inactivity was categorized into “High” and “Low” groups based on the median value to allow stratified analysis and visualization.

# Grouped Summary Statistics

## Stratified Analysis
```{r, results='hide'}

#
summary_table <- data_clean %>%
  group_by(Inactivity_Group) %>%
  summarise(
    Mean_Obesity = mean(Obesity),
    SD_Obesity = sd(Obesity),
    Median_Obesity = median(Obesity),
    N = n()
  )

summary_table
```

## Interpretation

Counties with high physical inactivity show a higher mean obesity prevalence compared to counties with low inactivity. This suggests a possible positive association between inactivity and obesity at the county level.

# Comparative Visualizations

## Boxplot

```{r, results='hide'}

# 
ggplot(data_clean, aes(x = Inactivity_Group, y = Obesity)) +
  geom_boxplot() +
  labs(
    title = "Obesity Prevalence by Physical Inactivity Group",
    x = "Physical Inactivity Group",
    y = "Obesity Prevalence (%)"
  ) +
  theme_minimal()
```
The boxplot shows that counties with high physical inactivity have higher obesity prevalence compared to counties with low inactivity. The median obesity rate is clearly higher in the high inactivity group. In addition, the overall distribution of obesity values appears shifted upward for counties with higher inactivity levels.

Although there is some overlap between the two groups, the pattern suggests a positive association between physical inactivity and obesity at the county level. Counties with higher levels of inactivity tend to have higher obesity prevalence.

## Scatterplot

```{r, results='hide'}

#
ggplot(data_clean, aes(x = Inactivity, y = Obesity)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Association Between Physical Inactivity and Obesity",
    x = "Physical Inactivity (%)",
    y = "Obesity (%)"
  ) +
  theme_minimal()
```
The scatterplot shows a clear positive association between physical inactivity and obesity prevalence. As the percentage of adults with no leisure-time physical activity increases, the obesity rate also tends to increase. The upward slope of the regression line supports this pattern.

Although there is some variability among counties, the overall trend suggests that higher levels of physical inactivity are associated with higher obesity prevalence. This finding is consistent with the hypothesis that physical inactivity is associated with obesity at the population level.

## Save Publication-Style PDF Figure

```{r, results='hide'}
#
pdf("Figure_Obesity_Inactivity.pdf", width = 6, height = 4)

ggplot(data_clean, aes(x = Inactivity, y = Obesity)) +
  geom_point(size = 1.2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  theme_classic(base_size = 12) +
  labs(
    x = "Physical Inactivity (%)",
    y = "Obesity Prevalence (%)"
  )

dev.off()
```

# References

<div id="refs"></div>
  
  # AI Use Disclosure Statement
  
  *As part of this assignment, please indicate whether you used any AI-based tools (e.g., ChatGPT, Claude, Copilot, Gemini, etc.). Indicate:*
  
  * *Did you use AI? Yes / No*
  
  * *If yes: Write a short disclosure statement (2–3 sentences) describing:*
  
  * *Which tool(s) you used.*
  
  * *How you used it (e.g., to help decide on an analysis approach, to generate a first draft of code, to improve or debug code you had written yourself, etc.).*
  
  *Example: “This document was generated using Claude to generate original code, which was then reviewed and adjusted” or "This document was generate using ChatGPT to assist with debugging of code generated by the author".*
  
  *Please keep your statement brief and honest. The goal is transparency, not detail: we do not need exact prompts or transcripts, just a clear sense of how AI supported this work.*
  
  Yes. This document was generated with assistance from ChatGPT, which was used to help debug and improve code written by the author. The primary code and analysis were completed by the author.