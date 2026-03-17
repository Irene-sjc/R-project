---
  title: "VTPEH 6270 - Check Point 02"
author: "Jinchan Sun"
date: "`r Sys.Date()`"
output:
  pdf_document:
  toc: true
toc_depth: 1
number_sections: true
urlcolor: blue
editor_options:
  chunk_output_type: console
---
  
  ```{r setup, include=FALSE}

#
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

# Introduction

## Background

County-level estimates help describe how health risks are distributed across the United States and can highlight geographic disparities. Obesity is a major public health concern because it increases the risk of diabetes, cardiovascular disease, and other chronic conditions. Using CDC PLACES county data, this report explores the distribution of adult obesity prevalence across counties and examines whether obesity prevalence varies with county population size.

## Project Question

What is the distribution of adult obesity prevalence across U.S. counties, and is adult obesity prevalence associated with county total population?
  
  ## Data Set Overview
  
  This dataset comes from CDC PLACES: Local Data for Better Health, which provides model-based county-level estimates for health measures among U.S. adults aged 18 years and older. Measures are primarily derived from BRFSS data and statistical modeling. The dataset includes all U.S. counties and reports each measure with its unit and confidence limits.

Dataset link:
  https://www.cdc.gov/places

# Data Set Exploration

## Import Data as a Data Frame

```{r, results='hide'}

# Packages
library(ggplot2)
# Load file
places_raw <- read.csv(
  "data/raw/PLACES__Local_Data_for_Better_Health_County_Data_2025.csv",
  stringsAsFactors = FALSE
)
# Check structure
head(places_raw)
```

## Data Cleaning and Tidy Format

```{r, results='hide'}

# Convert numeric columns
to_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL")] <- NA
  suppressWarnings(as.numeric(x))
}

places_raw$Data_Value <- to_num(places_raw$Data_Value)
places_raw$Low_Confidence_Limit <- to_num(places_raw$Low_Confidence_Limit)
places_raw$High_Confidence_Limit <- to_num(places_raw$High_Confidence_Limit)
places_raw$TotalPopulation <- to_num(places_raw$TotalPopulation)
```

## Variable Overview table

```{r variable_overview}
variable_table <- data.frame(
  Variable = c("StateAbbr","LocationName","Measure","Data_Value","Data_Value_Unit",
               "Low_Confidence_Limit","High_Confidence_Limit","TotalPopulation"),
  Type = c("Categorical","Categorical","Categorical","Continuous","Categorical",
           "Continuous","Continuous","Continuous"),
  Class = c(class(places_raw$StateAbbr)[1], class(places_raw$LocationName)[1],
            class(places_raw$Measure)[1], class(places_raw$Data_Value)[1],
            class(places_raw$Data_Value_Unit)[1], class(places_raw$Low_Confidence_Limit)[1],
            class(places_raw$High_Confidence_Limit)[1], class(places_raw$TotalPopulation)[1]),
  row.names = NULL
)

knitr::kable(
  variable_table,
  caption = "Table 1. Overview of selected variables in the CDC PLACES county dataset."
)
```

## Data Visualization (Histogram)

```{r histogram_obesity}

# Chosen measure: Adult obesity prevalence
chosen_measure <- "Obesity among adults"

if (!chosen_measure %in% places_raw$Measure) {
  stop("chosen_measure not found. Check spelling with: unique(places_raw$Measure)[1:20]")
}

obesity_data <- places_raw[places_raw$Measure == chosen_measure & !is.na(places_raw$Data_Value), ]

unit_val <- unique(obesity_data$Data_Value_Unit[!is.na(obesity_data$Data_Value_Unit)])[1]
if (is.na(unit_val)) unit_val <- ""
# Pre-calc mean/median so legend is clear (and stable)
mean_val <- mean(obesity_data$Data_Value)
median_val <- median(obesity_data$Data_Value)

ggplot(obesity_data, aes(x = Data_Value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(aes(xintercept = mean_val, color = "Mean"), linewidth = 1) +
  geom_vline(aes(xintercept = median_val, color = "Median"), linewidth = 1, linetype = "dashed") +
  scale_color_manual(values = c("Mean" = "red", "Median" = "black")) +
  labs(
    title = paste("Distribution of", chosen_measure, "Across U.S. Counties"),
    x = paste0("Prevalence (", unit_val, ")"),
    y = "Number of Counties",
    color = "",
    caption = "Histogram of county-level estimates from CDC PLACES."
  ) +
  theme_minimal()
```

## Data Visualization (Scatterplot)

```{r scatter_obesity_population}

#
scatter_data <- obesity_data[
  !is.na(obesity_data$TotalPopulation) &
    !is.na(obesity_data$Data_Value),
]
#
ggplot(scatter_data, aes(x = TotalPopulation, y = Data_Value)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  labs(
    title = paste(chosen_measure, "vs County Total Population"),
    x = "Total Population (log10 scale)",
    y = paste0("Prevalence (", unit_val, ")"),
    caption = "Scatterplot of county-level estimates from CDC PLACES."
  ) +
  theme_classic()
```

# Discussion

## Interpretation of the Histogram

The histogram shows the distribution of adult obesity prevalence across U.S. counties. Most counties have obesity prevalence between approximately 30% and 45%, with a clear peak around the high 30s. The mean and median are close to each other, suggesting a roughly symmetric distribution with no extreme skewness. However, there are still some counties with notably lower or higher prevalence, indicating geographic variation in obesity burden across the United States.

## Interpretation of the Scatterplot

The scatterplot examines the relationship between county total population (on a log10 scale) and adult obesity prevalence. Overall, there is no strong or clear linear association between population size and obesity prevalence. Counties with both small and large populations show a wide range of obesity prevalence values. This suggests that county population size alone does not strongly predict adult obesity prevalence. 

## Conclusion

These results suggest that adult obesity prevalence varies substantially across U.S. counties, but this variation does not appear to be strongly related to county population size.

# AI Use Disclosure Statement

*As part of this assignment, please indicate whether you used any AI-based tools (e.g., ChatGPT, Claude, Copilot, Gemini, etc.). Indicate:*
  
  * *Did you use AI? Yes / No*
  
  * *If yes: Write a short disclosure statement (2–3 sentences) describing:*
  
  * *Which tool(s) you used.*
  
  * *How you used it (e.g., to help decide on an analysis approach, to generate a first draft of code, to improve or debug code you had written yourself, etc.).*
  
  *Example: “This document was generated using Claude to generate original code, which was then reviewed and adjusted” or "This document was generate using ChatGPT to assist with debugging of code generated by the author".*
  
  *Please keep your statement brief and honest. The goal is transparency, not detail: we do not need exact prompts or transcripts, just a clear sense of how AI supported this work.*
  
  Yes. This document was generated with assistance from ChatGPT, which was used to help debug and improve code written by the author. The primary code and analysis were completed by the author.