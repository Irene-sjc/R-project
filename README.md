# 📓 County-Level Analysis of Obesity and Physical Inactivity in the United States

## 📝 Project Description
This project analyzes county-level health data from the CDC PLACES dataset to explore patterns and relationships in adult obesity prevalence across the United States. The project combines exploratory data analysis, comparative analysis, and simulation to better understand how behavioral risk factors, such as physical inactivity, are associated with obesity.

---

## 🙋🏻‍♀️ Author and Affiliation
Jinchan Sun  
Cornell University  
Master of Public Health (MPH), Food Systems and Health  

---

## 📪 Contact Information
Email: js3875@cornell.edu  

---

## 🎯 Research Questions / Objectives
- What is the distribution of adult obesity prevalence across U.S. counties?  
- Is obesity prevalence associated with county population size?  
- Is there an association between physical inactivity and obesity at the county level?  
- How do effect size, noise, and sample size influence statistical results in simulation?  

---

## 📚 Data Source and Description
The dataset used in this project is:

CDC PLACES: Local Data for Better Health, County Data (2025 Release)  
https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb/about_data  

This dataset provides model-based estimates of health outcomes, prevention measures, and risk factors for U.S. adults aged 18 years and older at the county level. The estimates are primarily derived from the Behavioral Risk Factor Surveillance System (BRFSS) and statistical modeling.

---

## ⚠️ Data and Reproducibility Note

Due to file size limitations, this repository includes a reduced sample of the original dataset (`data/raw/PLACES_sample.csv`) to ensure reproducibility.

All analysis scripts (`.R` files) and figures stored in the `output/` directory are generated using this sample dataset. This allows the code to run successfully on any machine and ensures that the results can be fully reproduced.

The PDF reports (CP02–CP04), also stored in the `output/` directory, were generated using the full dataset, which is not included in this repository due to its large size. Therefore, numerical results and visualizations in the reports may differ slightly from those produced by running the scripts with the sample dataset.

To maintain clarity and reproducibility, only one consistent set of figures (generated from the sample dataset) is included in the `output/` folder.

---

## 🗂️ Repository Structure

R-project/
│
├── data/
│   └── raw/
│       └── PLACES_sample.csv
│
├── scripts/
│   ├── Data_exploration.R
│   ├── Analysis.R
│   └── Simulation.R
│
├── output/
│   ├── figures/
│   └── reports/
│
├── Shiny app/
│   ├── app.R
│   └── PLACES_sample_shinyapp.csv
│
├── Check Point 06/
│   ├── VTPEH6270-CP06.Rmd
│   ├── VTPEH6270--CP06.pdf
│   └── references.bib
│
├── README.md
├── .gitignore
└── R-project.Rproj

---

## 🔗 Links to Reports or Deliverables
This repository includes:
- Data exploration and visualization scripts  
- Comparative analysis results  
- Simulation analysis outputs  
- Figures and visualizations generated from the analysis  

(All results are contained within this repository.)

### 📑 Reports

- **CP02 – Exploratory Data Analysis**  
  [View Report](./output/reports/CP02_report.pdf)

- **CP03 – Comparative Analysis**  
  [View Report](./output/reports/CP03_report.pdf)

- **CP04 – Simulation Study**  
  [View Report](./output/reports/CP04_report.pdf)
  
---

## 🖥️ AI Tool Disclosure
This project used ChatGPT to assist with debugging and improving R code. All analytical decisions, interpretations, and final outputs were completed by the author.

---

## 📖 References
Centers for Disease Control and Prevention (CDC). PLACES: Local Data for Better Health, County Data (2025 Release).  
https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb/about_data  

World Health Organization. Physical activity. 2024.  
https://www.who.int/news-room/fact-sheets/detail/physical-activity

Silveira EA, Mendonça CR, Delpino FM, et al. Sedentary behavior, physical inactivity, abdominal obesity and obesity in adults and older adults: A systematic review and meta-analysis. Clin Nutr ESPEN. 2022;50:63-73. https://doi.org/10.1016/j.clnesp.2022.06.001

Pietiläinen KH, Kaprio J, Borg P, et al. Physical inactivity and obesity: a vicious circle. Obesity (Silver Spring). 2008;16(2):409-414. https://doi.org/10.1038/oby.2007.72
