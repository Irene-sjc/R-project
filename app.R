# -----------------------------
# VTPEH 6270 - Check Point 07
# Shiny App
# Topic: Physical Inactivity and Obesity
# Author: Jinchan Sun (Irene)
# -----------------------------

# Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

# -----------------------------
# 1. Load and prepare data
# -----------------------------

# Load smaller sample data for GitHub reproducibility
# Make sure PLACES_sample_shinyapp.csv is uploaded to the same folder as app.R,
# or inside a data/ folder.

possible_files <- c(
  "PLACES_sample_shinyapp.csv",
  "data/PLACES_sample_shinyapp.csv"
)

data_file <- possible_files[file.exists(possible_files)][1]

if (is.na(data_file)) {
  stop("Data file not found. Please upload PLACES_sample_shinyapp.csv to the app folder or data/ folder.")
}

places_raw <- read.csv(
  data_file,
  stringsAsFactors = FALSE
)

# Make sure Data_Value is numeric
places_raw$Data_Value <- as.numeric(places_raw$Data_Value)

# Keep crude prevalence and year 2023 only
places_clean <- places_raw %>%
  filter(
    Data_Value_Type == "Crude prevalence",
    Year == 2023
  )

# Keep only the 2 measures we need
real_data_long <- places_clean %>%
  filter(
    Measure %in% c(
      "Obesity among adults",
      "No leisure-time physical activity among adults"
    )
  ) %>%
  select(StateAbbr, LocationName, Measure, Data_Value)

# If duplicate rows exist for the same county + measure, average them
real_data <- real_data_long %>%
  group_by(StateAbbr, LocationName, Measure) %>%
  summarise(
    Data_Value = mean(Data_Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Measure,
    values_from = Data_Value
  ) %>%
  rename(
    State = StateAbbr,
    County = LocationName,
    Obesity = `Obesity among adults`,
    Physical_Inactivity = `No leisure-time physical activity among adults`
  ) %>%
  drop_na()

# Create inactivity group for boxplot
median_inactivity <- median(real_data$Physical_Inactivity, na.rm = TRUE)

real_data <- real_data %>%
  mutate(
    Inactivity_Group = ifelse(
      Physical_Inactivity > median_inactivity,
      "High Inactivity",
      "Low Inactivity"
    )
  )

# State choices for user input
state_choices <- c("All States", sort(unique(as.character(real_data$State))))

# -----------------------------
# 2. Simulation function
# -----------------------------

simulate_data <- function(effect_size, noise_sd, sample_size, x_mean, x_sd) {
  x <- rnorm(sample_size, mean = x_mean, sd = x_sd)
  epsilon <- rnorm(sample_size, mean = 0, sd = noise_sd)
  y <- effect_size * x + 10 + epsilon
  
  data.frame(
    Physical_Inactivity = x,
    Obesity = y
  )
}

# -----------------------------
# 3. UI
# -----------------------------

ui <- navbarPage(
  title = div(icon("chart-column"), "CDC PLACES Explorer"),
  
  header = tags$head(
    tags$style(HTML("
      body {
        background-color: #f7fafc;
        font-family: Arial, Helvetica, sans-serif;
      }

      .navbar-default {
        background-color: #1f4e79;
        border-color: #163a5c;
      }

      .navbar-default .navbar-brand,
      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-weight: 600;
      }

      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > .active > a:hover,
      .navbar-default .navbar-nav > .active > a:focus {
        background-color: #2e75b6 !important;
        color: white !important;
      }

      .well {
        background-color: #eef5fb;
        border: 1px solid #c9dff2;
        border-radius: 10px;
      }

      .btn-default, .btn-primary {
        background-color: #2e75b6 !important;
        color: white !important;
        border-color: #2e75b6 !important;
        border-radius: 6px;
      }

      .btn-default:hover, .btn-primary:hover {
        background-color: #1f5d96 !important;
        border-color: #1f5d96 !important;
      }

      h2, h3, h4 {
        color: #1f4e79;
        font-weight: 700;
      }

      .info-box {
        background-color: #ffffff;
        border-left: 5px solid #2e75b6;
        padding: 14px 16px;
        margin-bottom: 14px;
        border-radius: 6px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06);
      }

      .section-card {
        background-color: #ffffff;
        border: 1px solid #dde7f0;
        border-radius: 8px;
        padding: 18px;
        margin-bottom: 16px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.05);
      }

      table {
        background-color: white;
      }

      a {
        color: #2e75b6;
        font-weight: 600;
      }
    "))
  ),
  
  # -----------------------------
  # Overview tab
  # -----------------------------
  tabPanel(
    "Overview",
    fluidPage(
      br(),
      div(
        class = "section-card",
        h2("Exploring the Association Between Physical Inactivity and Obesity"),
        p("This Shiny app examines the relationship between county-level physical inactivity and adult obesity prevalence using CDC PLACES data."),
        p("Users can explore the observed association in real county-level data and also use a simulation tool to examine how effect size, noise, and sample size influence statistical results.")
      ),
      
      fluidRow(
        column(
          6,
          div(
            class = "info-box",
            h4("Author"),
            p("Jinchan Sun (Irene)")
          )
        ),
        column(
          6,
          div(
            class = "info-box",
            h4("Research Question"),
            p("Is adult obesity prevalence associated with physical inactivity at the county level?")
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          div(
            class = "info-box",
            h4("Data Source"),
            p("PLACES: Local Data for Better Health, County Data, 2025 release."),
            tags$p(
              tags$a(
                href = "https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-County-Data-20/swc5-untb/about_data",
                target = "_blank",
                "Open the PLACES data source website"
              )
            )
          )
        ),
        column(
          6,
          div(
            class = "info-box",
            h4("GitHub Repository"),
            tags$p(
              tags$a(
                href = "https://github.com/Irene-sjc/R-project.git",
                target = "_blank",
                "Open project GitHub repository"
              )
            )
          )
        )
      ),
      
      div(
        class = "section-card",
        h4("Data File Note"),
        p("Because the original CDC PLACES 2025 county dataset is too large for GitHub, this repository uses a smaller sample file named PLACES_sample_shinyapp.csv. The sample retains the variables needed to reproduce the Shiny app.")
      ),
      
      div(
        class = "section-card",
        h4("How to Use This App"),
        tags$ul(
          tags$li("Use the Real Data Explorer tab to visualize the observed relationship in the CDC PLACES dataset."),
          tags$li("Choose a state and plot type, then click the button to update the display."),
          tags$li("Use the Simulation Explorer tab to change effect size, noise, and sample size, then run the simulation."),
          tags$li("Review the plot, summary table, and interpretation under each output.")
        )
      ),
      
      div(
        class = "section-card",
        h4("Methods Summary"),
        p("The real-data section uses county-level crude prevalence estimates for 2023. The app focuses on two variables: obesity among adults and no leisure-time physical activity among adults. The simulation section generates synthetic data based on a simple linear model to illustrate how effect size, variability, and sample size can affect results.")
      ),
      
      div(
        class = "section-card",
        h4("AI Use Disclosure"),
        p("This app was developed with assistance from ChatGPT to support debugging, code refinement, and interface improvement. The primary code, analysis design, project topic, and final review were completed by the author.")
      )
    )
  ),
  
  # -----------------------------
  # Real Data Explorer tab
  # -----------------------------
  tabPanel(
    "Real Data Explorer",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "state_select",
          "Select state:",
          choices = state_choices,
          selected = "All States"
        ),
        
        selectInput(
          "plot_type",
          "Select plot type:",
          choices = c("Scatterplot", "Boxplot", "Histogram"),
          selected = "Scatterplot"
        ),
        
        checkboxInput(
          "show_lm",
          "Add regression line (for scatterplot only)",
          value = TRUE
        ),
        
        sliderInput(
          "point_alpha",
          "Point transparency:",
          min = 0.1,
          max = 1,
          value = 0.5,
          step = 0.1
        ),
        
        actionButton(
          "update_real",
          "Update Plot"
        )
      ),
      
      mainPanel(
        plotOutput("real_plot", height = "500px"),
        br(),
        tableOutput("summary_table"),
        br(),
        htmlOutput("real_text")
      )
    )
  ),
  
  # -----------------------------
  # Simulation Explorer tab
  # -----------------------------
  tabPanel(
    "Simulation Explorer",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "effect_size",
          "Effect size (slope):",
          min = 0.1,
          max = 1.5,
          value = 0.5,
          step = 0.1
        ),
        
        sliderInput(
          "noise_sd",
          "Noise (SD):",
          min = 1,
          max = 15,
          value = 5,
          step = 1
        ),
        
        sliderInput(
          "sample_size",
          "Sample size:",
          min = 50,
          max = 3000,
          value = 500,
          step = 50
        ),
        
        actionButton(
          "run_sim",
          "Run Simulation"
        )
      ),
      
      mainPanel(
        plotOutput("sim_plot", height = "500px"),
        br(),
        tableOutput("sim_summary"),
        br(),
        htmlOutput("sim_text")
      )
    )
  )
)

# -----------------------------
# 4. Server
# -----------------------------

server <- function(input, output, session) {
  
  # -----------------------------
  # Real data reactive subset
  # -----------------------------
  filtered_real_data <- eventReactive(input$update_real, {
    
    if (input$state_select == "All States") {
      df <- real_data
    } else {
      df <- real_data %>%
        filter(State == input$state_select)
    }
    
    df
  }, ignoreNULL = FALSE)
  
  # -----------------------------
  # Real data plot
  # -----------------------------
  output$real_plot <- renderPlot({
    df <- filtered_real_data()
    
    req(nrow(df) > 0)
    
    if (input$plot_type == "Scatterplot") {
      
      p <- ggplot(df, aes(x = Physical_Inactivity, y = Obesity)) +
        geom_point(alpha = input$point_alpha, color = "#2e75b6") +
        labs(
          title = "Association Between Physical Inactivity and Obesity",
          x = "Physical Inactivity (%)",
          y = "Obesity Prevalence (%)"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#1f4e79"),
          axis.title = element_text(face = "bold")
        )
      
      if (input$show_lm) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "#d62728", linewidth = 1.2)
      }
      
      p
      
    } else if (input$plot_type == "Boxplot") {
      
      ggplot(df, aes(x = Inactivity_Group, y = Obesity, fill = Inactivity_Group)) +
        geom_boxplot(alpha = 0.9) +
        scale_fill_manual(values = c("High Inactivity" = "#f28e2b", "Low Inactivity" = "#59a14f")) +
        labs(
          title = "Obesity Prevalence by Physical Inactivity Group",
          x = "Physical Inactivity Group",
          y = "Obesity Prevalence (%)"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#1f4e79"),
          axis.title = element_text(face = "bold"),
          legend.position = "none"
        )
      
    } else {
      
      ggplot(df, aes(x = Obesity)) +
        geom_histogram(bins = 30, fill = "#4e79a7", color = "white", alpha = 0.95) +
        labs(
          title = "Distribution of Adult Obesity Prevalence",
          x = "Obesity Prevalence (%)",
          y = "Number of Counties"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", color = "#1f4e79"),
          axis.title = element_text(face = "bold")
        )
    }
  })
  
  # -----------------------------
  # Real data summary table
  # -----------------------------
  output$summary_table <- renderTable({
    df <- filtered_real_data()
    
    data.frame(
      Statistic = c(
        "Number of counties",
        "Mean obesity (%)",
        "Median obesity (%)",
        "Mean physical inactivity (%)",
        "Median physical inactivity (%)"
      ),
      Value = c(
        nrow(df),
        round(mean(df$Obesity, na.rm = TRUE), 2),
        round(median(df$Obesity, na.rm = TRUE), 2),
        round(mean(df$Physical_Inactivity, na.rm = TRUE), 2),
        round(median(df$Physical_Inactivity, na.rm = TRUE), 2)
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # -----------------------------
  # Real data interpretation text
  # -----------------------------
  output$real_text <- renderUI({
    df <- filtered_real_data()
    
    if (input$plot_type == "Scatterplot") {
      model <- lm(Obesity ~ Physical_Inactivity, data = df)
      slope <- coef(model)[2]
      r2 <- summary(model)$r.squared
      
      div(
        style = "margin-top: 10px; padding: 12px; border: 1px solid #c9dff2; border-radius: 8px; background-color: #f4f9fe;",
        paste0(
          "Interpretation: In the selected data, obesity prevalence tends to ",
          ifelse(slope > 0, "increase", "decrease"),
          " as physical inactivity increases. The estimated slope is ",
          round(slope, 3),
          ", and the model R-squared is ",
          round(r2, 3),
          "."
        )
      )
      
    } else if (input$plot_type == "Boxplot") {
      
      high_mean <- mean(df$Obesity[df$Inactivity_Group == "High Inactivity"], na.rm = TRUE)
      low_mean <- mean(df$Obesity[df$Inactivity_Group == "Low Inactivity"], na.rm = TRUE)
      
      div(
        style = "margin-top: 10px; padding: 12px; border: 1px solid #c9dff2; border-radius: 8px; background-color: #f4f9fe;",
        paste0(
          "Interpretation: Counties in the high inactivity group have an average obesity prevalence of ",
          round(high_mean, 2),
          "%, compared with ",
          round(low_mean, 2),
          "% in the low inactivity group."
        )
      )
      
    } else {
      
      div(
        style = "margin-top: 10px; padding: 12px; border: 1px solid #c9dff2; border-radius: 8px; background-color: #f4f9fe;",
        "Interpretation: This histogram shows how obesity prevalence is distributed across the selected counties."
      )
    }
  })
  
  # -----------------------------
  # Simulated data
  # -----------------------------
  sim_data_reactive <- eventReactive(input$run_sim, {
    simulate_data(
      effect_size = input$effect_size,
      noise_sd = input$noise_sd,
      sample_size = input$sample_size,
      x_mean = mean(real_data$Physical_Inactivity, na.rm = TRUE),
      x_sd = sd(real_data$Physical_Inactivity, na.rm = TRUE)
    )
  }, ignoreNULL = FALSE)
  
  # -----------------------------
  # Simulation plot
  # -----------------------------
  output$sim_plot <- renderPlot({
    sim_df <- sim_data_reactive()
    
    ggplot(sim_df, aes(x = Physical_Inactivity, y = Obesity)) +
      geom_point(alpha = 0.5, color = "#6f42c1") +
      geom_smooth(method = "lm", se = FALSE, color = "#d62728", linewidth = 1.2) +
      labs(
        title = "Simulated Association Between Physical Inactivity and Obesity",
        x = "Physical Inactivity (%)",
        y = "Obesity (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", color = "#1f4e79"),
        axis.title = element_text(face = "bold")
      )
  })
  
  # -----------------------------
  # Simulation summary table
  # -----------------------------
  output$sim_summary <- renderTable({
    sim_df <- sim_data_reactive()
    model <- lm(Obesity ~ Physical_Inactivity, data = sim_df)
    tidy_model <- broom::tidy(model)
    
    slope_row <- tidy_model[tidy_model$term == "Physical_Inactivity", ]
    
    data.frame(
      Statistic = c(
        "Sample size",
        "Mean obesity (%)",
        "Mean physical inactivity (%)",
        "Estimated slope",
        "P-value"
      ),
      Value = c(
        nrow(sim_df),
        round(mean(sim_df$Obesity), 2),
        round(mean(sim_df$Physical_Inactivity), 2),
        round(slope_row$estimate, 3),
        signif(slope_row$p.value, 3)
      )
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")
  
  # -----------------------------
  # Simulation interpretation text
  # -----------------------------
  output$sim_text <- renderUI({
    sim_df <- sim_data_reactive()
    model <- lm(Obesity ~ Physical_Inactivity, data = sim_df)
    slope <- coef(model)[2]
    r2 <- summary(model)$r.squared
    
    div(
      style = "margin-top: 10px; padding: 12px; border: 1px solid #e2d9f3; border-radius: 8px; background-color: #faf7ff;",
      paste0(
        "Interpretation: In this simulation, the estimated relationship between physical inactivity and obesity is positive when the slope is above zero. With the current settings, the estimated slope is ",
        round(slope, 3),
        " and the model R-squared is ",
        round(r2, 3),
        ". Larger effect sizes usually make the trend easier to detect, while higher noise makes the relationship less clear."
      )
    )
  })
}

# -----------------------------
# 5. Run app
# -----------------------------
shinyApp(ui = ui, server = server)