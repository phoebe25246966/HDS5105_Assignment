#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(survival)
library(rlang) 

dig_raw <- read.csv("/Users/sararaees/Downloads/DIG-1.csv")

dig <- dig_raw %>%
  mutate(
    TRTMT = factor(TRTMT, levels = c(0, 1),
                   labels = c("Placebo", "Digoxin")),
    SEX   = factor(SEX,   levels = c(1, 2),
                   labels = c("Male", "Female")),
    RACE  = factor(RACE,  levels = c(1, 2),
                   labels = c("White", "Non-white")),
    DEATH = factor(DEATH, levels = c(0, 1),
                   labels = c("Alive", "Dead")),
    HOSP  = factor(HOSP,  levels = c(0, 1),
                   labels = c("No", "Yes")),
    CVD   = factor(CVD,   levels = c(0, 1),
                   labels = c("No", "Yes")),
    WHF   = factor(WHF,   levels = c(0, 1),
                   labels = c("No", "Yes")),
    #Month = DEATHDAY / 30, rounded to nearest whole number
    Month = round(DEATHDAY / 30)
  )

exclude_vars <- c("ID")
all_vars <- setdiff(names(dig), exclude_vars)

is_numeric_var <- function(x) is.numeric(x) && length(unique(x[!is.na(x)])) > 10

# Define UI for application that draws a histogram
# My First App
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "DIG Trial Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Baseline characteristics", tabName = "baseline", icon = icon("table")),
      menuItem("Variable explorer", tabName = "univariate", icon = icon("chart-bar")),
      menuItem("Relationship explorer", tabName = "bivariate", icon = icon("project-diagram")),
      menuItem("Survival (Months)", tabName = "survival", icon = icon("heartbeat")),
      br(),
      tags$hr(),
      menuItem("Global filters", icon = icon("filter"), startExpanded = TRUE,
               selectInput(
                 "filter_trt", "Treatment group",
                 choices = c("Both" = "both",
                             "Placebo" = "Placebo",
                             "Digoxin" = "Digoxin"),
                 selected = "both"
               ),
               selectInput(
                 "filter_sex", "Sex",
                 choices = c("Both" = "both",
                             "Male" = "Male",
                             "Female" = "Female"),
                 selected = "both"
               ),
               sliderInput(
                 "filter_age", "Age (years)",
                 min = floor(min(dig$AGE, na.rm = TRUE)),
                 max = ceiling(max(dig$AGE, na.rm = TRUE)),
                 value = c(floor(min(dig$AGE, na.rm = TRUE)),
                           ceiling(max(dig$AGE, na.rm = TRUE))),
                 step = 1
               )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      

      # Overview tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_n"),
          valueBoxOutput("vb_age"),
          valueBoxOutput("vb_death"),
          valueBoxOutput("vb_followup")
        ),
        fluidRow(
          box(
            width = 6,
            title = "Patients by treatment group",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_trt_counts", height = 300)
          ),
          box(
            width = 6,
            title = "Death and hospitalisation by treatment",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_outcomes_by_trt", height = 300)
          )
        )
      ),
      

      # Baseline characteristics tab
      tabItem(
        tabName = "baseline",
        fluidRow(
          box(
            width = 12,
            title = "Baseline characteristics by treatment group",
            status = "primary",
            solidHeader = TRUE,
            DTOutput("tbl_baseline")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Age distribution by treatment",
            status = "info",
            solidHeader = TRUE,
            plotOutput("plot_age_trt")
          ),
          box(
            width = 6,
            title = "Ejection fraction by treatment",
            status = "info",
            solidHeader = TRUE,
            plotOutput("plot_ejf_trt")
          )
        )
      ),
      

      # Variable explorer tab
      tabItem(
        tabName = "univariate",
        fluidRow(
          box(
            width = 4,
            title = "Choose a variable",
            status = "primary",
            solidHeader = TRUE,
            selectInput("var_single", "Variable",
                        choices = all_vars,
                        selected = "AGE"),
            helpText("Numeric variables → histogram; categorical variables → bar chart.")
          ),
          box(
            width = 8,
            title = "Distribution of selected variable",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_single"),
            br(),
            verbatimTextOutput("summary_single")
          )
        )
      ),
      

      # Relationship explorer tab
      tabItem(
        tabName = "bivariate",
        fluidRow(
          box(
            width = 4,
            title = "Choose variables",
            status = "primary",
            solidHeader = TRUE,
            selectInput("xvar", "X-axis variable", choices = all_vars, selected = "AGE"),
            selectInput("yvar", "Y-axis / outcome variable", choices = all_vars, selected = "EJF_PER"),
            checkboxInput("color_by_trt", "Colour by treatment group", value = TRUE),
            helpText("Numeric vs numeric → scatter; numeric vs categorical → boxplot; categorical vs categorical → stacked bar.")
          ),
          box(
            width = 8,
            title = "Relationship between variables",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("plot_bivariate"),
            br(),
            verbatimTextOutput("summary_bivariate")
          )
        )
      ),
      

      # Survival (Months) tab
      tabItem(
        tabName = "survival",
        fluidRow(
          box(
            width = 4,
            title = "Follow-up in Months",
            status = "primary",
            solidHeader = TRUE,
            helpText("Month is defined as round(DEATHDAY / 30)."),
            verbatimTextOutput("summary_month")
          ),
          box(
            width = 8,
            title = "Overall survival curve (Question 9)",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("km_overall", height = 300),
            helpText("Step curve shows the estimated probability of remaining alive (survival rate) over months.")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Survival by treatment (Question 10)",
            status = "info",
            solidHeader = TRUE,
            plotOutput("km_trt", height = 300),
            tableOutput("tbl_surv_trt")
          ),
          box(
            width = 6,
            title = "Survival by treatment and CVD (Question 11)",
            status = "info",
            solidHeader = TRUE,
            plotOutput("km_trt_cvd", height = 300),
            tableOutput("tbl_surv_trt_cvd")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  

  # Global filtered dataset
  filtered_data <- reactive({
    dat <- dig
    
    if (input$filter_trt != "both") {
      dat <- dat %>% filter(TRTMT == input$filter_trt)
    }
    if (input$filter_sex != "both") {
      dat <- dat %>% filter(SEX == input$filter_sex)
    }
    dat <- dat %>% filter(AGE >= input$filter_age[1],
                          AGE <= input$filter_age[2])
    dat
  })
  
  # Overview value boxes
  output$vb_n <- renderValueBox({
    n_pat <- nrow(filtered_data())
    valueBox(
      formatC(n_pat, big.mark = ","),
      subtitle = "Patients in current selection",
      icon = icon("users"),
      color = "teal"
    )
  })
  
  output$vb_age <- renderValueBox({
    dat <- filtered_data()
    age_mean <- mean(dat$AGE, na.rm = TRUE)
    age_sd   <- sd(dat$AGE, na.rm = TRUE)
    valueBox(
      sprintf("%.1f (SD %.1f)", age_mean, age_sd),
      subtitle = "Age (years)",
      icon = icon("user-clock"),
      color = "purple"
    )
  })
  
  output$vb_death <- renderValueBox({
    dat <- filtered_data()
    if (!is.factor(dat$DEATH)) dat$DEATH <- factor(dat$DEATH)
    p_dead <- mean(dat$DEATH == "Dead", na.rm = TRUE)
    valueBox(
      sprintf("%.1f%%", 100 * p_dead),
      subtitle = "Died during follow-up",
      icon = icon("heart-broken"),
      color = "red"
    )
  })
  
  output$vb_followup <- renderValueBox({
    dat <- filtered_data()
    med_fu <- median(dat$DEATHDAY, na.rm = TRUE)
    valueBox(
      sprintf("%.0f days", med_fu),
      subtitle = "Median follow-up",
      icon = icon("calendar-alt"),
      color = "orange"
    )
  })
  

  # Overview plots
  output$plot_trt_counts <- renderPlot({
    dat <- filtered_data()
    ggplot(dat, aes(x = TRTMT)) +
      geom_bar() +
      labs(x = "Treatment group", y = "Number of patients") +
      theme_minimal(base_size = 13)
  })
  
  output$plot_outcomes_by_trt <- renderPlot({
    dat <- filtered_data()
    dat <- dat %>%
      select(TRTMT, DEATH, HOSP) %>%
      mutate(
        death_event = DEATH == "Dead",
        hosp_event  = HOSP == "Yes"
      ) %>%
      group_by(TRTMT) %>%
      summarise(
        Death = mean(death_event, na.rm = TRUE),
        Hospitalisation = mean(hosp_event, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(cols = c("Death", "Hospitalisation"),
                          names_to = "Outcome",
                          values_to = "Rate")
    
    ggplot(dat, aes(x = TRTMT, y = Rate, fill = Outcome)) +
      geom_col(position = position_dodge()) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Treatment group", y = "Event rate") +
      theme_minimal(base_size = 13)
  })
  

  # Baseline table
  output$tbl_baseline <- renderDT({
    dat <- filtered_data()
    
    baseline <- dat %>%
      group_by(TRTMT) %>%
      summarise(
        N = n(),
        `Age, mean (SD)` = sprintf("%.1f (%.1f)",
                                   mean(AGE, na.rm = TRUE),
                                   sd(AGE, na.rm = TRUE)),
        `Male, n (%%)` = sprintf("%d (%.1f%%)",
                                 sum(SEX == "Male", na.rm = TRUE),
                                 100 * mean(SEX == "Male", na.rm = TRUE)),
        `BMI, mean (SD)` = sprintf("%.1f (%.1f)",
                                   mean(BMI, na.rm = TRUE),
                                   sd(BMI, na.rm = TRUE)),
        `EF %, mean (SD)` = sprintf("%.1f (%.1f)",
                                    mean(EJF_PER, na.rm = TRUE),
                                    sd(EJF_PER, na.rm = TRUE)),
        `Diabetes, n (%%)` = sprintf("%d (%.1f%%)",
                                     sum(DIABETES == 1, na.rm = TRUE),
                                     100 * mean(DIABETES == 1, na.rm = TRUE)),
        `Hypertension, n (%%)` = sprintf("%d (%.1f%%)",
                                         sum(HYPERTEN == 1, na.rm = TRUE),
                                         100 * mean(HYPERTEN == 1, na.rm = TRUE)),
        .groups = "drop"
      )
    
    datatable(baseline,
              rownames = FALSE,
              options = list(pageLength = 5, dom = "tip"))
  })
  

  # Baseline plots
  output$plot_age_trt <- renderPlot({
    dat <- filtered_data()
    ggplot(dat, aes(x = TRTMT, y = AGE, fill = TRTMT)) +
      geom_boxplot(alpha = 0.7) +
      labs(x = "Treatment group", y = "Age (years)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })
  
  output$plot_ejf_trt <- renderPlot({
    dat <- filtered_data()
    ggplot(dat, aes(x = TRTMT, y = EJF_PER, fill = TRTMT)) +
      geom_boxplot(alpha = 0.7) +
      labs(x = "Treatment group", y = "Ejection fraction (%)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })
  

  # Variable explorer (univariate)
  output$plot_single <- renderPlot({
    req(input$var_single)
    dat <- filtered_data()
    varname <- input$var_single
    x <- dat[[varname]]
    
    if (is_numeric_var(x)) {
      ggplot(dat, aes(x = .data[[varname]])) +
        geom_histogram(bins = 30, na.rm = TRUE) +
        labs(x = varname, y = "Count") +
        theme_minimal(base_size = 13)
    } else {
      ggplot(dat, aes(x = as.factor(.data[[varname]]))) +
        geom_bar(na.rm = TRUE) +
        labs(x = varname, y = "Count") +
        theme_minimal(base_size = 13)
    }
  })
  
  output$summary_single <- renderPrint({
    req(input$var_single)
    dat <- filtered_data()
    varname <- input$var_single
    summary(dat[[varname]])
  })
  

  # Relationship explorer (bivariate)
  output$plot_bivariate <- renderPlot({
    req(input$xvar, input$yvar)
    dat <- filtered_data()
    xvar <- input$xvar
    yvar <- input$yvar
    
    x <- dat[[xvar]]
    y <- dat[[yvar]]
    
    if (is_numeric_var(x) && is_numeric_var(y)) {
      if (input$color_by_trt) {
        ggplot(dat, aes(x = .data[[xvar]], y = .data[[yvar]], color = TRTMT)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = FALSE) +
          labs(x = xvar, y = yvar, color = "Treatment") +
          theme_minimal(base_size = 13)
      } else {
        ggplot(dat, aes(x = .data[[xvar]], y = .data[[yvar]])) +
          geom_point(alpha = 0.5, color = "deeppink") +
          geom_smooth(method = "lm", se = FALSE) +
          labs(x = xvar, y = yvar) +
          theme_minimal(base_size = 13)
      }
      
    } else if (is_numeric_var(x) && !is_numeric_var(y)) {
      p <- ggplot(dat, aes(x = as.factor(.data[[yvar]]), y = .data[[xvar]])) +
        geom_boxplot(alpha = 0.7) +
        labs(x = yvar, y = xvar) +
        theme_minimal(base_size = 13)
      if (input$color_by_trt) p <- p + aes(fill = TRTMT) + labs(fill = "Treatment")
      p
      
    } else if (!is_numeric_var(x) && is_numeric_var(y)) {
      p <- ggplot(dat, aes(x = as.factor(.data[[xvar]]), y = .data[[yvar]])) +
        geom_boxplot(alpha = 0.7) +
        labs(x = xvar, y = yvar) +
        theme_minimal(base_size = 13)
      if (input$color_by_trt) p <- p + aes(fill = TRTMT) + labs(fill = "Treatment")
      p
      
    } else {
      p <- ggplot(dat, aes(x = as.factor(.data[[xvar]]), fill = as.factor(.data[[yvar]]))) +
        geom_bar(position = "fill") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(x = xvar, y = "Proportion", fill = yvar) +
        theme_minimal(base_size = 13)
      if (input$color_by_trt) p <- p + facet_wrap(~ TRTMT)
      p
    }
  })
  
  output$summary_bivariate <- renderPrint({
    req(input$xvar, input$yvar)
    dat <- filtered_data()
    xvar <- input$xvar
    yvar <- input$yvar
    
    x <- dat[[xvar]]
    y <- dat[[yvar]]
    
    if (is_numeric_var(x) && is_numeric_var(y)) {
      cat("Correlation (Pearson):\n")
      print(cor(x, y, use = "complete.obs"))
    } else if (is_numeric_var(x) && !is_numeric_var(y)) {
      cat("Numeric vs categorical: group means of", xvar, "by", yvar, "\n\n")
      print(tapply(x, as.factor(y), mean, na.rm = TRUE))
    } else if (!is_numeric_var(x) && is_numeric_var(y)) {
      cat("Numeric vs categorical: group means of", yvar, "by", xvar, "\n\n")
      print(tapply(y, as.factor(x), mean, na.rm = TRUE))
    } else {
      cat("Categorical vs categorical: contingency table\n\n")
      print(table(as.factor(x), as.factor(y)))
    }
  })
  

  # Survival tab (Months)
  output$summary_month <- renderPrint({
    dat <- filtered_data()
    summary(dat$Month)
  })
  
  surv_data <- reactive({
    dat <- filtered_data()
    dat <- dat %>% filter(!is.na(Month), !is.na(DEATH))
    if (nrow(dat) == 0) return(NULL)
    dat
  })
  
  output$km_overall <- renderPlot({
    dat <- surv_data()
    req(dat)
    fit <- survfit(Surv(Month, DEATH == "Dead") ~ 1, data = dat)
    df <- data.frame(time = fit$time, surv = fit$surv)
    
    ggplot(df, aes(x = time, y = surv)) +
      geom_step() +
      labs(x = "Month", y = "Survival probability") +
      theme_minimal(base_size = 13)
  })
  
  output$tbl_surv_trt <- renderTable({
    dat <- surv_data()
    req(dat)
    fit <- survfit(Surv(Month, DEATH == "Dead") ~ TRTMT, data = dat)
    s <- summary(fit)
    data.frame(
      Month    = s$time,
      Survival = round(s$surv, 3),
      Group    = s$strata
    )
  })
  
  output$km_trt <- renderPlot({
    dat <- surv_data()
    req(dat)
    fit <- survfit(Surv(Month, DEATH == "Dead") ~ TRTMT, data = dat)
    s <- summary(fit)
    df <- data.frame(
      Month    = s$time,
      Survival = s$surv,
      Group    = s$strata
    )
    
    ggplot(df, aes(x = Month, y = Survival, color = Group)) +
      geom_step() +
      labs(x = "Month", y = "Survival probability", color = "Group") +
      theme_minimal(base_size = 13)
  })
  
  output$tbl_surv_trt_cvd <- renderTable({
    dat <- surv_data()
    req(dat)
    fit <- survfit(Surv(Month, DEATH == "Dead") ~ TRTMT + CVD, data = dat)
    s <- summary(fit)
    data.frame(
      Month    = s$time,
      Survival = round(s$surv, 3),
      Group    = s$strata
    )
  })
  
  output$km_trt_cvd <- renderPlot({
    dat <- surv_data()
    req(dat)
    fit <- survfit(Surv(Month, DEATH == "Dead") ~ TRTMT + CVD, data = dat)
    s <- summary(fit)
    df <- data.frame(
      Month    = s$time,
      Survival = s$surv,
      Group    = s$strata
    )
    
    ggplot(df, aes(x = Month, y = Survival, color = Group)) +
      geom_step() +
      labs(x = "Month", y = "Survival probability", color = "Treatment + CVD") +
      theme_minimal(base_size = 13)
  })
}


# Run the app
shinyApp(ui = ui, server = server)

