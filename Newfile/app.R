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
library(tidyverse)
library(DT)
library(survival)

dig_raw <- read.csv("/Users/sararaees/Downloads/DIG-1.csv")

DIG_df <- dig_raw %>%
  drop_na() %>%
  mutate(
    TRTMT = factor(TRTMT, levels = c(0, 1),
                   labels = c("Placebo", "Digoxin")),
    SEX   = factor(SEX,   levels = c(1, 2),
                   labels = c("Male", "Female")),
    RACE  = factor(RACE,  levels = c(1, 2),
                   labels = c("White", "Nonwhite")),
    DEATH = factor(DEATH, levels = c(0, 1),
                   labels = c("Alive", "Death")),
    CVD   = factor(CVD,   levels = c(0, 1),
                   labels = c("No", "Yes")),
    WHF   = factor(WHF,   levels = c(0, 1),
                   labels = c("No", "Yes")),
    Month = round(DEATHDAY / 30)
  )

age_min <- floor(min(DIG_df$AGE, na.rm = TRUE))
age_max <- ceiling(max(DIG_df$AGE, na.rm = TRUE))


# UI

ui <- dashboardPage(
  
  dashboardHeader(title = "DIG"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Baseline", tabName = "Baseline", icon = icon("dashboard")),
      menuItem("Hospitalization", tabName = "Hospitalization", icon = icon("hospital")),
      menuItem("Mortality", tabName = "Mortality", icon = icon("skull")),
      menuItem("Survival (Months)", tabName = "survival", icon = icon("heartbeat")),
      
      menuItem(
        "Global filters",
        icon = icon("filter"),
        startExpanded = TRUE,
        
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
          min   = age_min,
          max   = age_max,
          value = c(age_min, age_max),
          step  = 1
        )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # 1) Baseline tab
      
      tabItem(
        tabName = "Baseline",
        h2("Baseline Characteristics"),
        box(
          title = "Controls",
          selectInput(
            inputId = "treatment",
            label   = "Select treatment type:",
            choices = levels(DIG_df$TRTMT),   # "Placebo", "Digoxin"
            multiple = FALSE
          ),
          radioButtons(
            inputId = "gender",
            label   = "Select gender",
            choices = levels(DIG_df$SEX),     # "Male", "Female"
            inline  = TRUE
          ),
          sliderInput(
            "age", "Select age",
            min   = age_min,
            max   = age_max,
            value = c(60, 70),
            step  = 1
          ),
          selectInput(
            inputId = "race",
            label   = "Select race type:",
            choices = levels(DIG_df$RACE),    # "White", "Nonwhite"
            multiple = FALSE
          ),
          radioButtons(
            inputId = "color",
            label   = "Select colour:",
            choices = c("purple", "darkgreen", "blue"),
            inline  = TRUE
          )
        ),
        
        fluidRow(
          box(plotOutput("distPlot",  height = 200)),
          box(plotOutput("distPlot2", height = 200)),
          box(plotOutput("distPlot3", height = 200)),
          dataTableOutput("tab_table1")
        )
      ),
      
      
      # 2) Hospitalization tab
      
      tabItem(
        tabName = "Hospitalization",
        h2("Hospitalization among different category"),
        box(
          selectInput(
            inputId = "category",
            label   = "Select category",
            choice  = c("WHF", "Treatment"),
            multiple = FALSE
          ),
          selectInput(
            inputId = "multicategory",
            label   = "Show multiple category",
            choice  = c("False", "True"),
            multiple = FALSE
          )
        ),
        fluidRow(
          box(plotOutput("tab2_distPlot", height = 200)),
          dataTableOutput("tab2_table1"),
          box(plotOutput("tab2_distPlot2", height = 200, width = "100%")),
          dataTableOutput("tab2_table2")
        )
      ),
      
      
      # 3) Mortality tab
      
      tabItem(
        tabName = "Mortality",
        h2("Mortality among different category"),
        box(
          selectInput(
            inputId = "category2",
            label   = "Select category",
            choice  = c("CVD", "Treatment"),
            multiple = FALSE
          ),
          selectInput(
            inputId = "multicategory2",
            label   = "Show multiple category",
            choice  = c("False", "True"),
            multiple = FALSE
          )
        ),
        fluidRow(
          box(plotOutput("tab3_distPlot", height = 200)),
          dataTableOutput("tab3_table1"),
          box(plotOutput("tab3_distPlot2", height = 200, width = "100%")),
          dataTableOutput("tab3_table2")
        )
      ),
      
      
      # 4) Survival tab
      
      tabItem(
        tabName = "survival",
        fluidRow(
          box(
            width        = 4,
            title        = "Follow-up in Months",
            status       = "primary",
            solidHeader  = TRUE,
            helpText("Month is defined as round(DEATHDAY / 30)."),
            verbatimTextOutput("summary_month")
          ),
          box(
            width        = 8,
            title        = "Overall survival curve (Question 9)",
            status       = "primary",
            solidHeader  = TRUE,
            plotOutput("km_overall", height = 300),
            helpText("Step curve shows the estimated probability of remaining alive (survival rate) over months.")
          )
        ),
        fluidRow(
          box(
            width        = 6,
            title        = "Survival by treatment (Question 10)",
            status       = "info",
            solidHeader  = TRUE,
            plotOutput("km_trt", height = 300),
            tableOutput("tbl_surv_trt")
          ),
          box(
            width        = 6,
            title        = "Survival by treatment and CVD (Question 11)",
            status       = "info",
            solidHeader  = TRUE,
            plotOutput("km_trt_cvd", height = 300),
            tableOutput("tbl_surv_trt_cvd")
          )
        )
      )
    )
  )
)


# SERVER

server <- function(input, output, session) {
  
  
  # Reactives for Baseline
  
  DIG_sub <- reactive({
    DIG_df %>%
      select(AGE, TRTMT, SEX, RACE) %>%
      filter(TRTMT == input$treatment,
             SEX   == input$gender,
             RACE  == input$race,
             AGE   >= input$age[1],
             AGE   <= input$age[2])
  })
  
  
  # Reactives for Hospitalization
  
  DIG_sub2 <- reactive({
    category_col <- switch(
      input$category,
      "WHF"       = "WHF",
      "Treatment" = "TRTMT"
    )
    
    DIG_df %>%
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>%
      summarise(
        hospitalization = sum(HOSP),
        total           = n(),
        .groups         = "drop"
      )
  })
  
  DIG_sub3 <- reactive({
    DIG_df %>%
      group_by(WHF, TRTMT) %>%
      summarise(
        hospitalization = sum(HOSP),
        total           = n(),
        .groups         = "drop"
      )
  })
  
  
  # Reactives for Mortality
  
  DIG_sub4 <- reactive({
    category_col2 <- switch(
      input$category2,
      "CVD"       = "CVD",
      "Treatment" = "TRTMT"
    )
    
    DIG_df %>%
      mutate(category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>%
      summarise(
        death = sum(DEATH == "Death"),
        total = n(),
        .groups = "drop"
      )
  })
  
  DIG_sub5 <- reactive({
    DIG_df %>%
      group_by(CVD, TRTMT) %>%
      summarise(
        death = sum(DEATH == "Death"),
        total = n(),
        .groups = "drop"
      )
  })
  
  
  # Global filtered data (for Survival tab)
  
  filtered_data <- reactive({
    dat <- DIG_df
    
    if (input$filter_trt != "both") {
      dat <- dat %>% filter(TRTMT == input$filter_trt)
    }
    if (input$filter_sex != "both") {
      dat <- dat %>% filter(SEX == input$filter_sex)
    }
    
    dat %>%
      filter(AGE >= input$filter_age[1],
             AGE <= input$filter_age[2])
  })
  
  
  # Baseline outputs
  
  output$distPlot <- renderPlot({
    ggplot(DIG_sub(), aes(x = AGE)) +
      geom_histogram(fill = input$color, bins = 30) +
      labs(x = "Age", y = "Count")
  })
  
  output$tab_table1 <- renderDataTable({
    DIG_sub()
  })
  
  output$distPlot2 <- renderPlot({
    ggplot(DIG_sub(), aes(x = SEX)) +
      geom_bar(fill = input$color) +
      labs(x = "Sex", y = "Count")
  })
  
  output$distPlot3 <- renderPlot({
    ggplot(DIG_sub(), aes(x = RACE)) +
      geom_bar(fill = input$color) +
      labs(x = "Race", y = "Count")
  })
  
  
  # Hospitalization outputs
  
  output$tab2_distPlot <- renderPlot({
    ggplot(DIG_sub2(),
           aes(x = factor(category_var),
               y = hospitalization,
               fill = factor(category_var))) +
      geom_col() +
      labs(x = "Category", y = "Hospitalizations", fill = "Category")
  })
  
  output$tab2_table1 <- renderDataTable({
    DIG_sub2()
  })
  
  output$tab2_distPlot2 <- renderPlot({
    req(input$multicategory != "False")
    ggplot(DIG_sub3(),
           aes(x = WHF, y = hospitalization, fill = TRTMT)) +
      geom_col(position = "dodge") +
      labs(
        x     = "WHF",
        y     = "Hospitalizations",
        fill  = "Treatment",
        title = "Hospitalization by WHF and Treatment"
      )
  })
  
  output$tab2_table2 <- renderDataTable({
    DIG_sub3()
  })
  
  
  # Mortality outputs
  
  output$tab3_distPlot <- renderPlot({
    ggplot(DIG_sub4(),
           aes(x = factor(category_var2),
               y = death,
               fill = factor(category_var2))) +
      geom_col() +
      labs(x = "Category", y = "Deaths", fill = "Category")
  })
  
  output$tab3_table1 <- renderDataTable({
    DIG_sub4()
  })
  
  output$tab3_distPlot2 <- renderPlot({
    req(input$multicategory2 != "False")
    ggplot(DIG_sub5(),
           aes(x = CVD, y = death, fill = TRTMT)) +
      geom_col(position = "dodge") +
      labs(
        x     = "CVD",
        y     = "Deaths",
        fill  = "Treatment",
        title = "Mortality by CVD and Treatment"
      )
  })
  
  output$tab3_table2 <- renderDataTable({
    DIG_sub5()
  })
  
  
  # Survival tab outputs
  
  output$summary_month <- renderPrint({
    summary(DIG_df$Month)
  })
  
  surv_data <- reactive({
    dat <- filtered_data() %>%
      filter(!is.na(Month), !is.na(DEATH))
    if (nrow(dat) == 0) return(NULL)
    dat
  })
  
  # Overall survival
  output$km_overall <- renderPlot({
    dat <- surv_data()
    req(dat)
    
    fit <- survfit(Surv(Month, DEATH == "Death") ~ 1, data = dat)
    df  <- data.frame(time = fit$time, surv = fit$surv)
    
    ggplot(df, aes(x = time, y = surv)) +
      geom_step() +
      labs(x = "Month", y = "Survival probability") +
      theme_minimal(base_size = 13)
  })
  
  # Survival by treatment
  output$tbl_surv_trt <- renderTable({
    dat <- surv_data()
    req(dat)
    
    fit <- survfit(Surv(Month, DEATH == "Death") ~ TRTMT, data = dat)
    s   <- summary(fit)
    
    data.frame(
      Month    = s$time,
      Survival = round(s$surv, 3),
      Group    = s$strata
    )
  })
  
  output$km_trt <- renderPlot({
    dat <- surv_data()
    req(dat)
    
    fit <- survfit(Surv(Month, DEATH == "Death") ~ TRTMT, data = dat)
    s   <- summary(fit)
    
    df <- data.frame(
      Month    = s$time,
      Survival = s$surv,
      Group    = s$strata
    )
    
    ggplot(df, aes(x = Month, y = Survival, color = Group)) +
      geom_step() +
      labs(x = "Month", y = "Survival probability", color = "Treatment") +
      theme_minimal(base_size = 13)
  })
  
  # Survival by treatment + CVD
  output$tbl_surv_trt_cvd <- renderTable({
    dat <- surv_data()
    req(dat)
    
    fit <- survfit(Surv(Month, DEATH == "Death") ~ TRTMT + CVD, data = dat)
    s   <- summary(fit)
    
    data.frame(
      Month    = s$time,
      Survival = round(s$surv, 3),
      Group    = s$strata
    )
  })
  
  output$km_trt_cvd <- renderPlot({
    dat <- surv_data()
    req(dat)
    
    fit <- survfit(Surv(Month, DEATH == "Death") ~ TRTMT + CVD, data = dat)
    s   <- summary(fit)
    
    df <- data.frame(
      Month    = s$time,
      Survival = s$surv,
      Group    = s$strata
    )
    
    ggplot(df, aes(x = Month, y = Survival, color = Group)) +
      geom_step() +
      labs(
        x     = "Month",
        y     = "Survival probability",
        color = "Treatment + CVD"
      ) +
      theme_minimal(base_size = 13)
  })
}

# Run the first application

shinyApp(ui, server)