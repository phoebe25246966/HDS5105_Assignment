#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shinydashboard)
library(tidyverse)
library(plotly)
library(survival)
library(reactable)
library(shinyWidgets)
library(flexdashboard)
shinyWidgets::shinyWidgetsGallery()

dig_raw <- read.csv("DIG-1.csv")

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
  skin = "purple",
  dashboardHeader(title = "DIG Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Baseline",          tabName = "Baseline", icon = icon("home")),
      menuItem("Hospitalization",   tabName = "Hospitalization", icon = icon("table")),
      menuItem("Mortality",         tabName = "Mortality", icon = icon("skull")),
      menuItem("Risk factor explorer", tabName = "risk",    icon = icon("chart-line")),
      menuItem("Outcomes summary",     tabName = "outcomes",icon = icon("chart-column")),
      menuItem("Survival (Months)",    tabName = "survival",icon = icon("heartbeat")),
      
      menuItem(
        "Global filters",
        icon = icon("filter"),
        startExpanded = TRUE,
        pickerInput(
          "filter_trt", "Treatment group",
          choices = c("Both" = "both",
                      "Placebo" = "Placebo",
                      "Digoxin" = "Digoxin"),
          selected = "both",
          multiple = FALSE,
          options = list(`live-search` = TRUE)
        ),
        pickerInput(
          "filter_sex", "Sex",
          choices = c("Both" = "both",
                      "Male" = "Male",
                      "Female" = "Female"),
          selected = "both",
          multiple = FALSE,
          options = list(`live-search` = TRUE)
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
    tags$head(
      tags$style(HTML("
        body { background-color: #f4f3fb; }
        .skin-purple .main-header .logo {
          background-color: #3b2f63;
          font-weight: bold;
        }
        .skin-purple .main-header .logo:hover {
          background-color: #322854;
        }
        .skin-purple .main-header .navbar {
          background-color: #3b2f63;
        }
        .skin-purple .main-sidebar {
          background-color: #2c2443;
        }
        .skin-purple .sidebar-menu>li>a {
          color: #ecf0f1;
        }
        .skin-purple .sidebar-menu>li.active>a,
        .skin-purple .sidebar-menu>li:hover>a {
          background-color: #8e44ad;
          color: #ffffff;
        }
        .box.box-primary  { border-top-color: #8e44ad; }
        .box.box-info     { border-top-color: #16a085; }
        .box.box-warning  { border-top-color: #f39c12; }
        .box.box-danger   { border-top-color: #e74c3c; }
        .content-wrapper, .right-side {
          background-color: #f4f3fb;
        }
      "))
    ),
    
    tabItems(
      
      #BASELINE
      tabItem(
        tabName = "Baseline",
        h2("Baseline Characteristics"),
        fluidRow(
          valueBoxOutput("sum_n"),
          valueBoxOutput("sum_hyperten"),
          valueBoxOutput("sum_SYSBP"),
          valueBoxOutput("sum_DIABP"),
          valueBoxOutput("sum_hosp"),
          valueBoxOutput("sum_age"),
          valueBoxOutput("sum_deathday")
        ),
        fluidRow(
          box(
            title = "Filter Categorical Variable",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput(
              inputId = "filter_cat",
              label   = "Select filter",
              choices = c("Sex","Hypertension"),
              multiple = FALSE
            )
          ),
          box(
            title="Analysis",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("distPlot_cat")
          )
        ),
        fluidRow(
          box(
            title = "Filter Continuous Variable",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            selectInput(
              inputId = "filter",
              label   = "Select filter(continuous variable)",
              choices = c("Age","DiaBP","SysBP","KLevel","Creat"),
              multiple = FALSE
            ),
            selectInput(
              inputId = "type",
              label   = "Select graph type",
              choices = c("BarPlot","LinePlot"),
              multiple = FALSE
            )
          ),
          box(
            title="Analysis",
            status="primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("distPlot")
          )
        )
        
      ),
      #2nd tab
      tabItem(tabName = "Hospitalization",
              h2("Relationship of hospitalization with different types of medical history in each treatment group"),
              fluidRow(
                box(
                  title = "Select Main Focus",
                  status = "primary",
                  solidHeader = TRUE, 
                  column(4,prettyCheckboxGroup(inputId = "category_main", label=NULL,choices=c("Worsening heart failure","Pulmonary congestion","Diabetes","Hypertension")
                                               ,outline = TRUE,animation = "tada",status="success"))
                )
              ),
              #Only show when checkbox is selected
              conditionalPanel(
                condition = "input.category_main.includes('Worsening heart failure')",
                #WHF
                fluidRow(
                  box(
                    column(4,selectInput(inputId = "category",label="Select category",choice=c("WHF"),multiple = FALSE) ),
                    column(4,switchInput(inputId = "multicategory",label="Show multiple category",onLabel = "Enable",
                                         offLabel = "Disable",value=FALSE))
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Single Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab2_distPlot",height=200)),
                    reactableOutput("tab2_table1")
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Multiple Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab2_distPlot2",height=200)),
                    reactableOutput("tab2_table2")
                  )
                )
              ),
              #Only show when checkbox is selected
              conditionalPanel(
                condition = "input.category_main.includes('Pulmonary congestion')",
                #PULCONG
                fluidRow(
                  box(
                    column(4,selectInput(inputId = "category_tab2_2",label="Select category",choice=c("Pulmonary congestion"),multiple = FALSE) ),
                    column(4,switchInput(inputId = "multicategory_tab2_2",label="Show multiple category",onLabel = "Enable",
                                         offLabel = "Disable",value=FALSE))
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Single Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab2_distPlot3",height=200)),
                    reactableOutput("tab2_table3")
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Multiple Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab2_distPlot4",height=200)),
                    reactableOutput("tab2_table4")
                  )
                )
              ),
              #Only show when checkbox is selected
              conditionalPanel(
                condition = "input.category_main.includes('Diabetes')",
                #DIABETES
                fluidRow(
                  box(
                    column(4,selectInput(inputId = "category_tab2_3",label="Select category",choice=c("Diabetes"),multiple = FALSE) ),
                    column(4,switchInput(inputId = "multicategory_tab2_3",label="Show multiple category",onLabel = "Enable",
                                         offLabel = "Disable",value=FALSE))
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Single Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab2_distPlot5",height=200)),
                    reactableOutput("tab2_table5")
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Multiple Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab2_distPlot6",height=200)),
                    reactableOutput("tab2_table6")
                  )
                )
              ),
              #Only show when checkbox is selected
              conditionalPanel(
                condition = "input.category_main.includes('Hypertension')",
                #HYPERTEN
                fluidRow(
                  box(
                    column(4,selectInput(inputId = "category_tab2_4",label="Select category",choice=c("Hypertension"),multiple = FALSE) ),
                    column(4,switchInput(inputId = "multicategory_tab2_4",label="Show multiple category",onLabel = "Enable",
                                         offLabel = "Disable",value=FALSE))
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Single Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab2_distPlot7",height=200)),
                    reactableOutput("tab2_table7")
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Multiple Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab2_distPlot8",height=200)),
                    reactableOutput("tab2_table8")
                  )
                )
              )
      ),
      #3rd tab
      tabItem(tabName = "Mortality",
              h2("Relationship of mortality with different types of medical history in each treatment group"),
              fluidRow(
                box(
                  title = "Select Main Focus",
                  status = "primary",
                  solidHeader = TRUE, 
                  column(4,prettyCheckboxGroup(inputId = "category_main_m",label=NULL,choices=c("Worsening heart failure","CVD","Diabetes","Hypertension"),outline = TRUE,
                                               animation = "tada",status="success"))
                )
              ),
              conditionalPanel(
                condition = "input.category_main_m.includes('CVD')",
                fluidRow(
                  box(
                    column(4,selectInput(inputId = "category2",label="Select category",choice=c("CVD"),multiple = FALSE)),
                    column(4,switchInput(inputId = "multicategory2",label="Show multiple category",onLabel = "Enable",
                                         offLabel = "Disable",value=FALSE))
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Single Category Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab3_distPlot",height=200)),
                    reactableOutput("tab3_table1")
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Multiple Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab3_distPlot2",height=200)),
                    reactableOutput("tab3_table2"),
                  )
                )
              ),
              #Only show when checkbox is selected
              conditionalPanel(
                condition = "input.category_main_m.includes('Worsening heart failure')",
                #WHF
                fluidRow(
                  box(
                    column(4,selectInput(inputId = "category2_tab3_2",label="Select category",choice=c("Worsening Heart Failure"),multiple = FALSE) ),
                    column(4,switchInput(inputId = "multicategory2_tab3_2",label="Show multiple category",onLabel = "Enable",
                                         offLabel = "Disable",value=FALSE))
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Single Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab3_distPlot3",height=200)),
                    reactableOutput("tab3_table3")
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Multiple Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab3_distPlot4",height=200)),
                    reactableOutput("tab3_table4")
                  )
                )
              ),
              #Only show when checkbox is selected
              conditionalPanel(
                condition = "input.category_main_m.includes('Diabetes')",
                #WHF
                fluidRow(
                  box(
                    column(4,selectInput(inputId = "category2_tab3_3",label="Select category",choice=c("Diabetes"),multiple = FALSE) ),
                    column(4,switchInput(inputId = "multicategory2_tab3_3",label="Show multiple category",onLabel = "Enable",
                                         offLabel = "Disable",value=FALSE))
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Single Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab3_distPlot5",height=200)),
                    reactableOutput("tab3_table5")
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Multiple Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab3_distPlot6",height=200)),
                    reactableOutput("tab3_table6")
                  )
                )
              ),
              #Only show when checkbox is selected
              conditionalPanel(
                condition = "input.category_main_m.includes('Hypertension')",
                #HYPERTEN
                fluidRow(
                  box(
                    column(4,selectInput(inputId = "category2_tab3_4",label="Select category",choice=c("Hypertension"),multiple = FALSE) ),
                    column(4,switchInput(inputId = "multicategory2_tab3_4",label="Show multiple category",onLabel = "Enable",
                                         offLabel = "Disable",value=FALSE))
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Single Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab3_distPlot7",height=200)),
                    reactableOutput("tab3_table7")
                  )
                ),
                fluidRow(
                  box(
                    width=10,
                    title="Multiple Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=6,plotlyOutput("tab3_distPlot8",height=200)),
                    reactableOutput("tab3_table8")
                  )
                )
              )
              
      ),
      
      # RISK (SARA) 
      tabItem(
        tabName = "risk",
        h2("Risk factor explorer"),
        
        fluidRow(
          box(
            width       = 4,
            title       = "Risk factor controls",
            status      = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            helpText("Analyses here respect the global filters (treatment, sex, age) in the sidebar."),
            
            pickerInput(
              "risk_outcome", "Outcome",
              choices = c(
                "Hospitalization"                      = "hosp",
                "Death"                                = "death",
                "Hospitalization or death (composite)" = "composite"
              ),
              selected = "death",
              multiple = FALSE,
              options = list(`live-search` = TRUE)
            ),
            
            pickerInput(
              "risk_var", "Clinical predictor",
              choices = c(
                "Age (years)"               = "AGE",
                "Systolic BP"               = "SYSBP",
                "Diastolic BP"              = "DIABP",
                "Serum potassium (KLEVEL)"  = "KLEVEL",
                "Creatinine (CREAT)"        = "CREAT"
              ),
              selected = "AGE",
              multiple = FALSE,
              options = list(`live-search` = TRUE)
            ),
            
            materialSwitch(
              "risk_adjust_trt",
              "Adjust for treatment in model",
              value  = TRUE,
              status = "primary"
            )
          ),
          
          box(
            width       = 8,
            title       = "Distribution of predictor by outcome",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("risk_boxplot", height = 320)
          )
        ),
        
        fluidRow(
          box(
            width       = 6,
            title       = "Density plot of predictor by outcome",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("risk_density", height = 320)
          ),
          box(
            width       = 6,
            title       = "Logistic regression",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            helpText("Odds ratios for the selected predictor (and treatment, if selected) with respect to the chosen outcome."),
            reactableOutput("risk_model_table")
          )
        ),
        
        fluidRow(
          box(
            width       = 12,
            title       = "Scatterplot: predictor vs outcome by treatment",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("risk_scatter", height = 320)
          )
        ),
        
        fluidRow(
          box(
            width       = 12,
            title       = "Correlation between key clinical variables (filtered data)",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            reactableOutput("risk_corr_table")
          )
        )
      ),
      
      #OUTCOMES (SARA)
      tabItem(
        tabName = "outcomes",
        h2("Outcomes summary"),
        
        fluidRow(
          box(
            width       = 4,
            title       = "Outcomes controls",
            status      = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            helpText("All analyses here respect the global filters (treatment, sex, age) in the sidebar."),
            
            pickerInput(
              "outcome_type", "Outcome",
              choices = c(
                "Hospitalization"                      = "hosp",
                "Death"                                = "death",
                "Hospitalization or death (composite)" = "composite"
              ),
              selected = "composite",
              multiple = FALSE,
              options = list(`live-search` = TRUE)
            ),
            
            pickerInput(
              "outcome_group", "Group by",
              choices = c(
                "Treatment"          = "TRTMT",
                "CVD"                = "CVD",
                "WHF"                = "WHF",
                "Sex"                = "SEX",
                "Age group (5-year)" = "AGE_GROUP"
              ),
              selected = "TRTMT",
              multiple = FALSE,
              options = list(`live-search` = TRUE)
            ),
            
            radioGroupButtons(
              "outcome_y", "Show on y-axis",
              choices  = c("Event count" = "count", "Event rate (%)" = "rate"),
              selected = "rate",
              status   = "primary",
              direction = "vertical",
              justified = FALSE
            )
          ),
          
          box(
            width       = 8,
            title       = "Event rates by group",
            status      = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("outcome_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            width       = 12,
            title       = "Table of outcomes by group",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            reactableOutput("outcome_table")
          )
        )
      ),
      
      #SURVIVAL (SARA)
      tabItem(
        tabName = "survival",
        h2("Survival analysis (all-cause mortality)"),
        
        fluidRow(
          box(
            width       = 4,
            title       = "Survival controls",
            status      = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            helpText("Data are already filtered by the global sidebar filters (treatment, sex, age)."),
            
            pickerInput(
              "surv_group", "Group curves by",
              choices = c(
                "Overall only"    = "none",
                "Treatment"       = "TRTMT",
                "CVD"             = "CVD",
                "Treatment + CVD" = "TRTMT_CVD",
                "Sex"             = "SEX"
              ),
              selected = "TRTMT",
              multiple = FALSE,
              options = list(`live-search` = TRUE)
            ),
            
            sliderInput(
              "surv_time_range", "Time range to display (months)",
              min   = 0,
              max   = max(DIG_df$Month, na.rm = TRUE),
              value = c(0, max(DIG_df$Month, na.rm = TRUE)),
              step  = 1
            ),
            
            numericInput(
              "surv_landmark_month",
              "Summarise survival at month:",
              value = 12,
              min   = 0,
              max   = max(DIG_df$Month, na.rm = TRUE),
              step  = 1
            ),
            
            materialSwitch(
              "surv_show_ci",
              "Show 95% confidence bands",
              value  = TRUE,
              status = "primary"
            )
          ),
          
          box(
            width       = 8,
            title       = "Kaplan–Meier survival curves",
            status      = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("km_main", height = 350),
            br(),
            verbatimTextOutput("surv_landmark_text")
          )
        ),
        
        fluidRow(
          box(
            width       = 6,
            title       = "Survival heatmap (time × group)",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("km_heatmap", height = 350)
          ),
          box(
            width       = 6,
            title       = "Landmark survival summary",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            uiOutput("surv_flex_valueboxes")
          )
        ),
        
        fluidRow(
          box(
            width       = 6,
            title       = "Number at risk / events at chosen month",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("tbl_surv_summary")
          ),
          box(
            width       = 6,
            title       = "Median survival time by group",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            tableOutput("tbl_surv_median")
          )
        ),
        
        fluidRow(
          box(
            width       = 12,
            title       = "Row-level data used in survival analysis",
            status      = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            reactableOutput("tbl_surv_indiv")
          )
        ),
        
        
      )
    )
  )
  
)



#SERVER

server <- function(input, output) {
  
  #  Phoebe: baseline / hosp / mortality 
  
  DIG <- reactive({ DIG_df })
  
  DIG_sum <- reactive({
    DIG_df %>%
      summarise(
        n_hosp     = sum(HOSP),
        n_death    = sum(DEATH == "Death"),
        mean_age   = mean(AGE),
        n_hyperten = sum(factor(HYPERTEN) == 1),
        mean_SYSBP = mean(SYSBP),
        mean_DIABP = mean(DIABP)
      )
  })
  
  DIG_sub_cat <- reactive({
    filter <- req(input$filter_cat)
    colname <- switch(
      filter,
      "Sex"         = "SEX",
      "Hypertension"= "HYPERTEN"
    )
    DIG_df %>%
      mutate(category_var = .data[[colname]]) %>%
      group_by(TRTMT, category_var) %>%
      summarise(total = n(), .groups = "drop")
  })
  
  DIG_sub <- reactive({
    filter <- req(input$filter)
    colname <- switch(
      filter,
      "Creat"       = "CREAT",
      "Sex"         = "SEX",
      "Hypertension"= "HYPERTEN",
      "DiaBP"       = "DIABP",
      "SysBP"       = "SYSBP",
      "KLevel"      = "KLEVEL",
      "Age"         = "AGE"
    )
    
    DIG_df %>%
      mutate(category_var = .data[[colname]]) %>%
      group_by(TRTMT, category_var) %>%
      summarise(total = n(), .groups = "drop")
  })
  
  # Hospitalization reactives
  DIG_sub2 <- reactive({
    category_col <- switch(input$category, "WHF" = "WHF")
    DIG_df %>%
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>%
      summarise(hospitalization = sum(HOSP), total = n())
  })
  
  DIG_sub2_pulcong <- reactive({
    category_col <- switch(input$category_tab2_2, "Pulmonary congestion" = "PULCONG")
    DIG_df %>%
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>%
      summarise(hospitalization = sum(HOSP), total = n())
  })
  
  DIG_sub2_pulcong_2 <- reactive({
    DIG_df %>%
      group_by(PULCONG, TRTMT) %>%
      summarise(hospitalization = sum(HOSP), total = n())
  })
  
  DIG_sub2_dia <- reactive({
    category_col <- switch(input$category_tab2_3, "Diabetes" = "DIABETES")
    DIG_df %>%
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>%
      summarise(hospitalization = sum(HOSP), total = n())
  })
  
  DIG_sub2_dia_2 <- reactive({
    DIG_df %>%
      group_by(DIABETES, TRTMT) %>%
      summarise(hospitalization = sum(HOSP))
  })
  
  DIG_sub2_hyp <- reactive({
    category_col <- switch(
      input$category_tab2_4,
      "Hypertension" = "HYPERTEN",
      "Treatment"    = "TRTMT"
    )
    DIG_df %>%
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>%
      summarise(hospitalization = sum(HOSP))
  })
  
  DIG_sub2_hyp_2 <- reactive({
    DIG_df %>%
      group_by(HYPERTEN, TRTMT) %>%
      summarise(hospitalization = sum(HOSP))
  })
  
  DIG_sub3 <- reactive({
    DIG_df %>%
      group_by(WHF, TRTMT) %>%
      summarise(hospitalization = sum(HOSP))
  })
  
  # Mortality reactives
  DIG_sub4 <- reactive({
    category_col2 <- switch(input$category2, "CVD" = "CVD")
    DIG_df %>%
      mutate(category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>%
      summarise(death = sum(DEATH == "Death"))
  })
  
  DIG_sub5 <- reactive({
    DIG_df %>%
      group_by(CVD, TRTMT) %>%
      summarise(death = sum(DEATH == "Death"))
  })
  
  DIG_sub_WHF_m <- reactive({
    category_col2 <- switch(input$category2_tab3_2, "Worsening Heart Failure" = "WHF")
    DIG_df %>%
      mutate(category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>%
      summarise(death = sum(DEATH == "Death"))
  })
  
  DIG_sub_WHF_m_2 <- reactive({
    DIG_df %>%
      group_by(WHF, TRTMT) %>%
      summarise(death = sum(DEATH == "Death"))
  })
  
  DIG_sub_dia_m <- reactive({
    category_col2 <- switch(input$category2_tab3_3, "Diabetes" = "DIABETES")
    DIG_df %>%
      mutate(category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>%
      summarise(death = sum(DEATH == "Death"))
  })
  
  DIG_sub_dia_m_2 <- reactive({
    DIG_df %>%
      group_by(DIABETES, TRTMT) %>%
      summarise(death = sum(DEATH == "Death"))
  })
  
  DIG_sub_hyp_m <- reactive({
    category_col2 <- switch(input$category2_tab3_4, "Hypertension" = "HYPERTEN")
    DIG_df %>%
      mutate(category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>%
      summarise(death = sum(DEATH == "Death"))
  })
  
  DIG_sub_hyp_m_2 <- reactive({
    DIG_df %>%
      group_by(HYPERTEN, TRTMT) %>%
      summarise(death = sum(DEATH == "Death"))
  })
  
  #Value boxes (use shinydashboard::valueBox explicitly)
  output$sum_n <- renderValueBox({
    n_pat <- nrow(DIG())
    shinydashboard::valueBox(
      formatC(n_pat, big.mark = ","),
      subtitle = "Patients in current selection",
      icon     = icon("users"),
      color    = "teal"
    )
  })
  
  output$sum_hyperten <- renderValueBox({
    shinydashboard::valueBox(
      value    = DIG_sum()$n_hyperten,
      subtitle = "Total no. history of hypertension",
      icon     = icon("heart"),
      color    = "light-blue"
    )
  })
  
  output$sum_SYSBP <- renderValueBox({
    shinydashboard::valueBox(
      value    = round(DIG_sum()$mean_SYSBP, 2),
      subtitle = "Average of Systolic BP",
      icon     = icon("info"),
      color    = "light-blue"
    )
  })
  
  output$sum_DIABP <- renderValueBox({
    shinydashboard::valueBox(
      value    = round(DIG_sum()$mean_DIABP, 2),
      subtitle = "Average of Diastolic BP",
      icon     = icon("info"),
      color    = "light-blue"
    )
  })
  
  output$sum_hosp <- renderValueBox({
    shinydashboard::valueBox(
      value    = round(DIG_sum()$n_hosp, 2),
      subtitle = "Number of hospitalization",
      icon     = icon("heart-broken"),
      color    = "red"
    )
  })
  
  output$sum_deathday <- renderValueBox({
    shinydashboard::valueBox(
      value    = round(DIG_sum()$n_death),
      subtitle = "Average death",
      icon     = icon("skull"),
      color    = "blue"
    )
  })
  
  output$sum_age <- renderValueBox({
    shinydashboard::valueBox(
      value    = round(DIG_sum()$mean_age),
      subtitle = "Average age",
      icon     = icon("id-card"),
      color    = "yellow"
    )
  })
  
  #Baseline plots
  output$distPlot <- renderPlotly({
    filter <- req(input$filter)
    if (input$type == "BarPlot") {
      p <- ggplot(DIG_sub(), aes(x = category_var, y = total, fill = TRTMT)) +
        geom_col(position = "dodge") +
        labs(x = filter, title = paste("Plot between", filter, "and treatment")) +
        theme_minimal()
      ggplotly(p)
    } else if (input$type == "LinePlot") {
      p <- ggplot(DIG_sub(), aes(x = category_var, y = total, color = TRTMT)) +
        geom_line() +
        labs(x = filter, title = paste("Plot between", filter, "and treatment")) +
        theme_minimal()
      ggplotly(p)
    }
  })
  
  output$distPlot_cat <- renderPlotly({
    filter_cat <- req(input$filter_cat)
    p <- ggplot(DIG_sub_cat(), aes(x = factor(category_var), y = total, fill = TRTMT)) +
      geom_col(position = "dodge") +
      labs(x = filter_cat, title = paste("Plot between", filter_cat, "and treatment")) +
      theme_minimal()
    ggplotly(p)
  })
  
  #Hospitalization plots
  output$tab2_distPlot <- renderPlotly({
    category1 <- req(input$category)
    p <- ggplot(DIG_sub2(),
                aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col() +
      labs(
        x     = category1,
        y     = "Hospitalization",
        fill  = category1,
        title = paste("Plot between", category1, "and hospitalization")
      )
    ggplotly(p)
  })
  output$tab2_table1 <- renderReactable({ reactable(DIG_sub2()) })
  
  output$tab2_distPlot2 <- renderPlotly({
    req(input$multicategory)
    p <- ggplot(DIG_sub3(), aes(x = factor(WHF), y = hospitalization, fill = factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(
        x     = "WHF",
        y     = "Hospitalization",
        fill  = "Treatment",
        title = "Plot between WHF, HOSP and Treatment"
      )
    ggplotly(p)
  })
  output$tab2_table2 <- renderReactable({
    req(input$multicategory)
    reactable(DIG_sub3())
  })
  
  output$tab2_distPlot3 <- renderPlotly({
    category1 <- req(input$category_tab2_2)
    p <- ggplot(DIG_sub2_pulcong(),
                aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col() +
      labs(
        x     = category1,
        y     = "Hospitalization",
        fill  = category1,
        title = paste("Plot between", category1, "and hospitalization")
      )
    ggplotly(p)
  })
  output$tab2_table3 <- renderReactable({ reactable(DIG_sub2_pulcong()) })
  
  output$tab2_distPlot4 <- renderPlotly({
    req(input$multicategory_tab2_2)
    p <- ggplot(DIG_sub2_pulcong_2(),
                aes(x = factor(PULCONG), y = hospitalization, fill = factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(
        x     = "PULCONG",
        y     = "Hospitalization",
        fill  = "Treatment",
        title = "Plot between PULCONG, HOSP and Treatment"
      )
    ggplotly(p)
  })
  output$tab2_table4 <- renderReactable({
    req(input$multicategory_tab2_2)
    reactable(DIG_sub2_pulcong_2())
  })
  
  output$tab2_distPlot5 <- renderPlotly({
    category1 <- req(input$category_tab2_3)
    p <- ggplot(DIG_sub2_dia(),
                aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col() +
      labs(
        x     = category1,
        y     = "Hospitalization",
        fill  = category1,
        title = paste("Plot between", category1, "and hospitalization")
      )
    ggplotly(p)
  })
  output$tab2_table5 <- renderReactable({ reactable(DIG_sub2_dia()) })
  
  output$tab2_distPlot6 <- renderPlotly({
    req(input$multicategory_tab2_3)
    p <- ggplot(DIG_sub2_dia_2(),
                aes(x = factor(DIABETES), y = hospitalization, fill = factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(
        x     = "Diabetes",
        y     = "Hospitalization",
        fill  = "Treatment",
        title = "Plot between Diabetes, HOSP and Treatment"
      )
    ggplotly(p)
  })
  output$tab2_table6 <- renderReactable({
    req(input$multicategory_tab2_3)
    reactable(DIG_sub2_dia_2())
  })
  
  output$tab2_distPlot7 <- renderPlotly({
    category1 <- req(input$category_tab2_4)
    p <- ggplot(DIG_sub2_hyp(),
                aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col() +
      labs(
        x     = category1,
        y     = "Hospitalization",
        fill  = category1,
        title = paste("Plot between", category1, "and hospitalization")
      )
    ggplotly(p)
  })
  output$tab2_table7 <- renderReactable({ reactable(DIG_sub2_hyp()) })
  
  output$tab2_distPlot8 <- renderPlotly({
    req(input$multicategory_tab2_4)
    p <- ggplot(DIG_sub2_hyp_2(),
                aes(x = factor(HYPERTEN), y = hospitalization, fill = factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(
        x     = "Hypertension",
        y     = "Hospitalization",
        fill  = "Treatment",
        title = "Plot between Hypertension, HOSP and Treatment"
      )
    ggplotly(p)
  })
  output$tab2_table8 <- renderReactable({
    req(input$multicategory_tab2_4)
    reactable(DIG_sub2_hyp_2())
  })
  
  #Mortality plots
  output$tab3_distPlot <- renderPlotly({
    category2 <- req(input$category2)
    p <- ggplot(DIG_sub4(),
                aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col() +
      labs(
        x     = category2,
        y     = "Death",
        fill  = category2,
        title = paste("Plot between", category2, "and death")
      )
    ggplotly(p)
  })
  output$tab3_table1 <- renderReactable({ reactable(DIG_sub4()) })
  
  output$tab3_distPlot2 <- renderPlotly({
    req(input$multicategory2)
    p <- ggplot(DIG_sub5(),
                aes(x = factor(CVD), y = death, fill = factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(
        x     = "CVD",
        y     = "Death",
        fill  = "Treatment",
        title = "Plot between CVD, Death and Treatment"
      )
    ggplotly(p)
  })
  output$tab3_table2 <- renderReactable({
    req(input$multicategory2)
    reactable(DIG_sub5())
  })
  
  output$tab3_distPlot3 <- renderPlotly({
    category2 <- req(input$category2_tab3_2)
    p <- ggplot(DIG_sub_WHF_m(),
                aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col() +
      labs(
        x     = category2,
        y     = "Death",
        fill  = category2,
        title = paste("Plot between", category2, "and death")
      )
    ggplotly(p)
  })
  output$tab3_table3 <- renderReactable({ reactable(DIG_sub_WHF_m()) })
  
  output$tab3_distPlot4 <- renderPlotly({
    req(input$multicategory2_tab3_2)
    p <- ggplot(DIG_sub_WHF_m_2(),
                aes(x = factor(WHF), y = death, fill = factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(
        x     = "WHF",
        y     = "Death",
        fill  = "Treatment",
        title = "Plot between WHF, Death and Treatment"
      )
    ggplotly(p)
  })
  output$tab3_table4 <- renderReactable({
    req(input$multicategory2_tab3_2)
    reactable(DIG_sub_WHF_m_2())
  })
  
  output$tab3_distPlot5 <- renderPlotly({
    category2 <- req(input$category2_tab3_3)
    p <- ggplot(DIG_sub_dia_m(),
                aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col() +
      labs(
        x     = category2,
        y     = "Death",
        fill  = category2,
        title = paste("Plot between", category2, "and death")
      )
    ggplotly(p)
  })
  output$tab3_table5 <- renderReactable({ reactable(DIG_sub_dia_m()) })
  
  output$tab3_distPlot6 <- renderPlotly({
    req(input$multicategory2_tab3_3)
    p <- ggplot(DIG_sub_dia_m_2(),
                aes(x = factor(DIABETES), y = death, fill = factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(
        x     = "Diabetes",
        y     = "Death",
        fill  = "Treatment",
        title = "Plot between Diabetes, Death and Treatment"
      )
    ggplotly(p)
  })
  output$tab3_table6 <- renderReactable({
    req(input$multicategory2_tab3_3)
    reactable(DIG_sub_dia_m_2())
  })
  
  output$tab3_distPlot7 <- renderPlotly({
    category2 <- req(input$category2_tab3_4)
    p <- ggplot(DIG_sub_hyp_m(),
                aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col() +
      labs(
        x     = category2,
        y     = "Death",
        fill  = category2,
        title = paste("Plot between", category2, "and death")
      )
    ggplotly(p)
  })
  output$tab3_table7 <- renderReactable({ reactable(DIG_sub_hyp_m()) })
  
  output$tab3_distPlot8 <- renderPlotly({
    req(input$multicategory2_tab3_4)
    p <- ggplot(DIG_sub_hyp_m_2(),
                aes(x = factor(HYPERTEN), y = death, fill = factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(
        x     = "Hypertension",
        y     = "Death",
        fill  = "Treatment",
        title = "Plot between Hypertension, Death and Treatment"
      )
    ggplotly(p)
  })
  output$tab3_table8 <- renderReactable({
    req(input$multicategory2_tab3_4)
    reactable(DIG_sub_hyp_m_2())
  })
  
  #SARA: global filtered data 
  
  filtered_data <- reactive({
    dat <- DIG_df
    if (input$filter_trt != "both") dat <- dat %>% filter(TRTMT == input$filter_trt)
    if (input$filter_sex != "both") dat <- dat %>% filter(SEX == input$filter_sex)
    dat %>% filter(AGE >= input$filter_age[1], AGE <= input$filter_age[2])
  })
  
  #Risk factor explorer
  risk_data <- reactive({
    dat <- filtered_data()
    if (nrow(dat) == 0) return(NULL)
    
    dat <- dat %>%
      mutate(
        RISK_OUTCOME = dplyr::case_when(
          input$risk_outcome == "hosp"      ~ as.numeric(HOSP == 1),
          input$risk_outcome == "death"     ~ as.numeric(DEATH == "Death"),
          input$risk_outcome == "composite" ~ as.numeric(HOSP == 1 | DEATH == "Death"),
          TRUE                              ~ 0
        ),
        RISK_OUTCOME_F = factor(RISK_OUTCOME, levels = c(0, 1),
                                labels = c("No event", "Event"))
      )
    
    pred_col <- input$risk_var
    dat <- dat %>% filter(!is.na(.data[[pred_col]]))
    if (nrow(dat) == 0) return(NULL)
    dat
  })
  
  #Boxplot
  output$risk_boxplot <- renderPlotly({
    dat <- risk_data()
    req(dat)
    
    pred_col <- input$risk_var
    pred_lab <- names(which(c(
      AGE    = "AGE",
      SYSBP  = "SYSBP",
      DIABP  = "DIABP",
      KLEVEL = "KLEVEL",
      CREAT  = "CREAT"
    ) == pred_col))
    
    p <- ggplot(dat,
                aes(x = RISK_OUTCOME_F, y = .data[[pred_col]], fill = RISK_OUTCOME_F)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = c("No event" = "#8e44ad", "Event" = "#e74c3c")) +
      labs(x = "Outcome", y = pred_lab) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  #Density
  output$risk_density <- renderPlotly({
    dat <- risk_data()
    req(dat)
    
    pred_col <- input$risk_var
    pred_lab <- names(which(c(
      AGE    = "AGE",
      SYSBP  = "SYSBP",
      DIABP  = "DIABP",
      KLEVEL = "KLEVEL",
      CREAT  = "CREAT"
    ) == pred_col))
    
    p <- ggplot(dat,
                aes(x = .data[[pred_col]], fill = RISK_OUTCOME_F, colour = RISK_OUTCOME_F)) +
      geom_density(alpha = 0.3) +
      scale_fill_manual(values   = c("No event" = "#8e44ad", "Event" = "#e74c3c")) +
      scale_colour_manual(values = c("No event" = "#8e44ad", "Event" = "#e74c3c")) +
      labs(x = pred_lab, y = "Density", fill = "Outcome", colour = "Outcome") +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  # ScatterPlot
  output$risk_scatter <- renderPlotly({
    dat <- risk_data()
    req(dat)
    
    pred_col <- input$risk_var
    pred_lab <- names(which(c(
      AGE    = "AGE",
      SYSBP  = "SYSBP",
      DIABP  = "DIABP",
      KLEVEL = "KLEVEL",
      CREAT  = "CREAT"
    ) == pred_col))
    
    p <- ggplot(dat,
                aes(x = .data[[pred_col]], y = RISK_OUTCOME, colour = TRTMT)) +
      geom_jitter(height = 0.05, alpha = 0.6, size = 1.8) +
      scale_y_continuous(breaks = c(0, 1), labels = c("No event", "Event")) +
      scale_colour_manual(values = c("Placebo" = "#3498db", "Digoxin" = "#16a085")) +
      labs(x = pred_lab, y = "Outcome", colour = "Treatment") +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  output$risk_model_table <- renderReactable({
    dat <- risk_data()
    req(dat)
    
    pred_col <- input$risk_var
    if (input$risk_adjust_trt) {
      fml <- as.formula(paste("RISK_OUTCOME ~", pred_col, "+ TRTMT"))
    } else {
      fml <- as.formula(paste("RISK_OUTCOME ~", pred_col))
    }
    
    fit      <- glm(fml, data = dat, family = binomial)
    coef_tab <- summary(fit)$coefficients
    OR       <- exp(coef_tab[, "Estimate"])
    CI       <- exp(confint(fit))
    
    res <- data.frame(
      Term        = rownames(coef_tab),
      OR          = round(OR, 2),
      CI_lower    = round(CI[, 1], 2),
      CI_upper    = round(CI[, 2], 2),
      p_value     = round(coef_tab[, "Pr(>|z|)"], 3),
      check.names = FALSE
    )
    
    reactable(res,
              searchable      = FALSE,
              sortable        = FALSE,
              defaultPageSize = nrow(res),
              striped         = TRUE,
              highlight       = TRUE)
  })
  
  output$risk_corr_table <- renderReactable({
    dat <- filtered_data()
    req(dat)
    
    vars    <- c("AGE", "SYSBP", "DIABP", "KLEVEL", "CREAT")
    dat_num <- dat[, vars]
    
    cmat    <- cor(dat_num, use = "pairwise.complete.obs")
    corr_df <- as.data.frame(round(cmat, 2))
    corr_df$Variable <- rownames(corr_df)
    corr_df <- corr_df[, c("Variable", vars)]
    
    reactable(corr_df,
              searchable      = FALSE,
              sortable        = TRUE,
              defaultPageSize = nrow(corr_df),
              striped         = TRUE,
              highlight       = TRUE)
  })
  
  #Outcomes summary
  outcome_data <- reactive({
    dat <- filtered_data()
    if (nrow(dat) == 0) return(NULL)
    
    dat <- dat %>%
      mutate(
        AGE_GROUP = cut(
          AGE,
          breaks = seq(
            floor(min(AGE, na.rm = TRUE)),
            ceiling(max(AGE, na.rm = TRUE)) + 1,
            by = 5
          ),
          right          = FALSE,
          include.lowest = TRUE
        )
      )
    
    dat <- dat %>%
      mutate(
        OUTCOME_FLAG = dplyr::case_when(
          input$outcome_type == "hosp"      ~ as.numeric(HOSP == 1),
          input$outcome_type == "death"     ~ as.numeric(DEATH == "Death"),
          input$outcome_type == "composite" ~ as.numeric(HOSP == 1 | DEATH == "Death"),
          TRUE                              ~ 0
        )
      )
    
    group_col <- input$outcome_group
    
    dat %>%
      group_by(.data[[group_col]]) %>%
      summarise(
        N_total   = n(),
        Events    = sum(OUTCOME_FLAG, na.rm = TRUE),
        EventRate = ifelse(N_total > 0, 100 * Events / N_total, NA_real_),
        .groups   = "drop"
      ) %>%
      rename(Group = 1)
  })
  
  output$outcome_plot <- renderPlotly({
    dat <- outcome_data()
    req(dat)
    
    y_var <- if (input$outcome_y == "count") "Events" else "EventRate"
    y_lab <- if (input$outcome_y == "count") "Number of events" else "Event rate (%)"
    
    n_grp <- length(unique(dat$Group))
    pal   <- colorRampPalette(
      c("#8e44ad", "#3498db", "#16a085", "#f39c12", "#e74c3c")
    )(n_grp)
    
    p <- ggplot(dat, aes(x = Group, y = .data[[y_var]], fill = Group)) +
      geom_col() +
      scale_fill_manual(values = pal) +
      labs(x = "", y = y_lab) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$outcome_table <- renderReactable({
    dat <- outcome_data()
    req(dat)
    
    reactable(
      dat %>% mutate(EventRate = round(EventRate, 1)),
      searchable      = TRUE,
      sortable        = TRUE,
      defaultPageSize = 10,
      striped         = TRUE,
      highlight       = TRUE
    )
  })
  
  #Advanced survival (SARA)
  surv_data <- reactive({
    dat <- filtered_data() %>%
      filter(!is.na(Month), !is.na(DEATH)) %>%
      filter(
        Month >= input$surv_time_range[1],
        Month <= input$surv_time_range[2]
      )
    if (nrow(dat) == 0) return(NULL)
    dat
  })
  
  surv_fit <- reactive({
    dat <- surv_data()
    req(dat)
    
    surv_obj <- Surv(dat$Month, dat$DEATH == "Death")
    
    switch(
      input$surv_group,
      "none"      = survfit(surv_obj ~ 1,           data = dat),
      "TRTMT"     = survfit(surv_obj ~ TRTMT,       data = dat),
      "CVD"       = survfit(surv_obj ~ CVD,         data = dat),
      "TRTMT_CVD" = survfit(surv_obj ~ TRTMT + CVD, data = dat),
      "SEX"       = survfit(surv_obj ~ SEX,         data = dat)
    )
  })
  
  output$km_main <- renderPlot({
    fit <- surv_fit()
    req(fit)
    
    s <- summary(fit)
    df <- data.frame(
      Month    = s$time,
      Survival = s$surv,
      Lower    = s$lower,
      Upper    = s$upper,
      Group    = if (is.null(s$strata)) "Overall" else s$strata
    ) %>%
      filter(
        Month >= input$surv_time_range[1],
        Month <= input$surv_time_range[2]
      )
    
    p <- ggplot(df, aes(x = Month, y = Survival, colour = Group)) +
      geom_step(size = 1) +
      labs(x = "Month", y = "Survival probability") +
      theme_minimal(base_size = 13)
    
    if (input$surv_show_ci) {
      p <- p +
        geom_ribbon(
          aes(ymin = Lower, ymax = Upper, fill = Group),
          alpha     = 0.15,
          colour    = NA,
          linewidth = 0
        )
    }
    p
  })
  
  output$surv_landmark_text <- renderPrint({
    fit <- surv_fit()
    req(fit)
    
    t0 <- input$surv_landmark_month
    s  <- summary(fit, times = t0)
    grp <- if (is.null(s$strata)) "Overall" else s$strata
    
    res <- data.frame(
      Group         = grp,
      Month         = s$time,
      Surv          = round(s$surv, 3),
      `No. at risk` = s$n.risk,
      Events        = s$n.event
    )
    
    cat("Survival at month", t0, "\n\n")
    print(res, row.names = FALSE)
  })
  
  output$tbl_surv_summary <- renderTable({
    fit <- surv_fit()
    req(fit)
    
    t0 <- input$surv_landmark_month
    s  <- summary(fit, times = t0)
    grp <- if (is.null(s$strata)) "Overall" else s$strata
    
    data.frame(
      Group                  = grp,
      Month                  = s$time,
      `Survival probability` = round(s$surv, 3),
      `No. at risk`          = s$n.risk,
      Events                 = s$n.event,
      check.names            = FALSE
    )
  })
  
  output$tbl_surv_median <- renderTable({
    fit <- surv_fit()
    req(fit)
    
    tab <- summary(fit)$table
    if (is.null(dim(tab))) {
      tab <- matrix(tab, nrow = 1, dimnames = list("Overall", names(tab)))
    }
    
    data.frame(
      Group      = rownames(tab),
      Median     = round(tab[, "median"], 1),
      `0.95 LCL` = round(tab[, "0.95LCL"], 1),
      `0.95 UCL` = round(tab[, "0.95UCL"], 1),
      check.names = FALSE
    )
  })
  
  output$tbl_surv_indiv <- renderReactable({
    dat <- surv_data()
    req(dat)
    
    reactable(
      dat %>% dplyr::select(TRTMT, SEX, AGE, CVD, WHF, Month, DEATH),
      searchable      = TRUE,
      sortable        = TRUE,
      pagination      = TRUE,
      defaultPageSize = 10
    )
  })
  
  # heatmap
  output$km_heatmap <- renderPlotly({
    fit <- surv_fit()
    req(fit)
    
    s <- summary(fit)
    grp <- if (is.null(s$strata)) "Overall" else s$strata
    
    df <- data.frame(
      Month    = s$time,
      Group    = grp,
      Survival = s$surv
    ) %>%
      filter(
        Month >= input$surv_time_range[1],
        Month <= input$surv_time_range[2]
      )
    
    p <- ggplot(df, aes(x = Month, y = Group, fill = Survival)) +
      geom_tile() +
      scale_fill_gradient(low = "#fdfbff", high = "#5b2c6f") +
      labs(x = "Month", y = "", fill = "Survival") +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  # flexdashboard value boxes
  output$surv_flex_valueboxes <- renderUI({
    fit <- surv_fit()
    req(fit)
    
    t0 <- input$surv_landmark_month
    s  <- summary(fit, times = t0)
    grp <- if (is.null(s$strata)) "Overall" else s$strata
    surv_vals <- round(100 * s$surv, 1)
    
    vb_list <- lapply(seq_along(grp), function(i) {
      col <- if (surv_vals[i] >= 80) "green"
      else if (surv_vals[i] >= 60)  "orange"
      else                          "red"
      
      flexdashboard::valueBox(
        value   = paste0(surv_vals[i], "%"),
        caption = paste("Survival at", t0, "months –", grp[i]),
        icon    = icon("heartbeat"),
        color   = col
      )
    })
    
    do.call(fluidRow, vb_list)
    
    
  })
}

#RUN APP
shinyApp(ui = ui, server = server)