#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shinyWidgets)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(survival)
library(reactable)

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

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  
  # Application title
  dashboardHeader(title="DIG Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Baseline", tabName = "Baseline", icon = icon("home")),
      menuItem("Hospitalization", tabName = "Hospitalization", icon = icon("table")),
      menuItem("Mortality", tabName = "Mortality", icon = icon("skull")),
      menuItem("Survival (Months)", tabName = "survival", icon = icon("heartbeat")),
      
      #Filter for survival
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
      #1st tab
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
      
      #4th tab
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


server <- function(input, output) {
  
  #Phoebe
  
  #Baseline characteristics (reactive)
  DIG <- reactive({
    DIG_df
  })
  
  DIG_sum <- reactive({
    data <- DIG_df%>%
      summarise(
        n_hosp=sum(HOSP),
        n_death=sum(DEATH == "Death"),
        mean_age=mean(AGE),
        n_hyperten=sum(factor(HYPERTEN) == 1),
        mean_SYSBP=mean(SYSBP),
        mean_DIABP=mean(DIABP)
      )
  })
  
  DIG_sub_cat <- reactive({
    filter <- req(input$filter_cat)
    colname <- switch(
      filter,
      "Sex" = "SEX",
      "Hypertension" = "HYPERTEN"
    )
    DIG_df%>%
      mutate(category_var = .data[[colname]]) %>%
      group_by(TRTMT,category_var)%>%
      summarise(
        total=n(),
        .groups = "drop")
    
  })
  
  DIG_sub <- reactive({
    filter <- req(input$filter)
    colname <- switch(
      filter,
      "Creat" = "CREAT",
      "Sex" = "SEX",
      "Hypertension" = "HYPERTEN",
      "DiaBP" = "DIABP",
      "SysBP" = "SYSBP",
      "KLevel" = "KLEVEL",
      "Age" = "AGE"
    )
    
    DIG_df%>%
      mutate(category_var = .data[[colname]]) %>%
      group_by(TRTMT,category_var)%>%
      summarise(
        total=n(),
        .groups = "drop")
    
  })
  
  #Hospitalization (reactive)
  
  DIG_sub2 <- reactive({
    category_col <- switch(
      input$category,
      "WHF" = "WHF"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>% 
      summarise(
        hospitalization = sum(HOSP),
        total = n()
      )
  })
  
  DIG_sub2_pulcong <- reactive({
    category_col <- switch(
      input$category_tab2_2,
      "Pulmonary congestion" = "PULCONG"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>% 
      summarise(
        hospitalization = sum(HOSP),
        total = n()
      )
  })
  
  DIG_sub2_pulcong_2 <- reactive({
    DIG_df %>%
      group_by(PULCONG,TRTMT) %>% 
      summarise(
        hospitalization = sum(HOSP),
        total = n()
      )
  })
  
  DIG_sub2_dia <- reactive({
    category_col <- switch(
      input$category_tab2_3,
      "Diabetes" = "DIABETES"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>% 
      summarise(
        hospitalization = sum(HOSP),
        total = n()
      )
  })
  
  DIG_sub2_dia_2 <- reactive({
    DIG_df %>%
      group_by(DIABETES,TRTMT) %>% 
      summarise(
        hospitalization = sum(HOSP)
      )
  })
  
  DIG_sub2_hyp <- reactive({
    category_col <- switch(
      input$category_tab2_4,
      "Hypertension" = "HYPERTEN",
      "Treatment" = "TRTMT"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(category_var = .data[[category_col]]) %>%
      group_by(category_var) %>% 
      summarise(
        hospitalization = sum(HOSP)
      )
  })
  
  DIG_sub2_hyp_2 <- reactive({
    DIG_df %>%
      group_by(HYPERTEN,TRTMT) %>% 
      summarise(
        hospitalization = sum(HOSP)
      )
  })
  
  
  DIG_sub3 <- reactive({
    DIG_df %>%
      group_by(WHF,TRTMT) %>% 
      summarise(
        hospitalization = sum(HOSP)
      )
  })
  
  #Mortality (reactive)
  
  DIG_sub4 <- reactive({
    category_col2 <- switch(
      input$category2,
      "CVD" = "CVD"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(
        category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>% 
      summarise(
        death = sum(DEATH == "Death")
      )
  })
  
  DIG_sub5 <- reactive({
    DIG_df %>%
      group_by(CVD,TRTMT) %>% 
      summarise(
        death = sum(DEATH == "Death")
      )
  })
  
  DIG_sub_WHF_m <- reactive({
    category_col2 <- switch(
      input$category2_tab3_2,
      "Worsening Heart Failure" = "WHF"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(
        category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>% 
      summarise(
        death = sum(DEATH == "Death")
      )
  })
  
  DIG_sub_WHF_m_2 <- reactive({
    DIG_df %>%
      group_by(WHF,TRTMT) %>% 
      summarise(
        death = sum(DEATH == "Death")
      )
  })
  
  DIG_sub_dia_m <- reactive({
    category_col2 <- switch(
      input$category2_tab3_3,
      "Diabetes" = "DIABETES"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(
        category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>% 
      summarise(
        death = sum(DEATH == "Death")
      )
  })
  
  DIG_sub_dia_m_2 <- reactive({
    DIG_df %>%
      group_by(DIABETES,TRTMT) %>% 
      summarise(
        death = sum(DEATH == "Death")
      )
  })
  
  DIG_sub_hyp_m <- reactive({
    category_col2 <- switch(
      input$category2_tab3_4,
      "Hypertension" = "HYPERTEN"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(
        category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>% 
      summarise(
        death = sum(DEATH == "Death")
      )
  })
  
  DIG_sub_hyp_m_2 <- reactive({
    DIG_df %>%
      group_by(HYPERTEN,TRTMT) %>% 
      summarise(
        death = sum(DEATH == "Death")
      )
  })
  
  
  
  #valuebox output
  output$sum_n <- renderValueBox({
    n_pat <- nrow(DIG())
    valueBox(
      formatC(n_pat, big.mark = ","),
      subtitle = "Patients in current selection",
      icon = icon("users"),
      color = "teal"
    )
  })
  
  output$sum_hyperten <- renderValueBox({
    valueBox(
      value = DIG_sum()$n_hyperten,
      subtitle = "Total no. history of hypertension",
      icon = icon("heart"),
      color = "light-blue"
    )
  })
  
  output$sum_SYSBP <- renderValueBox({
    valueBox(
      value = round(DIG_sum()$mean_SYSBP,2),
      subtitle = "Average of Sysolic BP",
      icon = icon("info"),
      color = "light-blue"
    )
  })
  
  output$sum_DIABP <- renderValueBox({
    valueBox(
      value = round(DIG_sum()$mean_DIABP,2),
      subtitle = "Average of Diastolic BP",
      icon = icon("info"),
      color = "light-blue"
    )
  })
  
  output$sum_hosp <- renderValueBox({
    valueBox(
      value = round(DIG_sum()$n_hosp,2),
      subtitle = "Number of hospitalization",
      icon = icon("heart-broken"),
      color = "red"
    )
  })
  
  output$sum_deathday <- renderValueBox({
    valueBox(
      value = round(DIG_sum()$n_death),
      subtitle = "Average death",
      icon = icon("skull"),
      color = "blue"
    )
  })
  
  output$sum_age <- renderValueBox({
    valueBox(
      value = round(DIG_sum()$mean_age),
      subtitle = "Average age",
      icon = icon("id-card"),
      color = "yellow"
    )
  })
  
  
  #Baseline
  
  output$distPlot <- renderPlotly({
    filter <- req(input$filter)
    if(input$type=="BarPlot"){
    plot <- ggplot(DIG_sub(), aes(x = category_var, y=total,fill = TRTMT)) +
      geom_col(position = "dodge")+labs(x=paste(filter),title = paste("Plot between ",filter," and treatment"))+theme_minimal()
    ggplotly(plot)
    }
    else if(input$type=="LinePlot"){
      plot <- ggplot(DIG_sub(), aes(x = category_var, y=total,color = TRTMT)) +
        geom_line()+labs(x=paste(filter),title = paste("Plot between ",filter," and treatment"))+theme_minimal()
      ggplotly(plot)
    }
  })
  
  output$distPlot_cat <- renderPlotly({
    filter_cat <- req(input$filter_cat)
      plot <- ggplot(DIG_sub_cat(), aes(x = factor(category_var), y=total,fill = TRTMT)) +
        geom_col(position = "dodge")+labs(x=paste(filter_cat),title = paste("Plot between ",filter_cat," and treatment"))+theme_minimal()
      ggplotly(plot)
  })
  
  #Hospitalization output
  
  output$tab2_distPlot <- renderPlotly({
    category1 <- req(input$category)
    plot<-ggplot(DIG_sub2(),
           aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col()+labs(x = paste(category1),y="Hospitalization",fill=paste(category1),title = paste("Plot between ",category1," and hospitalization"))
    ggplotly(plot)
    })
  output$tab2_table1 <- renderReactable({
    reactable(DIG_sub2())
  })
  
  
  output$tab2_distPlot2 <- renderPlotly({
    #req value from input-> only display when switch is on
    req(input$multicategory)
    plot <- ggplot(DIG_sub3(), aes(x=factor(WHF), y = hospitalization,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="WHF",y="Hospitalization",fill="Treatment",title = "Plot between WHF, HOSP and Treatment")
    ggplotly(plot)
    })
  output$tab2_table2 <- renderReactable({
    req(input$multicategory)
    reactable(DIG_sub3())
  })
  
  
  
  output$tab2_distPlot3 <- renderPlotly({
    category1 <- req(input$category_tab2_2)
    plot<-ggplot(DIG_sub2_pulcong(),
                 aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col()+labs(x = paste(category1),y="Hospitalization",fill=paste(category1),title = paste("Plot between ",category1," and hospitalization"))
    ggplotly(plot)
  })
  output$tab2_table3 <- renderReactable({
    reactable(DIG_sub2_pulcong())
  })
  
  
  output$tab2_distPlot4 <- renderPlotly({
    #req value from input
    req(input$multicategory_tab2_2)
    plot <- ggplot(DIG_sub2_pulcong_2(), aes(x=factor(PULCONG), y = hospitalization,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="PULCONG",y="Hospitalization",fill="Treatment",title = "Plot between PULCONG, HOSP and Treatment")
    ggplotly(plot)
  })
  output$tab2_table4 <- renderReactable({
    req(input$multicategory_tab2_2)
    reactable(DIG_sub2_pulcong_2())
  })
  
  
  output$tab2_distPlot5 <- renderPlotly({
    category1 <- req(input$category_tab2_3)
    plot<-ggplot(DIG_sub2_dia(),
                 aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col()+labs(x = paste(category1),y="Hospitalization",fill=paste(category1),title = paste("Plot between ",category1," and hospitalization"))
    ggplotly(plot)
  })
  output$tab2_table5 <- renderReactable({
    reactable(DIG_sub2_dia())
  })
  
  
  output$tab2_distPlot6 <- renderPlotly({
    #req value from input
    req(input$multicategory_tab2_3)
    plot <- ggplot(DIG_sub2_dia_2(), aes(x=factor(DIABETES), y = hospitalization,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="Diabetes",y="Hospitalization",fill="Treatment",title = "Plot between Diabetes, HOSP and Treatment")
    ggplotly(plot)
  })
  output$tab2_table6 <- renderReactable({
    req(input$multicategory_tab2_3)
    reactable(DIG_sub2_dia_2())
  })
  
  output$tab2_distPlot7 <- renderPlotly({
    category1 <- req(input$category_tab2_4)
    plot<-ggplot(DIG_sub2_hyp(),
                 aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col()+labs(x = paste(category1),y="Hospitalization",fill=paste(category1),title = paste("Plot between ",category1," and hospitalization"))
    ggplotly(plot)
  })
  output$tab2_table7 <- renderReactable({
    reactable(DIG_sub2_hyp())
  })
  
  
  output$tab2_distPlot8 <- renderPlotly({
    #req value from input
    req(input$multicategory_tab2_4)
    plot <- ggplot(DIG_sub2_hyp_2(), aes(x=factor(HYPERTEN), y = hospitalization,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="Hypertension",y="Hospitalization",fill="Treatment",title = "Plot between Hypertension, HOSP and Treatment")
    ggplotly(plot)
  })
  output$tab2_table8 <- renderReactable({
    req(input$multicategory_tab2_4)
    reactable(DIG_sub2_hyp_2())
  })
  
  #Mortality output
  
  output$tab3_distPlot <- renderPlotly({
    category2 <- req(input$category2)
    plot <- ggplot(DIG_sub4(),
           aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col()+labs(x = paste(category2),y="Death",fill=paste(category2),title = paste("Plot between ",category2," and death"))
    ggplotly(plot)
    })
  output$tab3_table1 <- renderReactable({
    reactable(DIG_sub4())
  })
  
  
  output$tab3_distPlot2 <- renderPlotly({
    #req value from input
    req(input$multicategory2)
    plot <- ggplot(DIG_sub5(), aes(x=factor(CVD), y = death,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="CVD",y="Death",fill="Treatment",title = "Plot between CVD, Death and Treatment")
    ggplotly(plot)
    })
  output$tab3_table2 <- renderReactable({
    req(input$multicategory2)
    reactable(DIG_sub5())
  })
  
  output$tab3_distPlot3 <- renderPlotly({
    category2 <- req(input$category2_tab3_2)
    plot <- ggplot(DIG_sub_WHF_m(),
                   aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col()+labs(x = paste(category2),y="Death",fill=paste(category2),title = paste("Plot between ",category2," and death"))
    ggplotly(plot)
  })
  output$tab3_table3 <- renderReactable({
    reactable(DIG_sub_WHF_m())
  })
  
  
  output$tab3_distPlot4 <- renderPlotly({
    #req value from input
    req(input$multicategory2_tab3_2)
    plot <- ggplot(DIG_sub_WHF_m_2(), aes(x=factor(WHF), y = death,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="WHF",y="Death",fill="Treatment",title = "Plot between WHF, Death and Treatment")
    ggplotly(plot)
  })
  output$tab3_table4 <- renderReactable({
    req(input$multicategory2_tab3_2)
    reactable(DIG_sub_WHF_m_2())
  })
  
  
  output$tab3_distPlot5 <- renderPlotly({
    category2 <- req(input$category2_tab3_3)
    plot <- ggplot(DIG_sub_dia_m(),
                   aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col()+labs(x = paste(category2),y="Death",fill=paste(category2),title = paste("Plot between ",category2," and death"))
    ggplotly(plot)
  })
  output$tab3_table5 <- renderReactable({
    reactable(DIG_sub_dia_m())
  })
  
  
  output$tab3_distPlot6 <- renderPlotly({
    #req value from input
    req(input$multicategory2_tab3_3)
    plot <- ggplot(DIG_sub_dia_m_2(), aes(x=factor(DIABETES), y = death,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="Diabetes",y="Death",fill="Treatment",title = "Plot between Diabetes, Death and Treatment")
    ggplotly(plot)
  })
  output$tab3_table6 <- renderReactable({
    req(input$multicategory2_tab3_3)
    reactable(DIG_sub_dia_m_2())
  })
  
  
  output$tab3_distPlot7 <- renderPlotly({
    category2 <- req(input$category2_tab3_4)
    plot <- ggplot(DIG_sub_dia_m(),
                   aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col()+labs(x = paste(category2),y="Death",fill=paste(category2),title = paste("Plot between ",category2," and death"))
    ggplotly(plot)
  })
  output$tab3_table7 <- renderReactable({
    reactable(DIG_sub_hyp_m())
  })
  
  
  output$tab3_distPlot8 <- renderPlotly({
    #req value from input
    req(input$multicategory2_tab3_4)
    plot <- ggplot(DIG_sub_hyp_m_2(), aes(x=factor(HYPERTEN), y = death,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="Hypertension",y="Death",fill="Treatment",title = "Plot between Hypertension, Death and Treatment")
    ggplotly(plot)
  })
  output$tab3_table8 <- renderReactable({
    req(input$multicategory2_tab3_4)
    reactable(DIG_sub_hyp_m_2())
  })
  
  #SARA
  
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

# Run the application 
shinyApp(ui = ui, server = server)
