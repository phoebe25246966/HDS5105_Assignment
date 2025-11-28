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
          box(
            title = "Filters",
            selectInput(
              inputId = "filter",
              label   = "Select filter",
              choices = c("Age","Sex","Hypertension","DiaBP","SysBP","KLevel","Creat"),
              multiple = FALSE
            ),
          ),
          
          fluidRow(
           box(plotOutput("distPlot",  height = 200))
          )
      ),
      #2nd tab
      tabItem(tabName = "Hospitalization",
              h2("Hospitalization among different category"),
              fluidRow(
                box(
                column(4,selectInput(inputId = "category",label="Select category",choice=c("WHF","Treatment"),multiple = FALSE) ),
                column(4,radioButtons(inputId = "multicategory",label="Show multiple category",choices=c("Disable","Enable")))
                )
                ),
              fluidRow(
                box(
                    width=10,
                    title="Single Categories Analysis",
                    status="warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    column(width=4,plotOutput("tab2_distPlot",height=200)),
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
                  column(width=4,plotOutput("tab2_distPlot2",height=200)),
                  reactableOutput("tab2_table2")
                  )
                )  
        ),
      #3rd tab
      tabItem(tabName = "Mortality",
              h2("Mortality among different category"),
              fluidRow(
              box(
                column(4,selectInput(inputId = "category2",label="Select category",choice=c("CVD","Treatment"),multiple = FALSE)),
                column(4,radioButtons(inputId = "multicategory2",label="Show multiple category",choices=c("Disable","Enable"))),
              )
                ),
              fluidRow(
                box(
                width=10,
                title="Single Category Analysis",
                status="warning",
                solidHeader = TRUE,
                collapsible = TRUE,
                column(width=4,plotOutput("tab3_distPlot",height=200)),
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
                column(width=4,plotOutput("tab3_distPlot2",height=200)),
                reactableOutput("tab3_table2"),
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
  
  #Reactive
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
      total=n(),.groups = "drop")
    
  })
  
  
  DIG_sub2 <- reactive({
    category_col <- switch(
      input$category,
      "WHF" = "WHF",
      "Treatment" = "TRTMT"
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
  
  DIG_sub3 <- reactive({
    DIG_df %>%
      group_by(WHF,TRTMT) %>% 
      summarise(
        hospitalization = sum(HOSP),
        total = n()
      )
  })
  
  DIG_sub4 <- reactive({
    category_col2 <- switch(
      input$category2,
      "CVD" = "CVD",
      "Treatment" = "TRTMT"
    )
    
    DIG_df %>%
      #save the categorical variable
      mutate(
        category_var2 = .data[[category_col2]]) %>%
      group_by(category_var2) %>% 
      summarise(
        death = sum(DEATH == "Death"),
        total = n()
      )
  })
  
  DIG_sub5 <- reactive({
    DIG_df %>%
      group_by(CVD,TRTMT) %>% 
      summarise(
        death = sum(DEATH == "Death"),
        total = n()
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
  
  output$distPlot <- renderPlot({
    ggplot(DIG_sub(), aes(x = category_var, y=total,fill = TRTMT)) +
      geom_col(position = "dodge")
  })

  
  #Hospitalization output
    
    output$tab2_distPlot <- renderPlot({
      category1 <- req(input$category)
      ggplot(DIG_sub2(),
             aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
             geom_col()+labs(x = paste(category1),y="Hospitalization",fill=paste(category1),title = paste("Plot between ",category1," and hospitalization"))
    })
    output$tab2_table1 <- renderReactable({
      reactable(DIG_sub2())
    })
    

    output$tab2_distPlot2 <- renderPlot({
      #req value from input
      req(input$multicategory!="Disable")
      ggplot(DIG_sub3(), aes(x=factor(WHF), y = hospitalization,fill=factor(TRTMT))) +
        geom_col(position = "dodge") +
        labs(x="WHF",y="Hospitalization",fill="Treatment",title = "Plot between WHF, HOSP and Treatment")
    })
    output$tab2_table2 <- renderReactable({
      req(input$multicategory!="Disable")
      reactable(DIG_sub3())
    })
    
    #Mortality output
    
    output$tab3_distPlot <- renderPlot({
      category2 <- req(input$category2)
      ggplot(DIG_sub4(),
             aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
        geom_col()+labs(x = paste(category2),y="Death",fill=paste(category2),title = paste("Plot between ",category2," and death"))
    })
    output$tab3_table1 <- renderReactable({
      reactable(DIG_sub4())
    })
    
    
    output$tab3_distPlot2 <- renderPlot({
      #req value from input
      req(input$multicategory2!="Disable")
      ggplot(DIG_sub5(), aes(x=factor(CVD), y = death,fill=factor(TRTMT))) +
        geom_col(position = "dodge") +
        labs(x="CVD",y="Death",fill="Treatment",title = "Plot between CVD, Death and Treatment")
    })
    output$tab3_table2 <- renderReactable({
      req(input$multicategory2!="Disable")
      reactable(DIG_sub5())
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
