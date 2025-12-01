#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- dashboardPage(
<<<<<<< Updated upstream

    # Application title
    dashboardHeader(title="DIG"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Baseline", tabName = "Baseline", icon = icon("Dashboard")),
        menuItem("Survival", tabName = "Survival", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
=======
  
  
  # Application title
  dashboardHeader(title="DIG Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Baseline", tabName = "Baseline", icon = icon("home")),
      menuItem("Hospitalization", tabName = "Hospitalization", icon = icon("table")),
      menuItem("Mortality", tabName = "Mortality", icon = icon("skull")),
      menuItem("Outcomes summary", tabName = "outcomes", icon = icon("chart-column")),
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
              h2("Hospitalization among different categories"),
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
      #3rd tab
      tabItem(tabName = "Mortality",
              h2("Mortality among different categories"),
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
      
      # NEW TAB: Outcomes summary (SARA)
      tabItem(
        tabName = "outcomes",
        h2("Outcomes summary"),
        fluidRow(
          box(
            width       = 4,
            title       = "Outcomes controls",
            status      = "primary",
            solidHeader = TRUE,
            helpText("All analyses here respect the global filters (treatment, sex, age) in the sidebar."),
            
            selectInput(
              "outcome_type", "Outcome",
              choices = c(
                "Hospitalization"        = "hosp",
                "Death"                  = "death",
                "Hospitalization or death (composite)" = "composite"
              ),
              selected = "composite"
            ),
            
            selectInput(
              "outcome_group", "Group by",
              choices = c(
                "Treatment"          = "TRTMT",
                "CVD"                = "CVD",
                "WHF"                = "WHF",
                "Sex"                = "SEX",
                "Age group (5-year)" = "AGE_GROUP"
              ),
              selected = "TRTMT"
            ),
            
            radioButtons(
              "outcome_y", "Show on y-axis",
              choices = c("Event count" = "count", "Event rate (%)" = "rate"),
              selected = "rate"
            )
          ),
          
          box(
            width       = 8,
            title       = "Event rates by group",
            status      = "primary",
            solidHeader = TRUE,
            plotlyOutput("outcome_plot", height = 350)
          )
        ),
        
        fluidRow(
          box(
            width       = 12,
            title       = "Table of outcomes by group",
            status      = "info",
            solidHeader = TRUE,
            reactableOutput("outcome_table")
          )
        )
      ),
      
      
      #4th tab
      tabItem(
        tabName = "survival",
        h2("Survival analysis (all-cause mortality)"),
        fluidRow(
          box(
            width       = 4,
            title       = "Survival controls (only affect this tab)",
            status      = "primary",
            solidHeader = TRUE,
            helpText("Data are already filtered by the global sidebar filters (treatment, sex, age)."),
            
            selectInput(
              "surv_group", "Group curves by",
              choices = c(
                "Overall only"         = "none",
                "Treatment"            = "TRTMT",
                "CVD"                  = "CVD",
                "Treatment + CVD"      = "TRTMT_CVD",
                "Sex"                  = "SEX"
              ),
              selected = "TRTMT"
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
            
            checkboxInput(
              "surv_show_ci",
              "Show 95% confidence bands",
              value = TRUE
            )
          ),
          
          box(
            width       = 8,
            title       = "Kaplan–Meier survival curves",
            status      = "primary",
            solidHeader = TRUE,
            plotOutput("km_main", height = 350),
            br(),
            verbatimTextOutput("surv_landmark_text")
          )
        ),
        
        fluidRow(
          box(
            width       = 6,
            title       = "Number at risk / events at chosen month",
            status      = "info",
            solidHeader = TRUE,
            tableOutput("tbl_surv_summary")
          ),
          box(
            width       = 6,
            title       = "Median survival time by group",
            status      = "info",
            solidHeader = TRUE,
            tableOutput("tbl_surv_median")
          )
        ),
        
        fluidRow(
          box(
            width       = 12,
            title       = "Row-level data used in survival analysis (after all filters)",
            status      = "info",
            solidHeader = TRUE,
            reactableOutput("tbl_surv_indiv")
          )
        )
>>>>>>> Stashed changes
      )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
<<<<<<< Updated upstream

  DIG_df <- read.csv("DIG-1.csv")
  DIG_df <- DIG_df %>% drop_na()
=======
  
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
    #req value from input
    req(input$multicategory!="Disable")
    plot <- ggplot(DIG_sub3(), aes(x=factor(WHF), y = hospitalization,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="WHF",y="Hospitalization",fill="Treatment",title = "Plot between WHF, HOSP and Treatment")
    ggplotly(plot)
    })
  output$tab2_table2 <- renderReactable({
    req(input$multicategory!="Disable")
    reactable(DIG_sub3())
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
    req(input$multicategory2!="Disable")
    plot <- ggplot(DIG_sub5(), aes(x=factor(CVD), y = death,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="CVD",y="Death",fill="Treatment",title = "Plot between CVD, Death and Treatment")
    ggplotly(plot)
    })
  output$tab3_table2 <- renderReactable({
    req(input$multicategory2!="Disable")
    reactable(DIG_sub5())
  })
  
  
  #SARA
  
  # Global filtered data (for Survival tab)
  
  filtered_data <- reactive({
    dat <- DIG_df
    
    # sidebar filters
    if (input$filter_trt != "both") {
      dat <- dat %>% filter(TRTMT == input$filter_trt)
    }
    if (input$filter_sex != "both") {
      dat <- dat %>% filter(SEX == input$filter_sex)
    }
    dat <- dat %>%
      filter(AGE >= input$filter_age[1],
             AGE <= input$filter_age[2])
    
    dat
  })

  # ----- SARA: Outcomes summary tab -----
  outcome_data <- reactive({
    dat <- filtered_data()
    if (nrow(dat) == 0) return(NULL)
    
    # Create age groups for option "AGE_GROUP"
    dat <- dat %>%
      mutate(
        AGE_GROUP = cut(
          AGE,
          breaks = seq(floor(min(AGE, na.rm = TRUE)),
                       ceiling(max(AGE, na.rm = TRUE)) + 1,
                       by = 5),
          right = FALSE,
          include.lowest = TRUE
        )
      )
    
    # Choose outcome flag
    dat <- dat %>%
      mutate(
        OUTCOME_FLAG = dplyr::case_when(
          input$outcome_type == "hosp" ~ as.numeric(HOSP == 1),
          input$outcome_type == "death" ~ as.numeric(DEATH == "Death"),
          input$outcome_type == "composite" ~ as.numeric(HOSP == 1 | DEATH == "Death"),
          TRUE ~ 0
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
    
    p <- ggplot(dat, aes(x = Group, y = .data[[y_var]])) +
      geom_col(fill = "#3182bd") +
      labs(
        x = "",
        y = y_lab,
        title = paste(
          if (input$outcome_type == "hosp") "Hospitalization" else
            if (input$outcome_type == "death") "Death" else
              "Hospitalization or death",
          "by", 
          names(which(c(
            TRTMT    = "TRTMT",
            CVD      = "CVD",
            WHF      = "WHF",
            SEX      = "SEX",
            AGE_GROUP = "AGE_GROUP"
          ) == input$outcome_group))
        )
      ) +
      theme_minimal(base_size = 13)
    
    ggplotly(p)
  })
  
  output$outcome_table <- renderReactable({
    dat <- outcome_data()
    req(dat)
    
    reactable(
      dat %>%
        mutate(EventRate = round(EventRate, 1)),
      searchable = TRUE,
      sortable   = TRUE,
      defaultPageSize = 10,
      striped = TRUE,
      highlight = TRUE
    )
  })
  
  # Data used for survival analysis (includes Month + DEATH + time range)
  surv_data <- reactive({
    dat <- filtered_data() %>%
      filter(!is.na(Month), !is.na(DEATH)) %>%
      filter(Month >= input$surv_time_range[1],
             Month <= input$surv_time_range[2])
    
    if (nrow(dat) == 0) return(NULL)
    dat
  })
  
  # Fit Kaplan–Meier model according to chosen grouping
  surv_fit <- reactive({
    dat <- surv_data()
    req(dat)
    
    surv_obj <- Surv(dat$Month, dat$DEATH == "Death")
    
    fit <- switch(
      input$surv_group,
      "none" = survfit(surv_obj ~ 1, data = dat),
      "TRTMT" = survfit(surv_obj ~ TRTMT, data = dat),
      "CVD" = survfit(surv_obj ~ CVD, data = dat),
      "TRTMT_CVD" = survfit(surv_obj ~ TRTMT + CVD, data = dat),
      "SEX" = survfit(surv_obj ~ SEX, data = dat)
    )
    
    fit
  })
  
  # Main survival plot
  output$km_main <- renderPlot({
    fit <- surv_fit()
    req(fit)
    
    s <- summary(fit)
    
    df <- data.frame(
      Month = s$time,
      Survival = s$surv,
      Lower = s$lower,
      Upper = s$upper,
      Group = if (is.null(s$strata)) "Overall" else s$strata
    ) %>%
      filter(Month >= input$surv_time_range[1],
             Month <= input$surv_time_range[2])
    
    p <- ggplot(df, aes(x = Month, y = Survival, colour = Group)) +
      geom_step(size = 1) +
      labs(
        x = "Month",
        y = "Survival probability"
      ) +
      theme_minimal(base_size = 13)
    
    if (input$surv_show_ci) {
      p <- p +
        geom_ribbon(
          aes(ymin = Lower, ymax = Upper, fill = Group),
          alpha = 0.15,
          colour = NA,
          linewidth = 0
        )
    }
    
    p
  })
>>>>>>> Stashed changes
  
  # Text summary at chosen landmark month
  output$surv_landmark_text <- renderPrint({
    fit <- surv_fit()
    req(fit)
    
    t0 <- input$surv_landmark_month
    s  <- summary(fit, times = t0)
    
    grp <- if (is.null(s$strata)) "Overall" else s$strata
    
    res <- data.frame(
      Group       = grp,
      Month       = s$time,
      Surv        = round(s$surv, 3),
      `No. at risk` = s$n.risk,
      Events      = s$n.event
    )
    
    cat("Survival at month", t0, "\n\n")
    print(res, row.names = FALSE)
  })
  
  # Table: number at risk / events at landmark month
  output$tbl_surv_summary <- renderTable({
    fit <- surv_fit()
    req(fit)
    
    t0 <- input$surv_landmark_month
    s  <- summary(fit, times = t0)
    
    grp <- if (is.null(s$strata)) "Overall" else s$strata
    
    data.frame(
      Group        = grp,
      Month        = s$time,
      `Survival probability` = round(s$surv, 3),
      `No. at risk` = s$n.risk,
      Events       = s$n.event,
      check.names  = FALSE
    )
  })
  
  # Table: median survival and 95% CI for each group
  output$tbl_surv_median <- renderTable({
    fit <- surv_fit()
    req(fit)
    
    tab <- summary(fit)$table
    
    # When only one group, summary() returns a named vector
    if (is.null(dim(tab))) {
      tab <- matrix(tab, nrow = 1,
                    dimnames = list("Overall", names(tab)))
    }
    
    data.frame(
      Group      = rownames(tab),
      Median     = round(tab[, "median"], 1),
      `0.95 LCL` = round(tab[, "0.95LCL"], 1),
      `0.95 UCL` = round(tab[, "0.95UCL"], 1),
      check.names = FALSE
    )
  })
  
  # Table of individual rows used in the survival analysis
  output$tbl_surv_indiv <- renderReactable({
    dat <- surv_data()
    req(dat)
    
    reactable(
      dat %>%
        dplyr::select(TRTMT, SEX, AGE, CVD, WHF, Month, DEATH),
      searchable = TRUE,
      sortable   = TRUE,
      pagination = TRUE,
      defaultPageSize = 10
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
