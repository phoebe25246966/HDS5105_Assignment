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

    # Application title
    dashboardHeader(title="DIG"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Baseline", tabName = "Baseline", icon = icon("Dashboard")),
        menuItem("Hospitalization", tabName = "Hospitalization",icon = icon("hospital")),
        menuItem("Mortality", tabName = "Mortality",icon = icon("skull")),
        menuItem("Survival", tabName = "Survival", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Hospitalization",
                h2("Hospitalization among different categories"),
                box(
                  selectInput(inputId = "category",label="Select category",choice=c("WHF","Treatment"),multiple = FALSE),
                  selectInput(inputId = "multicategory",label="Show multiple category",choice=c("False","True"),multiple = FALSE),
                ),
                fluidRow(
                  box(plotOutput("tab2_distPlot",height=200)),
                  dataTableOutput("tab2_table1"),
                  box(plotOutput("tab2_distPlot2",height=200,width="100%")),
                  dataTableOutput("tab2_table2"),
                )
        ),
        tabItem(tabName = "Mortality",
                h2("Mortality among different categories"),
                box(
                  selectInput(inputId = "category2",label="Select category",choice=c("CVD","Treatment"),multiple = FALSE),
                  selectInput(inputId = "multicategory2",label="Show multiple category",choice=c("False","True"),multiple = FALSE),
                ),
                fluidRow(
                  box(plotOutput("tab3_distPlot",height=200)),
                  dataTableOutput("tab3_table1"),
                  box(plotOutput("tab3_distPlot2",height=200,width="100%")),
                  dataTableOutput("tab3_table2"),
                )
        )
      )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  DIG_df <- read.csv("DIG-1.csv")
  DIG_df <- DIG_df %>% drop_na()
  DIG_df$TRTMT <- factor(DIG_df$TRTMT,levels=c(0,1),labels=c("Placebo","Treatment"))
  DIG_df$RACE <- factor(DIG_df$RACE,levels=c(1,2),labels=c("White","Nonwhite"))
  DIG_df$SEX <- factor(DIG_df$SEX,levels=c(1,2),labels=c("Male","Female"))
  DIG_df$DEATH <- factor(DIG_df$DEATH,levels=c(1,0),labels=c("Death","Alive"))
  
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
  
  
  
  output$tab2_distPlot <- renderPlot({
    category <- req(input$category)
    ggplot(DIG_sub2(),
           aes(x = factor(category_var), y = hospitalization, fill = factor(category_var))) +
      geom_col()+labs(x = paste(category),y="Hospitalization",fill= paste(category),title = paste("Plot between ",category," and hospitalization"))
  })
  output$tab2_table1 <- renderDataTable({
    DIG_sub2()
  })
  
  
  output$tab2_distPlot2 <- renderPlot({
    #req value from input
    req(input$multicategory!="False")
    ggplot(DIG_sub3(), aes(x=factor(WHF), y = hospitalization,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="WHF",y="Hospitalization",fill="Treatment",title = "Plot between WHF, HOSP and Treatment")
  })
  output$tab2_table2 <- renderDataTable({
    DIG_sub3()
  })
  
  
  output$tab3_distPlot <- renderPlot({
    category2 <- req(input$category2)
    ggplot(DIG_sub4(),
           aes(x = factor(category_var2), y = death, fill = factor(category_var2))) +
      geom_col()+labs(x = paste(category2),y="Death",fill=paste(category2),title = paste("Plot between ",category2," and death"))
  })
  output$tab3_table1 <- renderDataTable({
    DIG_sub4()
  })
  
  
  output$tab3_distPlot2 <- renderPlot({
    #req value from input
    req(input$multicategory2!="False")
    ggplot(DIG_sub5(), aes(x=factor(CVD), y = death,fill=factor(TRTMT))) +
      geom_col(position = "dodge") +
      labs(x="CVD",y="Death",fill="Treatment",title = "Plot between CVD, Death and Treatment")
  })
  output$tab3_table2 <- renderDataTable({
    DIG_sub5()
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
