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
        menuItem("Survival", tabName = "Survival", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Hospitalization",
                h2("Hospitalization among different category"),
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
  
  DIG_sub3 <- reactive({
    DIG_df %>%
      group_by(WHF,TRTMT) %>% 
      summarise(
        hospitalization = sum(HOSP),
        total = n()
      )
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
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
