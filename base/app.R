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
        menuItem("Survival", tabName = "Survival", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
      )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  DIG_df <- read.csv("DIG-1.csv")
  DIG_df <- DIG_df %>% drop_na()
  
}

# Run the application 
shinyApp(ui = ui, server = server)
