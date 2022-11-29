#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('shiny')
#install.packages('shinydashboard')
# install.packages("readr")
library(readr)
library(shiny)
library(shinydashboard)
# Define UI for application that draws a histogram

sales <- readr::read_csv("mental-health-compiled.csv")
sales <- sales[c(
  "TERRITORY", "ORDERDATE", "ORDERNUMBER", "PRODUCTCODE", "QUANTITYORDERED", "PRICEEACH"
)]

ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "Saúde mental de dev" 
  ),
  dashboardSidebar(
    sidebarMenu(
      selectInput("selectedLocalInput", "Local", choices = unique(sales$TERRITORY)),
      sliderInput("peopleSlider", "Pessoas", min = 1, max = 1000, value = 270),
      menuItem(
        "Dados",
        tabName = "dadousados",
        icon = icon("th")
      ),
      menuItem(
        "Gráficos",
        tabName = "graficos",
        icon = icon("dashboard")
      ),
      menuItem(
        "Acessar código via Github",
        icon = icon("github"),
        href = "https://github.com/vivianequeiroz/ifsp-data-science-final-project-ICDA6"
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dadousados", h2("Dados"),

        tableOutput("tableLocal")
      ),
      tabItem(
        tabName = "graficos", h2("Histograma"),
        fluidRow(
          valueBox(
            uiOutput("peopleCard"),
            subtitle = "Pessoas",
            width = 4,
            icon = icon("person")
          )
        ),
        box(
          title = "Histograma da saúde mental",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("healthHist", height = "300")
        ),
      )
    )
  )
)


server <- function(input, output) {
  output$peopleCard <- renderText({
    prettyNum((input$peopleSlider))  
  })
  
  output$healthHist <- renderPlot({
    hist(rnorm(input$peopleSlider))
  })
  
  selectedLocal <- reactive(
    if(input$selectedLocalInput == "NA") {
      subset(sales, is.na(TERRITORY))
    } else {
      subset(sales, TERRITORY == input$selectedLocalInput)
    }
    #sales[sales$TERRITORY == input$selectedLocalInput,]
  )
  output$tableLocal <- renderTable(
    head(selectedLocal()),
    10
  )
}

shinyApp(ui = ui, server = server)
