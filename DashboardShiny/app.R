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

overview <- readr::read_csv("../MentalHealthTechIndustrySurvey/mental-health-in-tech-overview.csv")

overview$Country

overview$Age

# Monta a UI
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "Saúde mental" 
  ),
  dashboardSidebar(
    sidebarMenu(
      selectInput("selectedLocalInput", "Local", choices = unique(overview$Country)),
      sliderInput("peopleSlider", "Granularidade da idade", min = min(overview$Age), max = max(overview$Age), value = 32),
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
            subtitle = "Granularidade selecionada",
            width = 4,
            icon = icon("person")
          )
        ),
        box(
          title = "Histograma da idade das pessoas",
          status = "warning",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("healthHist", height = "300")
        ),
      )
    )
  )
)


# Monta o servidor
server <- function(input, output) {
  output$peopleCard <- renderText({
    prettyNum((input$peopleSlider))  
  })
  
  output$healthHist <- renderPlot({
    hist(overview$Age,  breaks = input$peopleSlider, col = 'darkgray', border = 'white', main='Histograma idade dos partipantes de pesquisa', xlab='Idade', ylab='Frequencia')
  })
  
  selectedLocal <- reactive(
    if(input$selectedLocalInput == "NA") {
      subset(overview, is.na(Country))
    } else {
      subset(overview, Country == input$selectedLocalInput)
    }
    #overview[overview$Country == input$selectedLocalInput,]
  )
  output$tableLocal <- renderTable(
    head(selectedLocal()),
    15
  )
}

shinyApp(ui = ui, server = server)
