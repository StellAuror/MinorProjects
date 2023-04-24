
library(tidyverse)
library(pdftools)
library(tidyverse)
library(shiny)
library(bs4Dash)
library(stringr)

body <- 
  bs4DashBody(
    fluidRow(
      column(
        width = 8,
        uiOutput("pdfview")
      ),
      column(
        width = 4,
        box(
          width = 12,
          solidHeader = T,
          status = "gray-dark",
          selectInput(
            inputId = "select_pdf",
            label = "Select the pdf file",
            choices = 
              list.files(path = "www/", pattern = "pdf$") %>%
              sapply(function(x) paste0("www/", x)),
            selected = ""
          )
        )
      )
    )
  )

sidebar <- 
  bs4DashSidebar(
    disable = T
  )

header <- 
  bs4DashNavbar(
    
  )


server <- 
  function(output, input) {
    
 
  output$pdfview <- renderUI({
    tags$iframe(
      style="height:900px; width:100%",
      src = substr(input$select_pdf, 4, 100)
    )
  })

    
  }


shinyApp(
  ui = bs4DashPage(
    header = header,
    sidebar = sidebar,
    body = body
  ),
  server = server
)

