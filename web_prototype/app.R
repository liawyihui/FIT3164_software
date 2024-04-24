# Load R packages
library(shiny)
library(shinythemes)


  # Define UI
  ui <- fluidPage(theme = shinytheme("united"),
    navbarPage(
      "Lymphedema Prediction",
      tabPanel("Prediction",
               sidebarPanel(
                 tags$h3("Input:"),
                 textInput("txt1", "Given Name:", ""), # txt1 will be sent to the server
                 textInput("txt2", "Surname:", ""), # txt2 will be sent to the server
                 
               ), # sidebarPanel
               mainPanel(
                            h1("Header 1"),
                            
                            h4("Output 1"),
                            verbatimTextOutput("txtout"), # txtout is generated from the server

               ) # mainPanel
               
      ), # Navbar 1, tabPanel
      tabPanel("About Model", "This panel is intentionally left blank")
  
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
