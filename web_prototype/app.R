# Load R packages
library(shiny)
library(shinythemes)


  # Define UI
  ui <- fluidPage(theme = shinytheme("united"),
    navbarPage(
      "Lymphedema Prediction",
      tabPanel("Prediction",
               sidebarPanel(
                 tags$h3("Upload dataset"),
                 fileInput("DataFile", "Upload Excel dataset file to predict Lymphedema:",
                           multiple = FALSE,
                           accept = c(".xls",".xlsx")),  
                 textInput("txt1", "Given Name:", ""), # txt1 will be sent to the server
                 textInput("txt2", "Surname:", ""), # txt2 will be sent to the server
    
               ), # sidebarPanel
               mainPanel(
                 tags$label(h3("Output")),
                 verbatimTextOutput("txtout"), # txtout is generated from the server
                 tableOutput('tabledata') # Prediction results table

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
