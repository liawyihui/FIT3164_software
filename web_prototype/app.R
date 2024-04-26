# Load R packages
library(shiny)
library(shinythemes)
library(readxl)
library(ggplot2)


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
      tabPanel("About Model",
               fluidRow(
                 column(12, tags$h3("Model Performance"))
               ),
               fluidRow(
                 column(12, tableOutput("table"))
               ),
               fluidRow(
                 column(4, tags$h3("ROC Curve")),
                 column(4, tags$h3("____")),
                 column(4, tags$h3("____"))
               ),
               fluidRow(
                 column(4, plotOutput("ROC", height = "400px")),
                 column(4, tags$h3("")),
                 column(4, tags$h3(""))
               )
      ) # Navbar 2, tabPanel
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output) {
    
    output$txtout <- renderText({
      paste( input$txt1, input$txt2, sep = " " )
    })

    output$table <- renderTable({
      performance_test <- read_xlsx("performance_test.xlsx")
      
      performance_test
    })
    
    output$ROC <- renderPlot({
      ROC_test <- read_xlsx("ROC_test. xlsx")

      p2 <- ggplot() +
      geom_line(data=ROC_test, aes(x=FPR, y=TPR), color="red") +
      theme(text=element_text(size=16,  face="bold")) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black"))
      
      p2
    })
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
