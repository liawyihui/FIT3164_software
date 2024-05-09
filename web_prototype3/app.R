# loading necessary libraries
library(shiny)
library(shinythemes)
library(readxl)
library(ggplot2)
library(ebmc)
library(shinydashboard)

# loading trained model
load("final_model.RData")

# define UI
ui <- fluidPage(theme = shinytheme("united"),
  navbarPage(
    "Lymphedema Prediction",
    tabPanel("Prediction",
             sidebarPanel(
               tags$h3("Upload dataset"),
               fileInput("DataFile", "Upload Excel dataset file to predict Lymphedema:",
                         multiple = FALSE,
                         accept = c(".xls",".xlsx", ".csv")),
               tags$h5("Format accepted: .xls, .xlsx, .csv"),
               tags$strong("Template of dataset:"),
               div(downloadButton("DownloadData", "Download"), style = "margin-bottom: 20px;"),
             ), # sidebarPanel
             mainPanel(
               tags$label(h3("Output")),
               verbatimTextOutput("txtout"), # txtout is generated from the server
               tableOutput('tabledata'), # Prediction results table
               p(strong("All patients result"),
                 style="font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"),
               p(strong("Lymphedema predicted score of the selected Patient ID:"),
                 style="text-align:left; color:black; padding:0px; border-radius:0px"),
               tableOutput("pred.single"),
               DT::dataTableOutput("pred.lymphedema")
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

# define server function  
server <- function(input, output) {
  
  # uploading dataset
  datasetInput <- reactive({
    inFile <- input$DataFile
    DataTable <- read.csv(inFile$datapath)
    return(DataTable)
  })
  
  # table of input dataset
  output$DataTable <- renderTable({
    datasetInput()
  })
  
  # downloadable csv template of selected dataset
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste("DataTemplate", "csv", sep=".")
    },
    content = function(file) {
      file.copy("DataTemplate.csv", file)
    },
    contentType = "ExcelFile"
  )  
  
  # predicting lymphedema for the user-input dataset
  output$pred.lymphedema <- DT::renderDataTable({
    validate(need(input$DataFile, "Missing data file!"))
    
    inFile <- input$DataFile
    DataTable <- read.csv(inFile$datapath)

    Table1 <- DataTable
    # Exclude the Endpoint variable before normalizing
    independent_variables <- setdiff(names(Table1), c("Endpoint", "ID"))
    Table1[, independent_variables] <- scale(Table1[, independent_variables])

    Pred.prob <- predict(model, Table1, type = "prob")
    OutputTable <- data.frame(Patient.ID = as.factor(DataTable$ID),
                              Predicted.Score = round(Pred.prob, 3),
                              Predicted.Lymphedema = ifelse(Pred.prob>0.5, "Yes", "No"))

  }, selection = "single")
  
  # predicting lymphedema for the selected patient
  output$pred.single <- renderValueBox({
    validate(need(input$DataFile, "Missing data file!"))
    
    inFile <- input$DataFile
    DataTable <- read.csv(inFile$datapath)
    DataTable$Patient.ID <- as.character(DataTable$ID)

    Table1 <- DataTable
    # Exclude the Endpoint variable before normalizing
    independent_variables <- setdiff(names(Table1), c("Patient.ID", "ID", "Endpoint"))
    Table1[, independent_variables] <- scale(Table1[, independent_variables])

    Pred.prob <- predict(model, Table1, type = "prob")
    Pred.Table <- data.frame(Patient.ID = as.factor(DataTable$ID),
                             Predicted.Score = round(Pred.prob, 3),
                             Predicted.Lymphedema = ifelse(Pred.prob > 0.5, "Yes", "No"))

    RowIndex <- input$pred.lymphedema_rows_selected
    Score <- ifelse(is.na(RowIndex),"NA", Pred.Table$Predicted.Score[RowIndex])
    DisplayScore <- paste("Score =", ifelse(is.numeric(RowIndex), Score, "NA"), sep = " ")
    
    color_lymph <- ifelse(!is.numeric(Score), "blue", 
                          ifelse(Score > 0.5, "red", "green")) 
    
    subtitle_lymph <- ifelse(!is.numeric(Score), "Patient ID not selected", 
                             ifelse(Score <= 0.5, paste("Patient ID ", Pred.Table$Patient.ID[RowIndex], " might be NEGATIVE to Lymphedema", sep = ""), 
                                    paste("Patient ID ", Pred.Table$Patient.ID[RowIndex], " might be POSITIVE to Lymphedema", sep = "")))
    
    valueBox(DisplayScore, subtitle_lymph, color = color_lymph, width = 3)
  })
    
  # output section
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
