# loading necessary libraries
library(shiny)
library(shinythemes)
library(readxl)
library(ggplot2)

# loading trained model
load("LR_MDS02.RData")

# define UI
ui <- fluidPage(
  theme = shinytheme("united"),
  tags$style(
    HTML("
           .container-fluid {
              padding-left: 0 !important;
              padding-right: 0 !important;
            }
           .tab-content {
              padding: 15px;
            }
           .navbar-brand{
              padding-left: 25px;
            }
           ")
  ),
  navbarPage(
    "Lymphedema Prediction",
    tabPanel(
      "Prediction",
      fluidRow(
        sidebarPanel(
          tags$h3("Upload dataset"),
          fileInput("DataFile", "Upload Excel dataset file to predict Lymphedema:",
            multiple = FALSE,
            accept = c(".xls", ".xlsx", ".csv")
          ),
          tags$h5("Format accepted: .xls, .xlsx, .csv"),
          tags$strong("Template of dataset:"),
          div(downloadButton("DownloadData", "Download"), style = "margin-bottom: 20px;"),
        ), # sidebarPanel
        mainPanel(
          tags$label(h3("Output")),
          verbatimTextOutput("txtout"), # txtout is generated from the server
          tableOutput("tabledata") # Prediction results table
        )
      ), # mainPanel
      selectInput(
        inputId = "feature", "Select feature for visualization:",
        c(
          "--Select--" = "select",
          "Age" = "age",
          "Gender" = "sex",
          "Number of Lymph Node Harvested" = "lnn",
          "Taxane-based Chemotherapy" = "tax",
          "Radiation Fraction" = "fx",
          "Amount of Radiation (Gray)" = "Gy",
          "Breast Reconstruction" = "recon",
          "Chemotherapy" = "che",
          "Axilla Radiation Therapy" = "axi",
          "Platelets" = "PLT",
          "Procalcitonin" = "PCT",
          "White Blood Cells" = "WBC",
          "Absolute Neutrophil Count" = "ANC",
          "Red Blood Cell" = "RBC",
          "Mean Platelet Volume" = "MPV",
          "Eosinophil" = "Eosinophil",
          "Basophil" = "Basophil",
          "Monocyte" = "Monocyte",
          "Hematocrit" = "Hct",
          "Segmented Neutrophil" = "Segmented.neutrophil",
          "Mean Corpuscular Hemoglobin Concentration" = "MCHC",
          "Hemoglobin" = "Hb",
          "Lymphocyte" = "Lymphocyte",
          "Mean Corpuscular Volume" = "MCV",
          "Mean Corpuscular Hemoglobin" = "MCH",
          "Potassium Serum" = "Potassium.serum",
          "Chloride Serum" = "Chloride.serum",
          "Sodium Serum" = "Sodium.serum"
        )
      ),
      plotOutput("FeatureDistribution") # Plot histogram
    ), # Navbar 1, tabPanel
    tabPanel(
      "About Model",
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
    DataTable <- read.csv(inFile$datapath, sheet = 1)
    return(DataTable)
  })

  # table of input dataset
  output$DataTable <- renderTable({
    datasetInput()
  })

  # downloadable csv template of selected dataset
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste("DataTemplate", "csv", sep = ".")
    },
    content = function(file) {
      file.copy("DataTemplate.csv", file)
    },
    contentType = "ExcelFile"
  )

  # Visualization of feature
  output$FeatureDistribution <- renderPlot({
    validate(need(input$DataFile, "Please upload data file."))
    validate(need(input$feature != "select", "Please select a feature."))

    # Read the uploaded dataset
    inFile <- input$DataFile
    file_ext <- tools::file_ext(inFile$name)

    if (file_ext %in% c("xlsx", "xls")) {
      DataTable <- read_excel(inFile$datapath)
    } else if (file_ext == "csv") {
      DataTable <- read.csv(inFile$datapath)
    } else {
      stop("Unsupported file format.")
    }

    feature_names <- c(
      "age" = "Age",
      "sex" = "Gender",
      "lnn" = "Number of Lymph Node Harvested",
      "tax" = "Taxane-based Chemotherapy",
      "fx" = "Radiation Fraction",
      "Gy" = "Amount of Radiation (Gray)",
      "recon" = "Breast Reconstruction",
      "che" = "Chemotherapy",
      "axi" = "Axilla Radiation Therapy",
      "PLT" = "Platelets",
      "PCT" = "Procalcitonin",
      "WBC" = "White Blood Cells",
      "ANC" = "Absolute Neutrophil Count",
      "RBC" = "Red Blood Cell",
      "MPV" = "Mean Platelet Volume",
      "Eosinophil" = "Eosinophil",
      "Basophil" = "Basophil",
      "Monocyte" = "Monocyte",
      "Hct" = "Hematocrit",
      "Segmented.neutrophil" = "Segmented Neutrophil",
      "MCHC" = "Mean Corpuscular Hemoglobin Concentration",
      "Hb" = "Hemoglobin",
      "Lymphocyte" = "Lymphocyte",
      "MCV" = "Mean Corpuscular Volume",
      "MCH" = "Mean Corpuscular Hemoglobin",
      "Potassium.serum" = "Potassium Serum",
      "Chloride.serum" = "Chloride Serum",
      "Sodium.serum" = "Sodium Serum"
    )

    if (input$feature == "sex" || input$feature == "recon" || input$feature == "che" || input$feature == "axi") {
      if (input$feature == "sex") {
        DataTable$sex <- ifelse(DataTable$sex == 1, "Male", "Female")
        custom_colour <- c("Male" = "#2d50ff", "Female" = "#ffa0d4")
        plots <- ggplot(DataTable, aes(x = "", fill = sex))
      } else if (input$feature == "che") {
        DataTable$che <- ifelse(DataTable$che == 1, "Yes", "No")
        custom_colour <- c("Yes" = "#bd87ff", "No" = "#ffa0a0")
        plots <- ggplot(DataTable, aes(x = "", fill = che))
      } else if (input$feature == "axi") {
        DataTable$axi <- ifelse(DataTable$axi == 1, "Yes", "No")
        custom_colour <- c("Yes" = "#bd87ff", "No" = "#ffa0a0")
        plots <- ggplot(DataTable, aes(x = "", fill = axi))
      }

      plots <- plots + geom_bar(width = 1, color = "black") +
        coord_polar(theta = "y") +
        labs(
          title = paste("Distribution of ", feature_names[input$feature], sep = ""),
          fill = feature_names[input$feature]
        ) +
        theme_void() +
        scale_fill_manual(values = custom_colour) +
        theme(text = element_text(size = 16, face = "bold"), plot.title = element_text(hjust = 0.5)) +
        geom_text(aes(label = paste0(format(round((..count..) / sum(..count..) * 100, 2), nsmall = 2), "%")), stat = "count", position = position_stack(vjust = 0.5), size = 5)
    } else {
      plots <- ggplot(DataTable, aes_string(x = input$feature)) +
        geom_histogram(color = "#000000", fill = "#6d85ff") +
        labs(
          title = paste("Histogram of ", feature_names[input$feature], sep = ""),
          x = feature_names[input$feature],
          y = "Number of People"
        ) +
        theme(text = element_text(size = 16, face = "bold"), plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black"))
    }

    return(plots)
  })

  # predicting lymphedema for the selected patient
  # to be added

  # output section
  output$txtout <- renderText({
    paste(input$txt1, input$txt2, sep = " ")
  })

  output$table <- renderTable({
    performance_test <- read_xlsx("performance_test.xlsx")

    performance_test$Value <- format(performance_test$Value, digits = 4)

    performance_test
  })

  output$ROC <- renderPlot({
    ROC_test <- read_xlsx("ROC_test.xlsx")

    p2 <- ggplot() +
      geom_line(data = ROC_test, aes(x = FPR, y = TPR), color = "red") +
      theme(text = element_text(size = 16, face = "bold")) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black"))

    p2
  })
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
