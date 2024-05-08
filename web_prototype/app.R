# loading necessary libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(ebmc)
library(openxlsx)

# loading trained model
load("final_model.RData")

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
              padding-top: 0px; !important
            }
           .navbar-brand{
              padding-left: 25px;
            }
           ")
  ),
  navbarPage(
    "Lymphedema Prediction",
    tabsetPanel(
      id = "switcher",
      tabPanel(
        "Home",
        fluidRow(
          column(12, div(style = "height:50px;", tags$h2("About Lymphedema")))
        ),
        fluidRow(
          column(
            6, p(
              style = "font-size: 17px; text-align: justify",
              "Lymphedema is a common condition that affects many breast cancer survivors.
                                Lymphedema in breast cancer patients is swelling that occurs typically in one of the
                                arms, often as a result of cancer treatment such as surgery or radiation therapy
                                which disrupts the drainage of lymph fluid by damaging or removing lymph nodes."
            ),
            p(
              style = "font-size: 17px; text-align: justify",
              "While there is currently no known cure for lymphedema, various treatments and
                                strategies can help reduce symptoms, manage swelling, and improve quality of life for
                                patients. These may include compression therapy, physical therapy, exercise, skin care,
                                and in some cases, surgery. Early detection can also play a crucial role in
                                preventing the progression of lymphedema and minimizing its impact on
                                patients' lives."
            )
          ),
          column(3, imageOutput("lymphedema_img")),
          column(3, imageOutput("lymphedema2_img"))
        ),
        fluidRow(
          column(12, div(style = "height:55px;", tags$h2("3 Steps to Use Our Tool")))
        ),
        fluidRow(
          column(1, imageOutput("number1_img", height = "240px")),
          column(
            3, tags$h2("Input your Dataset"),
            p(
              style = "font-size: 17px; text-align: justify",
              "Download the dataset template and fill in all necessary details for each patient.
                                 Ensure completeness and accuracy of the data."
            )
          ),
          column(1, imageOutput("number2_img", height = "240px")),
          column(
            3, tags$h2("Lymphedema Assessment"),
            p(
              style = "font-size: 17px; text-align: justify",
              "We evaluate the risk of lymphedema for each patient based on the dataset provided."
            )
          ),
          column(1, imageOutput("number3_img", height = "240px")),
          column(
            3, tags$h2("Your Results"),
            p(
              style = "font-size: 17px; text-align: justify",
              "Results of the lymphedema risk assessment will be generated for each patient.
                                 These results will include individual risk scores or probabilities."
            )
          )
        ),
        fluidRow(
          column(12, div(style = "text-align: center", actionButton("start_assess", "Start Assessment", style = "font-size: 17px; background-color: #337ab7; border-color: #2e6da4; width: 200px")))
        ),
        fluidRow(
          column(12, div(style = "height:110px;", ""))
        )
      ), # Navbar 1, tabPanel
      tabPanel(
        "Prediction",
        fluidRow(
          sidebarPanel(
            tags$h3("Upload dataset"),
            fileInput("DataFile", "Upload dataset file to predict Lymphedema:",
              multiple = FALSE,
              accept = c(".xls", ".xlsx", ".csv")
            ),
            tags$h5("Format accepted: .xls, .xlsx, .csv"),
            tags$strong("Template of dataset:"),
            div(downloadButton("DownloadData", "Download"), style = "margin-bottom: 20px;"),
            tags$strong("Sample dataset:"),
            div(downloadButton("SampleDataset", "Download"), style = "margin-bottom: 20px;")
          ), # sidebarPanel
          mainPanel(
            verbatimTextOutput("txtout"), # txtout is generated from the server
            tableOutput("tabledata"), # Prediction results table
            p(strong("All Patients' Result"),
              style = "font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"
            ),
            p(strong("Lymphedema predicted probability of the selected Patient ID:"),
              style = "text-align:left; color:black; padding:0px; border-radius:0px"
            ),
            tableOutput("pred.single"),
            uiOutput("downloadResultsButtonOutput"),
            DT::dataTableOutput("pred.lymphedema")
          )
        ), # mainPanel
        fluidRow(
          tabsetPanel(
            tabPanel(
              "Data Visualization",
              selectInput(
                inputId = "feature", label = "Select feature for uploaded data visualization:",
                choices = c(
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
              plotOutput("FeatureDistribution")
            ),
            tabPanel(
              "Prediction Results Visualization",
              selectInput(
                inputId = "result_feature", label = "Select feature for results visualization:",
                choices = c(
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
              plotOutput("prediction_plot")
            )
          ),
          style = "margin-top: 30px;"
        ), # Plot histogram
        fluidRow(
          column(12, div(style = "height:110px;", ""))
        )
      ), # Navbar 2, tabPanel
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
          column(4, tags$h3("Relative Variable Importance")),
          column(4, tags$h3("____"))
        ),
        fluidRow(
          column(4, plotOutput("ROC", height = "450px")),
          column(4, plotOutput("variable_impt", height = "450px")),
          column(4, tags$h3(""))
        ),
        fluidRow(
          column(12, div(style = "height:110px;", ""))
        )
      ) # Navbar 3, tabPanel
    ) # tabsetPanel
  ), # navbarPage
  tags$footer(
    style = "background-color: #CCCCCC; padding: 10px; text-align: center;",
    p(
      style = "font-size: 16.5px; text-align: center; font-style: italic; font-family: Arial; font-weight: bold;",
      "Monash University Malaysia"
    ),
    p(
      style = "font-size: 16px; text-align: center;",
      HTML(
        paste(
          "<b>Collaborators:</b> Liaw Yi Hui (32023707), Pang Eason (32024584), Chan Jia Xin (31859089)",
          "<b>Supervisor:</b> Dr. Ong Huey Fang",
          "Built with R, Shiny & Shiny Server",
          "13th May, 2024",
          sep = "<br>"
        )
      )
    )
  )
) # fluidPage

# define server function
server <- function(input, output) {
  prediction_results <- reactiveValues(data = NULL)

  output$lymphedema_img <- renderImage(
    {
      list(
        src = "img/Lymphedema-early-detection.jpg",
        width = "100%",
        height = 370
      )
    },
    deleteFile = FALSE
  )

  output$lymphedema2_img <- renderImage(
    {
      list(
        src = "img/10434_2014_3518_Fig1_HTML.jpg",
        width = "100%",
        height = 370
      )
    },
    deleteFile = FALSE
  )

  output$number1_img <- renderImage(
    {
      list(
        src = "img/number1.png",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$number2_img <- renderImage(
    {
      list(
        src = "img/number2.png",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  output$number3_img <- renderImage(
    {
      list(
        src = "img/number3.png",
        width = "100%"
      )
    },
    deleteFile = FALSE
  )

  observeEvent(input$start_assess, {
    updateTabsetPanel(inputId = "switcher", selected = "Prediction")
  })

  # uploading dataset
  datasetInput <- reactive({
    inFile <- input$DataFile
    file_ext <- tools::file_ext(inFile$name)

    if (file_ext %in% c("xlsx", "xls")) {
      DataTable <- read_excel(inFile$datapath, sheet = "DataTemplate")
    } else if (file_ext == "csv") {
      DataTable <- read.csv(inFile$datapath)
    } else {
      stop("Unsupported file format.")
    }

    return(DataTable)
  })

  # table of input dataset
  output$DataTable <- renderTable({
    datasetInput()
  })

  # downloadable xlsx template of dataset
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste("DataTemplate", "xlsx", sep = ".")
    },
    content = function(file) {
      file.copy("DataTemplate.xlsx", file)
    },
    contentType = "ExcelFile"
  )

  # downloadable xlsx sample dataset
  output$SampleDataset <- downloadHandler(
    filename = function() {
      paste("SampleDataset", "xlsx", sep = ".")
    },
    content = function(file) {
      file.copy("SampleDataset.xlsx", file)
    },
    contentType = "ExcelFile"
  )

  # predicting lymphedema for the user-input dataset
  output$pred.lymphedema <- DT::renderDataTable(
    {
      validate(need(input$DataFile, "Missing data file!"))

      inFile <- input$DataFile
      file_ext <- tools::file_ext(inFile$name)

      if (file_ext %in% c("xlsx", "xls")) {
        DataTable <- read_excel(inFile$datapath, sheet = "DataTemplate")
      } else if (file_ext == "csv") {
        DataTable <- read.csv(inFile$datapath)
      } else {
        stop("Unsupported file format.")
      }

      Normalized_DataTable <- DataTable
      # Exclude the Endpoint variable before normalizing
      independent_variables <- setdiff(names(Normalized_DataTable), c("Endpoint", "ID"))
      Normalized_DataTable[, independent_variables] <- scale(Normalized_DataTable[, independent_variables])

      Pred.prob <- predict(model, Normalized_DataTable, type = "prob")

      results_plot <- data.frame(
        DataTable,
        Predicted.Probability = round(Pred.prob, 3),
        Predicted.Lymphedema = ifelse(Pred.prob > 0.5, "Yes", "No")
      )
      prediction_results$data <- results_plot

      OutputTable <- data.frame(
        Patient.ID = as.factor(DataTable$ID),
        Predicted.Probability = round(Pred.prob, 3),
        Predicted.Lymphedema = ifelse(Pred.prob > 0.5, "Yes", "No")
      )

      colnames(OutputTable) <- c("Patient ID", "Predicted Probability", "PredictedLymphedema")

      OutputTable
    },
    selection = "single"
  )

  # predicting lymphedema for the selected patient
  output$pred.single <- renderValueBox({
    validate(need(input$DataFile, "Missing data file!"))

    inFile <- input$DataFile
    file_ext <- tools::file_ext(inFile$name)

    if (file_ext %in% c("xlsx", "xls")) {
      DataTable <- read_excel(inFile$datapath, sheet = "DataTemplate")
    } else if (file_ext == "csv") {
      DataTable <- read.csv(inFile$datapath)
    } else {
      stop("Unsupported file format.")
    }

    DataTable$Patient.ID <- as.character(DataTable$ID)

    Normalized_DataTable <- DataTable
    # Exclude the Endpoint variable before normalizing
    independent_variables <- setdiff(names(Normalized_DataTable), c("Patient.ID", "ID", "Endpoint"))
    Normalized_DataTable[, independent_variables] <- scale(Normalized_DataTable[, independent_variables])

    Pred.prob <- predict(model, Normalized_DataTable, type = "prob")
    Pred.Table <- data.frame(
      Patient.ID = as.factor(DataTable$ID),
      Predicted.Probability = round(Pred.prob, 3),
      Predicted.Lymphedema = ifelse(Pred.prob > 0.5, "Yes", "No")
    )

    Pred.Table2 <- data.frame(
      Patient.ID = as.factor(DataTable$ID),
      Predicted.Probability = round(Pred.prob, 3),
      Predicted.Lymphedema = ifelse(Pred.prob > 0.5, "Yes", "No"),
      Patient.Age = round(DataTable$age, 3),
      Patient.Sex = ifelse(DataTable$sex == 2, "Female", "Male")
    )

    RowIndex <- input$pred.lymphedema_rows_selected
    ID <- ifelse(is.na(RowIndex), "NA", Pred.Table$Patient.ID[RowIndex])
    DisplayID <- paste("", ifelse(is.numeric(RowIndex), ID, "N/A"), sep = " ")
    Age <- ifelse(is.na(RowIndex), "NA", Pred.Table2$Patient.Age[RowIndex])
    DisplayAge <- paste("", ifelse(is.numeric(RowIndex), Age, "N/A"), sep = " ")
    Sex <- ifelse(is.na(RowIndex), "NA", Pred.Table2$Patient.Sex[RowIndex])
    DisplaySex <- paste("", ifelse(is.numeric(RowIndex), Sex, "N/A"), sep = " ")
    Probability <- ifelse(is.na(RowIndex), "NA", Pred.Table$Predicted.Probability[RowIndex])
    DisplayProbability <- paste("", ifelse(is.numeric(RowIndex), Probability, "N/A"), sep = " ")
    YesNo <- ifelse(is.na(RowIndex), "NA", Pred.Table$Predicted.Lymphedema[RowIndex])
    DisplayYesNo <- paste("", ifelse(is.numeric(RowIndex), YesNo, "N/A"), sep = " ")

    color_lymph <- ifelse(!is.numeric(Probability), "blue",
      ifelse(Probability > 0.5, "red", "green")
    )

    subtitle_lymph <- ifelse(!is.numeric(Probability), "Patient ID not selected",
      ifelse(Probability <= 0.5, paste("Patient ID ", Pred.Table$Patient.ID[RowIndex], " has LOW risk of Lymphedema", sep = ""),
        paste("Patient ID ", Pred.Table$Patient.ID[RowIndex], " has HIGH risk of Lymphedema", sep = "")
      )
    )

    # Concatenate all values into a single string
    all_values <- paste(
      paste("<b style='font-size: 18px;'>Patient ID:</b>", "<span style='font-size: 18px;'>", DisplayID, "</span>", sep = ""),
      paste("<b style='font-size: 18px;'>Age:</b>", "<span style='font-size: 18px;'>", DisplayAge, "</span>", sep = ""),
      paste("<b style='font-size: 18px;'>Sex:</b>", "<span style='font-size: 18px;'>", DisplaySex, "</span>", sep = ""),
      paste("<b style='font-size: 18px;'>Probability of having Lymphedema:</b>", "<span style='font-size: 18px;'>", DisplayProbability, "</span>", sep = ""),
      paste("<b style='font-size: 18px;'>Prediction Outcome:</b>", "<span style='font-size: 18px;'>", DisplayYesNo, "</span>", sep = ""),
      "",
      "",
      sep = "<br>"
    )

    # Use HTML function to format text with line breaks
    valueBox(HTML(all_values), subtitle_lymph, color = color_lymph, width = 3)
  })

  output$downloadResult <- downloadHandler(
    filename = function() {
      paste("prediction_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(prediction_results$data, file, rowNames = FALSE)
    }
  )

  output$downloadResultsButtonOutput <- renderUI({
    if (!is.null(prediction_results$data)) {
      div(downloadButton("downloadResult", "Download Results"), style = "margin-bottom: 20px;")
    } else {
      NULL
    }
  })

  # Visualization based on prediction probabilities
  output$prediction_plot <- renderPlot(
    {
      validate(need(input$DataFile, "Please upload data file."))
      validate(need(input$result_feature != "select", "Please select a feature."))

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

      validate(need(input$result_feature %in% colnames(prediction_results$data), "Please ensure the selected feature is in the uploaded file."))

      converted_prediction_data <- prediction_results$data

      # Plotting
      if (input$result_feature == "sex") {
        converted_prediction_data$sex <- ifelse(converted_prediction_data$sex == 1, "Male", "Female")
      } else if (input$result_feature == "recon") {
        converted_prediction_data$recon <- ifelse(converted_prediction_data$recon == 0, "No Reconstruction", ifelse(converted_prediction_data$recon == 1, "TRAM flat", "Implant"))
      } else if (input$result_feature == "che") {
        converted_prediction_data$che <- ifelse(converted_prediction_data$che == 1, "Yes", "No")
      } else if (input$result_feature == "axi") {
        converted_prediction_data$axi <- ifelse(converted_prediction_data$axi == 1, "Yes", "No")
      } else if (input$result_feature == "tax") {
        converted_prediction_data$tax <- ifelse(converted_prediction_data$tax == 0, "No taxane", ifelse(converted_prediction_data$tax == 1, "Type 1", "Type 2"))
      }

      ggplot(converted_prediction_data, aes_string(x = input$result_feature, y = "Predicted.Probability")) +
        geom_point(color = "blue") +
        geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", linewidth = 1.2) +
        labs(
          title = paste("Prediction Probability vs ", feature_names[input$result_feature], sep = ""),
          x = feature_names[input$result_feature],
          y = "Prediction Probability"
        ) +
        theme(text = element_text(size = 16, face = "bold"), plot.title = element_text(hjust = 0.5)) +
        theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black", face = "plain")) +
        theme(axis.text.y = element_text(face = "plain"))
    },
    height = 500
  )

  # Visualization of feature
  output$FeatureDistribution <- renderPlot(
    {
      validate(need(input$DataFile, "Please upload data file."))
      validate(need(input$feature != "select", "Please select a feature."))

      # Read the uploaded dataset
      inFile <- input$DataFile
      file_ext <- tools::file_ext(inFile$name)

      if (file_ext %in% c("xlsx", "xls")) {
        DataTable <- read_excel(inFile$datapath, sheet = "DataTemplate")
      } else if (file_ext == "csv") {
        DataTable <- read.csv(inFile$datapath)
      } else {
        stop("Unsupported file format.")
      }

      validate(need(input$feature %in% colnames(DataTable), "Please ensure the selected feature is in the uploaded file."))

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

      if (input$feature == "sex" || input$feature == "recon" || input$feature == "che" || input$feature == "axi" || input$feature == "tax") {
        if (input$feature == "sex") {
          DataTable$sex <- ifelse(DataTable$sex == 1, "Male", "Female")
          custom_colour <- c("Male" = "#2d50ff", "Female" = "#ffa0d4")
          plots <- ggplot(DataTable, aes(x = "", fill = sex))
        } else if (input$feature == "recon") {
          DataTable$recon <- ifelse(DataTable$recon == 0, "No Reconstruction", ifelse(DataTable$recon == 1, "TRAM flat", "Implant"))
          custom_colour <- c("No Reconstruction" = "#bd87ff", "TRAM flat" = "#ffa0a0", "Implant" = "#deff09")
          plots <- ggplot(DataTable, aes(x = "", fill = recon))
        } else if (input$feature == "che") {
          DataTable$che <- ifelse(DataTable$che == 1, "Yes", "No")
          custom_colour <- c("Yes" = "#bd87ff", "No" = "#ffa0a0")
          plots <- ggplot(DataTable, aes(x = "", fill = che))
        } else if (input$feature == "axi") {
          DataTable$axi <- ifelse(DataTable$axi == 1, "Yes", "No")
          custom_colour <- c("Yes" = "#bd87ff", "No" = "#ffa0a0")
          plots <- ggplot(DataTable, aes(x = "", fill = axi))
        } else if (input$feature == "tax") {
          DataTable$tax <- ifelse(DataTable$tax == 0, "No taxane", ifelse(DataTable$tax == 1, "Type 1", "Type 2"))
          custom_colour <- c("No taxane" = "#bd87ff", "Type 1" = "#ffa0a0", "Type 2" = "#deff09")
          plots <- ggplot(DataTable, aes(x = "", fill = tax))
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
          geom_text(aes(label = paste0(format(round((..count..) / sum(..count..) * 100, 2), nsmall = 2), "%")), stat = "count", position = position_stack(vjust = 0.5), size = 4.5)
      } else {
        plots <- ggplot(DataTable, aes_string(x = input$feature)) +
          geom_histogram(color = "#000000", fill = "#6d85ff") +
          labs(
            title = paste("Histogram of ", feature_names[input$feature], sep = ""),
            x = feature_names[input$feature],
            y = "Number of People"
          ) +
          theme(text = element_text(size = 16, face = "bold"), plot.title = element_text(hjust = 0.5)) +
          theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black", face = "plain")) +
          theme(axis.text.y = element_text(face = "plain"))
      }

      return(plots)
    },
    height = 500
  )

  # output section
  output$txtout <- renderText({
    paste(input$txt1, input$txt2, sep = " ")
  })

  output$table <- renderTable({
    performance_test <- read_xlsx("performance_test.xlsx")

    performance_test$Value <- format(performance_test$Value, digits = 4)

    performance_test
  })

  output$variable_impt <- renderPlot({
    variable_impt <- read_xlsx("variance_importance.xlsx")

    p1 <- ggplot(variable_impt, aes(x = reorder(variable, +score), y = score)) +
      geom_bar(stat = "identity", fill = "skyblue2") +
      geom_text(aes(label = round(score, 1)), position = position_stack(vjust = 0.5), size = 3, colour = "black") +
      labs(x = "", y = "Relative Importance") +
      theme(
        text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1, colour = "black")
      ) +
      coord_flip() # Horizontal bar plot

    p1
  })

  output$ROC <- renderPlot({
    ROC_test <- read_xlsx("ROC_test.xlsx")

    p2 <- ggplot() +
      geom_line(data = ROC_test, aes(x = FPR, y = TPR), color = "blue") +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = 2) +
      theme(text = element_text(size = 16, face = "bold")) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black"))

    p2
  })
} # server

# Create Shiny object
shinyApp(ui = ui, server = server)
