# loading required libraries
library(shinythemes)
library(shinydashboard)
library(shiny)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Lymphedema Prediction", titleWidth = 300),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(
                        menuItem("Prediction", tabName = "ModelInput", icon = icon("desktop")),              # tab 1
                        menuItem("About model", tabName = "ModelInfo", icon = icon("fas fa-chart-bar"))       # tab 2
                        
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        
                        # Begin of tab 1
                        tabItem(tabName = "ModelInput",
                                
                                # Uploading data
                                fluidRow(
                                  
                                  # 1st column for Uploading data 
                                  box(
                                    # Title: Uploading data
                                    p(strong("Uploading data"),
                                      style="font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"
                                    ),
                                    
                                    # Upload excel file of dataset
                                    fileInput("DataFile", "Upload Excel dataset file that you want to predict Lymphedema:",
                                              multiple = FALSE,
                                              accept = c(".xls",".xlsx")),  
                                    
                                    
                                    # Select parameter for histogram visualization
                                    selectInput(inputId = "Xvar", "Choose parameter for visualization:",
                                                c("Age" = "age",
                                                  "Gender" = "sex",
                                                  "Number of lymph node harvested" = "lnn",
                                                  "Taxane-based chemotherapy" = "tax",
                                                  "Radiation fraction" = "fx",
                                                  "Amount of radiation (Gray)" = "Gy",
                                                  "Breast reconstruction" = "recon",
                                                  "Chemotherapy" = "che",
                                                  "Axilla radiation therapy" = "axi",
                                                  "Platelets" = "PLT",
                                                  "Procalcitonin" = "PCT",
                                                  "White blood cells" = "WBC",
                                                  "Absolute neutrophil count" = "ANC",
                                                  "Red blood cell" = "RBC",
                                                  "Mean platelet volume" = "MPV",
                                                  "Eosinophil" = "Eosinophil",
                                                  "Basophil" = "Basophil",
                                                  "Monocyte" = "Monocyte",
                                                  "Hematocrit" = "Hct",
                                                  "Segmented neutrophil" = "Segmented.neutrophil",
                                                  "Mean corpuscular hemoglobin concentration" = "MCHC",
                                                  "Hemoglobin" = "Hb",
                                                  "Lymphocyte" = "Lymphocyte",
                                                  "Mean corpuscular volume" = "MCV",
                                                  "Mean corpuscular hemoglobin" = "MCH",
                                                  "Potassium serum" = "Potassium.serum",
                                                  "Chloride serum" = "Chloride.serum",
                                                  "Sodium serum" = "Sodium.serum")
                                                
                                    ), 
                                    # Plot histogram
                                    plotOutput("DistributionXvar"),
                                    
                                    
                                    
                                    # Download dataset template
                                    p(strong("Template of dataset:"),
                                      style="text-align:justify; color:black; padding:0px; border-radius:0px"
                                    ),
                                    # Download Button
                                    downloadButton("DownloadData", "Download"),
                                    
                                    width=5), 
                                  # End of 1st column
                                  
                                  
                                  # 2nd column for Predicted results
                                  box(
                                    # Title
                                    p(strong("All patients result"),
                                      style="font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"),
                                    
                                    p(strong("Lymphedema predicted score of the selected Patient ID:"),
                                      style="text-align:left; color:black; padding:0px; border-radius:0px"
                                    ),
                                    
                                    tableOutput("PatientPredict"),
                                    
                                    DT::dataTableOutput("Pred.Lymphedema"),
                                    
                                    width=5),
                                  # End of 2nd column
                                  
                                ),
                                
                        ), 
                        # End of tab 1
                        
                        
                        
                        # Begin of tab 2
                        
                        # Predicted results
                        tabItem(tabName = "ModelInfo",
                                
                                fluidRow(
                                  
                                  # Summary of model
                                  column(
                                    width = 12,
                                    
                                    p(strong("Model information"),
                                      style="font-size:24px; text-align:justify; color:black; background-color:papayawhip; padding:15px; border-radius:10px"
                                    ),
                                  ),
                                  
                                  column(
                                    width = 12,
                                    
                                    p(
                                      strong("Summary of model:"), "This machine learning model predicts lymphedema disease by blood test and therapy data. 
                                             When users input data by uploading the data of patients, the model will give score and prediction for patients based on their blood test and therapy data.
                                            A score over 0.25 would suggest that the patient might be positive to lymphedema. 
                                             The model is based on random forest algorithm and was built by using package \"caret\" in R.",
                                      
                                      style="font-size:16px;text-align:justify; color:black; background-color:white; padding:15px;border-radius:10px")
                                  ),
                                  
                                  
                                  box(
                                    p(strong("Relative importance of parameters to models:"),
                                      style="text-align:justify; color:black; background-color:white"),
                                    
                                    plotOutput("Descriptor.Importance"),
                                    
                                    width=4, height = 500),
                                  
                                  box(
                                    
                                    
                                    selectInput(inputId = "Xvar.t.test", "Choose parameter for visualization:",
                                                c("Age" = "age",
                                                  "Gender" = "sex",
                                                  "Number of lymph node harvested" = "lnn",
                                                  "Taxane-based chemotherapy" = "tax",
                                                  "Radiation fraction" = "fx",
                                                  "Amount of radiation (Gray)" = "Gy",
                                                  "Breast reconstruction" = "recon",
                                                  "Chemotherapy" = "che",
                                                  "Axilla radiation therapy" = "axi",
                                                  "Platelets" = "PLT",
                                                  "Procalcitonin" = "PCT",
                                                  "White blood cells" = "WBC",
                                                  "Absolute neutrophil count" = "ANC",
                                                  "Red blood cell" = "RBC",
                                                  "Mean platelet volume" = "MPV",
                                                  "Eosinophil" = "Eosinophil",
                                                  "Basophil" = "Basophil",
                                                  "Monocyte" = "Monocyte",
                                                  "Hematocrit" = "Hct",
                                                  "Segmented neutrophil" = "Segmented.neutrophil",
                                                  "Mean corpuscular hemoglobin concentration" = "MCHC",
                                                  "Hemoglobin" = "Hb",
                                                  "Lymphocyte" = "Lymphocyte",
                                                  "Mean corpuscular volume" = "MCV",
                                                  "Mean corpuscular hemoglobin" = "MCH",
                                                  "Potassium serum" = "Potassium.serum",
                                                  "Chloride serum" = "Chloride.serum",
                                                  "Sodium serum" = "Sodium.serum") ),
                                    
                                    plotOutput("t.test"),
                                    
                                    width=4, height = 500),
                                  
                                  
                                  box(
                                    p(strong("Model performance:"),
                                      style="text-align:justify; color:black; background-color:white"),
                                    
                                    plotOutput("AUC"),
                                    
                                    width=4, height = 500),
                                  
                                  
                                  column(
                                    br(),
                                    p("Author:", strong("MDS02"),
                                      style="font-size:16px; text-align:justify; color:black; background-color:papayawhip; padding:15px;border-radius:10px"),
                                    width=12),
                                  
                                  
                                  
                                )
                                # End of tab 2
                                
                        ) 
                      )
                    )
)