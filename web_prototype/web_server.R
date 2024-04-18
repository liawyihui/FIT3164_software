# loading required libraries
library(shiny)
library(shinythemes)
library(shinydashboard)
library(caret) 
library(randomForest)
library(data.table)
library(xlsx) 
library(readxl)
library(ggpubr)
library(grid)
library(ggplot2)
library(ggpmisc)
library(dplyr)

# loading data and trained predictive model
# need to change to own
#load("www/RFmodel_220604_21.RData")
#load("www/AboutModel.RData")

# defining the entire logic of the website prototype functioning server
server <- function(input, output) {
  
  # downloadable xlsx template of selected dataset
  output$DownloadData <- downloadHandler(
    filename = function() {
      paste("DataTemplate", "xlsx", sep = ".")
    },
    content = function(file) {
      file.copy("model.xlsx", file)
    },
    contentType = "ExcelFile"
  )
  
  # if user inputs their own dataset
  datasetInput <- reactive({
    inFile <- input$DataFile
    DataTable <- read_xlsx(inFile$datapath, sheet =  1)
    return(DataTable)
  })
  
  # table of user input dataset
  output$DataTable <- renderTable({
    datasetInput()
  })
  
  # plot distribution of parameters in the input dataset
  output$DistributionXvar <- renderPlot({
    validate(need(input$DataFile, "Please upload data file!"))
    
    inFile <- input$DataFile
    DataTable <- read_xlsx(inFile$datapath, sheet =  1)
    p <- ggplot(DataTable, aes_string(x=input$Xvar)) + 
      geom_histogram(color="#C4961A", fill="#FFDB6D") +
      labs(title= paste("Histogram of ", input$Xvar, sep = ""), x=input$Xvar, y = "Frequency") +
      theme(text=element_text(size=16,  face="bold")) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black"))
    
    p
  })
  
  # predict Lymphedema for user-input dataset
  output$Pred.Lymphedema <- DT::renderDataTable({
    validate(need(input$DataFile, ""))
    
    inFile <- input$DataFile
    DataTable <- read_xlsx(inFile$datapath, sheet =  1)
    Pred.prob <- as.data.frame(predict(RFmodel, DataTable, type = "prob"))
    OutputTable <- data.frame(Patient.ID = as.factor(DataTable$Patient.ID),
                              Predicted.Score = Pred.prob$`1`,
                              Predicted.Lymphedema = ifelse(Pred.prob$`1`>0.25, "Yes", "No"))
  }, selection = "single")
  
  # predict Lymphedema for selected patient
  output$PatientPredict <- renderValueBox({
    validate(need(input$DataFile, "Please upload data file!"))
    
    inFile <- input$DataFile
    DataTable <- read_xlsx(inFile$datapath, sheet =  1)
    DataTable$Patient.ID <- as.character(DataTable$Patient.ID)
    
    Pred.prob <- as.data.frame(predict(RFmodel, DataTable, type = "prob"))
    Pred.Table <- data.frame(Patient.ID = as.factor(DataTable$Patient.ID),
                             Predicted.Score = Pred.prob$`1`,
                             Predicted.Lymphedema = ifelse(Pred.prob$`1`>0.25, "Yes", "No"))

    RowIndex <- input$Pred.Lymphedema_rows_selected
    Score <- ifelse(is.na(RowIndex),"NA", Pred.Table$Predicted.Score[RowIndex])
    DisplayScore <- paste("Score =", ifelse(is.numeric(RowIndex), Score, "NA"), sep = " ")
    
    color_lymph <- ifelse(!is.numeric(Score), "blue", 
                          ifelse(Score > 0.25, "red", "green")) 
    icon_lymph <- ifelse(!is.numeric(Score), "meh-o", 
                         ifelse(Score > 0.25, "exclamation-triangle", "fas fa-envira"))
    
    subtitle_lymph <- ifelse(!is.numeric(Score), "Patient ID not selected", 
                             ifelse(Score <= 0.25, paste("Patient ID ", Pred.Table$Patient.ID[RowIndex], " might be NEGATIVE to Lymphedema", sep = ""), 
                                    paste("Patient ID ", Pred.Table$Patient.ID[RowIndex], " might be POSITIVE to Lymphedema", sep = "")))
    
    valueBox(DisplayScore, subtitle_lymph, color = color_lymph, icon(icon_lymph), width = 3)
  })
  
  # plot relative importance of parameters
  output$Descriptor.Importance <- renderPlot({
    #DataTable2 <- read_xlsx("www/RF_output_220604_21.xlsx", sheet =  2)
    #p2 <- ggplot(DataTable2, aes(x=reorder(Descriptor, -Value), y=Value)) + 
    #  geom_bar(stat="identity") +
    #  #labs(title= "Importance of parameters to model") +
    #  xlab("") +
    #  ylab("Relative importance") +
    #  theme(text=element_text(size=16,  face="bold")) +
    #  theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black")) +
    #  coord_flip() # Horizontal bar plot
    p2 
  })
  
  # plot t-test results 
  output$t.test <- renderPlot({
    #DataTable1 <- read_xlsx("www/RF_output_220604_21.xlsx", sheet =  1)
    my_comparisons <- list( c("0", "1") )
    Tab <- select(DataTable1, input$Xvar.t.test)
    p1 <- ggboxplot(DataTable1, x = "le", y = input$Xvar.t.test,  color = "le", palette = "jco", add = "jitter", legend = "none") + 
      stat_compare_means(method = "t.test", comparisons = my_comparisons, label = "p.signif")+ # Add pairwise comparisons p-value
      stat_compare_means(method = "t.test", label.x = 1.25, label.y = 1.4*max(Tab))+     # Add global p-value
      xlab("Lymphedema?")+ # x axis title 
      scale_x_discrete(labels=c("0" = "No", "1" = "Yes")) + # x-axis tick label
      theme(text=element_text(size=16,  face="bold")) +
      theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black"))
    
    plot(ggpar(p1, ylim = c(0.6*min(Tab), 1.4*max(Tab))))    
  })
  
  # plot AUC result of model
  output$AUC <- renderPlot({
    #ROC_train <- read_xlsx("www/RF_output_220604_21.xlsx", sheet =  8)
    #ROC_test <- read_xlsx("www/RF_output_220604_21.xlsx", sheet =  10)
    #AUC_test <- read_xlsx("www/RF_output_220604_21.xlsx", sheet =  11)
    #AUC_test_value <- as.numeric(colnames(AUC_test))
    #AUC_test_tab <- data.frame("Parameter" = "AUC",
    #                           "Value"      = as.numeric(colnames(AUC_test)))
    
    #Perf.Table <- as.data.frame(read_xlsx("www/RF_output_220604_21.xlsx", sheet =  6))
    
    #Perf.Table.all <- rbind(Perf.Table,AUC_test_tab)
    #Perf.Table.all$Value <- round(Perf.Table.all$Value, digits = 3)
    #Perf.Table.all <- Perf.Table.all[-c(3,4,8:10),]
    
    
    #p3 <- ggplot() +
    #     geom_line(data=ROC_test, aes(x=FPR, y=TPR), color="red") +
    #    theme(text=element_text(size=16,  face="bold")) +
    #    theme(axis.text.x = element_text(angle = 0, hjust = 1, colour = "black")) +
    #    annotate(geom = "table",x = 1.0, y = 0.0, label = list(cbind(Perf.Table.all)))
    
    p3 
  })
  
}