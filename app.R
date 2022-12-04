#
# BaroWaveletApp
#
# Author: ALVARO CHAO ECIJA

# LOAD PACKAGES ##############################################################################
library(shiny)
require("BaroWavelet")
##############################################################################################

##############################################################################################
####       BAROWAVELET'S UI            #######################################################
##############################################################################################
ui <- fluidPage(
    titlePanel("BaroWavelet"),
    fluidRow(
      column(6,
             wellPanel(h2("Upload file with cardiovascular data"),
                       p("Upload and store data from a specific subject",
                        style="text-align:justify;color:black;background-color:grey80"),
          
          uiOutput("data_file"),
          tags$hr(),
          radioButtons("separator", "Choose separator", choices = c(Semicolon =";",
                                                                    Comma = ",",
                                                                    Tab ="\t"), selected = ";"),
          tags$hr(),
          radioButtons("preprocessing", "Choose preprocessing algorithm", choices = c("Filter with RHRV and Interpolate",
                                                                                      "Interpolate",
                                                                                      "Do not Interpolate"), selected = "Filter with RHRV and Interpolate"
                       ),
          numericInput("int_freq", "Frequency", value = 4, width = 70),
          tags$hr(),
          textInput("subject_name_input", "Identifiers", value = "Subjects"),
          tags$hr(),
          actionButton("upload_subject_data", "Upload Subject Data")
      )
          
      ),
      column(6,
             
             wellPanel(h2("Studdy settings"),
                       p("Change global settings. These settings are intended to be applied to all analyses
                         to avoid biased comparisons",
                         style="text-align:justify;color:black;background-color:grey80"),
                      textInput("framework_name", "Name", value = "BaroWavelet Study"),
                      tags$hr(),
                      selectInput("wavelet", "Wavelet (for Discrete Wavelet Transform)",
                                  choices = c("bl14", "bl20","bs3.1","d2", "d4",
                                              "d8", "d16", "fk4", "fk6", "fk14",
                                              "fk22", "haar", "la8" ,"la16", "la20",
                                              "mb4", "mb8", "mb16", "mb24", "w4"
                                              ), selected = "d4"),
                      tags$hr(),
                      fluidRow(
                               column(4,
                                      numericInput("HF_val", "HF", value = 0.4, min = 0.15, step = 0.001)
                                      ),
                               column(4,
                                      numericInput("LF_val", "LF", value = 0.15, min = 0.04, max = 0.15, step = 0.001)
                                      ),
                               column(4,
                                      numericInput("VLF_val", "VLF", value = 0.04, min = 0, max = 0.04, step = 0.0001)
                               )
                      ),
                      tags$hr(),
                      checkboxInput("use_thr", "Use coherence threshold (for Continuous Wavelet Transform)", 
                                    value = TRUE),
                      tags$hr(),
                      actionButton("change_main_sets", "Change Framework Settings"),
                      tags$hr(),
                      h3("Current settings"),
                      h4(textOutput("text_HF")),
                      h4(textOutput("text_LF")),
                      h4(textOutput("text_wavelet")),
                      h4(textOutput("text_thr"))
               
             )
      )
             
    ),
    fluidRow(
      column(6,
             wellPanel(
               h3("Upload / Download Study"),
               br(),
               uiOutput("study_file"),
               actionButton("confirm_study", "Use Uploaded Study"),
               tags$hr(),
               downloadButton("rds", "Download Study")
             )
             ),
      column(6,
             wellPanel(
               # Panel to create Intervals
               h3("Set Intervals"),
               br(),
               textInput("interval_names", "Name the Intervals", value = "Intervals"),
               actionButton("confirm_interval_names", "Confirm Interval Names")
             ))
    ),
    fluidRow(
      column(12,
             wellPanel(
               # Box to display subject info
               h3("Study Info"),
               tags$hr(),
               h4(textOutput("text_globalname")),
               h4(textOutput("text_n")),
               h4(textOutput("text_nintervals")),
               h4(textOutput("text_ntests"))
             ))
    ),
    fluidRow(
      column(12,
             wellPanel(
               # Box to visualize results from individual analyses
               h2("Subject Analysis"),
               tags$hr(),
               fluidRow(
                        column(4,
                               selectInput("subject_input", "Select Subject", 
                                           choices = "No subjects have been loaded")),
                        column(4,
                               selectInput("interval_input", "Select Interval", 
                                           choices = "No intervals have been set")),
                        column(4,
                               selectInput("control_input", "Set Interval as Control", 
                                           choices = "No control has been set"))
                        ),
               h3("Recordings & HRV"),
               tags$hr(),
               #checkboxInput("var", "Causal-filter the time segment", value = FALSE),
               wellPanel(style = "background:white",
                         plotOutput("Raw", brush = "brush_raw", dblclick = "dbc_raw")
               ),
               fluidRow(
                 column(6,
                        h4(textOutput("Estimate_HR")),
                        br(),
                        h4(textOutput("pvalue_HR"))
                 ),
                 column(6,
                        h4(textOutput("Estimate_SBP")),
                        br(),
                        h4(textOutput("pvalue_SBP"))
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                          h3("Baroreflex Sensitivity (CWT)"),
                          tags$hr(),
                          sidebarLayout(
                            sidebarPanel(width = 2, style = "height: 90vh, overflow-y: auto;",
                                         sliderInput("coherence_val", "Coherence Threshold", min = 0, max = 1, 
                                                     value = 0.5)
                            ),
                            mainPanel(width = 10,
                                      imageOutput("CWT_plot"))
                          ),
                          
                          
                          
                          
                          br(),
                          br(),
                          h3("Time-averaged Gain Transfer Function"),
                          br(),
                          wellPanel(style = "background:white",
                                    fluidRow(
                                      column(6, 
                                             plotOutput("Analyzed_TF_Plot1_cwt", brush = brushOpts(id = "Analyzed_TF_brush1",
                                                                                                   resetOnNew = TRUE),
                                                        dblclick = "Analyzed_TF_dbc1")
                                      ),
                                      column(6,
                                             plotOutput("Analyzed_TF_Plot2_cwt", brush = brushOpts(id = "Analyzed_TF_brush1",
                                                                                                   resetOnNew = TRUE),
                                                        dblclick = "Analyzed_TF_dbc1")
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             h4(textOutput("CWT_Estimate_HF")),
                                             br(),
                                             h4(textOutput("pvalue_HF_cwt"))
                                      ),
                                      column(6,
                                             h4(textOutput("CWT_Estimate_LF")),
                                             br(),
                                             h4(textOutput("pvalue_LF_cwt"))
                                      )
                                    )
                                    
                                    
                                    
                          ),
                          br(),
                          h3("Phase shift"),
                          br(),
                          wellPanel(style = "background:white",
                                    fluidRow(
                                      column(6, 
                                             plotOutput("phase1_cwt", brush = brushOpts(id = "Analyzed_TF_brush1",
                                                                                        resetOnNew = TRUE),
                                                        dblclick = "Analyzed_TF_dbc1")
                                      ),
                                      column(6,
                                             plotOutput("phase2_cwt", brush = brushOpts(id = "Analyzed_TF_brush1",
                                                                                        resetOnNew = TRUE),
                                                        dblclick = "Analyzed_TF_dbc1")
                                      )
                                    )
                          )
                          ),
                          
                          br(),
                          tags$hr(),
                          br(),
                          br(),
                          br(),
                          h3("Baroreflex Sensitivity (DWT)"),
                          br(),
                          tags$hr(),
                          wellPanel(style = "background:white",
                                    fluidRow(
                                      column(6, 
                                             plotOutput("Analyzed_TF_Plot1", brush = brushOpts(id = "Analyzed_TF_brush1",
                                                                                               resetOnNew = TRUE),
                                                        dblclick = "Analyzed_TF_dbc1")
                                      ),
                                      column(6,
                                             plotOutput("Analyzed_TF_Plot2", brush = brushOpts(id = "Analyzed_TF_brush1",
                                                                                               resetOnNew = TRUE),
                                                        dblclick = "Analyzed_TF_dbc1")
                                      )
                                    ),
                                    fluidRow(
                                      column(6,
                                             h4(textOutput("DWT_Estimate_HF")),
                                             br(),
                                             h4(textOutput("pvalue_HF"))
                                      ),
                                      column(6,
                                             h4(textOutput("DWT_Estimate_LF")),
                                             br(),
                                             h4(textOutput("pvalue_LF"))
                                      )
                                    )
                                    
                                    
                                    
                          ),
                          tags$hr(),
                          sidebarLayout(
                            sidebarPanel(width = 2, style = "height: 90vh, overflow-y: auto;",
                                         sliderInput("maxEst_dwt", "Max value", min = 0, max = 200, 
                                                     value = 60, step = 0.05)
                            ),
                            mainPanel(style = "background:white", width = 10,
                                      plotOutput("ExpectedVals_DWT_Plot"))
                          )
                        
                 )
               ),
               br(),
               fluidRow(
                 column(12,
                        wellPanel(
                          h3("Heart Rate Variability (DWT)"),
                          br(),
                          br(),
                          h3("LF/HF ratio"),
                          br(),
               wellPanel(style = "background:white",
                         plotOutput("LF/HF", brush = "brush_raw", dblclick = "dbc_raw")
               ),
               fluidRow(
                 column(12,
                        h4(textOutput("pvalue_HRV_LFHF"))
               )),
               fluidRow(
                 column(6, 
                        h3("HF band (vagal)"),
                        br()),
                 column(6, 
                        h3("LF band (sympathetic)"),
                        br(),
               )),
               wellPanel(style = "background:white",
               fluidRow(
                 column(6, 
                        br(),
                        plotOutput("HF", brush = "brush_raw", dblclick = "dbc_raw")),
                 column(6, 
                        br(),
                        plotOutput("LF", brush = "brush_raw", dblclick = "dbc_raw"))
               )),
               fluidRow(
                 column(6,
                        h4(textOutput("Estimate_HF")),
                        br(),
                        h4(textOutput("pvalue_HRV_HF"))
                 ),
                 column(6,
                        h4(textOutput("Estimate_LF")),
                        br(),
                        h4(textOutput("pvalue_HRV_LF"))
                 )
               )
               )
                 )
               )
      )
             ),


fluidRow(
  column(12,
         wellPanel(
           h3("Test Variables"),
           br(),
           fluidRow(
           column(4, textInput("test_name", "Name the Test", value = "Test")),
           column(4, selectInput("test_var_in_test", "Select testing variable", 
                       choices = "No testing variable has been selected")),
           column(4, selectInput("con_var_in_test", "Select control variable", 
                       choices = "No control variable has been selected"))
           ),
           fluidRow(
           column(4, actionButton("confirm_test", "Test")),
           column(4, checkboxInput("norm_hrv", "Normalize HRV", value = FALSE))
           )
         )
         )
),

fluidRow(
  column(6,
         wellPanel(
           h3("HRV Test Results"),
           selectInput("select_testHRV", "Select test", 
                       choices = "No test has been selected"),
           br(),
           wellPanel(style = "background:white",
                     plotOutput("testing_resultsHRV")
           )
         )),
  column(6,
         wellPanel(
           h3("BRS Test Results"),
           selectInput("select_test", "Select test", 
                       choices = "No test has been selected"),
           br(),
           wellPanel(style = "background:white",
                     plotOutput("testing_results")
           )
         )
    
  )
  
),

fluidRow(
  column(12,
         wellPanel(
             uiOutput("clinic_file"),
             actionButton("load_clin", "Load Clinical Data"),
           br(),
           br(),
           br(),
           fluidRow(
             column(4,
             selectInput("select_variable1", "Select variable", 
                         choices = c("BRS (HF)", "BRS (LF)", "HRV (HF)", "HRV (LF)", "HRV (LF/HF)"))),
             column(4, selectInput("select_variable2", "Select variable", 
                                   choices = "No variable has been selected")),
             column(4,
                    selectInput("select_variable3", "Select clinical variable", 
                                choices = "No clinical variable has been selected"))
           ),
           actionButton("make_linear", "Model variables"),
           br(),
           wellPanel(style = "background:white",
                     plotOutput("plot_linear")
             
           ),
           h4(textOutput("model_stats"))
         )
  )
)
)
##################### END OF UI #############################################################


##############################################################################################
####       BAROWAVELET'S SERVER             ##################################################
##############################################################################################


server <- function(input, output, session) {
  
  ####### 1. RENDER UIS #######################################################################
  options(shiny.maxRequestSize = 1000*1024^2) # To change default size for uploaded data
  output$data_file <- renderUI({
    fileInput("data_file", "Upload data file",
              multiple = TRUE, accept = c("text/csv",
                                           "text/comma-separated-values", "text/plain",
                                           ".csv", ".txt"))
  })
  output$study_file <- renderUI({
    fileInput("study_file", "Upload study file",
              multiple = FALSE, accept = ".RDS")
  })
  output$clinic_file <- renderUI({
    fileInput("clinic_file", "Upload clinical data",
              multiple = FALSE, accept = c("text/csv",
                                          "text/comma-separated-values", "text/plain",
                                          ".csv", ".txt"))
  })
  
  ##########################################################################################
  
  ###### 2. INITIAL SETUP #####################################################################
  
  database <- reactiveValues()
  RAW_database <- reactiveValues()
  framework <- BuildStructure(name = "BaroWavelet Study")
  #framework <- BuildStructure(name = "BaroWavelet Study", use.weight = FALSE)
  database$framework <- framework
  RAW_database$RAW <- framework
  text_globalname <- paste("Name of this study:", isolate(database$framework)$Name)
  text_n <- paste("Number of subjects contained in this study:", isolate(database$framework)$n, "subjects.")
  text_nintervals <- paste("Number of intervals analyzed in this study: ", length(isolate(database$framework)$ExpectedVals), "intervals.")
  text_ntests <- paste("Number of tests performed:", length(isolate(database$framework)$Tests), "tests.")
  text_HF <- paste("HF Interval: from", framework$"General Data"$HF, "to", framework$"General Data"$LF, "Hz.")
  text_LF <- paste("LF Interval: from", framework$"General Data"$LF, "to", framework$"General Data"$VLF, "Hz.")
  text_wavelet <- paste("DWT wavelet:", framework$"General Data"$Wavelet)
  text_thr <- "A coherence threshold is being used to calculate the estimates."
  output$text_globalname <- renderText({text_globalname})
  output$text_n <- renderText({text_n})
  output$text_nintervals <- renderText({text_nintervals})
  output$text_ntests <- renderText({text_ntests})
  output$text_HF <- renderText({text_HF})
  output$text_LF <- renderText({text_LF})
  output$text_wavelet <- renderText({text_wavelet})
  output$text_thr <- renderText({text_thr})
  
  #########################################################################################################
  
  ###### 3. Update Numeric Outputs for Band Limits ######################################################
  
  observeEvent(input$LF_val, {
    updateNumericInput(session, "VLF_val", "VLF", value = input$VLF_val, min = 0, max = input$LF_val, step = 0.0001)
    updateNumericInput(session, "HF_val", "HF", value = input$HF_val, min = input$LF_val, step = 0.001)
  })
  observeEvent(input$VLF_val, {
    updateNumericInput(session, "LF_val", "LF", value = input$LF_val, min = input$VLF_val, max = input$HF_val, step = 0.001)
  })
  
  #########################################################################################################
  
  ###### 4. DOWNLOAD STUDY ###############################################################################
  
  output$rds <- 
    downloadHandler(
      filename = function(){
        paste(isolate(database$framework)$Name, ".RDS", sep = "")
      },
      content = function(file){
        framework <- isolate(database$framework)
        RAW <- isolate(RAW_database$RAW)
        analyses <- ShowIndexes(RAW, "analyses")[2,]
        if(length(analyses) > 0){
          for(n in 1:length(analyses)){
            framework$Analyses[[n]]$Data <- RAW$Analyses[[n]]$Data
            framework <- AnalyzeTransferFun(framework, n)
          }
        }
        if(length(framework$ExpectedVals) > 0){
          framework <- PrepareIntervalSlots(framework, "dwt")
          framework <- PrepareIntervalSlots(framework, "cwt")
        }
        saveRDS(framework, file  = file)
      }
    )
  ##################################################################################################
  
  
  ####### 5. Change Study Settings ################################################################
  
  observeEvent(input$change_main_sets, {
    framework <- isolate(database$framework)
    framework$Name <- input$framework_name
    output$text_globalname <- renderText({paste("Name of this study:",framework$Name)})
    framework$"General Data"$Wavelet <- input$wavelet
    framework$"General Data"$Threshold <- input$use_thr
    framework$"General Data"$HF <- input$HF_val
    framework$"General Data"$LF <- input$LF_val
    framework$"General Data"$VLF <- input$VLF_val
    # Allow usage of simulation
    if(framework$Name == "Simulation"){
      RAW <- isolate(RAW_database$RAW)
      nosySim <- InterpolateData(DataSimulation())
      denoisedSim <- InterpolateData(DataSimulation(use.noise = FALSE))
      framework <- AddAnalysis(framework, name = "Simulation with noisy signals")
      framework <- AddDataToAnalysis(framework, length(framework$Analyses),
                                     time = nosySim$Time, RR = nosySim$RR,
                                     SBP = nosySim$SBP)
      RAW <- AddAnalysis(RAW, name = "Simulation with noisy signals")
      RAW <- AddDataToAnalysis(RAW, length(RAW$Analyses),
                                     time = nosySim$Time, RR = nosySim$RR,
                                     SBP = nosySim$SBP)
      framework <- AnalyzeTransferFun(framework, length(framework$Analyses))
      framework <- AddAvgCwtData(framework, length(framework$Analyses))
      framework <- AddCausalCoupling(framework, length(framework$Analyses))
      framework <- AddAnalysis(framework, name = "Simulation with denoised signals")
      framework <- AddDataToAnalysis(framework, length(framework$Analyses),
                                     time = denoisedSim$Time, RR = denoisedSim$RR,
                                     SBP = denoisedSim$SBP)
      RAW <- AddAnalysis(RAW, name = "Simulation with denoised signals")
      RAW <- AddDataToAnalysis(RAW, length(RAW$Analyses),
                               time = denoisedSim$Time, RR = denoisedSim$RR,
                               SBP = denoisedSim$SBP)
      RAW_database$RAW <- RAW
      framework <- AnalyzeTransferFun(framework, length(framework$Analyses))
      framework <- AddAvgCwtData(framework, length(framework$Analyses))
      framework <- AddCausalCoupling(framework, length(framework$Analyses))
      new_analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      updateSelectInput(session, "subject_input", "Select Subject", choices = new_analysis_choices)
      text_n <- paste("Number of subjects contained in this study:", framework$n, "subjects.")  
      output$text_n <- renderText({text_n})
    }
    text_wavelet <- paste("DWT wavelet:", framework$"General Data"$Wavelet)
    text_thr <- ifelse(framework$"General Data"$Threshold, "A coherence threshold is being used to calculate the estimates.",
                       "No coherence threshold is being used to calculate the estimates.")
    text_weights <- ifelse(framework$"General Data"$Weight, "Results are being weighted for estimate calculations.",
                           "Results are not being weighted for estimate calculations.")
    text_HF <- paste("HF Interval: from", framework$"General Data"$HF, "to", framework$"General Data"$LF, "Hz.")
    text_LF <- paste("LF Interval: from", framework$"General Data"$LF, "to", framework$"General Data"$VLF, "Hz.")
    output$text_HF <- renderText({text_HF})
    output$text_LF <- renderText({text_LF})
    output$text_wavelet <- renderText({text_wavelet})
    output$text_thr <- renderText({text_thr})
    database$framework <- framework
  })
  
  ##############################################################################################################
  
  ############# 6. Load Study: #################################################################################
  
  observeEvent(input$confirm_study, {
    req(input$study_file)
    framework <- readRDS(input$study_file$datapath)
    output$study_file <- renderUI({
      fileInput("study_file", "Upload study file",
                multiple = FALSE, accept = c(".RDS"))
    })
    text_globalname <- paste("Name of this study:", framework$Name)
    text_n <- paste("Number of subjects contained in this study:", framework$n, "subjects.")
    text_nintervals <- paste("Number of intervals analyzed in this study: ", length(framework$ExpectedVals), "intervals.")
    text_ntests <- paste("Number of tests performed:", length(framework$Tests), "tests.")
    text_HF <- paste("HF Interval: from", framework$"General Data"$HF, "to", framework$"General Data"$LF, "Hz.")
    text_LF <- paste("LF Interval: from", framework$"General Data"$LF, "to", framework$"General Data"$VLF, "Hz.")
    text_wavelet <- paste("DWT wavelet:", framework$"General Data"$Wavelet)
    text_thr <- "A coherence threshold is being used to calculate the estimates."
    text_weights <- "Results are being weighted for estimate calculations."
    if(length(framework$Analyses) > 0) new_analysis_choices <- ShowIndexes(framework, "analyses")[2,]
    if(length(framework$ExpectedVals) > 0) new_interval_choices <- ShowIndexes(framework, "intervals")[2,]
    if(length(framework$Tests) > 0) new_test_choices <- ShowIndexes(framework, "tests")[2,]
    output$text_globalname <- renderText({text_globalname})
    output$text_n <- renderText({text_n})
    output$text_nintervals <- renderText({text_nintervals})
    output$text_ntests <- renderText({text_ntests})
    output$text_HF <- renderText({text_HF})
    output$text_LF <- renderText({text_LF})
    output$text_wavelet <- renderText({text_wavelet})
    output$text_thr <- renderText({text_thr})
    output$text_weights <- renderText({text_weights})
    if(length(framework$Analyses) > 0) updateSelectInput(session, "subject_input", "Select Subject", choices = new_analysis_choices)
    if(length(framework$ExpectedVals) > 0) updateSelectInput(session, "interval_input", "Select Interval", choices = c("No intervals have been set", new_interval_choices))
    if(length(framework$ExpectedVals) > 0) updateSelectInput(session, "control_input", "Set Interval as Control", choices = c("No control has been set", new_interval_choices))
    if(length(framework$ExpectedVals) > 0) updateSelectInput(session, "test_var_in_test", "Select testing variable", choices = c("No testing variable has been selected", new_interval_choices))
    if(length(framework$ExpectedVals) > 0) updateSelectInput(session, "con_var_in_test", "Select control variable", choices = c("No control variable has been selected", new_interval_choices))
    if(length(framework$ExpectedVals) > 0) updateSelectInput(session, "select_variable2", "Select variable", choices = c("No variable has been selected", new_interval_choices))
    if(length(framework$Tests) > 0) updateSelectInput(session, "select_test", "Select test", choices = c("No test has been selected", new_test_choices))
    if(length(framework$TestsHRV) > 0) updateSelectInput(session, "select_testHRV", "Select test", choices = c("No test has been selected", new_test_choices))
    if(length(framework$Clinical) > 0){
      clinic_names <- names(framework$Clinical[1,])[-1]
      updateSelectInput(session, "select_variable3", "Select clinical variable", choices = c("No clinical variable has been selected", clinic_names))
    } 
    database$framework <- framework
    RAW_database$RAW <- framework
  })
  #############################################################################################################
  
  ############# 7. Create Intervals ###########################################################################
  
  observeEvent(input$confirm_interval_names, {
    framework <- isolate(database$framework)
    names <- strsplit(input$interval_names, ", ")[[1]]
    if(names[1] != "Intervals"){
      for(n in 1:length(names)){
        framework <- AddTimeInterval(framework, name = names[n])
      }
      text_nintervals <- paste("Number of intervals analyzed in this study: ", length(framework$ExpectedVals), "intervals.")
      output$text_nintervals <- renderText({text_nintervals})
      choices <- ShowIndexes(framework, "intervals")[2,]
      updateSelectInput(session, "interval_input", "Select Interval", choices = c("No intervals have been set", choices))
      updateSelectInput(session, "control_input", "Set Interval as Control", choices = c("No control has been set", choices))
      updateSelectInput(session, "test_var_in_test", "Select testing variable", choices = c("No testing variable has been selected", choices))
      updateSelectInput(session, "con_var_in_test", "Select control variable", choices = c("No control variable has been selected", choices))
      updateSelectInput(session, "select_variable2", "Select variable", choices = c("No variable has been selected", choices))
      framework <- PrepareIntervalSlots(framework, "dwt")
      framework <- PrepareIntervalSlots(framework, "cwt")
      database$framework <- framework
    }
  })
  
  ##############################################################################################################
  
  ############## 8. LOAD INDIVIDUAL SUBJECT DATA ##############################################################
  observeEvent(input$upload_subject_data, {
    names <- strsplit(input$subject_name_input, ", ")[[1]]
    framework <- isolate(database$framework)
    RAW <- isolate(RAW_database$RAW)
    req(input$data_file)
    if(nrow(input$data_file) == length(names)){
      N <- length(names)
      for(n in 1:N){
        if(tools::file_ext(input$data_file[[n, "datapath"]]) == "csv"){
        data <- read.csv(input$data_file[[n, "datapath"]],
                         header = TRUE, sep = input$separator,
                         quote  ="")
        } else if(tools::file_ext(input$data_file[[n, "datapath"]]) == "txt"){
          data <- read.table(input$data_file[[n, "datapath"]], header = TRUE)
          data$Time <- cumsum(data$Time)
        }
        if(is.null(data$RR)){
          data$RR <- c(data$Time[[1]]*1000, diff(data$Time*1000))
        } else if(is.null(data$Time)){
          data$Time <- cumsum(data$RR/1000)
        }
        if(input$preprocessing == "Interpolate"){
          #data <- PreprocessData(data, use.RHRV = FALSE)
          data <- InterpolateData(data, input$int_freq)
        } else if(input$preprocessing == "Filter with RHRV and Interpolate"){
          data <- PreprocessData(data, use.RHRV = TRUE)
          data <- InterpolateData(data, input$int_freq)
        }
        framework <- AddAnalysis(framework, name = names[[n]])
        framework <- AddDataToAnalysis(framework, length(framework$Analyses),
                                       time = data$Time, RR = data$RR,
                                       SBP = data$SBP)
        RAW <- AddAnalysis(RAW, name = names[[n]])
        RAW <- AddDataToAnalysis(RAW, length(RAW$Analyses),
                                 time = data$Time, RR = data$RR,
                                 SBP = data$SBP)
        framework <- AnalyzeTransferFun(framework, length(framework$Analyses))
        framework <- AddAvgCwtData(framework, length(framework$Analyses))
        framework <- AddCausalCoupling(framework, length(framework$Analyses))
        if(length(framework$ExpectedVals) > 0){
          framework <- PrepareIntervalSlots(framework, "dwt")
          framework <- PrepareIntervalSlots(framework, "cwt")
        }
      }
      output$data_file <- renderUI({
        fileInput("data_file", "Upload data file",
                  multiple = TRUE, accept = c("text/csv",
                                               "text/comma-separated-values", "text/plain",
                                               ".csv", ".txt"))
      })
      new_analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      updateSelectInput(session, "subject_input", "Select Subject", choices = new_analysis_choices)
      database$framework <- framework
      RAW_database$RAW <- RAW
      text_n <- paste("Number of subjects contained in this study:", isolate(database$framework)$n, "subjects.")  
      output$text_n <- renderText({text_n})
      
    }
    })
  #############################################################################################################
  
  ################### 9. Load Clinical Data ###################################################################
  
  observeEvent(input$load_clin, {
    framework <- isolate(database$framework)
    req(input$clinic_file)
        if(tools::file_ext(input$study_file$datapath) == "csv"){
          data <- read.csv(input$study_file$datapath,
                           header = TRUE, sep = input$separator,
                           quote  ="")
        } else if(tools::file_ext(input$study_file$datapath) == "txt"){
          data <- read.table(input$study_file$datapath, header = TRUE)
        }
        framework$Clinical <- data
        database$framework <- framework
      output$clinic_file <- renderUI({
        fileInput("clinic_file", "Upload clinical data",
                  multiple = FALSE, accept = c("text/csv",
                                              "text/comma-separated-values", "text/plain",
                                              ".csv", ".txt"))
      })
      new_analysis_choices <- colnames(data)[-1]
      updateSelectInput(session, "select_variable3", "Select clinical variable", choices = c("No clinical variable has been selected",
                                                                                             new_analysis_choices))
  })
  
  #Model clinical data
  
  observeEvent(input$make_linear, {
    framework <- isolate(database$framework)
    if((input$select_variable2 !="No variable has been selected") &
       (input$select_variable3 !="No clinical variable has been selected")){
      if(input$select_variable1 == "BRS (HF)"){
        type = "BRS"
        band = "HF"
      } else if(input$select_variable1 == "BRS (LF)"){
        type = "BRS"
        band = "LF"
      } else if(input$select_variable1 == "HRV (LF)"){
        type = "HRV"
        band = "LF"
      } else if(input$select_variable1 == "HRV (HF)"){
        type = "HRV"
        band = "HF"
      } else if(input$select_variable1 == "HRV (LF/HF)"){
        type = "HRV"
        band = "LFHF"
      }
      clin <- colnames(framework$Clinical)[-1]
      variable <- match(input$select_variable3, clin) 
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$select_variable2, intervals)
      model <- ModelClinicalData(framework, type = type, band = band, segment = interval, variable = variable)
      output$plot_linear <- renderPlot({
        model$Plot
      })
      output$model_stats <- renderText({
        paste("R-squared value of", round(model$r,4), "with a p-value of", round(model$p,4))
      })
    }
  })
  
  ################# 10. MAKE SUBJECT-SPECIFIC PLOTS ##################################################
  ####################################################################################################
  
  
  ################### 10.1. CARDIOVASCULAR DATA SERIES PLOTS (IBI, BP) ##############################
  output$Raw <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      RAW <- isolate(RAW_database$RAW)
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(RAW, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      raw_data <- RAW$Analyses[[chosen_analysis]]$Data
      raw_data[,"RR"] <- 60000/raw_data[,"RR"]
      result <- ggplot2::ggplot(data = data.frame(raw_data), ggplot2::aes(Time)) +
        ggplot2::geom_line(ggplot2::aes(y = RR, colour = "HR")) + 
        ggplot2::geom_line(ggplot2::aes(y = SBP, colour = "SBP")) +
        ggplot2::theme(axis.title.y = ggplot2::element_blank())
      raw_time <- raw_data[,"Time"]
      Time <- framework$Analyses[[chosen_analysis]]$Data[,"Time"]
      if(max(raw_time) != max(Time) | min(raw_time) != min(Time)){
        result <- result + ggplot2::annotate("rect", fill = "red", 
                                               alpha = 0.5, xmin = 
                                                 min(Time),
                                               xmax = max(Time),
                                               ymin = -Inf, ymax = Inf)
      }
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            result <- result + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            result <- result + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(result)
      } else {
        return(result)
      }
    } 
  })
  #####################################################################################################
  
  ############## 10.2. LF/HF RATIO PLOT ###############################################################
  output$"LF/HF" <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedHRV(framework, chosen_analysis, plotLF = TRUE, plotHF = TRUE, newPlot = FALSE)
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(Results)
      } else {
        return(Results)
      }
    } 
    
  })
  ###########################################################################################################
  
  
  ############### 10.3. HRV LF PLOT #########################################################################
  output$HF <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedHRV(framework, chosen_analysis, plotLF = FALSE, plotHF = TRUE, newPlot = FALSE)
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(Results)
      } else {
        return(Results)
      }
    } 
    
  })
  ##############################################################################################
  
  
  ################## 10.4. HRV LF PLOT #########################################################
  output$LF <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedHRV(framework, chosen_analysis, plotHF = FALSE, plotLF = TRUE, newPlot = FALSE)
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(Results)
      } else {
        return(Results)
      }
    } 
    
  })
  ##########################################################################################################
  
  

  ####################### 10.5.DWT ALPHA INDEX HF BAND #############################################################
  output$Analyzed_TF_Plot1 <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
    analysis_choices <- ShowIndexes(framework, "analyses")[2,]
    chosen_analysis <- match(input$subject_input, analysis_choices)
    Results <- PlotAnalyzedTF(framework, chosen_analysis, "dwt", newPlot = FALSE, plotLF = FALSE)
    if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
      if(input$interval_input != "No intervals have been set"){
        intervals <- ShowIndexes(framework, "intervals")[2,]
        interval <- match(input$interval_input, intervals)
        if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
          Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                 alpha = 0.5, xmin = 
                                                   framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                 xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                 ymin = -Inf, ymax = Inf)
        }
      }
      if(input$control_input != "No control has been set"){
        intervals <- ShowIndexes(framework, "intervals")[2,]
        interval <- match(input$control_input, intervals)
        if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
          Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                 alpha = 0.5, xmin = 
                                                   framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                 xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                 ymin = -Inf, ymax = Inf)
        }
      }
      return(Results)
    } else {
      return(Results)
    }
    } 
    
  })
  ######################################################################################################
  
  
  
  ########### 10.6. CWT TF PHASE PLOT HF BAND #################################################################
  output$phase1_cwt <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedTF(framework, chosen_analysis, "cwt.phase", newPlot = FALSE, plotLF = FALSE, thr = input$coherence_val)
      Results <- Results + ggplot2::ylim(-pi, pi)
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(Results)
      } else {
        return(Results)
      }
    } 
    
  })
  #####################################################################################################
  
  
  ############# 10.7. DWT ALPHA INDEX LF BAND #######################################################
  output$Analyzed_TF_Plot2 <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedTF(framework, chosen_analysis, "dwt", newPlot = FALSE, plotHF = FALSE)
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(Results)
        } else {
          return(Results)
        }
      } 
      
  })
  ####################################################################################################
  
  
  
  ############# 10.8. CWT SCALE-AVERAGED GAIN TRANSFER FUNCTION HF BAND ##############################
  output$Analyzed_TF_Plot1_cwt <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedTF(framework, chosen_analysis, "cwt.avg", newPlot = FALSE, plotLF = FALSE, thr = input$coherence_val)
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(Results)
      } else {
        return(Results)
      }
    } 
    
  })
  ##########################################################################################################
  
  
  ############# 10.9. CWT SCALE-AVERAGED GAIN TRANSFER FUNCTION LF BAND ##############################
  output$Analyzed_TF_Plot2_cwt <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedTF(framework, chosen_analysis, "cwt.avg", newPlot = FALSE, plotHF = FALSE, thr = input$coherence_val)
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(Results)
      } else {
        return(Results)
      }
    } 
    
  })
  ##########################################################################################################
  
  ########### 10.10. CWT TF PHASE PLOT HF BAND #################################################################
  output$phase2_cwt <- renderPlot({
    if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedTF(framework, chosen_analysis, "cwt.phase", newPlot = FALSE, plotHF = FALSE, thr = input$coherence_val)
      Results <- Results + ggplot2::ylim(-pi, pi)
      if(input$interval_input != "No intervals have been set" | input$control_input != "No control has been set"){
        if(input$interval_input != "No intervals have been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$interval_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "red", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        if(input$control_input != "No control has been set"){
          intervals <- ShowIndexes(framework, "intervals")[2,]
          interval <- match(input$control_input, intervals)
          if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis])){
            Results <- Results + ggplot2::annotate("rect", fill = "blue", 
                                                   alpha = 0.5, xmin = 
                                                     framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]*60,
                                                   xmax = framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis]*60,
                                                   ymin = -Inf, ymax = Inf)
          }
        }
        return(Results)
      } else {
        return(Results)
      }
    } 
    
  })
  ######################################################################################################
  


  
  ################# 10.11. CWT TRANSFER FUNCTION ########################################################
  
  output$CWT_plot <- renderImage(
    { if(input$subject_input !="No subjects have been loaded"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Results <- PlotAnalyzedTF(framework, index = chosen_analysis, method = "cwt", tem = TRUE, newPlot = FALSE,
                                thr = input$coherence_val)
      return(list(src = Results, contentType = "image/png", width = 1500, height = 400, alt = "CWT Transfer Function"))
    } else {
      blank <- tempfile(fileext = ".png")
      png(filename = blank, width = 1500, height = 400)
      plot(0, type = "l", xlab = "", ylab  ="", xaxt = "n", yaxt = "n")
      dev.off()
      return(list(src = blank, contentType = "image/png", width = 1500, height = 400, alt = "CWT Transfer Function"))
    }} , deleteFile = TRUE)
  ######################################################################################################
  
  #####################################################################################################
  ####################################################################################################
  
  
  ####################### 11. ESTIMATE TEXTS ######################################################
  
  output$DWT_Estimate_HF<- renderText({
    if(input$subject_input !="No subjects have been loaded" & !is.null(input$Analyzed_TF_brush1)){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Data <- framework$"General Data"
      Analysis <- framework$Analyses[[chosen_analysis]]
      tf <- Analysis$BRS$DWT
      tf$Time <- Analysis$Data[,1]
      tf$type <- "TFun_dwt"
      EVals <- ExpectedValues(tf, c(input$Analyzed_TF_brush1$xmin / 60, input$Analyzed_TF_brush1$xmax / 60),
                              weight = Data$Weight, use.coherence = Data$Threshold,
                              thr = Data$Coherence)
      paste("Estimate at HF band between", round(ifelse(input$Analyzed_TF_brush1$xmin > 0,  input$Analyzed_TF_brush1$xmin/ 60, 0), 3), "and", round(input$Analyzed_TF_brush1$xmax / 60, 3), "min:", round(EVals[1],3), "ms/mmHg")
    }
    })
  
  output$CWT_Estimate_HF<- renderText({
    if(input$subject_input !="No subjects have been loaded" & !is.null(input$Analyzed_TF_brush1)){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Data <- framework$"General Data"
      Analysis <- framework$Analyses[[chosen_analysis]]
      tf <- AssembleCwtTransferFun(framework, chosen_analysis)
      tf <- SplitByCoherence(tf, thr = input$coherence_val, phase.rest = NULL)
      tf$Time <- Analysis$Data[,1]
      tf$type <- "TFun_dwt"
      EVals <- ExpectedValues(tf, c(input$Analyzed_TF_brush1$xmin / 60, input$Analyzed_TF_brush1$xmax / 60),
                              weight = Data$Weight, use.coherence = Data$Threshold,
                              thr = Data$Coherence)
      paste("Estimate at HF band between", round(ifelse(input$Analyzed_TF_brush1$xmin > 0,  input$Analyzed_TF_brush1$xmin/ 60, 0), 3), "and", round(input$Analyzed_TF_brush1$xmax / 60, 3), "min:", round(EVals[1],3), "ms/mmHg")
    }
  })
  
  output$DWT_Estimate_LF<- renderText({
    if(input$subject_input !="No subjects have been loaded" & !is.null(input$Analyzed_TF_brush1)){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Data <- framework$"General Data"
      Analysis <- framework$Analyses[[chosen_analysis]]
      tf <- Analysis$BRS$DWT
      tf$Time <- Analysis$Data[,1]
      tf$type <- "TFun_dwt"
      EVals <- ExpectedValues(tf, c(input$Analyzed_TF_brush1$xmin / 60, input$Analyzed_TF_brush1$xmax / 60),
                              weight = Data$Weight, use.coherence = Data$Threshold,
                              thr = Data$Coherence)
      paste("Estimate at LF band between", round(ifelse(input$Analyzed_TF_brush1$xmin > 0,  input$Analyzed_TF_brush1$xmin/ 60, 0), 3), "and", round(input$Analyzed_TF_brush1$xmax / 60, 3), "min:", round(EVals[2],3), "ms/mmHg")
    }
    })
  
  output$CWT_Estimate_LF<- renderText({
    if(input$subject_input !="No subjects have been loaded" & !is.null(input$Analyzed_TF_brush1)){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Data <- framework$"General Data"
      Analysis <- framework$Analyses[[chosen_analysis]]
      tf <- Analysis$BRS$AvgCWT
      tf$Time <- Analysis$Data[,1]
      tf$type <- "TFun_dwt"
      tf <- AssembleCwtTransferFun(framework, chosen_analysis)
      tf <- SplitByCoherence(tf, thr = input$coherence_val, phase.rest = NULL)
      tf$Time <- Analysis$Data[,1]
      tf$type <- "TFun_dwt"
      EVals <- ExpectedValues(tf, c(input$Analyzed_TF_brush1$xmin / 60, input$Analyzed_TF_brush1$xmax / 60),
                              weight = FALSE, use.coherence = Data$Threshold,
                              thr = Data$Coherence)
      paste("Estimate at LF band between", round(ifelse(input$Analyzed_TF_brush1$xmin > 0,  input$Analyzed_TF_brush1$xmin/ 60, 0), 3), "and", round(input$Analyzed_TF_brush1$xmax / 60, 3), "min:", round(EVals[2],3), "ms/mmHg")
    }
  })
  
  
  output$Estimate_HF <- renderText({
    if(input$subject_input !="No subjects have been loaded" & !is.null(input$brush_raw)){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Data <- framework$"General Data"
      Analysis <- framework$Analyses[[chosen_analysis]]
      hrv <- Analysis$HRV
      hrv$Time <- Analysis$Data[,1]
      hrv$type <- "TFun_dwt"
      EVals <- ExpectedValues(hrv, c(input$brush_raw$xmin / 60, input$brush_raw$xmax / 60),
                              weight = Data$Weight, use.coherence = Data$Threshold,
                              thr = Data$Coherence)
      paste("Estimate at HF band between", round(ifelse(input$brush_raw$xmin > 0,  input$brush_raw$xmin/ 60, 0), 3), "and", round(input$brush_raw$xmax / 60, 3), "min:", round(EVals[1],3), "ms2")
    }
  })
  
  output$Estimate_LF<- renderText({
    if(input$subject_input !="No subjects have been loaded" & !is.null(input$brush_raw)){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Data <- framework$"General Data"
      Analysis <- framework$Analyses[[chosen_analysis]]
      hrv <- Analysis$HRV
      hrv$Time <- Analysis$Data[,1]
      hrv$type <- "TFun_dwt"
      EVals <- ExpectedValues(hrv, c(input$brush_raw$xmin / 60, input$brush_raw$xmax / 60),
                              weight = Data$Weight, use.coherence = Data$Threshold,
                              thr = Data$Coherence)
      paste("Estimate at LF band between", round(ifelse(input$brush_raw$xmin > 0,  input$brush_raw$xmin/ 60, 0), 3), "and", round(input$brush_raw$xmax / 60, 3), "min:", round(EVals[2],3), "ms2")
    }
  })
  
  output$Estimate_HR<- renderText({
    if(input$subject_input !="No subjects have been loaded" & !is.null(input$brush_raw)){
      framework <- isolate(database$framework)
      RAW <- isolate(RAW_database$RAW)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Data <- framework$"General Data"
      Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
      raw <- RAW$Analyses[[chosen_analysis]]$Data
      fun <- list(Time = raw[,"Time"], HR = 60000/raw[,"RR"], SBP = raw[,"SBP"])
      select_time <- fun$Time[(fun$Time >= input$brush_raw$xmin) &
                                 (fun$Time <= input$brush_raw$xmax )]
      select_time <- match(select_time, fun$Time)
      HR <- mean(fun$HR[select_time])
      paste("Mean HR between", round(ifelse(input$brush_raw$xmin > 0,  input$brush_raw$xmin/ 60, 0), 3), "and", round(input$brush_raw$xmax / 60, 3), "min:", round(HR,3), "bpm")
    }
  })
  
  output$Estimate_SBP<- renderText({
    if(input$subject_input !="No subjects have been loaded" & !is.null(input$brush_raw)){
      framework <- isolate(database$framework)
      RAW <- isolate(RAW_database$RAW)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      Data <- framework$"General Data"
      Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
      raw <- RAW$Analyses[[chosen_analysis]]$Data
      fun <- list(Time = raw[,"Time"], HR = 60000/raw[,"RR"], SBP = raw[,"SBP"])
      select_time <- fun$Time[(fun$Time >= input$brush_raw$xmin ) &
                                (fun$Time <= input$brush_raw$xmax)]
      select_time <- match(select_time, fun$Time)
      SBP <- mean(fun$SBP[select_time])
      paste("Mean SBP between", round(ifelse(input$brush_raw$xmin > 0,  input$brush_raw$xmin/ 60, 0), 3), "and", round(input$brush_raw$xmax / 60, 3), "min:", round(SBP,3), "mmHg")
    }
  })
  
  
  
 ###################################################################################################
  
 ###################### 12. SIGNIFICANCE TESTINGS #################################################
  
  output$pvalue_HF <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        tf <- framework$Analyses[[chosen_analysis]]$BRS$DWT
        tf$Time <- Data$Data[,1]
        tf$type <- "TFun_dwt"
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        evaluation <- TestIndBRS(tf, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        sig <- ifelse(evaluation[2,1] <= 0.05, "Significant", "No significant")
        if(evaluation[2,1] <= 0.001){
          code <- "***"
        } else if(evaluation[2,1] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,1] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste(sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,1], " ms/mmHg (SE ", evaluation[3,1], " ms/mmHg), with a p value of ", 
                      evaluation[2,1], " (", code, ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  output$pvalue_HF_cwt <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        tf <- framework$Analyses[[chosen_analysis]]$BRS$AvgCWT
        tf$Time <- Data$Data[,1]
        tf$type <- "TFun_dwt"
        tf <- AssembleCwtTransferFun(framework, chosen_analysis)
        tf <- SplitByCoherence(tf, thr = input$coherence_val, phase.rest = NULL)
        tf$Time <- Data$Data[,1]
        tf$type <- "TFun_dwt"
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        #evaluation <- TestIndBRS(tf, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        evaluation <- TestIndBRS(tf, time_flags1, time_flags2, weight = FALSE)
        sig <- ifelse(evaluation[2,1] <= 0.05, "Significant", "No significant")
        if(evaluation[2,1] <= 0.001){
          code <- "***"
        } else if(evaluation[2,1] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,1] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste(sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,1], " ms/mmHg (SE ", evaluation[3,1], " ms/mmHg), with a p value of ", 
                      evaluation[2,1], " (", code, ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  output$pvalue_LF <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        tf <- framework$Analyses[[chosen_analysis]]$BRS$DWT
        tf$Time <- Data$Data[,1]
        tf$type <- "TFun_dwt"
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        evaluation <- TestIndBRS(tf, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        sig <- ifelse(evaluation[2,2] <= 0.05, "Significant", "No significant")
        if(evaluation[2,2] <= 0.001){
          code <- "***"
        } else if(evaluation[2,2] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,2] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste(sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,2], " ms/mmHg (SE ", evaluation[3,2], " ms/mmHg), with a p value of ", 
                      evaluation[2,2], " (", code , ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  output$pvalue_LF_cwt <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        tf <- framework$Analyses[[chosen_analysis]]$BRS$AvgCWT
        tf$Time <- Data$Data[,1]
        tf$type <- "TFun_dwt"
        tf <- AssembleCwtTransferFun(framework, chosen_analysis)
        tf$type <- "TFun_cwt"
        tf <- AssembleCwtTransferFun(framework, chosen_analysis)
        tf <- SplitByCoherence(tf, thr = input$coherence_val, phase.rest = NULL)
        tf$Time <- Data$Data[,1]
        tf$type <- "TFun_dwt"
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        #evaluation <- TestIndBRS(tf, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        evaluation <- TestIndBRS(tf, time_flags1, time_flags2, weight = FALSE)
        sig <- ifelse(evaluation[2,2] <= 0.05, "Significant", "No significant")
        if(evaluation[2,2] <= 0.001){
          code <- "***"
        } else if(evaluation[2,2] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,2] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste(sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,2], " ms/mmHg (SE ", evaluation[3,2], " ms/mmHg), with a p value of ", 
                      evaluation[2,2], " (", code , ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  output$pvalue_HRV_LF <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        hrv <- framework$Analyses[[chosen_analysis]]$HRV
        hrv$Time <- Data$Data[,1]
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        evaluation <- TestIndHRV(hrv, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        sig <- ifelse(evaluation[2,2] <= 0.05, "Significant", "No significant")
        if(evaluation[2,2] <= 0.001){
          code <- "***"
        } else if(evaluation[2,2] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,2] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste(sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,2], " ms2 (SE ", evaluation[3,2], " ms2), with a p value of ", 
                      evaluation[2,2], " (", code , ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  output$pvalue_HRV_HF <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        hrv <- framework$Analyses[[chosen_analysis]]$HRV
        hrv$Time <- Data$Data[,1]
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        evaluation <- TestIndHRV(hrv, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        sig <- ifelse(evaluation[2,1] <= 0.05, "Significant", "No significant")
        if(evaluation[2,1] <= 0.001){
          code <- "***"
        } else if(evaluation[2,1] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,1] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste(sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,1], " ms2 (SE ", evaluation[3,1], " ms2), with a p value of ", 
                      evaluation[2,1], " (", code , ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  output$pvalue_HRV_LFHF <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        hrv <- framework$Analyses[[chosen_analysis]]$HRV
        hrv$Time <- Data$Data[,1]
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        evaluation <- TestIndHRV(hrv, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        sig <- ifelse(evaluation[2,3] <= 0.05, "Significant", "No significant")
        if(evaluation[2,3] <= 0.001){
          code <- "***"
        } else if(evaluation[2,3] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,3] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste(sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,3], " (SE ", evaluation[3,3], " ), with a p value of ", 
                      evaluation[2,3], " (", code , ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  output$pvalue_HR <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      RAW <- isolate(RAW_database$RAW)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        raw_data <- RAW$Analyses[[chosen_analysis]]$Data
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        evaluation <- TestIndRaw(raw_data, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        sig <- ifelse(evaluation[2,1] <= 0.05, "Significant", "No significant")
        if(evaluation[2,1] <= 0.001){
          code <- "***"
        } else if(evaluation[2,1] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,1] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste("HR: ", sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,1], " bpm (SE ", evaluation[3,1], " bpm), with a p value of ", 
                      evaluation[2,1], " (", code , ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  output$pvalue_SBP <- renderText({
    if(input$interval_input != "No intervals have been set" & input$control_input != "No control has been set"){
      framework <- isolate(database$framework)
      RAW <- isolate(RAW_database$RAW)
      analysis_choices <- ShowIndexes(framework, "analyses")[2,]
      chosen_analysis <- match(input$subject_input, analysis_choices)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      interval <- match(input$interval_input, intervals)
      control <- match(input$control_input, intervals)
      if(!is.na(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis]) &
         !is.na(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis])){
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        raw_data <- RAW$Analyses[[chosen_analysis]]$Data
        time_flags1 <- c(framework$ExpectedVals[[interval]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[interval]]$Time_DWT[2,chosen_analysis])
        time_flags2 <- c(framework$ExpectedVals[[control]]$Time_DWT[1,chosen_analysis],
                         framework$ExpectedVals[[control]]$Time_DWT[2,chosen_analysis])
        evaluation <- TestIndRaw(raw_data, time_flags1, time_flags2, weight = framework$`General Data`$Weight)
        sig <- ifelse(evaluation[2,2] <= 0.05, "Significant", "No significant")
        if(evaluation[2,2] <= 0.001){
          code <- "***"
        } else if(evaluation[2,2] <= 0.01){ 
          code <- "**"
        } else if(evaluation[2,2] <= 0.05){ 
          code <- "*"
        } else { 
          code <- "ns"
        }  
        text <- paste("SBP: ", sig, " difference in estimates between interval ", input$control_input, " (set as control) and
                      interval ", input$interval_input, ": ", evaluation[1,2], " mmHg (SE ", evaluation[3,2], " mmHg), with a p value of ", 
                      evaluation[2,2], " (", code , ")", sep = "")
        return(text)
      }
      
    }
    
  })
  
  ########################################################################################################
    
  ############## 13. DOUBLE-CLICK TO SELECT INTERVALS #################################################
  observeEvent(input$dbc_raw, {
    check_brush <- !is.null(input$brush_raw)
    check_subject <- input$subject_input !="No subjects have been loaded"
    check_interval <- input$interval_input != "No intervals have been set"
    if(check_subject & check_interval & check_brush){
      framework <- isolate(database$framework)
      analyses <- ShowIndexes(framework, "analyses")[2,]
      intervals <- ShowIndexes(framework, "intervals")[2,]
      analysis <- match(input$subject_input, analyses)
      interval <- match(input$interval_input, intervals)
      framework <- GetExpectedValues(framework, analysis, interval, 
                                       c(input$brush_raw$xmin / 60, 
                                         input$brush_raw$xmax / 60))
      
      database$framework <- framework
      #########################################################################################################
      
################### 14. PLOT ESTIMATES ########################################################################
      
    output$ExpectedVals_DWT_Plot <- renderPlot({
        framework <- isolate(database$framework)
        analyses <- ShowIndexes(framework, "analyses")[2,]
        analysis <- match(input$subject_input, analyses)
        restrict <- NULL
        for(n in 1:length(framework$ExpectedVals)){
            if(length(framework$ExpectedVals[[n]]$DWT) != 0 && 
               !is.na(framework$ExpectedVals[[n]]$DWT[1,analysis])) restrict <- c(restrict, n)
        }
        if(!is.null(restrict)){
           PlotIndexesFromAnalysis(framework, analysis, "dwt", newPlot = FALSE,
                                    restrict = restrict, ymax = input$maxEst_dwt) 
        }
    })
    }
  })
  
  output$ExpectedVals_DWT_Plot <- renderPlot({
    check_subject <- input$subject_input !="No subjects have been loaded"
    check_interval <- input$interval_input != "No intervals have been set"
    if(check_subject & check_interval){
    framework <- isolate(database$framework)
    analyses <- ShowIndexes(framework, "analyses")[2,]
    analysis <- match(input$subject_input, analyses)
    restrict <- NULL
    for(n in 1:length(framework$ExpectedVals)){
      if(length(framework$ExpectedVals[[n]]$DWT) != 0 && 
         !is.na(framework$ExpectedVals[[n]]$DWT[1,analysis])) restrict <- c(restrict, n)
    }
    if(!is.null(restrict)){
      PlotIndexesFromAnalysis(framework, analysis, "dwt", newPlot = FALSE,
                              restrict = restrict, ymax = input$maxEst_dwt) 
    }
    }
  })
  #######################################################################################################
  
################# 15. PERFORM STATISTICAL TESTS ###############################################################
  
  observeEvent(input$confirm_test, {
    check_test <- input$test_var_in_test !="No testing variable has been selected"
    check_con <- input$con_var_in_test != "No control variable has been selected"
    if(check_test & check_con){
      framework <- isolate(database$framework)
      intervals <- ShowIndexes(framework, "intervals")[2,]
      test <- match(input$test_var_in_test , intervals)
      control <- match(input$con_var_in_test, intervals)
      framework <- TestGroups(framework, test, control, name = input$test_name, method = "t.test")
      framework <- TestHRV(framework, test, control, name = input$test_name, method = "t.test", normalize = input$norm_hrv)
      tests <- ShowIndexes(framework, "tests")[2,]
      updateSelectInput(session, "select_testHRV", "Select test", choices = c("No test has been selected", tests))
      updateSelectInput(session, "select_test", "Select test", choices = c("No test has been selected", tests))
      text_ntests <- paste("Number of tests performed:", length(framework$Tests), "tests.")
      output$text_ntests <- renderText({text_ntests})
      database$framework <- framework
    }
    
  })
  
###########################################################################################################
  
################### 16. PLOT TEST RESULTS ################################################################
  
  output$testing_resultsHRV <- renderPlot({
    if(input$select_testHRV != "No test has been selected"){
      framework <- isolate(database$framework)
      tests <- ShowIndexes(framework, "tests")[2,]
      test <- match(input$select_testHRV , tests)
      results <- PlotHRVTestResults(framework, test, newPlot = FALSE)
      return(results)
    }
    
  })
    
  
  output$testing_results <- renderPlot({
    if(input$select_test != "No test has been selected"){
      framework <- isolate(database$framework)
      tests <- ShowIndexes(framework, "tests")[2,]
      test <- match(input$select_test , tests)
      results <- PlotTestResults(framework, test, newPlot = FALSE)
      return(results)
    }
    
  })
  
  
##############################################################################################  
############### 17. WHAT HAPPENS WHEN THE APP IS CLOSED ######################################  
  
  session$onSessionEnded(function(){
    stopApp()
    if(.Platform$GUI != "RStudio") q(save = "no")
  }) 
#############################################################################################
  
}

############# END OF SERVER#################################################################

# RUN BaroWavelet
shinyApp(ui = ui, server = server)
