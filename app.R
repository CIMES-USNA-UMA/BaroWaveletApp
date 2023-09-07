#
# BaroWaveletApp
#
# Author: ALVARO CHAO ECIJA


# LOAD PACKAGES ##############################################################################
library(shiny)
library(tools) # Part of R. Used for file extension checks
library(ggplot2) # To improve plots
library(BaroWavelet)
##############################################################################################

##############################################################################################
####       BAROWAVELET'S UI            #######################################################
##############################################################################################
ui <- fluidPage(
  titlePanel("BaroWavelet"),
  fluidRow(column(
    6,
    wellPanel(
      h2("Upload file with cardiovascular data"),
      p("Upload and store data from a specific subject"),
      
      uiOutput("data_file"),
      tags$hr(),
      radioButtons(
        "subject_data_sep",
        "Choose separator (for CSV files)",
        choices = c(
          Semicolon = ";",
          Comma = ",",
          Tab =
            "\t"
        ),
        selected = ";"
      ),
      tags$hr(),
      radioButtons(
        "preprocessing",
        "Choose preprocessing algorithm",
        choices = c(
          "Filter with RHRV and Interpolate",
          "Interpolate",
          "Do not Interpolate"
        ),
        selected = "Filter with RHRV and Interpolate"
      ),
      numericInput("int_freq", "Frequency", value = 4, width = 70),
      tags$hr(),
      textInput("subject_name_input", "Identifiers", value = "Subjects"),
      tags$hr(),
      actionButton("upload_subject_data", "Upload Subject Data")
    )
    
  ),
  column(
    6,
    
    wellPanel(
      h2("Study settings"),
      p(
        "Change global settings. These settings will be applied to every analysis."
      ),
      textInput("framework_name", "Name", value = "BaroWavelet Study"),
      tags$hr(),
      fluidRow(
        column(
          4,
          numericInput(
            "HF_val",
            "HF",
            value = 0.4,
            min = 0.15,
            step = 0.001
          )
        ),
        column(
          4,
          numericInput(
            "LF_val",
            "LF",
            value = 0.15,
            min = 0.04,
            max = 0.15,
            step = 0.001
          )
        ),
        column(
          4,
          numericInput(
            "VLF_val",
            "VLF",
            value = 0.04,
            min = 0,
            max = 0.04,
            step = 0.0001
          )
        ),
      ),
      tags$hr(),
      fluidRow(
        column(
          4,
          selectInput(
            "wavelet",
            "DWT wavelet",
            choices = c(
              "bl14",
              "bl20",
              "bs3.1",
              "d2",
              "d4",
              "d8",
              "d16",
              "fk4",
              "fk6",
              "fk14",
              "fk22",
              "haar",
              "la8" ,
              "la16",
              "la20",
              "mb4",
              "mb8",
              "mb16",
              "mb24",
              "w4"
            ),
            selected = "d8"
          )
        ),
        column(
          4,
          numericInput(
            "dwt_error",
            "Error (tolerance)",
            value = 0.0005,
            min = 0.0001,
            step = 0.001
          )
        ),
        column(
          4,
          selectInput(
            "dwt_er_type",
            "Error type",
            choices = c("Absolute", "Relative"),
            selected = "Absolute"
          )
        ),
      ),
      tags$hr(),
      fluidRow(column(
        6,
        selectInput(
          "cwt_type",
          "Compute CWT:",
          choices = c("Transfer Function", "Alpha Index"),
          selected = "Transfer Function"
        )
      ),
      column(
        6,
        checkboxInput(
          "use_thr",
          "Use coherence threshold (for Continuous Wavelet Transform)",
          value = TRUE
        )
      )),
      tags$hr(),
      selectInput(
        "index_method",
        "Obtain individual indices with:",
        choices = c("Median", "Mean"),
        selected = "Median"
      ),
      tags$hr(),
      actionButton("change_main_sets", "Change Framework Settings"),
      
    )
  )),
  fluidRow(column(
    6,
    wellPanel(
      h3("Upload / Download Study"),
      br(),
      uiOutput("study_file"),
      actionButton("confirm_study", "Use Uploaded Study"),
      tags$hr(),
      downloadButton("rds", "Download Study")
    )
  ),
  column(
    6,
    wellPanel(
      # Panel to create Intervals
      h3("Set Intervals"),
      br(),
      textInput("interval_names", "Name the Intervals", value = "Intervals"),
      actionButton("confirm_interval_names", "Confirm Interval Names")
    )
  )),
  fluidRow(column(12,
                  wellPanel(
                    # Box to display subject info
                    fluidRow(
                      column(
                        6,
                        h3("Study Info"),
                        tags$hr(),
                        h4(textOutput("text_globalname")),
                        h4(textOutput("text_n")),
                        h4(textOutput("text_nintervals")),
                        h4(textOutput("text_ntests"))
                      ),
                      column(
                        6,
                        h3("Current settings"),
                        tags$hr(),
                        h4(textOutput("text_HF")),
                        h4(textOutput("text_LF")),
                        h4(textOutput("text_wavelet")),
                        h4(textOutput("text_error")),
                        h4(textOutput("text_cwt_type")),
                        h4(textOutput("text_thr")),
                        h4(textOutput("text_index_m"))
                      )
                    )
                  ))),
  fluidRow(column(
    12,
    wellPanel(
      h2("Subject Analysis"),
      tags$hr(),
      fluidRow(
        column(
          4,
          selectInput("subject_input", "Select Subject",
                      choices = "No subjects have been loaded")
        ),
        column(
          4,
          selectInput("interval_input", "Select Interval",
                      choices = "No intervals have been set")
        ),
        column(
          4,
          selectInput("control_input", "Set Interval as Control",
                      choices = "No control has been set")
        )
      ),
      h3("Recordings"),
      tags$hr(),
      wellPanel(
        style = "background:white",
        plotOutput("Raw", 
                   brush = "brush_raw", 
                   dblclick = "dbc_raw")
      ),
      fluidRow(column(6,
                      h4(
                        textOutput("Estimate_HR")
                      ),
                      br(),
                      h4(
                        textOutput("pvalue_HR")
                      )),
               column(6,
                      h4(
                        textOutput("Estimate_SBP")
                      ),
                      br(),
                      h4(
                        textOutput("pvalue_SBP")
                      ))),
      br(),
      fluidRow(
        column(
          12,
          h3("Baroreflex Sensitivity (CWT)"),
          tags$hr(),
          sidebarLayout(
            sidebarPanel(
              width = 2,
              style = "height: 90vh, overflow-y: auto;",
              sliderInput(
                "coherence_val",
                "Coherence Threshold",
                min = 0,
                max = 1,
                value = 0.5
              )
            ),
            mainPanel(width = 10,
                      imageOutput("CWT_plot"))
          ),
          br(),
          br(),
          h3("Time-averaged Gain Transfer Function"),
          br(),
          fluidRow(column(6,
                          h3("HF component"),
                          br()),
                   column(6,
                          h3("LF component"),
                          br())),
          wellPanel(style = "background:white",
                    fluidRow(
                      column(
                        6,
                        plotOutput(
                          "Analyzed_brs_Plot1_cwt",
                          brush = brushOpts(id = "Analyzed_brs_brush1",
                                            resetOnNew = TRUE),
                          dblclick = "Analyzed_brs_dbc1"
                        )
                      ),
                      column(
                        6,
                        plotOutput(
                          "Analyzed_brs_Plot2_cwt",
                          brush = brushOpts(id = "Analyzed_brs_brush1",
                                            resetOnNew = TRUE),
                          dblclick = "Analyzed_brs_dbc1"
                        )
                      )
                    ),
                    fluidRow(
                      column(
                        6,
                        h4(textOutput("CWT_Estimate_HF")),
                        br(),
                        h4(textOutput("interval_HF_cwt")),
                        br(),
                        h4(textOutput("pvalue_HF_cwt"))
                      ),
                      column(
                        6,
                        h4(textOutput("CWT_Estimate_LF")),
                        br(),
                        h4(textOutput("interval_LF_cwt")),
                        br(),
                        h4(textOutput("pvalue_LF_cwt"))
                      )
                    )),
          br(),
          h3("Phase shift"),
          br(),
          fluidRow(column(6,
                          h3("HF component"),
                          br()),
                   column(6,
                          h3("LF component"),
                          br())),
          wellPanel(style = "background:white",
                    fluidRow(
                      column(
                        6,
                        plotOutput(
                          "phase1_cwt",
                          brush = brushOpts(id = "Analyzed_brs_brush1",
                                            resetOnNew = TRUE),
                          dblclick = "Analyzed_brs_dbc1"
                        )
                      ),
                      column(
                        6,
                        plotOutput(
                          "phase2_cwt",
                          brush = brushOpts(id = "Analyzed_brs_brush1",
                                            resetOnNew = TRUE),
                          dblclick = "Analyzed_brs_dbc1"
                        )
                      )
                    ))
        ),
        br(),
        tags$hr(),
        br(),
        h3("Baroreflex Sensitivity (DWT)"),
        br(),
        tags$hr(),
        br(),
        fluidRow(column(6,
                        h3("HF component"),
                        br()),
                 column(6,
                        h3("LF component"),
                        br())),
        wellPanel(style = "background:white",
                  fluidRow(
                    column(
                      6,
                      plotOutput(
                        "Analyzed_brs_Plot1",
                        brush = brushOpts(id = "Analyzed_brs_brush1",
                                          resetOnNew = TRUE),
                        dblclick = "Analyzed_brs_dbc1"
                      )
                    ),
                    column(
                      6,
                      plotOutput(
                        "Analyzed_brs_Plot2",
                        brush = brushOpts(id = "Analyzed_brs_brush1",
                                          resetOnNew = TRUE),
                        dblclick = "Analyzed_brs_dbc1"
                      )
                    )
                  ),
                  fluidRow(
                    column(
                      6,
                      h4(textOutput("DWT_Estimate_HF")),
                      br(),
                      h4(textOutput("interval_HF_dwt")),
                      br(),
                      h4(textOutput("pvalue_HF"))
                    ),
                    column(
                      6,
                      h4(textOutput("DWT_Estimate_LF")),
                      br(),
                      h4(textOutput("interval_LF_dwt")),
                      br(),
                      h4(textOutput("pvalue_LF"))
                    )
                  )),
        tags$hr(),
        sidebarLayout(
          sidebarPanel(
            width = 2,
            style = "height: 90vh, overflow-y: auto;",
            sliderInput(
              "maxEst_dwt",
              "Max value",
              min = 0,
              max = 200,
              value = 60,
              step = 0.05
            )
          ),
          mainPanel(
            style = "background:white",
            width = 9,
            plotOutput("IndividualIndices_DWT_Plot")
          )
        )
      )
    ),
    br(),
    fluidRow(column(
      12,
      wellPanel(
        h3("Heart Rate Variability (DWT)"),
        br(),
        br(),
        h3("LF/HF ratio"),
        br(),
        wellPanel(
          style = "background:white",
          plotOutput("hrv_LFHF", 
                     brush = "brush_raw", 
                     dblclick = "dbc_raw"
                     )
        ),
        fluidRow(column(
          12,
          h4(textOutput("interval_HRV_LFHF")),
          br(),
          h4(textOutput("pvalue_HRV_LFHF"))
        )),
        fluidRow(column(6,
                        h3("HF band (vagal)"),
                        br()),
                 column(6,
                        h3(
                          "LF band (sympathetic)"
                        ),
                        br())),
        wellPanel(style = "background:white",
                  fluidRow(
                    column(
                      6,
                      br(),
                      plotOutput("hrv_HF", 
                                 brush = "brush_raw", 
                                 dblclick = "dbc_raw"
                                 )
                    ),
                    column(
                      6,
                      br(),
                      plotOutput("hrv_LF", 
                                 brush = "brush_raw",
                                 dblclick = "dbc_raw"
                                 )
                    )
                  )),
        fluidRow(
          column(
            6,
            h4(textOutput("Estimate_HF")),
            br(),
            h4(textOutput("interval_HRV_HF")),
            br(),
            h4(textOutput("pvalue_HRV_HF"))
          ),
          column(
            6,
            h4(textOutput("Estimate_LF")),
            br(),
            h4(textOutput("interval_HRV_LF")),
            br(),
            h4(textOutput("pvalue_HRV_LF"))
          )
        )
      )
    ))
  )),
  
  fluidRow(column(
    12,
    wellPanel(
      h3("Test Variables"),
      br(),
      fluidRow(
        column(4, textInput("test_name", "Name the Test", value = "Test")),
        column(
          4,
          selectInput("test_var_in_test", "Select testing variable",
                      choices = "No testing variable has been selected")
        ),
        column(
          4,
          selectInput("con_var_in_test", "Select control variable",
                      choices = "No control variable has been selected")
        )
      ),
      fluidRow(
        column(4, actionButton("confirm_test", "Test")),
        column(4, checkboxInput("norm_hrv", "Normalize HRV", value = FALSE)),
        column(
          4,
          checkboxInput("show_paired", "Show paired interactions", value = FALSE)
        )
      )
    )
  )),
  
  fluidRow(column(
    6,
    wellPanel(
      h3("HRV Test Results"),
      selectInput("select_testHRV", "Select test",
                  choices = "No test has been selected"),
      br(),
      wellPanel(style = "background:white",
                plotOutput("testing_resultsHRV"))
    )
  ),
  column(
    6,
    wellPanel(
      h3("BRS Test Results"),
      selectInput("select_test", "Select test",
                  choices = "No test has been selected"),
      br(),
      wellPanel(style = "background:white",
                plotOutput("testing_results"))
    )
  )),
  fluidRow(column(
    12,
    wellPanel(
      fluidRow(
        column(
          9,
          uiOutput("clinic_file"),
          downloadButton("C_template", "Download Table Template"),
          actionButton("load_clin", "Load Clinical Data"),
        ),
        column(
          3,
          radioButtons(
            "clin_data_sep",
            "Choose separator (for CSV files)",
            choices = c(
              Semicolon = ";",
              Comma = ",",
              Tab =
                "\t"
            ),
            selected = ";"
          )
        )
      ),
      tags$hr(),
      br(),
      fluidRow(
        column(
          4,
          selectInput(
            "select_variable1",
            "Select variable",
            choices = c("BRS (HF)", "BRS (LF)", "HRV (HF)", "HRV (LF)", "HRV (LF/HF)")
          )
        ),
        column(
          4,
          selectInput("select_variable2", "Select variable",
                      choices = "No variable has been selected")
        ),
        column(
          4,
          selectInput("select_variable3", "Select clinical variable",
                      choices = "No clinical variable has been selected")
        )
      ),
      actionButton("make_linear", "Model variables"),
      br(),
      wellPanel(style = "background:white",
                plotOutput("plot_linear")),
      h4(textOutput("model_stats"))
    )
  ))
)
##################### END OF UI #############################################################


##############################################################################################
####       BAROWAVELET'S SERVER             ##################################################
##############################################################################################


server <- function(input, output, session) {
  ####### 1. RENDER UIS #######################################################################
  options(shiny.maxRequestSize = 1000 * 1024 ^ 2) # To change default size for uploaded data
  output$data_file <- renderUI({
    fileInput(
      "data_file",
      "Upload data file",
      multiple = TRUE,
      accept = c(".csv",
                 ".txt")
    )
  })
  output$study_file <- renderUI({
    fileInput("study_file",
              "Upload study file",
              multiple = FALSE,
              accept = ".RDS")
  })
  output$clinic_file <- renderUI({
    fileInput(
      "clinic_file",
      "Upload clinical data",
      multiple = FALSE,
      accept = c(".csv",
                 ".txt")
    )
  })
  
  ##########################################################################################
  
  ###### 2. INITIAL SETUP #####################################################################
  
  database <- reactiveValues()
  framework <- BuildStructure(name = "BaroWavelet Study")
  database$framework <- framework
  text_globalname <-
    paste("Name of this study:", isolate(database$framework)$Name)
  text_n <-
    paste(
      "Number of subjects contained in this study:",
      isolate(database$framework)$n,
      "subjects."
    )
  text_nintervals <-
    paste(
      "Number of intervals analyzed in this study: ",
      length(isolate(database$framework)$IndividualIndices),
      "intervals."
    )
  text_ntests <-
    paste("Number of tests performed:",
          length(isolate(database$framework)$Tests),
          "tests.")
  text_HF <-
    paste(
      "HF Interval: from",
      framework$"General Data"$HF,
      "to",
      framework$"General Data"$LF,
      "Hz."
    )
  text_LF <-
    paste(
      "LF Interval: from",
      framework$"General Data"$LF,
      "to",
      framework$"General Data"$VLF,
      "Hz."
    )
  text_wavelet <-
    paste("DWT wavelet:", framework$"General Data"$Wavelet)
  text_error <-
    paste(
      "DWT tolerance: ",
      framework$"General Data"$Error,
      " (",
      framework$"General Data"$"Error Type",
      ")",
      sep = ""
    )
  text_cwt_type <-
    ifelse(
      framework$"General Data"$"CWT Type" == "alpha",
      "CWT BRS: Alpha index",
      "CWT BRS: Transfer function"
    )
  text_index_m <-
    ifelse(
      framework$"General Data"$"Index Method" == "median",
      "Individual indices obtained by: median",
      "Individual indices obtained by: mean"
    )
  text_thr <-
    "A coherence threshold is being used to calculate the estimates."
  output$text_globalname <- renderText({
    text_globalname
  })
  output$text_n <- renderText({
    text_n
  })
  output$text_nintervals <- renderText({
    text_nintervals
  })
  output$text_ntests <- renderText({
    text_ntests
  })
  output$text_HF <- renderText({
    text_HF
  })
  output$text_LF <- renderText({
    text_LF
  })
  output$text_wavelet <- renderText({
    text_wavelet
  })
  output$text_thr <- renderText({
    text_thr
  })
  output$text_error <- renderText({
    text_error
  })
  output$text_cwt_type <- renderText({
    text_cwt_type
  })
  output$text_index_m <- renderText({
    text_index_m
  })
  
  #########################################################################################################
  
  ###### 3. Update Numeric Outputs for Band Limits ######################################################
  
  observeEvent(input$LF_val, {
    tryCatch({
      updateNumericInput(
        session,
        "VLF_val",
        "VLF",
        value = input$VLF_val,
        min = 0,
        max = input$LF_val,
        step = 0.0001
      )
      updateNumericInput(
        session,
        "HF_val",
        "HF",
        value = input$HF_val,
        min = input$LF_val,
        step = 0.001
      )
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  observeEvent(input$VLF_val, {
    tryCatch({
      updateNumericInput(
        session,
        "LF_val",
        "LF",
        value = input$LF_val,
        min = input$VLF_val,
        max = input$HF_val,
        step = 0.001
      )
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  #########################################################################################################
  
  ###### 4. DOWNLOAD STUDY ###############################################################################
  
  output$rds <-
    downloadHandler(
      filename = function() {
        paste(isolate(database$framework)$Name, ".RDS", sep = "")
      },
      content = function(file) {
        framework <- isolate(database$framework)
        saveRDS(framework, file  = file)
      }
    )
  ##################################################################################################
  
  
  ####### 5. Change Study Settings ################################################################
  
  observeEvent(input$change_main_sets, {
    tryCatch({
      framework <- isolate(database$framework)
      framework$Name <- input$framework_name
      output$text_globalname <-
        renderText({
          paste("Name of this study:", framework$Name)
        })
      framework$"General Data"$Wavelet <- input$wavelet
      framework$"General Data"$Threshold <- input$use_thr
      framework$"General Data"$HF <- input$HF_val
      framework$"General Data"$LF <- input$LF_val
      framework$"General Data"$VLF <- input$VLF_val
      framework$"General Data"$Error <- input$dwt_error
      framework$"General Data"$"Error Type" <-
        ifelse(input$dwt_er_type == "Absolute",
               "absolute", "relative")
      framework$"General Data"$"CWT Type" <-
        ifelse(input$cwt_type == "Alpha",
               "alpha", "brs")
      framework$"General Data"$"Index Method" <-
        ifelse(input$index_method == "Median",
               "median", "mean")
      # Allow usage of simulation
      if (framework$Name == "Simulation") {
        Sim <- InterpolateData(DataSimulation(), f = input$int_freq)
        framework <- AddAnalysis(framework, name = "Simulation")
        framework <-
          AddDataToAnalysis(
            framework,
            length(framework$Analyses),
            time = Sim$Time,
            RR = Sim$RR,
            SBP = Sim$SBP
          )
        framework <- AnalyzeBRS(framework, length(framework$Analyses))
        framework <-
          AddAvgCwtData(framework, length(framework$Analyses))
        new_analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        updateSelectInput(session, "subject_input", "Select Subject", choices = new_analysis_choices)
        text_n <-
          paste("Number of subjects contained in this study:",
                framework$n,
                "subjects.")
        output$text_n <- renderText({
          text_n
        })
      }
      text_wavelet <-
        paste("DWT wavelet:", framework$"General Data"$Wavelet)
      text_thr <-
        ifelse(
          framework$"General Data"$Threshold,
          "A coherence threshold is being used to calculate the estimates.",
          "No coherence threshold is being used to calculate the estimates."
        )
      text_HF <-
        paste(
          "HF Interval: from",
          framework$"General Data"$HF,
          "to",
          framework$"General Data"$LF,
          "Hz."
        )
      text_LF <-
        paste(
          "LF Interval: from",
          framework$"General Data"$LF,
          "to",
          framework$"General Data"$VLF,
          "Hz."
        )
      text_error <-
        paste(
          "DWT tolerance: ",
          framework$"General Data"$Error,
          " (",
          framework$"General Data"$"Error Type",
          ")",
          sep = ""
        )
      text_cwt_type <-
        ifelse(
          framework$"General Data"$"CWT Type" == "alpha",
          "CWT BRS: Alpha index",
          "CWT BRS: Transfer function"
        )
      text_index_m <-
        ifelse(
          framework$"General Data"$"Index Method" == "median",
          "Individual indices obtained by: median",
          "Individual indices obtained by: mean"
        )
      output$text_HF <- renderText({
        text_HF
      })
      output$text_LF <- renderText({
        text_LF
      })
      output$text_wavelet <- renderText({
        text_wavelet
      })
      output$text_thr <- renderText({
        text_thr
      })
      output$text_error <- renderText({
        text_error
      })
      output$text_cwt_type <- renderText({
        text_cwt_type
      })
      output$text_index_m <- renderText({
        text_index_m
      })
      database$framework <- framework
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  ##############################################################################################################
  
  ############# 6. Load Study: #################################################################################
  
  observeEvent(input$confirm_study, {
    tryCatch({
      req(input$study_file)
      framework <- readRDS(input$study_file$datapath)
      output$study_file <- renderUI({
        fileInput(
          "study_file",
          "Upload study file",
          multiple = FALSE,
          accept = c(".RDS")
        )
      })
      text_globalname <- paste("Name of this study:", framework$Name)
      text_n <-
        paste("Number of subjects contained in this study:",
              framework$n,
              "subjects.")
      text_nintervals <-
        paste(
          "Number of intervals analyzed in this study: ",
          length(framework$IndividualIndices),
          "intervals."
        )
      text_ntests <-
        paste("Number of tests performed:",
              length(framework$Tests),
              "tests.")
      text_HF <-
        paste(
          "HF Interval: from",
          framework$"General Data"$HF,
          "to",
          framework$"General Data"$LF,
          "Hz."
        )
      text_LF <-
        paste(
          "LF Interval: from",
          framework$"General Data"$LF,
          "to",
          framework$"General Data"$VLF,
          "Hz."
        )
      text_wavelet <-
        paste("DWT wavelet:", framework$"General Data"$Wavelet)
      text_thr <-
        "A coherence threshold is being used to calculate the estimates."
      text_error <-
        paste(
          "DWT tolerance: ",
          framework$"General Data"$Error,
          " (",
          framework$"General Data"$"Error Type",
          ")",
          sep = ""
        )
      text_cwt_type <-
        ifelse(
          framework$"General Data"$"CWT Type" == "alpha",
          "CWT BRS: Alpha index",
          "CWT BRS: Transfer function"
        )
      text_index_m <-
        ifelse(
          framework$"General Data"$"Index Method" == "median",
          "Individual indices obtained by: median",
          "Individual indices obtained by: mean"
        )
      new_analysis_choices <-
        new_interval_choices <- new_test_choices <- NULL
      new_analysis_choices <-
        ShowLocatorIndices(framework, "analyses")[2, ]
      new_interval_choices <-
        ShowLocatorIndices(framework, "intervals")[2, ]
      new_test_choices <- ShowLocatorIndices(framework, "tests")[2, ]
      output$text_globalname <- renderText({
        text_globalname
      })
      output$text_n <- renderText({
        text_n
      })
      output$text_nintervals <- renderText({
        text_nintervals
      })
      output$text_ntests <- renderText({
        text_ntests
      })
      output$text_HF <- renderText({
        text_HF
      })
      output$text_LF <- renderText({
        text_LF
      })
      output$text_wavelet <- renderText({
        text_wavelet
      })
      output$text_thr <- renderText({
        text_thr
      })
      output$text_error <- renderText({
        text_error
      })
      output$text_cwt_type <- renderText({
        text_cwt_type
      })
      output$text_cwt_type <- renderText({
        text_cwt_type
      })
      updateSelectInput(session, "subject_input", "Select Subject", choices = new_analysis_choices)
      updateSelectInput(
        session,
        "interval_input",
        "Select Interval",
        choices = c("No intervals have been set", new_interval_choices)
      )
      updateSelectInput(
        session,
        "control_input",
        "Set Interval as Control",
        choices = c("No control has been set", new_interval_choices)
      )
      updateSelectInput(
        session,
        "test_var_in_test",
        "Select testing variable",
        choices = c(
          "No testing variable has been selected",
          new_interval_choices
        )
      )
      updateSelectInput(
        session,
        "con_var_in_test",
        "Select control variable",
        choices = c(
          "No control variable has been selected",
          new_interval_choices
        )
      )
      updateSelectInput(
        session,
        "select_variable2",
        "Select variable",
        choices = c("No variable has been selected", new_interval_choices)
      )
      updateSelectInput(
        session,
        "select_test",
        "Select test",
        choices = c("No test has been selected", new_test_choices)
      )
      updateSelectInput(
        session,
        "select_testHRV",
        "Select test",
        choices = c("No test has been selected", new_test_choices)
      )
      if (length(framework$Clinical) > 0) {
        clinic_names <- names(framework$Clinical[1, ])[-1]
        updateSelectInput(
          session,
          "select_variable3",
          "Select clinical variable",
          choices = c("No clinical variable has been selected", clinic_names)
        )
      }
      database$framework <- framework
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  #############################################################################################################
  
  ############# 7. Create Intervals ###########################################################################
  
  observeEvent(input$confirm_interval_names, {
    tryCatch({
      framework <- isolate(database$framework)
      names <- strsplit(input$interval_names, ", ")[[1]]
      if (names[1] != "Intervals") {
        for (n in 1:length(names)) {
          framework <- AddTimeInterval(framework, name = names[n])
        }
        text_nintervals <-
          paste(
            "Number of intervals analyzed in this study: ",
            length(framework$IndividualIndices),
            "intervals."
          )
        output$text_nintervals <- renderText({
          text_nintervals
        })
        choices <- ShowLocatorIndices(framework, "intervals")[2,]
        updateSelectInput(
          session,
          "interval_input",
          "Select Interval",
          choices = c("No intervals have been set", choices)
        )
        updateSelectInput(
          session,
          "control_input",
          "Set Interval as Control",
          choices = c("No control has been set", choices)
        )
        updateSelectInput(
          session,
          "test_var_in_test",
          "Select testing variable",
          choices = c("No testing variable has been selected", choices)
        )
        updateSelectInput(
          session,
          "con_var_in_test",
          "Select control variable",
          choices = c("No control variable has been selected", choices)
        )
        updateSelectInput(
          session,
          "select_variable2",
          "Select variable",
          choices = c("No variable has been selected", choices)
        )
        database$framework <- framework
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  ##############################################################################################################
  
  ############## 8. LOAD INDIVIDUAL SUBJECT DATA ##############################################################
  observeEvent(input$upload_subject_data,
               {
                 tryCatch({
                   names <- strsplit(input$subject_name_input, ", ")[[1]]
                   framework <- isolate(database$framework)
                   req(input$data_file)
                   if (nrow(input$data_file) == length(names)) {
                     N <- length(names)
                     for (n in 1:N) {
                       if (file_ext(input$data_file[[n, "datapath"]]) == "csv") {
                         data <- read.csv(
                           input$data_file[[n, "datapath"]],
                           header = TRUE,
                           sep = input$subject_data_sep,
                           quote  = ""
                         )
                       } else if (file_ext(input$data_file[[n, "datapath"]]) == "txt") {
                         data <- read.table(input$data_file[[n, "datapath"]], header = TRUE)
                       }
                       # Check if time is presented both in seconds and minute format (and fix it)
                       if (!is.null(data$Time) &&
                           all(data$Time[1:20] == sort(data$Time[1:20])) &&
                           # Currently it examines the beginning of the vector
                           sum(diff(data$Time) < 0) == 1) {
                         brpt <- match(TRUE, diff(data$Time) < 0) + 1
                         data$Time[brpt:NROW(data$Time)] <-
                           data$Time[brpt:NROW(data$Time)] * 60
                       }
                       # Check both time and RR
                       if (is.null(data$RR) & !is.null(data$Time)) {
                         if (!all(data$Time == sort(data$Time))) {
                           data$Time <- cumsum(data$Time)
                         }
                         data$RR <-
                           c(data$Time[[1]] * 1000, diff(data$Time * 1000))
                       } else if (is.null(data$Time) &
                                  !is.null(data$RR)) {
                         data$Time <- cumsum(data$RR / 1000)
                       }
                       if (input$preprocessing == "Interpolate") {
                         #data <- PreprocessData(data, use.RHRV = FALSE)
                         data <- InterpolateData(data, input$int_freq)
                       } else if (input$preprocessing == "Filter with RHRV and Interpolate") {
                         data <- PreprocessData(data, use.RHRV = TRUE)
                         data <- InterpolateData(data, input$int_freq)
                       }
                       framework <-
                         AddAnalysis(framework, name = names[[n]])
                       framework <-
                         AddDataToAnalysis(
                           framework,
                           length(framework$Analyses),
                           time = data$Time,
                           RR = data$RR,
                           SBP = data$SBP
                         )
                       framework <-
                         AnalyzeBRS(framework, length(framework$Analyses))
                       framework <-
                         AddAvgCwtData(framework, length(framework$Analyses))
                     }
                     output$data_file <- renderUI({
                       fileInput(
                         "data_file",
                         "Upload data file",
                         multiple = TRUE,
                         accept = c(".csv",
                                    ".txt")
                       )
                     })
                     new_analysis_choices <-
                       ShowLocatorIndices(framework, "analyses")[2, ]
                     updateSelectInput(session, "subject_input", "Select Subject", choices = new_analysis_choices)
                     database$framework <- framework
                     text_n <-
                       paste(
                         "Number of subjects contained in this study:",
                         isolate(database$framework)$n,
                         "subjects."
                       )
                     output$text_n <- renderText({
                       text_n
                     })
                     
                   }
                 }, error = function(barowavelet_error) {
                   showNotification(paste0(barowavelet_error),
                                    type = "error",
                                    duration = NULL)
                 })
               })
  #############################################################################################################
  
  ################### 9. Load and Model Clinical Data ###################################################################
  
  observeEvent(input$load_clin, {
    tryCatch({
      framework <- isolate(database$framework)
      req(input$clinic_file)
      if (file_ext(input$clinic_file$datapath) == "csv") {
        data <- read.csv(
          input$clinic_file$datapath,
          header = TRUE,
          sep = input$clin_data_sep,
          quote  = ""
        )
      } else if (file_ext(input$clinic_file$datapath) == "txt") {
        data <- read.table(input$clinic_file$datapath, header = TRUE)
      }
      framework$Clinical <- data
      database$framework <- framework
      output$clinic_file <- renderUI({
        fileInput(
          "clinic_file",
          "Upload clinical data",
          multiple = FALSE,
          accept = c(".csv",
                     ".txt")
        )
      })
      new_analysis_choices <- colnames(data)[-1]
      updateSelectInput(
        session,
        "select_variable3",
        "Select clinical variable",
        choices = c(
          "No clinical variable has been selected",
          new_analysis_choices
        )
      )
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  #Model clinical data
  
  observeEvent(input$make_linear, {
    tryCatch({
      framework <- isolate(database$framework)
      if ((input$select_variable2 != "No variable has been selected") &
          (input$select_variable3 != "No clinical variable has been selected")) {
        if (input$select_variable1 == "BRS (HF)") {
          type = "BRS"
          band = "HF"
        } else if (input$select_variable1 == "BRS (LF)") {
          type = "BRS"
          band = "LF"
        } else if (input$select_variable1 == "HRV (LF)") {
          type = "HRV"
          band = "LF"
        } else if (input$select_variable1 == "HRV (HF)") {
          type = "HRV"
          band = "HF"
        } else if (input$select_variable1 == "HRV (LF/HF)") {
          type = "HRV"
          band = "LFHF"
        }
        clin <- colnames(framework$Clinical)[-1]
        variable <- match(input$select_variable3, clin)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$select_variable2, intervals)
        model <-
          ModelClinicalData(
            framework,
            type = type,
            band = band,
            segment = interval,
            variable = variable
          )
        output$plot_linear <- renderPlot({
          model$Plot
        })
        output$model_stats <- renderText({
          paste(
            "R-squared value of",
            round(model$r, 4),
            "with a p-value of",
            round(model$p, 4)
          )
        })
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  ################# 10. MAKE SUBJECT-SPECIFIC PLOTS ##################################################
  ####################################################################################################
  
  
  ################### 10.1. CARDIOVASCULAR DATA SERIES PLOTS (IBI, BP) ##############################
  output$Raw <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        raw_data <- framework$Analyses[[chosen_analysis]]$Data
        raw_data[, "RR"] <- 60000 / raw_data[, "RR"]
        result <-
          ggplot(data = data.frame(raw_data), aes(Time)) +
          geom_line(aes(y = RR, colour = "HR")) +
          geom_line(aes(y = SBP, colour = "SBP")) +
          theme(axis.title.y = element_blank())
        raw_time <- raw_data[, "Time"]
        Time <- framework$Analyses[[chosen_analysis]]$Data[, "Time"]
        if (max(raw_time) != max(Time) | min(raw_time) != min(Time)) {
          result <- result + annotate(
            "rect",
            fill = "red",
            alpha = 0.5,
            xmin =
              min(Time),
            xmax = max(Time),
            ymin = -Inf,
            ymax = Inf
          )
        }
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              result <- result + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              result <- result + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(result)
        } else {
          return(result)
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  #####################################################################################################
  
  ############## 10.2. LF/HF RATIO PLOT ###############################################################
  output$hrv_LFHF <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedHRV(
            framework,
            chosen_analysis,
            plotLF = TRUE,
            plotHF = TRUE,
            ratio = TRUE,
            newPlot = FALSE,
            use.ggplot = TRUE
          )
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  ###########################################################################################################
  
  
  ############### 10.3. HRV HF PLOT #########################################################################
  output$hrv_HF <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedHRV(
            framework,
            chosen_analysis,
            plotLF = FALSE,
            plotHF = TRUE,
            newPlot = FALSE,
            use.ggplot = TRUE
          )
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  ##############################################################################################
  
  
  ################## 10.4. HRV LF PLOT #########################################################
  output$hrv_LF <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedHRV(
            framework,
            chosen_analysis,
            plotHF = FALSE,
            plotLF = TRUE,
            newPlot = FALSE,
            use.ggplot = TRUE
          )
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  ##########################################################################################################
  
  
  
  ####################### 10.5.DWT ALPHA INDEX HF BAND #############################################################
  output$Analyzed_brs_Plot1 <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedBRS(framework,
                          chosen_analysis,
                          "dwt",
                          newPlot = FALSE,
                          plotLF = FALSE,
                          use.ggplot = TRUE)
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  ######################################################################################################
  
  
  
  ########### 10.6. CWT BRS PHASE PLOT HF BAND #################################################################
  output$phase1_cwt <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedBRS(
            framework,
            chosen_analysis,
            "cwt.phase",
            newPlot = FALSE,
            plotLF = FALSE,
            thr = input$coherence_val,
            use.ggplot = TRUE
          )
        Results <- Results + ylim(-pi, pi)
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  #####################################################################################################
  
  
  ############# 10.7. DWT ALPHA INDEX LF BAND #######################################################
  output$Analyzed_brs_Plot2 <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedBRS(framework,
                          chosen_analysis,
                          "dwt",
                          newPlot = FALSE,
                          plotHF = FALSE,
                          use.ggplot = TRUE)
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  ####################################################################################################
  
  
  
  ############# 10.8. CWT SCALE-AVERAGED GAIN TRANSFER FUNCTION HF BAND ##############################
  output$Analyzed_brs_Plot1_cwt <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedBRS(
            framework,
            chosen_analysis,
            "cwt.avg",
            newPlot = FALSE,
            plotLF = FALSE,
            thr = input$coherence_val,
            use.ggplot = TRUE
          )
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  ##########################################################################################################
  
  
  ############# 10.9. CWT SCALE-AVERAGED GAIN TRANSFER FUNCTION LF BAND ##############################
  output$Analyzed_brs_Plot2_cwt <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedBRS(
            framework,
            chosen_analysis,
            "cwt.avg",
            newPlot = FALSE,
            plotHF = FALSE,
            thr = input$coherence_val,
            use.ggplot = TRUE
          )
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  ##########################################################################################################
  
  ########### 10.10. CWT BRS PHASE PLOT HF BAND #################################################################
  output$phase2_cwt <- renderPlot({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedBRS(
            framework,
            chosen_analysis,
            "cwt.phase",
            newPlot = FALSE,
            plotHF = FALSE,
            thr = input$coherence_val,
            use.ggplot = TRUE
          )
        Results <- Results + ylim(-pi, pi)
        if (input$interval_input != "No intervals have been set" |
            input$control_input != "No control has been set") {
          if (input$interval_input != "No intervals have been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$interval_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "red",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          if (input$control_input != "No control has been set") {
            intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
            interval <- match(input$control_input, intervals)
            if ((NROW(framework$IndividualIndices[[interval]]$Time_DWT[1, ]) > 0) &&
                (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]))) {
              Results <- Results + annotate(
                "rect",
                fill = "blue",
                alpha = 0.5,
                xmin =
                  framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis] *
                  60,
                xmax = framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis] *
                  60,
                ymin = -Inf,
                ymax = Inf
              )
            }
          }
          return(Results)
        } else {
          return(Results)
        }
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  ######################################################################################################
  
  
  
  
  ################# 10.11. CWT TIME-FREQUENCY ########################################################
  
  output$CWT_plot <- renderImage({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Results <-
          PlotAnalyzedBRS(
            framework,
            locator = chosen_analysis,
            method = "cwt",
            tem = TRUE,
            newPlot = FALSE,
            thr = input$coherence_val, 
            use.ggplot = TRUE
          )
        #return(list(src = Results, contentType = "image/png", width = 1500, height = 400, alt = "CWT Transfer Function"))
        return(
          list(
            src = Results,
            contentType = "image/png",
            width = "100%",
            height = "100%",
            alt = "CWT Transfer Function"
          )
        )
      } else {
        blank <- tempfile(fileext = ".png")
        png(filename = blank,
            width = 1500,
            height = 400)
        plot(
          0,
          type = "l",
          xlab = "",
          ylab  = "",
          xaxt = "n",
          yaxt = "n"
        )
        dev.off()
        return(
          list(
            src = blank,
            contentType = "image/png",
            width = "100%",
            height = "100%",
            alt = "CWT Transfer Function"
          )
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  }, deleteFile = TRUE)
  
  
  ######################################################################################################
  
  #####################################################################################################
  ####################################################################################################
  
  
  ####################### 11. ESTIMATE TEXTS ######################################################
  
  
  output$DWT_Estimate_HF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          !is.null(input$Analyzed_brs_brush1)) {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2,]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Data <- framework$"General Data"
        Analysis <- framework$Analyses[[chosen_analysis]]
        brs <- Analysis$BRS$DWT
        brs$Time <- Analysis$Data[, 1]
        brs$type <- "brs_dwt"
        indices <-
          IndividualIndices(
            brs,
            c(
              input$Analyzed_brs_brush1$xmin / 60,
              input$Analyzed_brs_brush1$xmax / 60
            ),
            use.coherence = Data$Threshold,
            thr = Data$Coherence,
            method = Data$"Index Method"
          )
        paste(
          "Estimate at HF band between",
          round(
            ifelse(
              input$Analyzed_brs_brush1$xmin > 0,
              input$Analyzed_brs_brush1$xmin / 60,
              0
            ),
            3
          ),
          "and",
          round(input$Analyzed_brs_brush1$xmax / 60, 3),
          "min:",
          round(indices[1, 1], 3),
          "ms/mmHg"
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$interval_HF_dwt <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          input$interval_input != "No intervals have been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis])) {
          brs <-
            framework$IndividualIndices[[interval]]$DWT[1, chosen_analysis]
          limits <-
            framework$IndividualIndices[[interval]]$Time_DWT[, chosen_analysis]
          paste(
            "HF estimate at chosen interval from",
            round(limits[1], 3),
            "to",
            round(limits[2], 3),
            "min:",
            round(brs, 3),
            "ms/mmHg"
          )
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  
  
  output$CWT_Estimate_HF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          !is.null(input$Analyzed_brs_brush1)) {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2,]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Data <- framework$"General Data"
        Analysis <- framework$Analyses[[chosen_analysis]]
        brs <- AssembleCwtBRS(framework, chosen_analysis)
        brs <- SplitByCoherence(brs, thr = input$coherence_val)
        brs$Time <- Analysis$Data[, 1]
        brs$type <- "brs_dwt"
        indices <-
          IndividualIndices(
            brs,
            c(
              input$Analyzed_brs_brush1$xmin / 60,
              input$Analyzed_brs_brush1$xmax / 60
            ),
            use.coherence = Data$Threshold,
            thr = Data$Coherence,
            method = Data$"Index Method"
          )
        paste(
          "Estimate at HF band between",
          round(
            ifelse(
              input$Analyzed_brs_brush1$xmin > 0,
              input$Analyzed_brs_brush1$xmin / 60,
              0
            ),
            3
          ),
          "and",
          round(input$Analyzed_brs_brush1$xmax / 60, 3),
          "min:",
          round(indices[1, 1], 3),
          "ms/mmHg"
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$interval_HF_cwt <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          input$interval_input != "No intervals have been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_CWT[1, chosen_analysis])) {
          brs <-
            framework$IndividualIndices[[interval]]$CWT[1, chosen_analysis]
          limits <-
            framework$IndividualIndices[[interval]]$Time_CWT[, chosen_analysis]
          paste(
            "HF estimate at chosen interval from",
            round(limits[1], 3),
            "to",
            round(limits[2], 3),
            "min:",
            round(brs, 3),
            "ms/mmHg"
          )
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$DWT_Estimate_LF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          !is.null(input$Analyzed_brs_brush1)) {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2,]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Data <- framework$"General Data"
        Analysis <- framework$Analyses[[chosen_analysis]]
        brs <- Analysis$BRS$DWT
        brs$Time <- Analysis$Data[, 1]
        brs$type <- "brs_dwt"
        indices <-
          IndividualIndices(
            brs,
            c(
              input$Analyzed_brs_brush1$xmin / 60,
              input$Analyzed_brs_brush1$xmax / 60
            ),
            use.coherence = Data$Threshold,
            thr = Data$Coherence
          )
        paste(
          "Estimate at LF band between",
          round(
            ifelse(
              input$Analyzed_brs_brush1$xmin > 0,
              input$Analyzed_brs_brush1$xmin / 60,
              0
            ),
            3
          ),
          "and",
          round(input$Analyzed_brs_brush1$xmax / 60, 3),
          "min:",
          round(indices[1, 2], 3),
          "ms/mmHg"
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$interval_LF_dwt <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          input$interval_input != "No intervals have been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis])) {
          brs <-
            framework$IndividualIndices[[interval]]$DWT[2, chosen_analysis]
          limits <-
            framework$IndividualIndices[[interval]]$Time_DWT[, chosen_analysis]
          paste(
            "LF estimate at chosen interval from",
            round(limits[1], 3),
            "to",
            round(limits[2], 3),
            "min:",
            round(brs, 3),
            "ms/mmHg"
          )
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$CWT_Estimate_LF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          !is.null(input$Analyzed_brs_brush1)) {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2,]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Data <- framework$"General Data"
        Analysis <- framework$Analyses[[chosen_analysis]]
        brs <- Analysis$BRS$AvgCWT
        brs$Time <- Analysis$Data[, 1]
        brs$type <- "brs_dwt"
        brs <- AssembleCwtBRS(framework, chosen_analysis)
        brs <- SplitByCoherence(brs, thr = input$coherence_val)
        brs$Time <- Analysis$Data[, 1]
        brs$type <- "brs_dwt"
        indices <-
          IndividualIndices(
            brs,
            c(
              input$Analyzed_brs_brush1$xmin / 60,
              input$Analyzed_brs_brush1$xmax / 60
            ),
            use.coherence = Data$Threshold,
            thr = Data$Coherence,
            method = Data$"Index Method"
          )
        paste(
          "Estimate at LF band between",
          round(
            ifelse(
              input$Analyzed_brs_brush1$xmin > 0,
              input$Analyzed_brs_brush1$xmin / 60,
              0
            ),
            3
          ),
          "and",
          round(input$Analyzed_brs_brush1$xmax / 60, 3),
          "min:",
          round(indices[1, 2], 3),
          "ms/mmHg"
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$interval_LF_cwt <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          input$interval_input != "No intervals have been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_CWT[1, chosen_analysis])) {
          brs <-
            framework$IndividualIndices[[interval]]$CWT[2, chosen_analysis]
          limits <-
            framework$IndividualIndices[[interval]]$Time_CWT[, chosen_analysis]
          paste(
            "LF estimate at chosen interval from",
            round(limits[1], 3),
            "to",
            round(limits[2], 3),
            "min:",
            round(brs, 3),
            "ms/mmHg"
          )
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  
  output$Estimate_HF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          !is.null(input$brush_raw)) {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2,]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Data <- framework$"General Data"
        Analysis <- framework$Analyses[[chosen_analysis]]
        hrv <- Analysis$HRV
        hrv$Time <- Analysis$Data[, 1]
        hrv$type <- "brs_dwt"
        indices <-
          IndividualIndices(
            hrv,
            c(input$brush_raw$xmin / 60, input$brush_raw$xmax / 60),
            use.coherence = Data$Threshold,
            thr = Data$Coherence,
            method = Data$"Index Method"
          )
        paste(
          "Estimate at HF band between",
          round(
            ifelse(input$brush_raw$xmin > 0,  input$brush_raw$xmin / 60, 0),
            3
          ),
          "and",
          round(input$brush_raw$xmax / 60, 3),
          "min:",
          round(indices[1], 3),
          "ms2"
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  
  output$interval_HRV_HF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          input$interval_input != "No intervals have been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis])) {
          hrv <-
            framework$IndividualIndices[[interval]]$HRV[, chosen_analysis]
          hrv <- hrv[1]
          limits <-
            framework$IndividualIndices[[interval]]$Time_DWT[, chosen_analysis]
          paste(
            "LF estimate at chosen interval from ",
            round(limits[1], 3),
            "to",
            round(limits[2], 3),
            "min:",
            round(hrv, 3),
            "ms2"
          )
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$Estimate_LF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          !is.null(input$brush_raw)) {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2,]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Data <- framework$"General Data"
        Analysis <- framework$Analyses[[chosen_analysis]]
        hrv <- Analysis$HRV
        hrv$Time <- Analysis$Data[, 1]
        hrv$type <- "brs_dwt"
        indices <-
          IndividualIndices(
            hrv,
            c(input$brush_raw$xmin / 60, input$brush_raw$xmax / 60),
            use.coherence = Data$Threshold,
            thr = Data$Coherence
          )
        paste(
          "Estimate at LF band between",
          round(
            ifelse(input$brush_raw$xmin > 0,  input$brush_raw$xmin / 60, 0),
            3
          ),
          "and",
          round(input$brush_raw$xmax / 60, 3),
          "min:",
          round(indices[2], 3),
          "ms2"
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  
  output$interval_HRV_LF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          input$interval_input != "No intervals have been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis])) {
          hrv <-
            framework$IndividualIndices[[interval]]$HRV[, chosen_analysis]
          hrv <- hrv[2]
          limits <-
            framework$IndividualIndices[[interval]]$Time_DWT[, chosen_analysis]
          paste(
            "LF estimate at chosen interval from ",
            round(limits[1], 3),
            "to",
            round(limits[2], 3),
            "min:",
            round(hrv, 3),
            "ms2"
          )
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  
  output$interval_HRV_LFHF <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          input$interval_input != "No intervals have been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis])) {
          hrv <-
            framework$IndividualIndices[[interval]]$HRV[3, chosen_analysis]
          limits <-
            framework$IndividualIndices[[interval]]$Time_DWT[, chosen_analysis]
          paste(
            "LF/HF ratio estimate at chosen interval from",
            round(limits[1], 3),
            "to",
            round(limits[2], 3),
            "min:",
            round(hrv, 3)
          )
        }
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$Estimate_HR <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          !is.null(input$brush_raw)) {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2,]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Data <- framework$"General Data"
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        raw <- framework$Analyses[[chosen_analysis]]$Data
        fun <-
          list(Time = raw[, "Time"],
               HR = 60000 / raw[, "RR"],
               SBP = raw[, "SBP"])
        select_time <- fun$Time[(fun$Time >= input$brush_raw$xmin) &
                                  (fun$Time <= input$brush_raw$xmax)]
        select_time <- match(select_time, fun$Time)
        method <- Data$"Index Method"
        method <- ifelse(method == "mean", mean, median)
        HR <- method(fun$HR[select_time])
        paste(
          "HR between",
          round(
            ifelse(input$brush_raw$xmin > 0,  input$brush_raw$xmin / 60, 0),
            3
          ),
          "and",
          round(input$brush_raw$xmax / 60, 3),
          "min:",
          round(HR, 3),
          "bpm"
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$Estimate_SBP <- renderText({
    tryCatch({
      if (input$subject_input != "No subjects have been loaded" &
          !is.null(input$brush_raw)) {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2,]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        Data <- framework$"General Data"
        Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
        raw <- framework$Analyses[[chosen_analysis]]$Data
        fun <-
          list(Time = raw[, "Time"],
               HR = 60000 / raw[, "RR"],
               SBP = raw[, "SBP"])
        select_time <- fun$Time[(fun$Time >= input$brush_raw$xmin) &
                                  (fun$Time <= input$brush_raw$xmax)]
        select_time <- match(select_time, fun$Time)
        method <- Data$"Index Method"
        method <- ifelse(method == "mean", mean, median)
        SBP <- method(fun$SBP[select_time])
        paste(
          "SBP between",
          round(
            ifelse(input$brush_raw$xmin > 0,  input$brush_raw$xmin / 60, 0),
            3
          ),
          "and",
          round(input$brush_raw$xmax / 60, 3),
          "min:",
          round(SBP, 3),
          "mmHg"
        )
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  
  
  ###################################################################################################
  
  ###################### 12. SIGNIFICANCE TESTINGS #################################################
  
  output$pvalue_HF <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          brs <- framework$Analyses[[chosen_analysis]]$BRS$DWT
          brs$Time <- Data$Data[, 1]
          brs$type <- "brs_dwt"
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndBRS(brs, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[1] <= 0.05, "Significant", "No significant")
          if (evaluation[1] <= 0.001) {
            code <- "***"
          } else if (evaluation[1] <= 0.01) {
            code <- "**"
          } else if (evaluation[1] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[1], 4),
              " (",
              code,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$pvalue_HF_cwt <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          brs <- framework$Analyses[[chosen_analysis]]$BRS$AvgCWT
          brs$Time <- Data$Data[, 1]
          brs$type <- "brs_dwt"
          brs <- AssembleCwtBRS(framework, chosen_analysis)
          brs <- SplitByCoherence(brs, thr = input$coherence_val)
          brs$Time <- Data$Data[, 1]
          brs$type <- "brs_dwt"
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndBRS(brs, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[1] <= 0.05, "Significant", "No significant")
          if (evaluation[1] <= 0.001) {
            code <- "***"
          } else if (evaluation[1] <= 0.01) {
            code <- "**"
          } else if (evaluation[1] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[1], 4),
              " (",
              code,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$pvalue_LF <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          brs <- framework$Analyses[[chosen_analysis]]$BRS$DWT
          brs$Time <- Data$Data[, 1]
          brs$type <- "brs_dwt"
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndBRS(brs, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[2] <= 0.05, "Significant", "No significant")
          if (evaluation[2] <= 0.001) {
            code <- "***"
          } else if (evaluation[2] <= 0.01) {
            code <- "**"
          } else if (evaluation[2] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[2], 4),
              " (",
              code ,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$pvalue_LF_cwt <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          brs <- framework$Analyses[[chosen_analysis]]$BRS$AvgCWT
          brs$Time <- Data$Data[, 1]
          brs$type <- "brs_dwt"
          brs <- AssembleCwtBRS(framework, chosen_analysis)
          brs$type <- "brs_cwt"
          brs <- AssembleCwtBRS(framework, chosen_analysis)
          brs <- SplitByCoherence(brs, thr = input$coherence_val)
          brs$Time <- Data$Data[, 1]
          brs$type <- "brs_dwt"
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndBRS(brs, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[2] <= 0.05, "Significant", "No significant")
          if (evaluation[2] <= 0.001) {
            code <- "***"
          } else if (evaluation[2] <= 0.01) {
            code <- "**"
          } else if (evaluation[2] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[2], 4),
              " (",
              code ,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$pvalue_HRV_LF <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          hrv <- list()
          hrv$HRV <- framework$Analyses[[chosen_analysis]]$HRV
          hrv$Time <- Data$Data[, 1]
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndHRV(hrv, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[2] <= 0.05, "Significant", "No significant")
          if (evaluation[2] <= 0.001) {
            code <- "***"
          } else if (evaluation[2] <= 0.01) {
            code <- "**"
          } else if (evaluation[2] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[2], 4),
              " (",
              code ,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$pvalue_HRV_HF <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          hrv <- list()
          hrv$HRV <- framework$Analyses[[chosen_analysis]]$HRV
          hrv$Time <- Data$Data[, 1]
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndHRV(hrv, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[1] <= 0.05, "Significant", "No significant")
          if (evaluation[1] <= 0.001) {
            code <- "***"
          } else if (evaluation[1] <= 0.01) {
            code <- "**"
          } else if (evaluation[1] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[1], 4),
              " (",
              code ,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$pvalue_HRV_LFHF <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          hrv <- list()
          hrv$HRV <- framework$Analyses[[chosen_analysis]]$HRV
          hrv$Time <- Data$Data[, 1]
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndHRV(hrv, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[3] <= 0.05, "Significant", "No significant")
          if (evaluation[3] <= 0.001) {
            code <- "***"
          } else if (evaluation[3] <= 0.01) {
            code <- "**"
          } else if (evaluation[3] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[3], 4),
              " (",
              code ,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$pvalue_HR <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          raw_data <- framework$Analyses[[chosen_analysis]]$Data
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndHRandBP(raw_data, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[1] <= 0.05, "Significant", "No significant")
          if (evaluation[1] <= 0.001) {
            code <- "***"
          } else if (evaluation[1] <= 0.01) {
            code <- "**"
          } else if (evaluation[1] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              "HR: ",
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[1], 4),
              " (",
              code ,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$pvalue_SBP <- renderText({
    tryCatch({
      if (input$interval_input != "No intervals have been set" &
          input$control_input != "No control has been set") {
        framework <- isolate(database$framework)
        analysis_choices <-
          ShowLocatorIndices(framework, "analyses")[2, ]
        chosen_analysis <-
          match(input$subject_input, analysis_choices)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        interval <- match(input$interval_input, intervals)
        control <- match(input$control_input, intervals)
        if (!is.na(framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis]) &
            !is.na(framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis])) {
          Data <- ExtractDataFromAnalysis(framework, chosen_analysis)
          raw_data <- framework$Analyses[[chosen_analysis]]$Data
          time_flags1 <-
            c(
              framework$IndividualIndices[[interval]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[interval]]$Time_DWT[2, chosen_analysis]
            )
          time_flags2 <-
            c(
              framework$IndividualIndices[[control]]$Time_DWT[1, chosen_analysis],
              framework$IndividualIndices[[control]]$Time_DWT[2, chosen_analysis]
            )
          evaluation <-
            TestIndHRandBP(raw_data, time_flags1 / 60, time_flags2 / 60)
          sig <-
            ifelse(evaluation[2] <= 0.05, "Significant", "No significant")
          if (evaluation[2] <= 0.001) {
            code <- "***"
          } else if (evaluation[2] <= 0.01) {
            code <- "**"
          } else if (evaluation[2] <= 0.05) {
            code <- "*"
          } else {
            code <- "ns"
          }
          text <-
            paste(
              "SBP: ",
              sig,
              " difference in estimates between interval ",
              input$control_input,
              " (set as control) and
                      interval ",
              input$interval_input,
              ", with a p value of ",
              round(evaluation[2], 4),
              " (",
              code ,
              ")",
              sep = ""
            )
          return(text)
        }
        
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  ########################################################################################################
  
  ############## 13. DOUBLE-CLICK TO SELECT INTERVALS #################################################
  observeEvent(input$dbc_raw, {
    tryCatch({
      check_brush <- !is.null(input$brush_raw)
      check_subject <-
        input$subject_input != "No subjects have been loaded"
      check_interval <-
        input$interval_input != "No intervals have been set"
      if (check_subject & check_interval & check_brush) {
        framework <- isolate(database$framework)
        analyses <- ShowLocatorIndices(framework, "analyses")[2, ]
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        analysis <- match(input$subject_input, analyses)
        interval <- match(input$interval_input, intervals)
        framework <- AnalyzeBRSIndices(
          framework,
          analysis,
          interval,
          c(input$brush_raw$xmin / 60,
            input$brush_raw$xmax / 60)
        )
        
        database$framework <- framework
        #########################################################################################################
        
        ################### 14. PLOT ESTIMATES ########################################################################
        
        output$IndividualIndices_DWT_Plot <- renderPlot({
          framework <- isolate(database$framework)
          analyses <- ShowLocatorIndices(framework, "analyses")[2, ]
          analysis <- match(input$subject_input, analyses)
          restrict <- NULL
          for (n in 1:length(framework$IndividualIndices)) {
            if (length(framework$IndividualIndices[[n]]$DWT) != 0 &&
                !is.na(framework$IndividualIndices[[n]]$DWT[1, analysis]))
              restrict <- c(restrict, n)
          }
          if (!is.null(restrict)) {
            PlotIndicesFromAnalysis(
              framework,
              analysis,
              "dwt",
              newPlot = FALSE,
              restrict = restrict,
              ymax = input$maxEst_dwt
            )
          }
        })
      }
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  output$IndividualIndices_DWT_Plot <- renderPlot({
    check_subject <-
      input$subject_input != "No subjects have been loaded"
    check_interval <-
      input$interval_input != "No intervals have been set"
    if (check_subject & check_interval) {
      framework <- isolate(database$framework)
      analyses <- ShowLocatorIndices(framework, "analyses")[2, ]
      analysis <- match(input$subject_input, analyses)
      restrict <- NULL
      for (n in 1:length(framework$IndividualIndices)) {
        if (length(framework$IndividualIndices[[n]]$DWT) != 0 &&
            !is.na(framework$IndividualIndices[[n]]$DWT[1, analysis]))
          restrict <- c(restrict, n)
      }
      if (!is.null(restrict)) {
        PlotIndicesFromAnalysis(
          framework,
          analysis,
          "dwt",
          newPlot = FALSE,
          restrict = restrict,
          ymax = input$maxEst_dwt
        )
      }
    }
  })
  #######################################################################################################
  
  ################# 15. PERFORM STATISTICAL TESTS ###############################################################
  
  observeEvent(input$confirm_test, {
    tryCatch({
      check_test <-
        input$test_var_in_test != "No testing variable has been selected"
      check_con <-
        input$con_var_in_test != "No control variable has been selected"
      if (check_test & check_con) {
        framework <- isolate(database$framework)
        intervals <- ShowLocatorIndices(framework, "intervals")[2, ]
        test <- match(input$test_var_in_test , intervals)
        control <- match(input$con_var_in_test, intervals)
        framework <-
          TestGroups(
            framework,
            test,
            control,
            name = input$test_name,
            method = NULL,
            showerror = FALSE
          )
        framework <-
          TestHRV(
            framework,
            test,
            control,
            name = input$test_name,
            method = NULL,
            normalize = input$norm_hrv,
            showerror = FALSE
          )
        tests <- ShowLocatorIndices(framework, "tests")[2, ]
        updateSelectInput(
          session,
          "select_testHRV",
          "Select test",
          choices = c("No test has been selected", tests)
        )
        updateSelectInput(
          session,
          "select_test",
          "Select test",
          choices = c("No test has been selected", tests)
        )
        text_ntests <-
          paste("Number of tests performed:",
                length(framework$Tests),
                "tests.")
        output$text_ntests <- renderText({
          text_ntests
        })
        database$framework <- framework
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  ###########################################################################################################
  
  ################### 16. PLOT TEST RESULTS ################################################################
  
  output$testing_resultsHRV <- renderPlot({
    tryCatch({
      if (input$select_testHRV != "No test has been selected") {
        framework <- isolate(database$framework)
        tests <- ShowLocatorIndices(framework, "tests")[2, ]
        test <- match(input$select_testHRV , tests)
        results <-
          PlotHRVTestResults(framework,
                             test,
                             newPlot = FALSE,
                             draw_paired = input$show_paired)
        return(results)
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  
  output$testing_results <- renderPlot({
    tryCatch({
      if (input$select_test != "No test has been selected") {
        framework <- isolate(database$framework)
        tests <- ShowLocatorIndices(framework, "tests")[2, ]
        test <- match(input$select_test , tests)
        results <- PlotTestResults(framework,
                                   test,
                                   newPlot = FALSE,
                                   draw_paired = input$show_paired)
        return(results)
      }
      
    }, error = function(barowavelet_error) {
      showNotification(paste0(barowavelet_error),
                       type = "error",
                       duration = NULL)
    })
  })
  
  #########################################################################################################
  
  ###### 17. DOWNLOAD TEMPLATE FOR CLINICAL DATA ##########################################################
  
  output$C_template <-
    downloadHandler(
      filename = "Clinical data template.csv",
      content = function(file) {
        framework <- isolate(database$framework)
        if (framework$n > 0) {
          template <-
            data.frame(Variable = c("Units", t(
              ShowLocatorIndices(framework, "analyses")[2, ]
            )))
        } else {
          template <-
            data.frame(Variable = c(
              "Units",
              "First subject",
              "Second subject",
              "Third subject"
            ))
        }
        write.csv(template,
                  file = file,
                  quote = FALSE,
                  row.names = FALSE)
      }
    )
  ##################################################################################################
  
  
  ##############################################################################################
  ############### 18. WHAT HAPPENS WHEN THE APP IS CLOSED ######################################
  
  session$onSessionEnded(function() {
    stopApp()
    if (.Platform$GUI != "RStudio")
      q(save = "no")
  })
  #############################################################################################
  
}

############# END OF SERVER#################################################################

# RUN BaroWavelet
shinyApp(ui = ui, server = server)
