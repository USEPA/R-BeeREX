### ~~~~ R-BeeREX ~~~ ###
### ~~~ 1/13/2026 ~~ ###
### ~~ N. Pollesch ~~ ###


#rsconnect::writeManifest()


RBeeREXVersion<-"0.2.2"
DeployedLast<-"January 26, 2026"
DeployedBy<-"N.Pollesch"

#### Check and install required packages ####
required_packages <- c(
  "shiny", "shinyjs", "data.table", "readxl", "dplyr","tidyr",
  "openxlsx", "DT", "ggplot2", "RColorBrewer", "writexl"
)

is_installed <- function(pkg) {
  is.element(pkg, installed.packages()[, "Package"])
}

for (pkg in required_packages) {
  if (!is_installed(pkg)) {
    install.packages(pkg)
  }
}

lapply(required_packages, library, character.only = TRUE)

library(shiny)
library(shinyjs)
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(writexl)

# Set significant digits, globally
options(digits=4)

# Helper: format numeric values (and numeric-looking characters) for display
format_mixed_num <- function(x, dec_digits = 4, sci_threshold = 1e-4, sci_digits = 4) {
  out <- character(length(x))
  na_idx <- is.na(x)
  out[na_idx] <- NA_character_
  y <- x[!na_idx]
  format_one <- function(v) {
    if (v == 0) return("0")
    absv <- abs(v)
    if (absv < sci_threshold) {
      return(formatC(v, format = "e", digits = sci_digits))
    }
    if (isTRUE(all.equal(v, round(v)))) {
      return(as.character(round(v)))
    } else {
      s <- sprintf(paste0("%.", dec_digits, "f"), round(v, dec_digits))
      s <- sub("\\.?0+$", "", s)
      return(s)
    }
  }
  out[!na_idx] <- vapply(y, format_one, character(1))
  out
}

# Helper: apply display formatting to all columns (numeric + numeric-like character)
dt_mixed <- function(df, pageLength = 10, dec_digits = 4, sci_threshold = 1e-4, sci_digits = 4) {
  df2 <- df
  is_int64 <- vapply(df2, function(x) inherits(x, "integer64"), logical(1))
  is_num   <- vapply(df2, is.numeric, logical(1))
  is_char  <- vapply(df2, is.character, logical(1))
  for (j in seq_along(df2)) {
    if (is_int64[j]) next
    x <- df2[[j]]
    if (is_num[j]) {
      df2[[j]] <- format_mixed_num(x, dec_digits, sci_threshold, sci_digits)
    } else if (is_char[j]) {
      suppressWarnings(num <- as.numeric(x))
      idx <- !is.na(num)
      if (any(idx)) {
        formatted <- x
        formatted[idx] <- format_mixed_num(num[idx], dec_digits, sci_threshold, sci_digits)
        df2[[j]] <- formatted
      }
    }
  }
  DT::datatable(df2, options = list(pageLength = pageLength, scrollX = TRUE))
}

# Load the species metadata from the specified path
load("data/species/species_metadata.RDA")

# Source the EECs and other functions from functions.R
source("functions.R")

# Deployment check helper
checkDeploymentStatus <- function(session) {
  appUrl <- session$clientData$url_hostname
  is_deployed_server <- !grepl("localhost|127.0.0.1", appUrl)
  if (is_deployed_server) {
    message("Running on a web server")
    return(TRUE)
  } else {
    message("Running locally")
    return(FALSE)
  }
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .filter-row {
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
        margin-bottom: 10px;
      }
      .filter-row .form-group {
        margin-bottom: 0;
        flex: 1;
        min-width: 150px;
      }
      .filter-row select {
        width: 100%;
        max-width: 150px;
      }
      .table-title {
        font-weight: bold;
        margin-top: 20px;
        margin-bottom: 10px;
      }
    "))
  ),
  titlePanel(
    tagList(
      tags$span(
        style = "display: inline-flex; align-items: center;",
        tags$img(src = "R-BeeREX_sm.png", height = "50", style = "margin-right: 10px;"),
        "R-BeeREX"
      )
    ),
    windowTitle = "R-BeeREX"
  ),
  tabsetPanel(id = "top_tabs",
              # Top-level: Overview
              tabPanel("Overview",
                       fluidRow(
                         column(width = 6, offset = 3,
                                br(),
                                tags$div(
                                  style = "text-align: center;",
                                  tags$img(src = "R-BeeREX.png", height = "200px", width = "200px")),
                                br(),
                                wellPanel(
                                  h5(tags$b("R-BeeREX Overview")),
                                  p(paste0("This version of R-BeeREX (",RBeeREXVersion,") is a part of the EFED tools team project to convert, extend, and modernize existing risk assessment tools.
                           These tools all utilize a standardized input template ('UST Template') for toxicity, exposure, and physiochemical data.
                           R-BeeREX has built in species-specific consumption rate tables for Honey Bee (Apis mellifera) and Bumble bee (Bombus) that can be selected and paired with data from the UST template. Workflow proceeds from Data input to modeling to a final export. Please contact Nate Pollesch (pollesch.nathan@epa.gov) with any questions.")),
                                  br(),
                                  p(tags$b("Standard and Expedited workflows available")),
                                  p("Multiple workflow options have been developed.  The standard workflow takes users through each step of the data processing and calculations, displaying additional details.  The ⚡Expedited workflow is meant for faster processing, without intermediate observations of data and scenarios, instead moving directly to the results."),
                                  br(),
                                  p(tags$b("App Environment Status (Online/Local)")),
                                  textOutput("deploymentStatusMain"),br(),
                                  p(tags$b("R-BeeREX Version: "),RBeeREXVersion),
                                  p(tags$b("Last Deployed: "),DeployedLast,tags$b(" By: "),DeployedBy)
                                )
                         )
                       )
              ),
              # Top-level: Standard Workflow
              tabPanel("Standard Workflow",
                       tabsetPanel(
                         tabPanel("1: Data Input",
                                  tabsetPanel(
                                    tabPanel("1.1: UST Template Upload",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Upload Formatted UST Template")),
                                                 p("This version of R-BeeREX runs exclusively from a the UST master input template.",tags$b("In order to use this tool, you must supply a properly formatted version of the UST Template excel file."), "This application will allow you to upload any file below, however, only the specified template will enable further functioning of the application.  Choose from one of the following upload options:"),
                                                 br(),
                                                 uiOutput("file_dropdown"),
                                                 actionButton("load_template", "Load template file"),
                                                 div(id="input_breaks",
                                                     br(),
                                                     h5(tags$b("-- Or --")),
                                                     br()
                                                 ),
                                                 fileInput("data_file", "Upload Excel template file from anywhere on your computer:", accept = c(".xlsx")),
                                                 actionButton("refresh_inputs", icon = icon("refresh"), label = "Refresh Template Inputs", disabled = FALSE, style="display:none;")
                                               ),
                                               mainPanel(
                                                 div(id = "template_selected",
                                                     br(),
                                                     br(),
                                                     h5("Template selected and uploaded. Move to 1.2 for Species selection."),
                                                     style = "display: none;"
                                                 )
                                               )
                                             )
                                    ),
                                    tabPanel("1.2: Species", 
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Species Selection")),
                                                 p("Choose the species to model. Species data includes life stage, caste, age, and consumption rates. Generally, Honey bee and Bumble bee will be selected. Honey bee (long) includes consumption rates for additional castes and ages and is being made available for reference to prior versions of BeeREX."),
                                                 shinyjs::disabled(
                                                   checkboxGroupInput("species_checkboxes", "Select Species", 
                                                                      choices = species_metadata$common_name)
                                                 ),
                                                 actionButton("select_species", "Select Species", disabled = TRUE),
                                                 actionButton("refresh_species", icon = icon("refresh"), label = NULL, disabled = TRUE)
                                               ),
                                               mainPanel(
                                                 conditionalPanel(
                                                   condition = "output.speciesTempAvailable",
                                                   h4("Selected Species"),
                                                   DT::dataTableOutput("species_temp_table")
                                                 )
                                               )
                                             )
                                    ),
                                    tabPanel("1.3: Toxicity", 
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Toxicity Data")),
                                                 p("Chemical specific toxicity data from the \"Endpoints for Terrestrial\" component of the UST Input Template. This data will be merged with the Selected Species data to match endpoints to relevant species and life stages."),
                                                 conditionalPanel(
                                                   condition = "output.dataUploaded",
                                                   checkboxInput("show_toxicity_raw", "Show Raw Toxicity Data", value = F)
                                                 ),
                                                 actionButton("store_toxicity_data", "Assemble BeeREX Toxicity and Species Data", disabled = TRUE)
                                               ),
                                               mainPanel(
                                                 conditionalPanel(
                                                   condition = "input.show_toxicity_raw == true",
                                                   h4("Raw Toxicity Data"),
                                                   DT::dataTableOutput("toxicity_raw_table")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "output.speciesToxicityAvailable",
                                                   h4("Species Toxicity Data"),
                                                   DT::dataTableOutput("species_toxicity_table")
                                                 )
                                               )
                                             )
                                    ),
                                    tabPanel("1.4: Exposure", 
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Use Site Exposure Data")),
                                                 p("Use site data from the \"USE SUMMARY TABLE\" component of the UST Input Template. This data will then be filtered for the BeeREX specific exposure information."),
                                                 conditionalPanel(
                                                   condition = "output.dataUploaded",
                                                   checkboxInput("show_exposure_raw", "Show Raw Exposure Data", value = F)
                                                 ),
                                                 actionButton("apply_beerex_filter", "Apply BeeREX Filter to Exposure Data", disabled = TRUE)
                                               ),
                                               mainPanel(
                                                 conditionalPanel(
                                                   condition = "input.show_exposure_raw == true",
                                                   h4("Raw Exposure Data"),
                                                   DT::dataTableOutput("exposure_raw_table")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "output.exposureAvailable",
                                                   h4("Filtered Exposure Data"),
                                                   DT::dataTableOutput("exposure_table")
                                                 )
                                               )
                                             )
                                    ),
                                    tabPanel("1.5: Physiochemical",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Physiochemical Data")),
                                                 p("Physiochemical data from the \"Physiochemical Data Entry\" component of the UST Input Template. This data is used within BeeREX for calculating the uptake rates for soil use exposure."),
                                                 conditionalPanel(
                                                   condition = "output.dataUploaded",
                                                   checkboxInput("show_physchem_raw", "Show Raw Physiochemical Data", value = F)
                                                 ),
                                                 actionButton("extract_physchem_data", "Apply BeeREX Filter to Physiochemical Data", disabled = TRUE)
                                               ),
                                               mainPanel(
                                                 conditionalPanel(
                                                   condition = "input.show_physchem_raw == true",
                                                   h4("Raw Physiochemical Data"),
                                                   DT::dataTableOutput("physiochemical_raw_table")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "output.physiochemicalAvailable",
                                                   h4("Physiochemical Data"),
                                                   DT::dataTableOutput("physiochemical_table")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "output.exposureChemAvailable",
                                                   h4("Exposure Chem Data"),
                                                   DT::dataTableOutput("exposure_chem_table")
                                                 )
                                               )
                                             )
                                    )
                                  )
                         ),
                         tabPanel("2: Model",
                                  tabsetPanel(
                                    tabPanel("2.1: EECs", 
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Calculate EECs")),
                                                 p("Calculate the EECs for each use scenario based on application rate, application type, and any relevant physiochemical data"),
                                                 conditionalPanel(
                                                   condition = "output.exposureChemAvailable",
                                                   checkboxInput("show_exposure_chem", "Show Exposure Chem Data", value = F)
                                                 ),
                                                 actionButton("calculate_eecs", "Calculate EECs", disabled = TRUE),
                                                 shinyjs::hidden(checkboxInput("toggle_eecs_table", "Show EECs results", value = TRUE))
                                               ),
                                               mainPanel(
                                                 conditionalPanel(
                                                   condition = "input.show_exposure_chem == true && output.exposureChemAvailable == true",
                                                   h4("Exposure Chem Data"),
                                                   DT::dataTableOutput("exposure_chem_display")
                                                 ),
                                                 conditionalPanel(
                                                   condition = "output.exposureChemEECsAvailable",
                                                   h4("Exposure Chem EECs Data"),
                                                   DT::dataTableOutput("exposure_chem_eecs_table")
                                                 )
                                               )
                                             )
                                    ),
                                    tabPanel("2.2: Dose, RQs, & MODs", 
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Calculate Total dose, RQs, & MODs")),
                                                 p("Using the species, toxicity, exposure data, and EECs now calculate Total dose, RQs, and MODs. Results are shown within species data tables organized by Use Site exposure scenarios."),
                                                 conditionalPanel(
                                                   condition = "output.exposureChemEECsAvailable",
                                                   checkboxInput("show_exposure_chem_eecs", "Show Exposure Chem EEC Data", value = F)
                                                 ),
                                                 actionButton("calculate_total_dose", "Calculate Dose, RQs, & MODs", disabled = TRUE),
                                                 shinyjs::hidden(checkboxInput("toggle_plot", "Show Plot", value = TRUE)),
                                                 shinyjs::hidden(checkboxInput("toggle_exposure", "Show Exposure Data and Filters", value = TRUE))
                                               ),
                                               mainPanel(
                                                 conditionalPanel(
                                                   condition = "input.show_exposure_chem_eecs == true && output.exposureChemEECsAvailable == true",
                                                   h4("Exposure Chem EEC Data"),
                                                   DT::dataTableOutput("exposure_chem_eecs_table_model")
                                                 ),
                                                 uiOutput("species_exposure_ui"),
                                                 DT::dataTableOutput("exposure_chem_eecs_row"),
                                                 conditionalPanel(
                                                   condition = "input.toggle_exposure == true",
                                                   DT::dataTableOutput("species_exposure_table")
                                                 ),
                                                 plotOutput("species_exposure_plot")
                                               )
                                             )
                                    ),
                                    tabPanel("2.3: Results",
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Summarize and display results")),
                                                 p("Click 'Summarize Results' to format and display the final tables. Use the selector below to switch between the two results tables."),
                                                 actionButton("summarize_results", "Summarize Results", disabled = TRUE),
                                                 uiOutput("results_selector_ui")
                                               ),
                                               mainPanel(
                                                 br(),
                                                 h5(tags$b("Formatted Results")),
                                                 DT::dataTableOutput("results_table_display")
                                               )
                                             )
                                    )
                                  )
                         ),
                         tabPanel("3: Export",
                                  tabsetPanel(
                                    tabPanel("3.1: To Excel", 
                                             sidebarLayout(
                                               sidebarPanel(
                                                 h5(tags$b("Export Model Results to Excel")),
                                                 p("Export the formatted results to an Excel workbook with two sheets: 'on_site' and 'off_site'."),
                                                 downloadButton("download_results_excel", "Export Results to Excel", style = "width:100%;")
                                               ),
                                               mainPanel(
                                                 br(),
                                                 p("Once results are summarized in Tab 2.3, use the button on the left to export them.")
                                               )
                                             )
                                    ),
                                    tabPanel("To R Code (for Developers)", 
                                             fluidRow(
                                               column(width = 6, offset = 3,
                                                      br(),
                                                      wellPanel(
                                                        h5(tags$b("Export Data to R Memory")),
                                                        p("Relevant data.tables can be sent to memory for additional use within the R environment. All objects are exported with the '.out' added to object names for identification. Data appears in R environment once the shiny app has stopped running."),
                                                        p("Note: This development feature is only relevant when the application is being used locally"),
                                                        br(),
                                                        p(tags$b("App Environment Status (Online/Local):")),
                                                        textOutput("deploymentStatus"),
                                                        br(),
                                                        actionButton("export_to_r", "Export All to R Environment")
                                                      )
                                               ))
                                    )
                                  )
                         )
                       )
              ),
              
              # Top-level: Expedited Workflow (single subtab)
              tabPanel("⚡ Expedited Workflow", value = "expedited",
                      # tabsetPanel(id = "expedited_tabs", type = "tabs", selected = "⚡1: Upload & Species Select",
                       #            tabPanel("⚡1: Upload & Species Select",
                                            sidebarLayout(
                                              sidebarPanel(
                                                h5(tags$b("Expedited Upload, Species Selection, and Export")),
                                                p("Upload the UST template, select species, then calculate and export results — all from this tab."),
                                                br(),
                                                h5(tags$b("⚡1.1: Upload Formatted UST Template")),
                                                uiOutput("file_dropdown_fast"),
                                                actionButton("load_template_fast", "Load template file"),
                                                div(id="input_breaks_fast",
                                                    br(),
                                                    h5(tags$b("-- Or --")),
                                                    br()
                                                ),
                                                fileInput("data_file_fast", "Upload Excel template file:", accept = c(".xlsx")),
                                                actionButton("refresh_inputs_fast", icon = icon("refresh"), label = "Refresh Template Inputs", disabled = FALSE, style="display:none;"),
                                                br(), hr(style = "border-top: 1px solid #000000;"), br(), 
                                                h5(tags$b("⚡1.2: Species Selection")),
                                                shinyjs::disabled(
                                                  checkboxGroupInput("species_checkboxes_fast", "Select Species", 
                                                                     choices = species_metadata$common_name)
                                                ),
                                                actionButton("select_species_and_calculate_fast",
                                                             "Select Species and Calculate",
                                                             disabled = TRUE,
                                                             class = "btn btn-primary"),
                                                actionButton("refresh_species_fast", icon = icon("refresh"), label = NULL, disabled = TRUE),
                                                br(), hr(style = "border-top: 1px solid #000000;"), br(),
                                                h5(tags$b("⚡1.3: Export Summarized Results to Excel")),
                                                p("Once results are processed, click the button below to download them."),
                                                shinyjs::hidden(downloadButton("download_results_excel_fast", "Download Results (.xlsx)", style = "width:100%;"))
                                              ),
                                              mainPanel(
                                                div(id = "template_selected_fast",
                                                    br(),
                                                    br(),
                                                    h5("Template selected and uploaded. Proceed to Species selection below."),
                                                    style = "display: none;"
                                                ),
                                                conditionalPanel(
                                                  condition = "output.speciesTempAvailable",
                                                  h4("Selected Species"),
                                                  DT::dataTableOutput("species_temp_table_fast")
                                                ),
                                                br(),
                                                h5(tags$b("Formatted Results (Expedited)")),
                                                uiOutput("results_selector_ui_fast"),
                                                DT::dataTableOutput("results_table_display_fast")
                            #                  )
             #                               )
                                   )
                       )
              )
  )
)

server <- function(input, output, session) {
  
  # Deployment status
  isDeployed <- reactive({
    appUrl <- session$clientData$url_hostname
    is_deployed_server <- !grepl("localhost|127.0.0.1", appUrl)
    if (is_deployed_server) {
      message("Running on a web server")
      TRUE
    } else {
      message("Running locally")
      FALSE
    }
  })
  output$deploymentStatusMain <- renderText({
    if (isDeployed()) "This app is running on a web server." else "This app is running locally."
  })
  output$deploymentStatus <- renderText({
    if (isDeployed()) "This app is running on a web server." else "This app is running locally."
  })
  
  # Auto-select expedited subtab when top-level expedited tab is chosen
  observeEvent(input$top_tabs, {
    if (identical(input$top_tabs, "expedited")) {
      updateTabsetPanel(session, "expedited_tabs", selected = "⚡1: Upload & Species Select")
    }
  })
  
  # Reactive values
  uploaded_data <- reactiveVal(FALSE)
  species.temp <- reactiveVal(NULL)
  species <- reactiveVal(NULL)
  toxicity.raw <- reactiveVal(NULL)
  species_toxicity <- reactiveVal(NULL)
  exposure.raw <- reactiveVal(NULL)
  exposure <- reactiveVal(NULL)
  exposure_chem <- reactiveVal(NULL)
  exposure_chem_eecs <- reactiveVal(NULL)
  physiochemical.raw <- reactiveVal(NULL)
  physiochemical <- reactiveVal(NULL)
  species_exposure <- reactiveVal(NULL)
  filtered_exposure <- reactiveVal(NULL)
  summarized_exposure <- reactiveVal(NULL)
  summarized_results <- reactiveVal(NULL)
  results_dataframes <- reactiveVal(NULL)  # list of two data.frames from formatResultsExport
  
  # File dropdown (Standard Workflow)
  output$file_dropdown <- renderUI({
    files <- list.files(path = "data/input_files", pattern = "\\.xlsx$")
    if (length(files) == 0) {
      selectInput("file_dropdown",
                  "Select Excel File from the 'input_files' folder:",
                  choices = c("No files found" = ""),
                  selected = "")
    } else {
      selectInput("file_dropdown",
                  "Select Excel File from the 'input_files' folder:",
                  choices = files)
    }
  })
  # File dropdown (Expedited Workflow)
  output$file_dropdown_fast <- renderUI({
    files <- list.files(path = "data/input_files", pattern = "\\.xlsx$")
    if (length(files) == 0) {
      selectInput("file_dropdown_fast",
                  "Select Excel File from the 'input_files' folder:",
                  choices = c("No files found" = ""),
                  selected = "")
    } else {
      selectInput("file_dropdown_fast",
                  "Select Excel File from the 'input_files' folder:",
                  choices = files)
    }
  })
  
  # Load template button - Standard Workflow
  observeEvent(input$load_template, {
    req(input$file_dropdown)
    validate(need(nchar(input$file_dropdown) > 0, "No template files found in data/input_files"))
    
    tryCatch({
      file_path <- file.path("data/input_files", input$file_dropdown)
      toxicity_data <- read_excel(file_path, sheet = "Endpoints to BeeREX")
      exposure_data <- read_excel(file_path, sheet = "USE SUMMARY TABLE", range = "A3:U53")
      physchem_data_raw_df <- read_excel(file_path, sheet = "Physiochemical Data Entry", range = "A2:E26")
      
      toxicity.raw(toxicity_data)
      exposure.raw(exposure_data)
      physiochemical.raw(physchem_data_raw_df)
      
      shinyjs::enable("store_toxicity_data")
      shinyjs::enable("apply_beerex_filter")
      shinyjs::enable("extract_physchem_data")
      shinyjs::enable("species_checkboxes")
      shinyjs::enable("species_checkboxes_fast")
      shinyjs::hide("data_file")
      shinyjs::hide("file_dropdown")
      shinyjs::hide("load_template")
      shinyjs::show("refresh_inputs")
      shinyjs::enable("refresh_inputs")
      shinyjs::show(id="template_selected")
      shinyjs::hide(id="input_breaks")
      
      shinyjs::show(id = "template_selected_fast")
      shinyjs::hide(id = "input_breaks_fast")
      shinyjs::hide("data_file_fast")
      shinyjs::hide("file_dropdown_fast")
      shinyjs::hide("load_template_fast")
      shinyjs::show("refresh_inputs_fast")
      shinyjs::enable("refresh_inputs_fast")
      
      uploaded_data(TRUE)
      showNotification("Template file loaded successfully", type = "message")
    }, error = function(e) {
      showNotification("Error in reading data from the selected file", type = "error")
    })
  })
  
  # Load template button - Expedited Workflow
  observeEvent(input$load_template_fast, {
    req(input$file_dropdown_fast)
    validate(need(nchar(input$file_dropdown_fast) > 0, "No template files found in data/input_files"))
    
    tryCatch({
      file_path <- file.path("data/input_files", input$file_dropdown_fast)
      toxicity_data <- read_excel(file_path, sheet = "Endpoints to BeeREX")
      exposure_data <- read_excel(file_path, sheet = "USE SUMMARY TABLE", range = "A3:U53")
      physchem_data_raw_df <- read_excel(file_path, sheet = "Physiochemical Data Entry", range = "A2:E26")
      
      toxicity.raw(toxicity_data)
      exposure.raw(exposure_data)
      physiochemical.raw(physchem_data_raw_df)
      
      shinyjs::enable("store_toxicity_data")
      shinyjs::enable("apply_beerex_filter")
      shinyjs::enable("extract_physchem_data")
      shinyjs::enable("species_checkboxes_fast")
      shinyjs::enable("species_checkboxes")
      
      shinyjs::hide("data_file_fast")
      shinyjs::hide("file_dropdown_fast")
      shinyjs::hide("load_template_fast")
      shinyjs::show("refresh_inputs_fast")
      shinyjs::enable("refresh_inputs_fast")
      shinyjs::show(id="template_selected_fast")
      shinyjs::hide(id="input_breaks_fast")
      
      uploaded_data(TRUE)
      showNotification("Template file loaded successfully (Expedited)", type = "message")
    }, error = function(e) {
      showNotification("Error in reading data from the selected file (Expedited)", type = "error")
    })
  })
  
  # File upload - Standard Workflow
  observeEvent(input$data_file, {
    req(input$data_file)
    tryCatch({
      toxicity_data <- read_excel(input$data_file$datapath, sheet = "Endpoints to BeeREX")
      exposure_data <- read_excel(input$data_file$datapath, sheet = "USE SUMMARY TABLE", range = "A3:U53")
      physchem_data_raw_df <- read_excel(input$data_file$datapath, sheet = "Physiochemical Data Entry", range = "A2:E26")
      
      toxicity.raw(toxicity_data)
      exposure.raw(exposure_data)
      physiochemical.raw(physchem_data_raw_df)
      
      shinyjs::enable("store_toxicity_data")
      shinyjs::enable("apply_beerex_filter")
      shinyjs::enable("extract_physchem_data")
      shinyjs::enable("species_checkboxes")
      shinyjs::enable("species_checkboxes_fast")
      shinyjs::hide("data_file")
      shinyjs::hide("file_dropdown")
      shinyjs::hide("load_template")
      shinyjs::show("refresh_inputs")
      shinyjs::enable("refresh_inputs")
      shinyjs::show(id="template_selected")
      shinyjs::hide(id="input_breaks")
      
      shinyjs::show(id="template_selected_fast")
      shinyjs::hide(id="input_breaks_fast")
      shinyjs::hide("data_file_fast")
      shinyjs::hide("file_dropdown_fast")
      shinyjs::hide("load_template_fast")
      shinyjs::show("refresh_inputs_fast")
      shinyjs::enable("refresh_inputs_fast")
      
      uploaded_data(TRUE)
      showNotification("Data file uploaded successfully", type = "message")
    }, error = function(e) {
      showNotification("Error in reading data from the uploaded file", type = "error")
    })
  })
  
  # File upload - Expedited Workflow
  observeEvent(input$data_file_fast, {
    req(input$data_file_fast)
    tryCatch({
      toxicity_data <- read_excel(input$data_file_fast$datapath, sheet = "Endpoints to BeeREX")
      exposure_data <- read_excel(input$data_file_fast$datapath, sheet = "USE SUMMARY TABLE", range = "A3:U53")
      physchem_data_raw_df <- read_excel(input$data_file_fast$datapath, sheet = "Physiochemical Data Entry", range = "A2:E26")
      
      toxicity.raw(toxicity_data)
      exposure.raw(exposure_data)
      physiochemical.raw(physchem_data_raw_df)
      
      shinyjs::enable("store_toxicity_data")
      shinyjs::enable("apply_beerex_filter")
      shinyjs::enable("extract_physchem_data")
      shinyjs::enable("species_checkboxes_fast")
      shinyjs::enable("species_checkboxes")
      shinyjs::hide("data_file_fast")
      shinyjs::hide("file_dropdown_fast")
      shinyjs::hide("load_template_fast")
      shinyjs::show("refresh_inputs_fast")
      shinyjs::enable("refresh_inputs_fast")
      shinyjs::show(id="template_selected_fast")
      shinyjs::hide(id="input_breaks_fast")
      
      uploaded_data(TRUE)
      showNotification("Data file uploaded successfully (Expedited)", type = "message")
    }, error = function(e) {
      showNotification("Error in reading data from the uploaded file (Expedited)", type = "error")
    })
  })
  
  # Refresh inputs - Standard Workflow
  observeEvent(input$refresh_inputs, {
    shinyjs::show("data_file")
    shinyjs::show("file_dropdown")
    shinyjs::show("load_template")
    shinyjs::disable("refresh_inputs")
    shinyjs::hide(id="template_selected")
    shinyjs::show(id="input_breaks")
    shinyjs::hide("refresh_inputs")
    shinyjs::reset("data_file")
    
    # Also reset expedited controls visibility
    shinyjs::show("data_file_fast")
    shinyjs::show("file_dropdown_fast")
    shinyjs::show("load_template_fast")
    shinyjs::disable("refresh_inputs_fast")
    shinyjs::hide(id="template_selected_fast")
    shinyjs::show(id="input_breaks_fast")
    shinyjs::hide("refresh_inputs_fast")
    shinyjs::reset("data_file_fast")
    shinyjs::hide("download_results_excel_fast")
    shinyjs::disable("select_species_and_calculate_fast")
    shinyjs::enable("species_checkboxes_fast")
    
    toxicity.raw(NULL)
    exposure.raw(NULL)
    physiochemical.raw(NULL)
    
    showNotification("Template Upload Refreshed. Raw toxicity, exposure, and physiochemical data reset", type = "warning")
  })
  
  # Refresh inputs - Expedited Workflow
  observeEvent(input$refresh_inputs_fast, {
    shinyjs::show("data_file_fast")
    shinyjs::show("file_dropdown_fast")
    shinyjs::show("load_template_fast")
    shinyjs::disable("refresh_inputs_fast")
    shinyjs::hide(id="template_selected_fast")
    shinyjs::show(id="input_breaks_fast")
    shinyjs::hide("refresh_inputs_fast")
    shinyjs::reset("data_file_fast")
    
    # Also reset standard controls visibility
    shinyjs::show("data_file")
    shinyjs::show("file_dropdown")
    shinyjs::show("load_template")
    shinyjs::disable("refresh_inputs")
    shinyjs::hide(id="template_selected")
    shinyjs::show(id="input_breaks")
    shinyjs::hide("refresh_inputs")
    shinyjs::reset("data_file")
    shinyjs::hide("download_results_excel_fast")
    shinyjs::disable("select_species_and_calculate_fast")
    shinyjs::enable("species_checkboxes_fast")
    
    toxicity.raw(NULL)
    exposure.raw(NULL)
    physiochemical.raw(NULL)
    
    showNotification("Template Upload Refreshed (Expedited). Raw toxicity, exposure, and physiochemical data reset", type = "warning")
  })
  
  # Availability flags
  output$dataUploaded <- reactive({ uploaded_data() })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  output$speciesTempAvailable <- reactive({ !is.null(species.temp()) })
  outputOptions(output, "speciesTempAvailable", suspendWhenHidden = FALSE)
  output$speciesToxicityAvailable <- reactive({ !is.null(species_toxicity()) })
  outputOptions(output, "speciesToxicityAvailable", suspendWhenHidden = FALSE)
  output$exposureAvailable <- reactive({ !is.null(exposure()) })
  outputOptions(output, "exposureAvailable", suspendWhenHidden = FALSE)
  output$physiochemicalAvailable <- reactive({ !is.null(physiochemical()) })
  outputOptions(output, "physiochemicalAvailable", suspendWhenHidden = FALSE)
  output$exposureChemAvailable <- reactive({ !is.null(exposure_chem()) })
  outputOptions(output, "exposureChemAvailable", suspendWhenHidden = FALSE)
  output$exposureChemEECsAvailable <- reactive({ !is.null(exposure_chem_eecs()) })
  outputOptions(output, "exposureChemEECsAvailable", suspendWhenHidden = FALSE)
  
  # Species selection - Standard
  observeEvent(input$species_checkboxes, {
    selected_names <- input$species_checkboxes
    if (length(selected_names) > 0) {
      combined_data <- rbindlist(lapply(selected_names, function(name) {
        file_path <- species_metadata$data_file[species_metadata$common_name == name]
        env <- new.env(parent = emptyenv())
        obj_names <- load(file.path("data/species", file_path), envir = env)
        env[[obj_names[[1]]]]
      }), fill = TRUE)
      species.temp(combined_data)
    } else {
      species.temp(NULL)
    }
    if (length(selected_names) > 0) {
      shinyjs::enable("select_species")
    } else {
      shinyjs::disable("select_species")
    }
  })
  
  observeEvent(input$select_species, {
    species(species.temp())
    shinyjs::disable("species_checkboxes")
    shinyjs::disable("select_species")
    shinyjs::enable("refresh_species")
    showNotification("Species selection successful", type = "message")
  })
  
  observeEvent(input$refresh_species, {
    shinyjs::enable("species_checkboxes")
    shinyjs::disable("select_species")
    shinyjs::disable("refresh_species")
    updateCheckboxGroupInput(session, "species_checkboxes", selected = character(0))
    species.temp(NULL)
  })
  
  # Species selection - Expedited (enable/disable combined button)
  observeEvent(input$species_checkboxes_fast, {
    selected_names <- input$species_checkboxes_fast
    if (length(selected_names) > 0) {
      combined_data <- rbindlist(lapply(selected_names, function(name) {
        file_path <- species_metadata$data_file[species_metadata$common_name == name]
        env <- new.env(parent = emptyenv())
        obj_names <- load(file.path("data/species", file_path), envir = env)
        env[[obj_names[[1]]]]
      }), fill = TRUE)
      species.temp(combined_data)
      shinyjs::enable("select_species_and_calculate_fast")
    } else {
      species.temp(NULL)
      shinyjs::disable("select_species_and_calculate_fast")
    }
  })
  
  # Combined: finalize species selection and run expedited workflow
  observeEvent(input$select_species_and_calculate_fast, {
    req(species.temp())
    species(species.temp())
    shinyjs::disable("species_checkboxes_fast")
    shinyjs::disable("select_species_and_calculate_fast")
    shinyjs::enable("refresh_species_fast")
    showNotification("Species selected. Running expedited calculations...", type = "message")
    
    # Run expedited pipeline
    req(toxicity.raw(), exposure.raw(), physiochemical.raw())
    withProgress(message = "Running Expedited Workflow...", value = 0, {
      incProgress(1/6, detail = "Assembling toxicity and species data...")
      merged <- merge(species(), toxicity.raw(), by = c("common_name", "life_stage"), all.x = TRUE, all.y = TRUE, allow.cartesian = TRUE)
      species_toxicity(merged)
      
      incProgress(2/6, detail = "Filtering exposure data...")
      exposure_raw_df <- as.data.frame(exposure.raw())
      filtered_exposure_df <- data.frame(
        index = exposure_raw_df[[1]],
        use_site = exposure_raw_df[[2]],
        max_application_rate = exposure_raw_df[[3]],
        application_type = exposure_raw_df[[7]],
        soil_or_foliar = exposure_raw_df[[16]],
        seed_treatment_rate = exposure_raw_df[[17]],
        udl_type = exposure_raw_df[[21]],
        check.names = FALSE
      ) %>%
        dplyr::filter(!is.na(use_site) & use_site != "")
      exposure(filtered_exposure_df)
      
      incProgress(3/6, detail = "Extracting physiochemical data...")
      physchem_data_raw_df <- physiochemical.raw()
      koc_value <- physchem_data_raw_df[1, 4]
      log_kow_value <- physchem_data_raw_df[20, 2]
      physiochemical_df <- data.frame(
        koc = as.numeric(koc_value),
        log_kow = as.numeric(log_kow_value)
      )
      physiochemical(physiochemical_df)
      exposure_chem_df <- exposure() %>%
        mutate(koc = physiochemical_df$koc, log_kow = physiochemical_df$log_kow)
      exposure_chem(exposure_chem_df)
      
      incProgress(4/6, detail = "Calculating EECs...")
      eecs_result <- EECs(exposure_chem())
      exposure_chem_eecs(eecs_result)
      
      incProgress(5/6, detail = "Calculating Dose, RQs, & MODs...")
      result <- totalDoseRQsMODs(species_toxicity(), exposure_chem_eecs(), progress = function(fraction) {
        incProgress(fraction/6, detail = "Calculating detailed results...")
      })
      species_exposure(result)
      
      incProgress(6/6, detail = "Summarizing results...")
      out <- formatResultsExport(species_exposure(), exposure_chem_eecs())
      validate(need(is.list(out) && length(out) >= 2, "Summarize Results failed to produce output."))
      nm <- names(out)
      if (is.null(nm) || any(nm == "")) {
        nm <- c("on_site", "off_site")[seq_len(length(out))]
        names(out) <- nm
      }
      results_dataframes(out)
      
      # Render results selector and show export button
      output$results_selector_ui_fast <- renderUI({
        radioButtons("results_table_select_fast", "Select Results Table",
                     choices = names(results_dataframes()),
                     selected = names(results_dataframes())[1],
                     inline = TRUE)
      })
      shinyjs::show("download_results_excel_fast")
      
      showNotification("Expedited workflow completed. Results are ready.", type = "message")
    })
  })
  
  observeEvent(input$refresh_species_fast, {
    shinyjs::enable("species_checkboxes_fast")
    shinyjs::disable("select_species_and_calculate_fast")
    shinyjs::disable("refresh_species_fast")
    updateCheckboxGroupInput(session, "species_checkboxes_fast", selected = character(0))
    species.temp(NULL)
    shinyjs::hide("download_results_excel_fast")
  })
  
  # Tables (all use dt_mixed for display formatting)
  output$species_temp_table <- DT::renderDT({
    req(species.temp())
    dt_mixed(species.temp())
  })
  output$species_temp_table_fast <- DT::renderDT({
    req(species.temp())
    dt_mixed(species.temp())
  })
  
  output$toxicity_raw_table <- DT::renderDT({
    req(toxicity.raw())
    dt_mixed(toxicity.raw())
  })
  
  # Standard Workflow: assemble toxicity + species
  observeEvent(input$store_toxicity_data, {
    req(toxicity.raw(), species.temp())
    merged <- merge(species.temp(), toxicity.raw(), by = c("common_name", "life_stage"), all.x = TRUE, all.y = TRUE, allow.cartesian = TRUE)
    species_toxicity(merged)
    output$species_toxicity_table <- DT::renderDT({
      req(species_toxicity())
      dt_mixed(species_toxicity())
    })
    showNotification("Toxicity data stored and merged successfully", type = "message")
    shinyjs::disable("store_toxicity_data")
  })
  
  output$exposure_raw_table <- DT::renderDT({
    req(exposure.raw())
    dt_mixed(exposure.raw())
  })
  
  # Standard Workflow: apply BeeREX exposure filter
  observeEvent(input$apply_beerex_filter, {
    req(exposure.raw())
    exposure_raw_df <- as.data.frame(exposure.raw())
    filtered_exposure_df <- data.frame(
      index = exposure_raw_df[[1]],
      use_site = exposure_raw_df[[2]],
      max_application_rate = exposure_raw_df[[3]],
      application_type = exposure_raw_df[[7]],
      soil_or_foliar = exposure_raw_df[[16]],
      seed_treatment_rate = exposure_raw_df[[17]],
      udl_type = exposure_raw_df[[21]],
      check.names = FALSE
    ) %>%
      dplyr::filter(!is.na(use_site) & use_site != "")
    exposure(filtered_exposure_df)
    output$exposure_table <- DT::renderDT({
      req(exposure())
      dt_mixed(exposure())
    })
    showNotification("BeeREX filter applied successfully", type = "message")
    shinyjs::disable("apply_beerex_filter")
  })
  
  output$physiochemical_raw_table <- DT::renderDT({
    req(physiochemical.raw())
    dt_mixed(physiochemical.raw())
  })
  
  # Standard Workflow: extract physchem and format exposure chem
  observeEvent(input$extract_physchem_data, {
    req(physiochemical.raw(), exposure())
    physchem_data_raw_df <- physiochemical.raw()
    koc_value <- physchem_data_raw_df[1, 4]
    log_kow_value <- physchem_data_raw_df[20, 2]
    physiochemical_df <- data.frame(
      koc = as.numeric(koc_value),
      log_kow = as.numeric(log_kow_value)
    )
    physiochemical(physiochemical_df)
    
    exposure_chem_df <- exposure() %>%
      mutate(koc = physiochemical_df$koc, log_kow = physiochemical_df$log_kow)
    exposure_chem(exposure_chem_df)
    
    output$physiochemical_table <- DT::renderDT({
      req(physiochemical())
      dt_mixed(physiochemical())
    })
    output$exposure_chem_table <- DT::renderDT({
      req(exposure_chem())
      dt_mixed(exposure_chem())
    })
    shinyjs::enable("calculate_eecs")
    showNotification("Physiochemical data extracted and exposure data formatted successfully", type = "message")
    shinyjs::disable("extract_physchem_data")
  })
  
  # Show Exposure Chem data (checkbox in 2.1)
  output$exposure_chem_display <- DT::renderDT({
    req(exposure_chem())
    dt_mixed(exposure_chem())
  })
  
  output$exposure_chem_eecs_table <- DT::renderDT({
    req(exposure_chem_eecs())
    dt_mixed(exposure_chem_eecs())
  })
  output$exposure_chem_eecs_table_model <- DT::renderDT({
    req(exposure_chem_eecs())
    dt_mixed(exposure_chem_eecs())
  })
  
  # Standard Workflow: calculate EECs
  observeEvent(input$calculate_eecs, {
    req(exposure_chem())
    eecs_result <- EECs(exposure_chem())
    exposure_chem_eecs(eecs_result)
    showNotification("EECs calculated successfully", type = "message")
    shinyjs::disable("calculate_eecs")
    shinyjs::enable("calculate_total_dose")
    shinyjs::show("toggle_eecs_table")
  })
  
  # Standard Workflow: calculate Total Dose, RQs & MODs
  observeEvent(input$calculate_total_dose, {
    req(species_toxicity(), exposure_chem_eecs())
    withProgress(message = 'Calculating Total Dose, RQs, & MODs...', value = 0, {
      result <- totalDoseRQsMODs(species_toxicity(), exposure_chem_eecs(), progress = function(fraction) {
        incProgress(fraction, detail = paste("Processing..."))
      })
      species_exposure(result)
      
      output$species_exposure_ui <- renderUI({
        selectInput("species_exposure_select", "Select Exposure Data", choices = names(species_exposure()))
      })
      
      observeEvent(input$species_exposure_select, {
        selected_index <- as.numeric(strsplit(input$species_exposure_select, " ")[[1]][1])
        filtered_exposure(species_exposure()[[input$species_exposure_select]])
        
        output$exposure_chem_eecs_row <- DT::renderDT({
          req(exposure_chem_eecs())
          dt_mixed(dplyr::filter(exposure_chem_eecs(), index == selected_index), pageLength = 1)
        })
        output$species_exposure_table <- DT::renderDT({
          req(filtered_exposure())
          dt_mixed(filtered_exposure())
        })
        output$species_exposure_plot <- renderPlot({
          req(filtered_exposure())
          data <- filtered_exposure()
          data <- data %>% dplyr::filter(rq_mod_foliar >= 0 | rq_mod_soil >= 0 | rq_mod_seed >= 0)
          data$level_of_organization <- factor(data$level_of_organization, 
                                               levels = c("non-listed", "listed individual", "listed population", "listed community"))
          validate(need(nrow(data) > 0, "No data to visualize"))
          ggplot(data, aes(x = common_name, y = rq_mod_foliar, fill = interaction(acute_subacute_chronic, life_stage))) +
            geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
            facet_wrap(~ level_of_organization) +
            labs(x = "Species", y = "RQ MOD Foliar", title = "Total Dose, RQs, and MODs by Species, Life Stage, and Exposure Type") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_fill_brewer(palette = "Set3") +
            guides(fill = guide_legend(title = "Exposure Type and Life Stage")) +
            theme_minimal() +
            theme(panel.grid.major = element_line(linewidth = 0.5, linetype = 'dashed', colour = "gray"),
                  panel.grid.minor = element_blank(),
                  strip.background = element_rect(fill = "lightblue", color = "black"),
                  strip.text = element_text(size = 12, face = "bold"))
        })
      })
      
      showNotification("Total Dose, RQs, and MODs calculated successfully", type = "message")
      shinyjs::disable("calculate_total_dose")
      shinyjs::show("toggle_plot")
      shinyjs::show("toggle_exposure")
      shinyjs::enable("summarize_results")
    })
  })
  
  observe({
    if (input$toggle_plot) {
      shinyjs::show("species_exposure_plot")
    } else {
      shinyjs::hide("species_exposure_plot")
    }
  })
  
  # Standard Workflow: Summarize Results
  observeEvent(input$summarize_results, {
    req(species_exposure(), exposure_chem_eecs())
    shinyjs::disable("summarize_results")
    showNotification("Formatting results for export...", type = "message")
    
    tryCatch({
      out <- formatResultsExport(species_exposure(), exposure_chem_eecs())
      if (!is.list(out) || length(out) < 2) {
        showNotification("formatResultsExport did not return a list of two data.frames.", type = "error")
        shinyjs::enable("summarize_results")
        return()
      }
      nm <- names(out)
      if (is.null(nm) || any(nm == "")) {
        nm <- c("on_site", "off_site")[seq_len(length(out))]
        names(out) <- nm
      }
      results_dataframes(out)
      
      output$results_selector_ui <- renderUI({
        radioButtons("results_table_select", "Select Results Table",
                     choices = names(results_dataframes()),
                     selected = names(results_dataframes())[1],
                     inline = TRUE)
      })
      
      showNotification("Results summarized successfully", type = "message")
    }, error = function(e) {
      showNotification(paste("Error summarizing results:", e$message), type = "error")
      shinyjs::enable("summarize_results")
    })
  })
  
  output$results_table_display <- DT::renderDT({
    req(results_dataframes())
    req(input$results_table_select)
    df <- results_dataframes()[[input$results_table_select]]
    dt_mixed(df)
  })
  
  output$download_results_excel <- downloadHandler(
    filename = function() {
      paste0("BeeREX_results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(results_dataframes())
      lst <- results_dataframes()
      to_write <- list(
        on_site = lst[[1]],
        off_site = lst[[2]]
      )
      writexl::write_xlsx(to_write, path = file)
    }
  )
  
  # Expedited: display results and download in same subtab
  output$results_table_display_fast <- DT::renderDT({
    req(results_dataframes())
    selected <- input$results_table_select_fast
    if (is.null(selected)) selected <- names(results_dataframes())[1]
    df <- results_dataframes()[[selected]]
    dt_mixed(df)
  })
  
  output$download_results_excel_fast <- downloadHandler(
    filename = function() {
      paste0("BeeREX_results_expedited_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(results_dataframes())
      lst <- results_dataframes()
      to_write <- list(
        on_site = lst[[1]],
        off_site = lst[[2]]
      )
      writexl::write_xlsx(to_write, path = file)
    }
  )
  
  # Developer export
  observeEvent(input$export_to_r, {
    exported_objects <- list()
    if (!is.null(species())) { assign("species.out", species(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "species.out") }
    if (!is.null(toxicity.raw())) { assign("toxicity.raw.out", toxicity.raw(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "toxicity.raw.out") }
    if (!is.null(species_toxicity())) { assign("species_toxicity.out", species_toxicity(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "species_toxicity.out") }
    if (!is.null(exposure.raw())) { assign("exposure.raw.out", exposure.raw(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "exposure.raw.out") }
    if (!is.null(exposure())) { assign("exposure.out", exposure(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "exposure.out") }
    if (!is.null(exposure_chem())) { assign("exposure_chem.out", exposure_chem(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "exposure_chem.out") }
    if (!is.null(exposure_chem_eecs())) { assign("exposure_chem_eecs.out", exposure_chem_eecs(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "exposure_chem_eecs.out") }
    if (!is.null(physiochemical.raw())) { assign("physiochemical.raw.out", physiochemical.raw(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "physiochemical.raw.out") }
    if (!is.null(physiochemical())) { assign("physiochemical.out", physiochemical(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "physiochemical.out") }
    if (!is.null(species_exposure())) { assign("species_exposure.out", species_exposure(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "species_exposure.out") }
    if (!is.null(results_dataframes())) { assign("results_dataframes.out", results_dataframes(), envir = .GlobalEnv); exported_objects <- c(exported_objects, "results_dataframes.out") }
    
    if (length(exported_objects) > 0) {
      showNotification(paste("Successfully exported:", paste(exported_objects, collapse = ", ")), type = "message")
    } else {
      showNotification("No objects were exported", type = "warning")
    }
    if (length(exported_objects) > 0) {
      shinyjs::disable("export_to_r")
    }
  })
}

shinyApp(ui = ui, server = server)