#' UI Module Definitions - Unified Version
#'
#' This file contains modular UI components that are used throughout the application.
#' Each component follows consistent patterns and shares common resources.

#' Common Section Box
#' 
#' Standardized section container used across all modules
#' @param title Section title
#' @param description Optional description text
#' @param ... Content to include in the section
#' @return Standardized section div
sectionBox <- function(title, description = NULL, ...) {
  div(
    class = "section-box",
    div(class = "section-title", title),
    if (!is.null(description)) {
      div(class = "section-description", description)
    },
    ...
  )
}

#' Common Header Layout
#' 
#' Standardized header with title and progress indicator
#' @param main_title Main page title
#' @param sub_title Sub-title or step description
#' @param current_step Current step number (for progress indicator)
#' @param show_progress Whether to show progress indicator
#' @return Standardized header layout
commonHeader <- function(main_title, sub_title = NULL, current_step = NULL, show_progress = TRUE) {
  fluidRow(
    column(
      width = if (show_progress && !is.null(current_step)) 6 else 12,
      div(
        h2(main_title),
        if (!is.null(sub_title)) h3(sub_title)
      )
    ),
    if (show_progress && !is.null(current_step)) {
      column(
        width = 6,
        stepProgressUI(current_step, "step_nav")
      )
    }
  )
}

#' Common Navigation Buttons
#' 
#' Standardized back/continue button layout
#' @param ns Namespace function
#' @param show_back Whether to show back button
#' @param continue_text Text for continue button
#' @param back_text Text for back button
#' @return Standardized button layout
commonNavigation <- function(ns, show_back = TRUE, continue_text = "Continue →", back_text = "← Back") {
  div(
    style = "text-align: right; margin-top: 20px;",
    if (show_back) {
      actionButton(
        ns("back"),
        back_text,
        class = "back-btn"
      )
    },
    actionButton(
      ns("continue"),
      continue_text,
      class = "continue-btn"
    )
  )
}

#' Create the Step Progress Indicator
#'
#' @param current_step Integer indicating current step (1-3)
#' @param id Module ID for namespacing
#' @return UI element for progress indicator
stepProgressUI <- function(current_step, id) {
  ns <- NS(id)
  
  # Calculate progress percentage
  progress_pct <- (current_step - 1) * 33.33
  if (current_step == 3) progress_pct <- 100
  
  div(
    class = "progress-container",
    div(
      class = "progress-bar-container",
      div(class = "progress-bar-fill", style = paste0("width: ", progress_pct, "%;"))
    ),
    div(
      style = "display: flex; justify-content: space-between; width: 100%;",
      lapply(1:3, function(step) {
        actionButton(
          inputId = ns(paste0("goto_step", step)),
          label = as.character(step),
          class = paste("btn-circle", ifelse(current_step >= step, "active", "inactive")),
          style = paste0(
            "width: 30px; height: 30px; border-radius: 50%; padding: 0; ",
            "background-color: ", ifelse(current_step >= step, "#3498db", "#e0e0e0"), "; ",
            "color: ", ifelse(current_step >= step, "white", "#666"), ";"
          )
        )
      })
    )
  )
}

#' Create Directory Selection Input
#'
#' @param id Base ID for the input elements
#' @param value Initial value for the directory path
#' @param placeholder Placeholder text for the input
#' @return UI element for directory selection
directoryInputUI <- function(id, value = "", placeholder = "Project directory path") {
  div(
    class = "directory-input",
    textInput(
      NS(id, "path"),
      label = NULL,
      value = value,
      placeholder = placeholder,
      width = "100%"
    ),
    shinyDirButton(
      NS(id, "select"),
      label = "...",
      title = "Select a project directory",
      class = "browse-btn"
    )
  )
}

#' File Browser UI Component
#' 
#' @param id Module ID
#' @param title Section title
#' @param description Section description
#' @param with_convert Whether to show convert link
#' @return File browser UI
fileBrowserUI <- function(id, title, description, with_convert = FALSE) {
  ns <- NS(id)
  
  sectionBox(
    title = title,
    description = if (with_convert) {
      tagList(
        description,
        " If your data are not yet in CSV format, start by ",
        actionLink(ns("convert"), "converting them", class = "blue-link")
      )
    } else {
      description
    },
    div(
      class = "file-browser",
      style = "background-color: white; border: 2px solid #ced4da; border-radius: 3px; padding: 10px; height: 200px; overflow-y: auto; margin-top: 10px;",
      uiOutput(ns("file_container"))
    )
  )
}

#' Optional Directories UI Component
#'
#' @param id Module ID
#' @return UI element for optional directories
optionalDirsUI <- function(id) {
  ns <- NS(id)
  
  sectionBox(
    title = "Optional Directories",
    description = "Select additional new directories to create in your project folder.",
    div(
      class = "file-browser",
      div(
        id = ns("container"),
        checkboxInput(ns("dir_analysis"), "analysis/", value = TRUE),
        checkboxInput(ns("dir_materials"), "materials/", value = TRUE),
        checkboxInput(ns("dir_results"), "results/", value = FALSE),
        checkboxInput(ns("dir_products"), "products/", value = FALSE),
        checkboxInput(ns("dir_documentation"), "documentation/", value = FALSE)
      )
    ),
    div(class = "custom-dir-section", "Add custom directory:"),
    div(
      class = "directory-input",
      textInput(
        ns("custom_name"),
        label = NULL,
        value = "",
        placeholder = "Directory name",
        width = "70%"
      ),
      actionButton(
        ns("add"),
        "Add",
        class = "browse-btn",
        style = "width: 25%; margin-left: 5%;"
      )
    )
  )
}

#' Step 1 UI: Project Directory and Data Files
#'
#' @param id Namespace ID for the module
#' @return UI for Step 1
step1UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    commonHeader(
      main_title = "Create Dataset",
      sub_title = "Step 1: Project Directory and Canonical Data Files",
      current_step = 1
    ),

    # Data Safety Banner
    div(
      class = "alert",
      style = "background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; padding: 15px; margin-bottom: 20px; display: flex; align-items: start;",
      icon("shield-alt", style = "color: #0c5460; font-size: 24px; margin-right: 15px; margin-top: 2px;"),
      div(
        style = "flex: 1;",
        strong("Your Original Files Are Safe", style = "color: #0c5460; display: block; margin-bottom: 5px; font-size: 16px;"),
        span(
          style = "color: #0c5460; line-height: 1.5;",
          "This tool will create a new Psych-DS dataset in a location you choose at the end of Step 3. ",
          strong("Your original files and directories will never be modified."),
          " We only read from your existing files to create standardized copies."
        )
      )
    ),
    
    
    sectionBox(
      title = "Project Directory",
      description = "The goal of Psych-DS is to standardize your data in the context of the rest of the research materials that relate to it. Select a project directory where you want your new `data/` folder to appear. This directory should also contain both your existing data files and optionally other project materials (e.g. analyses, papers.)",
      directoryInputUI(ns("project_dir"))
    ),
    
    fluidRow(
      column(
        width = 7,
        fileBrowserUI(
          ns("files"),
          "Select Data Files",
          "Select CSV files that contain the data you want to include in the Psych-DS data/ folder.",
          with_convert = TRUE
        )
      ),
      column(
        width = 5,
        optionalDirsUI(ns("opt_dirs"))
      )
    ),
    
    commonNavigation(ns, show_back = FALSE, continue_text = "Continue →")
  )
}

#' Step 2 UI: Dataset Metadata
#'
#' @param id Namespace ID for the module
#' @return UI for Step 2
step2UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    commonHeader(
      main_title = "Create Dataset",
      sub_title = "Step 2: Dataset Metadata",
      current_step = 2
    ),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      "We'll generate a text file called dataset_description.json containing information about your dataset. This file is part of Psych-DS - it is placed in your project directory and is used to confirm whether your specific dataset is organized to the Psych-DS standard."
    ),
    
    fluidRow(
      column(
        width = 6,
        sectionBox(
          title = "Detected Variables",
          description = "The following column headers were detected in your selected files. Variables with the same name across multiple files are assumed to have identical definitions.",
          div(
            style = "margin-top: 15px;",
            DT::dataTableOutput(ns("variables_table"))
          ),
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-radius: 3px;",
            p(
              style = "color: #856404; margin-bottom: 0;",
              "Note: If you need different definitions for variables that currently have the same name across files, you will need to rename them to be distinct. Make these changes in your CSV files and restart the process."
            )
          )
        )
      ),
      column(
        width = 6,
        sectionBox(
          title = "Dataset Information",
          div(
            style = "margin-bottom: 15px;",
            textInput(
              ns("dataset_name"),
              "Name *",
              placeholder = "e.g., Visual Attention Experiment 2023"
            )
          ),
          div(
            style = "margin-bottom: 15px;",
            textAreaInput(
              ns("dataset_description"),
              "Description *",
              placeholder = "Briefly describe your dataset",
              height = "100px"
            )
          )
        ),
        sectionBox(
          title = "Author Information",
          p("Author Names (separate with commas)"),
          div(
            id = ns("authors_container"),
            style = "max-height: 200px; overflow-y: auto; border: 1px solid #ced4da; border-radius: 3px; margin-bottom: 15px;",
            div(
              style = "display: flex; background-color: #f8f9fa; padding: 5px; border-bottom: 1px solid #ced4da;",
              div(style = "flex: 2;", strong("Name")),
              div(style = "flex: 2;", strong("ORCID ID"))
            ),
            uiOutput(ns("author_list"))
          ),
          actionButton(
            ns("add_author"),
            "Add New Author",
            class = "btn-primary",
            style = "margin-bottom: 15px;"
          )
        )
      )
    ),
    
    commonNavigation(ns, show_back = TRUE)
  )
}

#' Step 3 UI: Standardize Filenames - Enhanced Version
#'
#' @param id Namespace ID for the module
#' @return UI for Step 3 with multi-select and auto-naming features
step3UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    commonHeader(
      main_title = "Create Dataset",
      sub_title = "Step 3: Standardize Filenames",
      current_step = 3
    ),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      "Rename your data files to follow Psych-DS naming conventions. Each filename will be composed of a set of keywords and custom values to create a clear, consistent description.",
      tags$br(), tags$br(),
      strong("New features:"), 
      " Check boxes to select multiple files and apply keywords in batch. Use Auto-Name to automatically fill keyword values from constant columns in your data."
    ),
    
    fluidRow(
      # LEFT COLUMN - File Selection and Auto-Naming
      column(
        width = 6,
        sectionBox(
          title = "1. File Mapping",
          description = "Check boxes to select multiple files. Click a file to configure its keywords individually.",
          # Select All / Deselect All buttons
          div(
            style = "margin-bottom: 10px; display: flex; gap: 10px;",
            actionButton(
              ns("select_all_files"),
              "Select All",
              class = "btn btn-sm btn-info",
              style = "flex: 1;"
            ),
            actionButton(
              ns("deselect_all_files"),
              "Deselect All",
              class = "btn btn-sm btn-default",
              style = "flex: 1;"
            )
          ),
          div(
            class = "file-browser",
            style = "height: 250px; overflow-y: auto; margin-bottom: 15px;",
            div(
              class = "file-list-header",
              fluidRow(
                column(1, HTML("&nbsp;")),  # Space for checkbox
                column(5, strong("Original Filename")),
                column(6, strong("New Filename"))
              )
            ),
            div(
              id = ns("file_mapping_list"),
              class = "file-mapping-list",
              uiOutput(ns("file_mapping_rows"))
            )
          ),
          # Batch operations section
          div(
            style = "margin-top: 10px; padding: 10px; background-color: #f0f8ff; border-radius: 4px;",
            strong("Batch Operations:"),
            div(
              style = "margin-top: 8px;",
              uiOutput(ns("selected_count_text")),
              conditionalPanel(
                condition = paste0("output['", ns("has_selection"), "']")
              )
            )
          )
        ),
        
        div(
          id = ns("current_file_indicator"),
          class = "alert alert-info",
          style = "margin-top: 15px;",
          uiOutput(ns("current_file_text"))
        ),
        
        sectionBox(
          title = "2. Auto-Name from Data",
          description = "Automatically fill keyword values from constant columns across all selected files. This will apply the keyword and generate filenames for all selected files.",
          div(
            id = ns("auto_name_section"),
            conditionalPanel(
              condition = paste0("output['", ns("has_constant_columns"), "']"),
              fluidRow(
                column(
                  width = 5,
                  selectInput(
                    ns("auto_keyword"),
                    "Keyword to fill:",
                    choices = NULL,
                    width = "100%"
                  )
                ),
                column(
                  width = 5,
                  selectInput(
                    ns("auto_column"),
                    "From column:",
                    choices = NULL,
                    width = "100%"
                  )
                ),
                column(
                  width = 2,
                  tags$label(HTML("&nbsp;"), style = "display: block;"),
                  actionButton(
                    ns("apply_auto_name"),
                    icon("magic"),
                    class = "btn btn-success",
                    style = "width: 100%;",
                    title = "Apply auto-naming to selected files"
                  )
                )
              ),
              div(
                style = "margin-top: 10px; padding: 8px; background-color: #fff3cd; border-radius: 4px;",
                icon("info-circle", style = "color: #856404; margin-right: 5px;"),
                tags$small(
                  style = "color: #856404;",
                  "This will apply to ALL selected files and generate provisional filenames."
                )
              )
            ),
            conditionalPanel(
              condition = paste0("!output['", ns("has_constant_columns"), "']"),
              div(
                style = "padding: 15px; text-align: center; color: #999;",
                "No constant-value columns detected. Select files to see available columns.",
                tags$br(),
                tags$small("(Constant columns are those where all non-empty cells have the same value)")
              )
            )
          )
        )
      ),
      
      # RIGHT COLUMN - Manual Keyword Configuration
      column(
        width = 6,
        sectionBox(
          title = "3. Choose Keywords",
          div(id = ns("choose_keywords_section"), style = "position: relative; top: -20px;"),
          div(
            class = "section-description",
            style = "margin-bottom: 15px; padding: 10px; background-color: #e8f4f8; border-radius: 4px; border-left: 4px solid #3498db;",
            strong("Naming Rules:"),
            tags$ul(
              style = "margin: 8px 0; padding-left: 20px;",
              tags$li("Keyword values must contain only letters and numbers (a-z, A-Z, 0-9)"),
              tags$li("Custom keywords must contain only lowercase letters (a-z)"),
              tags$li("No spaces, punctuation, or special characters allowed")
            )
          ),
          div(
            class = "section-description",
            "Select keywords to use in your filename. Choose keywords that are meaningful for your dataset and use them consistently."
          ),
          div(
            class = "keyword-option-chips",
            style = "margin-bottom: 15px;",
            lapply(c("session", "subject", "study", "task", "condition", "stimulus", "trial", "description"), function(keyword) {
              actionButton(
                ns(paste0("keyword_", keyword)), 
                keyword, 
                class = "btn btn-sm btn-primary keyword-chip"
              )
            })
          ),
          div(class = "section-title", "Selected Keywords"),
          div(
            id = ns("selected_keywords_container"),
            class = "selected-keywords-container",
            style = "min-height: 80px; padding: 10px; background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; margin-bottom: 15px;",
            uiOutput(ns("selected_keywords"))
          )
        ),
        
        sectionBox(
          title = "4. Add Custom Keyword",
          description = "Custom keywords must contain only lowercase letters (a-z). No numbers, spaces, or punctuation.",
          div(
            class = "input-group",
            style = "margin-bottom: 15px;",
            textInput(ns("custom_keyword_name"), NULL, 
                      placeholder = "e.g., session, condition, group", width = "70%"),
            div(
              class = "input-group-btn",
              style = "width: 30%;",
              actionButton(ns("add_custom_keyword"), "Add", class = "btn btn-primary", style = "width: 100%;")
            )
          ),
          div(
            id = ns("custom_keyword_validation"),
            style = "margin-top: 5px; min-height: 20px;"
          )
        ),
        
        sectionBox(
          title = "5. Keyword Values",
          div(
            class = "keyword-values-section",
            style = "min-height: 250px;",
            uiOutput(ns("keyword_value_inputs"))
          )
        ),
        
        sectionBox(
          title = "6. Filename Preview",
          div(
            class = "well well-sm filename-preview",
            style = "background-color: #f8f9fa; border: 1px solid #ced4da; padding: 15px; text-align: center; margin: 10px 0; min-height: 60px;",
            uiOutput(ns("filename_preview"))
          ),
          div(
            style = "text-align: center; display: flex; gap: 10px;",
            actionButton(ns("preview_filename"), "Preview Filename", class = "btn btn-secondary", style = "flex: 1;"),
            actionButton(ns("generate_filename"), "Generate Filename", class = "btn btn-primary", style = "flex: 1;")
          )
        ),
        
        tags$div(
          id = ns("debug_output"),
          style = "display: none;",
          verbatimTextOutput(ns("debug_text"))
        )
      )
    ),
    
    commonNavigation(ns, show_back = TRUE)
  )
}

#' Validate Dataset UI Module
#'
#' @param id The module ID
#' @return UI elements for dataset validation
validateUI <- function(id) {
  ns <- NS(id)
  
  # Define validation steps structure
  validation_steps <- list(
    list(key = "start", message = list(imperative = "Start validation", pastTense = "Validation started"), subSteps = list()),
    list(key = "check-folder", message = list(imperative = "Find project folder", pastTense = "Project folder found"), 
         subSteps = list(list(key = "build-tree", message = list(imperative = "Crawl project folder and construct file tree", pastTense = "Project folder crawled and file tree constructed")))),
    list(key = "find-metadata", message = list(imperative = "Find metadata file", pastTense = 'Metadata file "dataset_description.json" found'), subSteps = list()),
    list(key = "find-data-dir", message = list(imperative = 'Find "data" subfolder', pastTense = '"data" subfolder found'), subSteps = list()),
    list(key = "parse-metadata", message = list(imperative = 'Parse metadata file', pastTense = 'Metadata file parsed successfully'),
         subSteps = list(
           list(key = "metadata-utf8", message = list(imperative = "Check metadata file for utf-8 encoding", pastTense = "Metadata file is utf-8 encoded")),
           list(key = "metadata-json", message = list(imperative = "Parse metadata file as JSON", pastTense = "Metadata file parsed successfully")),
           list(key = "metadata-jsonld", message = list(imperative = "Validate metadata file as JSON-LD", pastTense = "Metadata file is valid JSON-LD")),
           list(key = "metadata-fields", message = list(imperative = 'Check metadata file for required fields', pastTense = 'Metadata file contains required fields')),
           list(key = "metadata-type", message = list(imperative = 'Check metadata file for @type field', pastTense = 'Metadata file has correct @type field'))
         )),
    list(key = "check-for-csv", message = list(imperative = 'Check for CSV files', pastTense = 'CSV files found'), subSteps = list()),
    list(key = "validate-csvs", message = list(imperative = 'Validate CSV files', pastTense = 'CSV files validated'),
         subSteps = list(
           list(key = "csv-keywords", message = list(imperative = "Check filename for keyword formatting", pastTense = "Filename uses valid keyword formatting")),
           list(key = "csv-parse", message = list(imperative = "Parse data file as CSV", pastTense = "Data file successfully parsed as CSV")),
           list(key = "csv-header", message = list(imperative = "Check for header line", pastTense = "Header line found")),
           list(key = "csv-nomismatch", message = list(imperative = "Check all lines for equal number of cells", pastTense = "All lines have equal number of cells")),
           list(key = "csv-rowid", message = list(imperative = "Check for any row_id columns with non-unique values", pastTense = "All row_id columns have unique values"))
         )),
    list(key = "check-variableMeasured", message = list(imperative = 'Check column headers', pastTense = 'Column headers validated'), subSteps = list())
  )
  
  tagList(
    commonHeader(
      main_title = "Validate Dataset",
      sub_title = NULL,
      show_progress = FALSE
    ),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      "Select a directory containing a Psych-DS dataset to validate against the schema."
    ),
    
    sectionBox(
      title = "Select Dataset",
      description = "Select a Psych-DS dataset directory to validate.",
      div(
        class = "directory-input",
        textInput(
          "validate_dir",
          label = NULL,
          value = "",
          placeholder = "Path to Psych-DS dataset",
          width = "100%"
        ),
        shinyDirButton(
          "validate_dir_select",
          label = "...",
          title = "Select a dataset directory",
          class = "browse-btn"
        )
      ),
      div(
        style = "text-align: right; margin-top: 20px;",
        actionButton(
          "validate_btn",
          "Validate",
          class = "continue-btn"
        )
      )
    ),
    
    sectionBox(
      title = "Validation Results",
      div(
        id = ns("validation_results_ui"),
        h3("Validation Progress"),
        div(
          class = "validation-checklist",
          style = "max-height: 500px; overflow-y: auto;",
          lapply(validation_steps, function(superStep) {
            div(
              class = "step-item",
              `data-step-key` = superStep$key,
              style = "margin-bottom: 15px; padding: 10px; border-radius: 5px; background-color: #f9f9f9; border: 1px solid #ddd;",
              div(
                class = "step-header",
                style = "font-weight: bold; display: flex; align-items: center;",
                span(class = "step-icon", "⋯", style = "color: #ffc107; margin-right: 10px; font-size: 18px;"),
                span(
                  class = "step-message",
                  superStep$message$imperative,
                  `data-imperative` = superStep$message$imperative,
                  `data-past-tense` = superStep$message$pastTense
                )
              ),
              div(class = "step-issue-container"),
              if (length(superStep$subSteps) > 0) {
                div(
                  class = "substeps",
                  style = "margin-top: 10px; margin-left: 20px; padding-left: 10px; border-left: 2px solid #ddd;",
                  lapply(superStep$subSteps, function(subStep) {
                    div(
                      class = "substep-item",
                      `data-step-key` = subStep$key,
                      style = "margin-bottom: 8px; padding: 5px; display: flex; align-items: flex-start;",
                      span(class = "step-icon", "⋯", style = "color: #ffc107; margin-right: 10px; font-size: 14px;"),
                      div(
                        style = "flex: 1;",
                        span(
                          class = "step-message",
                          subStep$message$imperative,
                          `data-imperative` = subStep$message$imperative,
                          `data-past-tense` = subStep$message$pastTense
                        ),
                        div(class = "step-issue-container")
                      )
                    )
                  })
                )
              }
            )
          })
        )
      ),
      div(
        id = ns("validation_summary"),
        style = "display: none; margin-top: 20px;",
        h3("Results Summary"),
        div(
          id = ns("summary_content"),
          style = "padding: 15px; border-radius: 5px; background-color: #e8f5e9;"
        )
      )
    ),
    
    commonNavigation(ns, show_back = FALSE, continue_text = "Continue to Update Dictionary →")
  )
}

#' Data Dictionary UI Module
#'
#' @param id The module ID
#' @return UI elements for data dictionary editing
dataDictionaryUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    commonHeader(
      main_title = "Update Data Dictionary",
      sub_title = NULL,
      show_progress = FALSE
    ),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      "Define your dataset variables in detail. This information will be stored in your dataset_description.json file as machine-readable PropertyValue objects that help others understand your data."
    ),
    
    sectionBox(
      title = "Select Dataset",
      description = "Select a Psych-DS dataset directory to edit its data dictionary.",
      div(
        class = "directory-input",
        textInput(
          ns("dataset_dir"),
          label = NULL,
          value = "",
          placeholder = "Path to Psych-DS dataset",
          width = "100%"
        ),
        shinyDirButton(
          ns("dataset_dir_select"),
          label = "...",
          title = "Select a dataset directory",
          class = "browse-btn"
        )
      ),
      div(
        style = "text-align: right; margin-top: 20px;",
        actionButton(
          ns("load_dataset_btn"),
          "Load Dataset",
          class = "continue-btn"
        ),
        actionButton(
          ns("generate_dictionary"),
          "Generate Human-Readable Dictionary",
          class = "btn btn-outline-primary",
          style = "margin-left: 10px;"
        )
      ),
      conditionalPanel(
        condition = paste0("output['", ns("dataset_loaded"), "']"),
        div(
          class = "alert alert-info",
          style = "margin-top: 20px;",
          uiOutput(ns("dataset_info"))
        ),
        
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        sectionBox(
          title = "Variables",
          div(
            style = "margin-bottom: 15px;",
            textInput(
              ns("variable_search"),
              label = NULL,
              placeholder = "Search variables...",
              width = "100%"
            )
          ),
          div(
            class = "variables-list-container",
            style = "height: 480px; overflow-y: auto; border: 1px solid #ced4da; border-radius: 4px; background-color: white;",
            uiOutput(ns("variables_list"))
          )
        )
      ),
      
      column(
        width = 8,
        sectionBox(
          title = "Variable Details",
          conditionalPanel(
            condition = paste0("output['", ns("variable_selected"), "']"),
            div(
              class = "variable-header-section",
              style = "margin-bottom: 20px; padding: 15px; border-bottom: 1px solid #e0e0e0;",
              uiOutput(ns("variable_name_header")),
              div(
                class = "file-badges",
                div("Appears in files:", style = "color: #555; margin-bottom: 4px; font-weight: 500;"),
                div(
                  style = "max-height: 50px; overflow-y: auto; border: 1px solid #e0e0e0; border-radius: 4px; padding: 8px; background-color: #fafafa;",
                  div(
                    style = "display: flex; flex-wrap: wrap; gap: 5px;",
                    uiOutput(ns("file_badges_content"))
                  )
                )
              )
            ),
            
            div(
              class = "variable-form-container",
              style = "max-height: 400px; overflow-y: auto; padding: 15px; background-color: white; border: 1px solid #ddd;",
              div(
                class = "variable-form",
                div(
                  class = "form-group",
                  style = "margin-bottom: 20px;",
                  tags$label("Description", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #333;"),
                  textAreaInput(
                    ns("var_description"),
                    label = NULL,
                    placeholder = "Describe what this variable represents, how it was measured, and any important details...",
                    height = "100px",
                    width = "100%"
                  )
                ),
                
                fluidRow(
                  column(
                    width = 6,
                    div(
                      class = "form-group",
                      style = "margin-bottom: 20px;",
                      tags$label("Data Type", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #333;"),
                      selectInput(
                        ns("var_type"),
                        label = NULL,
                        choices = list(
                          "Text/String" = "string",
                          "Number (decimal)" = "number", 
                          "Integer (whole number)" = "integer",
                          "Boolean (true/false)" = "boolean",
                          "Categorical" = "categorical",
                          "Date (YYYY-MM-DD)" = "date",
                          "DateTime" = "datetime"
                        ),
                        width = "100%"
                      )
                    )
                  ),
                  column(
                    width = 6,
                    div(
                      class = "form-group",
                      style = "margin-bottom: 20px;",
                      tags$label("Unit of Measurement", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #333;"),
                      conditionalPanel(
                        condition = paste0("input['", ns("var_type"), "'] == 'number' || input['", ns("var_type"), "'] == 'integer'"),
                        textInput(
                          ns("var_unit"),
                          label = NULL,
                          placeholder = "e.g., milliseconds, years, points, etc.",
                          width = "100%"
                        )
                      ),
                      conditionalPanel(
                        condition = paste0("input['", ns("var_type"), "'] != 'number' && input['", ns("var_type"), "'] != 'integer'"),
                        div(
                          style = "padding: 8px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px; color: #6c757d;",
                          "Not applicable for this data type"
                        )
                      )
                    )
                  )
                ),
                
               
                
                # Categorical Values Editor - only show for categorical type
                conditionalPanel(
                  condition = paste0("input['", ns("var_type"), "'] == 'categorical'"),
                  div(
                    class = "form-group",
                    style = "margin-bottom: 20px;",
                    tags$label("Possible Values", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #333;"),
                    tags$small("Define the possible categorical values and their descriptions", 
                              style = "color: #666; display: block; margin-bottom: 8px;"),
                    
                    # Categorical values table
                    div(
                      id = ns("categorical_values_container"),
                      style = "border: 1px solid #ced4da; border-radius: 4px; margin-bottom: 10px;",
                      
                      # Table header
                      div(
                        style = "display: flex; background-color: #f8f9fa; padding: 8px; border-bottom: 1px solid #ced4da; font-weight: bold;",
                        div(style = "flex: 2; padding-right: 10px;", "Value"),
                        div(style = "flex: 2; padding-right: 10px;", "Label"),
                        div(style = "flex: 3; padding-right: 10px;", "Description"),
                        div(style = "flex: 0; width: 80px;", "Actions")
                      ),
                      
                      # Dynamic table content
                      uiOutput(ns("categorical_values_table"))
                    ),
                    
                    # Add new value form
                    div(
                      style = "padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
                      fluidRow(
                        column(
                          width = 3,
                          textInput(
                            ns("new_cat_value"),
                            label = "Value",
                            placeholder = "e.g., 1, A, true"
                          )
                        ),
                        column(
                          width = 3,
                          textInput(
                            ns("new_cat_label"),
                            label = "Label",
                            placeholder = "e.g., Group A"
                          )
                        ),
                        column(
                          width = 4,
                          textInput(
                            ns("new_cat_description"),
                            label = "Description",
                            placeholder = "What this value represents"
                          )
                        ),
                        column(
                          width = 2,
                          br(),
                          actionButton(
                            ns("add_cat_value"),
                            "Add",
                            class = "btn btn-primary",
                            style = "width: 100%; margin-top: 5px;"
                          )
                        )
                      )
                    )
                  )
                ),
                
                
                # Validation Properties Section
                # Conditional constraints section - only show for numeric types
                conditionalPanel(
                  condition = paste0("input['", ns("var_type"), "'] == 'number' || input['", ns("var_type"), "'] == 'integer'"),
                  div(
                    class = "constraints-section",
                    style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px;",
                    tags$label("Value Constraints", style = "font-weight: bold; margin-bottom: 15px; display: block; color: #333;"),
                    
                    # Use min/max checkbox
                    div(
                      style = "margin-bottom: 10px;",
                      checkboxInput(
                        ns("var_use_minmax"),
                        "Use min/max constraints",
                        value = FALSE
                      ),
                      tags$small(
                        style = "color: #6c757d; margin-left: 20px;",
                        "Specify the valid range of values for this variable"
                      )
                    ),
                    
                    conditionalPanel(
                      condition = paste0("input['", ns("var_use_minmax"), "']"),
                      fluidRow(
                        column(
                          width = 6,
                          textInput(
                            ns("var_min"),
                            "Minimum Value",
                            placeholder = "Minimum allowed value",
                            width = "100%"
                          )
                        ),
                        column(
                          width = 6,
                          textInput(
                            ns("var_max"),
                            "Maximum Value", 
                            placeholder = "Maximum allowed value",
                            width = "100%"
                          )
                        )
                      )
                    ),
                    
                    conditionalPanel(
                      condition = paste0("!input['", ns("var_use_minmax"), "']"),
                      div(
                        style = "padding: 8px; background-color: #e9ecef; border: 1px solid #dee2e6; border-radius: 4px; color: #6c757d; text-align: center;",
                        "Min/max constraints disabled"
                      )
                    )
                  )
                ),

# Data Quality Constraints - shown for ALL data types
div(
  class = "constraints-section",
  style = "margin-bottom: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px;",
  tags$label("Data Quality Constraints", style = "font-weight: bold; margin-bottom: 15px; display: block; color: #333;"),
  
  div(
    style = "margin-bottom: 15px;",
    checkboxInput(
      ns("var_required"),
      "Required",
      value = FALSE
    ),
    tags$small(
      style = "color: #6c757d; margin-left: 20px; display: block; margin-top: -10px;",
      "This variable must have a value (cannot be empty or missing)"
    )
  ),
  
  div(
    style = "margin-bottom: 15px;",
    checkboxInput(
      ns("var_unique"),
      "Unique",
      value = FALSE
    ),
    tags$small(
      style = "color: #6c757d; margin-left: 20px; display: block; margin-top: -10px;",
      "All values in this variable must be unique (e.g., participant IDs)"
    )
  ),
  
  div(
    style = "margin-bottom: 0;",
    textInput(
      ns("var_pattern"),
      "Pattern (Regular Expression)",
      placeholder = "e.g., ^[A-Z]{2}\\d{4}$ for format like AB1234",
      width = "100%"
    ),
    tags$small(
      style = "color: #6c757d; display: block; margin-top: 5px;",
      "Regular expression pattern that values must match (optional, for advanced validation)"
    )
  )
),
                
                div(
                  class = "metadata-section",
                  style = "margin-bottom: 20px; padding: 15px; background-color: #f0f8ff; border-radius: 4px; border: 1px solid #bee5eb;",
                  tags$label("Additional Metadata", style = "font-weight: bold; margin-bottom: 15px; display: block; color: #333;"),
                  div(
                    class = "form-group",
                    textInput(
                      ns("var_source"),
                      "Data Source/Instrument",
                      placeholder = "e.g., Qualtrics survey, eye tracker, etc.",
                      width = "100%"
                    )
                  )
                )
              )
            ),
          ),
          
          conditionalPanel(
            condition = paste0("!output['", ns("variable_selected"), "']"),
            div(
              style = "text-align: center; color: #6c757d; padding: 50px 20px;",
              icon("arrow-left", style = "font-size: 48px; margin-bottom: 20px; color: #dee2e6;"),
              h4("Select a variable to edit", style = "color: #6c757d; margin-bottom: 10px;"),
              p("Choose a variable from the list on the left to begin editing its properties.", 
                style = "color: #adb5bd; max-width: 300px; margin: 0 auto;")
            )
          )
        )
      )
    ),
    sectionBox(
      title = "Global Missing Value Codes",
      description = "Define values that should be treated as missing/NA across all variables in this dataset.",
      
      div(
        id = ns("missing_values_container"),
        style = "border: 1px solid #ced4da; border-radius: 4px; margin-bottom: 10px; background-color: white;",
        
        # Table header
        div(
          style = "display: flex; background-color: #e9ecef; padding: 8px; border-bottom: 1px solid #ced4da; font-weight: bold;",
          div(style = "flex: 3; padding-right: 10px;", "Missing Value Code"),
          div(style = "flex: 0; width: 60px;", "Actions")
        ),
        
        # Dynamic table content
        uiOutput(ns("missing_values_table"))
      ),
      
      # Add new value form
      div(
        style = "padding: 10px; background-color: white; border: 1px solid #ced4da; border-radius: 4px;",
        fluidRow(
          column(
            width = 9,
            textInput(
              ns("new_missing_value"),
              label = NULL,
              placeholder = "e.g., NA, -999, NULL, missing"
            )
          ),
          column(
            width = 3,
            br(),
            actionButton(
              ns("add_missing_value"),
              "Add",
              class = "btn btn-primary",
              style = "width: 100%; margin-top: 0px;"
            )
          )
        )
      )
    ),

    
    commonNavigation(ns, show_back = FALSE, continue_text = "Save Dictionary & Continue →")
  )
}

#' Dataset Explorer UI Module
#'
#' @param id The module ID
#' @return UI elements for dataset exploration
datasetExplorerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    commonHeader(
      main_title = "Dataset Explorer",
      sub_title = NULL,
      show_progress = FALSE
    ),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      "Now you have a valid Psych-DS dataset with a comprehensive dataset_description.json file. On this page, you can explore the dataset you've assembled by applying filters to either your filename keywords or column headers. To add additional files to your dataset, simply download it and add them back into either the /data folder or one of your auxiliary folders."
    ),
    
    sectionBox(
      title = "Select Dataset",
      description = "Select a Psych-DS dataset directory to explore.",
      div(
        class = "directory-input",
        textInput(
          ns("dataset_dir"),
          label = NULL,
          value = "",
          placeholder = "Path to Psych-DS dataset",
          width = "100%"
        ),
        shinyDirButton(
          ns("dataset_dir_select"),
          label = "...",
          title = "Select a dataset directory",
          class = "browse-btn"
        )
      ),
      div(
        style = "text-align: right; margin-top: 20px;",
        actionButton(
          ns("load_dataset_btn"),
          "Load Dataset",
          class = "continue-btn"
        )
      ),
      conditionalPanel(
        condition = paste0("output['", ns("dataset_loaded"), "']"),
        div(
          class = "alert alert-info",
          style = "margin-top: 20px;",
          uiOutput(ns("dataset_info"))
        )
      )
    ),
    
    # Only show exploration tools if dataset is loaded
    conditionalPanel(
      condition = paste0("output['", ns("dataset_loaded"), "']"),
      
      fluidRow(
        # Left Panel - Search and Filters
        column(
          width = 6,
          sectionBox(
            title = "Search",
            
            # Keyword Search
            div(
              style = "margin-bottom: 20px;",
              tags$label("Filter Files by Keyword/Value", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #333;"),
              fluidRow(
                column(
                  width = 5,
                  selectInput(
                    ns("keyword_select"),
                    label = NULL,
                    choices = character(0),
                    width = "100%"
                  )
                ),
                column(
                  width = 5,
                  selectizeInput(
                    ns("keyword_value"),
                    label = NULL,
                    choices = NULL,
                    options = list(
                      placeholder = "Select or type value...",
                      create = FALSE,  # Don't allow creating new values
                      maxOptions = 100
                    )
                  )
                ),
                column(
                  width = 2,
                  br(),
                  actionButton(
                    ns("add_keyword_filter"),
                    "Add",
                    class = "btn btn-primary",
                    style = "width: 100%;"
                  )
                )
              )
            ),
            
            # Column Search
            div(
              tags$label("Filter Rows by Column/Value", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #333;"),
              fluidRow(
                column(
                  width = 5,
                  selectInput(
                    ns("column_select"),
                    label = NULL,
                    choices = character(0),
                    width = "100%"
                  )
                ),
                column(
                  width = 5,
                  selectizeInput(
                    ns("column_value"),
                    label = NULL,
                    choices = NULL,
                    options = list(
                      placeholder = "Select or type value...",
                      create = FALSE,  # Don't allow creating new values
                      maxOptions = 100
                    )
                  )
                ),
                column(
                  width = 2,
                  br(),
                  actionButton(
                    ns("add_column_filter"),
                    "Add",
                    class = "btn btn-primary",
                    style = "width: 100%;"
                  )
                )
              )
            )
          )
        ),
        
        # Right Panel - Active Filters and Statistics
        column(
          width = 6,
          sectionBox(
            title = "Active Filters",
            
            # Keyword Filters
            div(
              style = "margin-bottom: 15px;",
              tags$label("Keyword Filters", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #555;"),
              div(
                id = ns("keyword_filters_container"),
                style = "min-height: 40px; padding: 8px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;",
                uiOutput(ns("keyword_filters_display"))
              )
            ),
            
            # Column Filters
            div(
              style = "margin-bottom: 15px;",
              tags$label("Column Filters", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #555;"),
              div(
                id = ns("column_filters_container"),
                style = "min-height: 40px; padding: 8px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;",
                uiOutput(ns("column_filters_display"))
              )
            ),
            
            div(
              tags$label("Variable Statistics", style = "font-weight: bold; margin-bottom: 8px; display: block; color: #555;"),
              selectInput(
                ns("stats_variable"),
                label = NULL,
                choices = character(0),
                width = "100%"
              ),
              
              # Statistics display area
              div(
                id = ns("stats_container"),
                style = "margin-top: 10px;",
                uiOutput(ns("variable_statistics"))
              )
            )
          )
        )
      ),
      
      # Data View Panel
      sectionBox(
        title = "Data View",
        
        # File selector tabs
        div(
          style = "margin-bottom: 15px;",
          uiOutput(ns("file_tabs"))
        ),
        
        # Data table
        div(
          style = "border: 1px solid #ced4da; border-radius: 4px; background-color: white;",
          DT::dataTableOutput(ns("data_table"))
        )
      )
    )
  )
}

#' OSF Upload UI Module
#'
#' @param id The module ID
#' @return UI elements for OSF upload
osfUploadUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    commonHeader(
      main_title = "Upload to OSF",
      sub_title = NULL,
      show_progress = FALSE
    ),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      "Upload your validated Psych-DS dataset to the Open Science Framework (OSF). You'll need an OSF account and a Personal Access Token."
    ),
    
    # Authentication Section
    sectionBox(
      title = "OSF Authentication",
      description = "Generate a Personal Access Token from your OSF account settings at osf.io/settings/tokens",
      
      div(
        style = "margin-bottom: 15px;",
        passwordInput(
          ns("osf_token"),
          "Personal Access Token",
          placeholder = "Paste your OSF token here",
          width = "100%"
        )
      ),
      
      div(
        style = "text-align: right;",
        actionButton(
          ns("test_auth"),
          "Test Connection",
          icon = icon("plug"),
          class = "btn btn-info"
        )
      ),
      
      # Auth status display
      div(
        id = ns("auth_status"),
        style = "margin-top: 15px;",
        uiOutput(ns("auth_status_display"))
      )
    ),
    
    # Dataset Selection Section
    conditionalPanel(
      condition = paste0("output['", ns("authenticated"), "']"),
      
      sectionBox(
        title = "Select Dataset",
        description = "Choose a validated Psych-DS dataset to upload",
        
        div(
          class = "directory-input",
          textInput(
            ns("dataset_dir"),
            label = NULL,
            value = "",
            placeholder = "Path to validated Psych-DS dataset",
            width = "100%"
          ),
          shinyDirButton(
            ns("dataset_dir_select"),
            label = "...",
            title = "Select dataset directory",
            class = "browse-btn"
          )
        ),
        
        # Quick validation check
        conditionalPanel(
          condition = paste0("input['", ns("dataset_dir"), "'] != ''"),
          div(
            style = "margin-top: 10px;",
            actionButton(
              ns("check_dataset"),
              "Verify Dataset",
              icon = icon("check"),
              class = "btn btn-secondary"
            )
          )
        ),
        
        div(
          id = ns("dataset_status"),
          style = "margin-top: 15px;",
          uiOutput(ns("dataset_status_display"))
        )
      )
    ),
    
    # OSF Project Selection Section - Wrapped in div with ID for shinyjs
    div(
      id = ns("osf_project_section"),
      style = "display: none;",  # Hidden by default
      
      sectionBox(
        title = "OSF Project Configuration",
        
        div(
          style = "margin-bottom: 15px;",
          radioButtons(
            ns("project_option"),
            "Upload destination:",
            choices = list(
              "Select existing project" = "existing",
              "Create new project" = "new"
            ),
            selected = "existing",
            inline = TRUE
          )
        ),
        
        # Existing project selection
        conditionalPanel(
          condition = paste0("input['", ns("project_option"), "'] == 'existing'"),
          
          div(
            style = "margin-bottom: 15px;",
            selectInput(
              ns("project_select"),
              "Your OSF Projects",
              choices = list("Loading..." = ""),
              width = "100%"
            ),
            actionButton(
              ns("refresh_projects"),
              icon = icon("refresh"),
              label = NULL,
              class = "btn btn-sm",
              style = "margin-top: 5px;"
            )
          ),
          
          div(
            style = "margin-bottom: 15px;",
            textInput(
              ns("project_id_manual"),
              "Or enter Project ID manually",
              placeholder = "e.g., abc123",
              width = "100%"
            )
          )
        ),
        
        # New project creation
        conditionalPanel(
          condition = paste0("input['", ns("project_option"), "'] == 'new'"),
          
          div(
            style = "margin-bottom: 15px;",
            textInput(
              ns("new_project_title"),
              "Project Title *",
              placeholder = "e.g., Visual Attention Study 2024",
              width = "100%"
            )
          ),
          
          div(
            style = "margin-bottom: 15px;",
            textAreaInput(
              ns("new_project_description"),
              "Project Description",
              placeholder = "Brief description of your research project",
              height = "100px",
              width = "100%"
            )
          ),
          
          div(
            style = "margin-bottom: 15px;",
            checkboxInput(
              ns("make_public"),
              "Make project public immediately",
              value = FALSE
            )
          )
        ),
        
        # Component selection (for existing projects)
        conditionalPanel(
          condition = paste0("input['", ns("project_option"), "'] == 'existing'"),
          div(
            style = "margin-bottom: 15px;",
            selectInput(
              ns("component_select"),
              "Upload to component (optional)",
              choices = list("Main project" = "main"),
              width = "100%"
            )
          )
        ),
        
        # Upload path configuration
        div(
          style = "margin-bottom: 15px;",
          textInput(
            ns("upload_path"),
            "Folder path on OSF (optional)",
            placeholder = "e.g., data/wave1 (leave empty for root)",
            width = "100%"
          )
        ),
        
        # Additional options
        div(
          style = "margin-bottom: 15px;",
          checkboxInput(
            ns("preserve_structure"),
            "Preserve dataset folder structure",
            value = TRUE
          )
        ),
        
        div(
          style = "margin-bottom: 15px;",
          checkboxInput(
            ns("create_readme"),
            "Generate and upload README file",
            value = TRUE
          )
        )
      )
    ),
    
    # Upload Progress Section - Also wrapped for shinyjs control
    div(
      id = ns("upload_summary_section"),
      style = "display: none;",  # Hidden by default
      
      sectionBox(
        title = "Upload Summary",
        
        div(
          id = ns("upload_summary"),
          style = "margin-bottom: 15px;",
          uiOutput(ns("upload_summary_display"))
        ),
        
        div(
          style = "text-align: right;",
          actionButton(
            ns("start_upload"),
            "Start Upload",
            icon = icon("cloud-upload"),
            class = "btn btn-success btn-lg"
          )
        )
      )
    ),
    
    # Upload progress display
    div(
      id = ns("upload_progress"),
      style = "margin-top: 20px; display: none;",
      uiOutput(ns("upload_progress_display"))
    )
  )
}