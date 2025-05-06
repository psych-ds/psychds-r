#' UI Module Definitions
#'
#' This file contains modular UI components that are used throughout the application.
#' Each component is designed to be reusable and self-contained.

#' Create the Step Progress Indicator
#'
#' @param current_step Integer indicating current step (1-3)
#' @return UI element for progress indicator
stepProgressUI <- function(current_step, id) {
  ns <- NS(id)

  # Calculate progress percentage
  progress_pct <- (current_step - 1) * 33.33
  if (current_step == 3) progress_pct <- 100

  div(class = "progress-container",
      div(class = "progress-bar-container",
          div(class = "progress-bar-fill", style = paste0("width: ", progress_pct, "%;"))
      ),
      div(style = "display: flex; justify-content: space-between; width: 100%;",
          # Step 1
          actionButton(
            inputId = ns("goto_step1"),
            label = "1",
            class = paste("btn-circle", ifelse(current_step >= 1, "active", "inactive")),
            style = paste0(
              "width: 30px; height: 30px; border-radius: 50%; padding: 0; ",
              "background-color: ", ifelse(current_step >= 1, "#3498db", "#e0e0e0"), "; ",
              "color: ", ifelse(current_step >= 1, "white", "#666"), ";"
            )
          ),

          # Step 2
          actionButton(
            inputId = ns("goto_step2"),
            label = "2",
            class = paste("btn-circle", ifelse(current_step >= 2, "active", "inactive")),
            style = paste0(
              "width: 30px; height: 30px; border-radius: 50%; padding: 0; ",
              "background-color: ", ifelse(current_step >= 2, "#3498db", "#e0e0e0"), "; ",
              "color: ", ifelse(current_step >= 2, "white", "#666"), ";"
            )
          ),

          # Step 3
          actionButton(
            inputId = ns("goto_step3"),
            label = "3",
            class = paste("btn-circle", ifelse(current_step >= 3, "active", "inactive")),
            style = paste0(
              "width: 30px; height: 30px; border-radius: 50%; padding: 0; ",
              "background-color: ", ifelse(current_step >= 3, "#3498db", "#e0e0e0"), "; ",
              "color: ", ifelse(current_step >= 3, "white", "#666"), ";"
            )
          )
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

fileBrowserUI <- function(id, title, description, with_convert = FALSE) {
  ns <- NS(id)

  div(
    class = "section-box",
    # Section title
    div(class = "section-title", title),

    # Description with optional convert link
    div(class = "section-description",
        if (with_convert) {
          tagList(
            description,
            " If your data are not yet in CSV format, start by ",
            actionLink(ns("convert"), "converting them", class = "blue-link")
          )
        } else {
          description
        }
    ),

    # File browser container
    div(
      class = "file-browser",
      style = "background-color: white; border: 2px solid #ced4da; border-radius: 3px; padding: 10px; height: 200px; overflow-y: auto; margin-top: 10px;",
      uiOutput(ns("file_container"))
    )
  )
}

#' Create Optional Directories UI
#'
#' @param id ID for the component
#' @return UI element for optional directories
optionalDirsUI <- function(id) {
  ns <- NS(id)

  div(
    class = "section-box",
    div(class = "section-title", "Optional Directories"),
    p(class = "section-description",
      "Select additional new directories to create in your project folder."),

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

#' Create Step 1 UI: Project Directory and Data Files
#'
#' @param id Namespace ID for the module
#' @return UI for Step 1
step1UI <- function(id) {
  ns <- NS(id)

  tagList(
    # Header
    fluidRow(
      column(
        width = 6,
        div(
          h2("Create Dataset"),
          h3("Step 1: Project Directory and Canonical Data Files")
        )
      ),
      column(
        width = 6,
        stepProgressUI(1,"step_nav")
      )
    ),

    # Project Directory Selection
    div(
      class = "section-box",
      div(class = "section-title", "Project Directory"),
      div(class = "section-description",
          "The goal of Psych-DS is to standardize your data in the context of the rest of the research
           materials that relate to it. Select a project directory where you want your new `data/` folder
           to appear. This directory should also contain both your existing data files and optionally
           other project materials (e.g. analyses, papers.)"),

      directoryInputUI(ns("project_dir"))
    ),

    # Two Column Layout for Files and Optional Directories
    fluidRow(
      # Data Files Selection Column
      column(
        width = 7,
        fileBrowserUI(
          ns("files"),
          "Select Data Files",
          "Select CSV files that contain the data you want to include in the Psych-DS data/ folder.",
          with_convert = TRUE
        )
      ),

      # Optional Directories Column
      column(
        width = 5,
        optionalDirsUI(ns("opt_dirs"))
      )
    ),

    # Continue Button
    div(
      style = "text-align: right;",
      actionButton(
        ns("continue"),
        "Continue →",
        class = "continue-btn"
      )
    )
  )
}

#' Create Step 2 UI: Data Dictionary
#'
#' @param id Namespace ID for the module
#' @return UI for Step 2
#' Debug version of Step 2 UI
#'
#' @param id Namespace ID for the module
#' @return UI for Step 2
step2UI <- function(id) {
  ns <- NS(id)

  tagList(
    # Header
    fluidRow(
      column(
        width = 6,
        div(
          h2("Create Dataset"),
          h3("Step 2: Dataset Metadata")
        )
      ),
      column(
        width = 6,
        stepProgressUI(2,"step_nav")
      )
    ),

    # Description text
    p(class = "section-description",
      "We'll generate a text file called dataset_description.json containing information about ",
      "your dataset. This file is part of Psych-DS - it is placed in your project directory and is ",
      "used to confirm whether your specific dataset is organized to the Psych-DS standard."
    ),

    # Main content - two columns
    fluidRow(
      # Left column - Detected Variables
      column(
        width = 6,
        div(
          class = "section-box",
          div(class = "section-title", "Detected Variables"),

          p(class = "section-description",
            "The following column headers were detected in your selected files. ",
            "Variables with the same name across multiple files are assumed to have ",
            "identical definitions."
          ),

          # Variable list
          div(
            style = "margin-top: 15px;",
            DT::dataTableOutput(ns("variables_table"))
          ),

          # Warning box
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-radius: 3px;",
            p(
              style = "color: #856404; margin-bottom: 0;",
              "Note: If you need different definitions for variables that currently have the same ",
              "name across files, you will need to rename them to be distinct. Make these changes in ",
              "your CSV files and restart the process."
            )
          )
        )
      ),

      # Right column - Dataset Info and Authors
      column(
        width = 6,
        # Dataset Information section
        div(
          class = "section-box",
          div(class = "section-title", "Dataset Information"),

          # Name field
          div(
            style = "margin-bottom: 15px;",
            textInput(
              ns("dataset_name"),
              "Name *",
              placeholder = "e.g., Visual Attention Experiment 2023"
            )
          ),

          # Description field
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

        # Author Information section
        div(
          class = "section-box",
          div(class = "section-title", "Author Information"),

          p("Author Names (separate with commas)"),

          # Author table
          div(
            id = ns("authors_container"),
            style = "max-height: 200px; overflow-y: auto; border: 1px solid #ced4da; border-radius: 3px; margin-bottom: 15px;",
            # Header
            div(
              style = "display: flex; background-color: #f8f9fa; padding: 5px; border-bottom: 1px solid #ced4da;",
              div(style = "flex: 2;", strong("Name")),
              div(style = "flex: 2;", strong("ORCID ID"))
            ),
            # Authors will be inserted here dynamically
            uiOutput(ns("author_list"))
          ),

          # Add author button
          actionButton(
            ns("add_author"),
            "Add New Author",
            class = "btn-primary",
            style = "margin-bottom: 15px;"
          )
        )
      )
    ),

    # Navigation Buttons
    div(
      style = "text-align: right; margin-top: 20px;",
      actionButton(
        ns("back"),
        "← Back",
        class = "back-btn"
      ),
      actionButton(
        ns("continue"),
        "Continue →",
        class = "continue-btn"
      )
    )
  )
}

#' Step 3 UI Module
#'
#' UI components for Step 3: Standardize Filenames
#'
#' @param id The module ID
#' @param session The current session object
step3UI <- function(id) {
  ns <- NS(id)

  tagList(
    # Header
    fluidRow(
      column(
        width = 6,
        div(
          h2("Create Dataset"),
          h3("Step 3: Standardize Filenames")
        )
      ),
      column(
        width = 6,
        stepProgressUI(3,"step_nav")
      )
    ),

    # Description
    p(class = "section-description",
      "Rename your data files to follow Psych-DS naming conventions. Each filename will be composed of a set of keywords
      and custom values to create a clear, consistent description."),

    # Main content area - Using a more balanced two-column layout
    fluidRow(
      # Left column - File Mapping with Keyword Configuration
      column(
        width = 6,
        # File Mapping Section
        div(
          class = "section-box",
          div(class = "section-title", "File Mapping"),
          p(class = "section-description",
            "Select each file to configure its standardized filename."),

          div(
            class = "file-browser",
            style = "height: 250px; overflow-y: auto; margin-bottom: 15px;", # Increased height
            div(
              class = "file-list-header",
              div(class = "row",
                  div(class = "col-xs-6", strong("Original Filename")),
                  div(class = "col-xs-6", strong("New Filename"))
              )
            ),
            div(
              id = ns("file_mapping_list"),
              class = "file-mapping-list",
              # Will be populated dynamically with file rows
              uiOutput(ns("file_mapping_rows"))
            )
          )
        ),

        # Current file indicator
        div(
          id = ns("current_file_indicator"),
          class = "alert alert-info",
          style = "margin-top: 15px;",
          uiOutput(ns("current_file_text"))
        ),

        # Keywords selection area
        div(
          class = "section-box",
          style = "margin-top: 15px;",
          div(class = "section-title", "Choose Keywords"),
          p(class = "section-description",
            "Select keywords to use in your filename. Choose keywords that are meaningful for your dataset and use them consistently."),

          div(
            class = "keyword-option-chips",
            style = "margin-bottom: 15px;",
            actionButton(ns("keyword_session"), "Session", class = "btn btn-sm btn-primary keyword-chip"),
            actionButton(ns("keyword_subject"), "Subject", class = "btn btn-sm btn-primary keyword-chip"),
            actionButton(ns("keyword_study"), "Study", class = "btn btn-sm btn-primary keyword-chip"),
            actionButton(ns("keyword_task"), "Task", class = "btn btn-sm btn-primary keyword-chip"),
            actionButton(ns("keyword_condition"), "Condition", class = "btn btn-sm btn-primary keyword-chip"),
            actionButton(ns("keyword_stimulus"), "Stimulus", class = "btn btn-sm btn-primary keyword-chip"),
            actionButton(ns("keyword_trial"), "Trial", class = "btn btn-sm btn-primary keyword-chip"),
            actionButton(ns("keyword_description"), "Description", class = "btn btn-sm btn-primary keyword-chip")
          ),

          div(
            class = "section-title",
            "Selected Keywords"
          ),
          div(
            id = ns("selected_keywords_container"),
            class = "selected-keywords-container",
            style = "min-height: 80px; padding: 10px; background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; margin-bottom: 15px;",
            # Using uiOutput instead of sortable::rank_list
            uiOutput(ns("selected_keywords"))
          )

        ),

        # Custom keywords section
        div(
          class = "section-box",
          style = "margin-top: 15px;",
          div(class = "section-title", "Add Custom Keyword"),
          div(
            class = "input-group",
            style = "margin-bottom: 15px;",
            textInput(ns("custom_keyword_name"), NULL, placeholder = "Custom keyword name", width = "70%"),
            div(
              class = "input-group-btn",
              style = "width: 30%;",
              actionButton(ns("add_custom_keyword"), "Add", class = "btn btn-primary", style = "width: 100%;")
            )
          )
        )
      ),

      # Right column - Keyword Values and Preview
      column(
        width = 6,
        # Keyword value inputs
        div(
          class = "section-box",
          div(class = "section-title", "Keyword Values"),
          div(
            class = "keyword-values-section",
            style = "min-height: 250px;", # Match the height of file mapping box
            uiOutput(ns("keyword_value_inputs"))
          )
        ),

        # Filename preview
        div(
          class = "section-box",
          style = "margin-top: 15px;",
          div(class = "section-title", "Filename Preview"),
          div(
            class = "well well-sm filename-preview",
            style = "background-color: #f8f9fa; border: 1px solid #ced4da; padding: 15px; text-align: center; margin: 10px 0; min-height: 60px;",
            uiOutput(ns("filename_preview"))
          ),
          div(
            style = "text-align: center;",
            actionButton(ns("generate_filename"), "Generate Filename", class = "btn btn-primary", style = "width: 100%;")
          )
        ),

        # Debugging div (hidden in production, used for troubleshooting)
        tags$div(
          id = ns("debug_output"),
          style = "display: none;",
          verbatimTextOutput(ns("debug_text"))
        )
      )
    ),

    # Navigation buttons
    div(
      style = "text-align: right; margin-top: 20px;",
      actionButton(
        ns("back"),
        "← Back",
        class = "back-btn"
      ),
      actionButton(
        ns("continue"),
        "Continue →",
        class = "continue-btn"
      )
    )
  )
}
