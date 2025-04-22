#' UI Module Definitions
#'
#' This file contains modular UI components that are used throughout the application.
#' Each component is designed to be reusable and self-contained.

#' Create the Step Progress Indicator
#'
#' @param current_step Integer indicating current step (1-3)
#' @return UI element for progress indicator
stepProgressUI <- function(current_step) {
  # Calculate progress percentage
  progress_pct <- (current_step - 1) * 33.33
  if (current_step == 3) progress_pct <- 100

  div(class = "progress-container",
      div(class = "progress-bar-container",
          div(class = "progress-bar-fill", style = paste0("width: ", progress_pct, "%;"))
      ),
      div(style = "display: flex; white-space: nowrap;", # Force horizontal layout
          span(class = ifelse(current_step >= 1, "step-circle active", "step-circle inactive"), "1"),
          span(class = ifelse(current_step >= 2, "step-circle active", "step-circle inactive"), "2"),
          span(class = ifelse(current_step >= 3, "step-circle active", "step-circle inactive"), "3")
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
        stepProgressUI(1)
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
step2UI <- function(id) {
  ns <- NS(id)

  tagList(
    # Header
    fluidRow(
      column(
        width = 6,
        div(
          h2("Create Dataset"),
          h3("Step 2: Create Data Dictionary")
        )
      ),
      column(
        width = 6,
        stepProgressUI(2)
      )
    ),

    # Data Dictionary Editor - This is a placeholder for the actual implementation
    div(
      class = "section-box",
      div(class = "section-title", "Data Dictionary"),
      div(class = "section-description",
          "A data dictionary provides metadata about each variable in your dataset.
           This information helps others understand and reuse your data."),

      # Tabs for each file
      uiOutput(ns("file_tabs"))
    ),

    # Navigation Buttons
    div(
      style = "text-align: right;",
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

#' Create Step 3 UI: Dataset Metadata
#'
#' @param id Namespace ID for the module
#' @return UI for Step 3
step3UI <- function(id) {
  ns <- NS(id)

  tagList(
    # Header
    fluidRow(
      column(
        width = 6,
        div(
          h2("Create Dataset"),
          h3("Step 3: Dataset Metadata")
        )
      ),
      column(
        width = 6,
        stepProgressUI(3)
      )
    ),

    # Dataset Metadata
    div(
      class = "section-box",
      div(class = "section-title", "Dataset Information"),
      div(class = "section-description",
          "Provide general information about your dataset."),

      # Dataset name and description
      textInput(ns("dataset_name"), "Dataset Name", placeholder = "e.g., Study on Memory and Recognition"),
      textAreaInput(ns("dataset_description"), "Description", placeholder = "Describe your dataset...", height = "100px"),

      # Authors
      div(class = "section-title", style = "margin-top: 15px;", "Authors"),
      div(
        id = ns("authors_container"),
        # Will be populated dynamically
      ),
      div(
        style = "margin-top: 10px;",
        actionButton(ns("add_author"), "Add Author", class = "add-btn")
      ),

      # License
      div(class = "section-title", style = "margin-top: 15px;", "License"),
      selectInput(ns("license"), NULL,
                  choices = c("CC-BY-4.0" = "CC-BY-4.0",
                              "CC0-1.0" = "CC0-1.0",
                              "MIT" = "MIT",
                              "Other" = "other"),
                  selected = "CC-BY-4.0")
    ),

    # Navigation Buttons
    div(
      style = "text-align: right;",
      actionButton(
        ns("back"),
        "← Back",
        class = "back-btn"
      ),
      actionButton(
        ns("finish"),
        "Finish",
        class = "finish-btn"
      )
    )
  )
}
