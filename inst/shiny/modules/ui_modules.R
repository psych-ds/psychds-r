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
        " If your data are not yet in CSV format, you'll need to start by ",
        tags$a(
          href = "https://support.microsoft.com/en-us/office/import-or-export-text-txt-or-csv-files-5250ac4c-663c-47ce-937b-339e391393ba",
          target = "_blank",
          class = "blue-link",
          "converting them"
        ),
        "."
      )
    } else {
      description
    },
    # Add helper text about interactions
    div(
      style = "font-size: 12px; color: #666; margin-top: 5px; margin-bottom: 8px;",
      icon("info-circle", style = "margin-right: 5px;"),
      "Click files to select them individually, or click directory names to select/deselect all files in that directory."
    ),
    # Add Select All button
    div(
      style = "margin-bottom: 8px;",
      actionButton(
        ns("select_all"),
        "Select All",
        icon = icon("check-square"),
        class = "btn btn-sm btn-primary",
        style = "margin-right: 5px;"
      ),
      actionButton(
        ns("deselect_all"),
        "Deselect All",
        icon = icon("square"),
        class = "btn btn-sm btn-secondary"
      )
    ),
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
    title = "Optional Subfolders",
    description = "This tool can create additional empty directories inside your project folder for you to move your other materials into.",
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
    div(class = "custom-dir-section", "Add a custom subdirectory:"),
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
    h2("Create Dataset"),
    h3("Step 1: Select Your Data"),

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
    
    # Project Directory Name and Data Directory Selection (side by side)
    fluidRow(
      column(
        width = 6,
        sectionBox(
          title = "Name Your Project Directory",
          description = "The goal of Psych-DS is to standardize how you store data within a scientific project. This tool will build a new project directory to store your data, with additional folders (analysis, materials, etc.) if you like.",
          textInput(
            ns("project_name"),
            label = NULL,
            value = "",
            placeholder = "Project directory name, e.g. MyFavoriteStudy, Experiment1, Dissertation",
            width = "100%"
          )
        )
      ),
      column(
        width = 6,
        sectionBox(
          title = "Select Data Directory",
          description = "Choose the folder on your computer that contains all the data files you want to include in your new project directory. It's okay if that folder also contains other things; you'll select the specific data files below.",
          directoryInputUI(ns("project_dir"))
        )
      )
    ),
    
    fluidRow(
      column(
        width = 7,
        fileBrowserUI(
          ns("files"),
          "Select Data Files",
          "Select the CSV files that you want to include in your Psych-DS data folder.",
          with_convert = TRUE
        )
      ),
      column(
        width = 5,
        optionalDirsUI(ns("opt_dirs"))
      )
    ),
    
    commonNavigation(ns, show_back = FALSE, continue_text = "Continue to step 2 of Create Dataset - no files will be saved yet")
  )
}

#' Step 2 UI: Dataset Metadata
#'
#' @param id Namespace ID for the module
#' @return UI for Step 2
step2UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Create Dataset"),
    h3("Step 2: Dataset Metadata"),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      p("Every Psych-DS project has a text file called ", tags$code("dataset_description.json"), " with information (metadata) about the dataset. This file is part of what makes Psych-DS work - it sits in your project directory and is used to confirm whether your specific dataset matches the standard."),
      p("Right now, we'll make you a basic version of this file that you can update later with more information. You can do this by either editing the text file by hand or using the \"Update Dictionary\" tool.")
    ),
    
    fluidRow(
      column(
        width = 6,
        sectionBox(
          title = "Review Detected Variables",
          description = "Psych-DS uses your column headers to create the list of variables in your dataset. Variable names used in more than one file are assumed to have identical definitions. Make sure the variable names you see make sense for your data files!",
          div(
            style = "margin-top: 15px;",
            DT::dataTableOutput(ns("variables_table"))
          ),
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #fff3cd; border-radius: 3px;",
            p(
              style = "color: #856404; margin-bottom: 0;",
              strong("Note:"), " Psych-DS only allows each variable name to have one variable definition. If you use the same name for more than one concept across data files (e.g. \"t\" means \"time\" in some files but \"temperature\" in others), you will need to rename these columns before continuing."
            )
          ),
          div(
            style = "margin-top: 10px; color: #666; font-size: 13px;",
            "If you need to change any variable names, exit this tool and open your CSV files to make those changes directly. Then, start again with Step 1 of the \"Create Dataset\" process."
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
              placeholder = "e.g., Dataset for Visual Attention Experiment 2023"
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
          # Add New Author button first
          actionButton(
            ns("add_author"),
            "Add New Author",
            icon = icon("plus"),
            class = "btn-primary",
            style = "margin-bottom: 15px;"
          ),
          # Authors table/list
          div(
            id = ns("authors_container"),
            style = "max-height: 200px; overflow-y: auto; border: 1px solid #ced4da; border-radius: 3px;",
            div(
              style = "display: flex; background-color: #f8f9fa; padding: 5px; border-bottom: 1px solid #ced4da;",
              div(style = "flex: 2;", strong("Name")),
              div(style = "flex: 2;", strong("ORCID ID"))
            ),
            uiOutput(ns("author_list"))
          )
        )
      )
    ),
    
    commonNavigation(ns, show_back = TRUE, continue_text = "Continue to step 3 of Create Dataset - no files will be saved yet")
  )
}

#' Step 3 UI: Standardize Filenames - Enhanced Version
#'
#' @param id Namespace ID for the module
#' @return UI for Step 3 with multi-select and auto-naming features
step3UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Create Dataset"),
    h3("Step 3: Standardize Filenames"),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      p("Psych-DS has specific naming conventions that your data files need to follow. This naming system is going to make you explain what each piece of a filename means: you can't just say \"347B\". Instead you have to use keywords to describe what that refers to. Is that participant 347B? Session 347B? Or even participant 347, session B?"),
      p("We encourage you to use keywords from the suggested list below, but you can add your own if needed.")
    ),
    
    # FULL WIDTH - File Selection at top
    fluidRow(
      column(
        width = 12,
        sectionBox(
          title = "1. Select Files",
          description = "Click on one file to create the name it will have in your Psych-DS folder, or use the check boxes to choose a batch of files to rename at the same time.",
          fluidRow(
            column(
              width = 8,
              # Select All / Deselect All buttons
              div(
                style = "margin-bottom: 10px; display: flex; gap: 10px;",
                actionButton(
                  ns("select_all_files"),
                  "Select All",
                  class = "btn btn-sm btn-info"
                ),
                actionButton(
                  ns("deselect_all_files"),
                  "Deselect All",
                  class = "btn btn-sm btn-default"
                )
              ),
              div(
                class = "file-browser",
                style = "height: 200px; overflow-y: auto;",
                div(
                  class = "file-list-header",
                  fluidRow(
                    column(1, HTML("&nbsp;")),
                    column(5, strong("Original Filename")),
                    column(6, strong("New Filename"))
                  )
                ),
                div(
                  id = ns("file_mapping_list"),
                  class = "file-mapping-list",
                  uiOutput(ns("file_mapping_rows"))
                )
              )
            ),
            column(
              width = 4,
              div(
                id = ns("current_file_indicator"),
                class = "alert alert-info",
                style = "margin-bottom: 10px;",
                uiOutput(ns("current_file_text"))
              ),
              # Auto-name section (compact)
              div(
                style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ddd; border-radius: 4px;",
                strong("Auto-Name from Data"),
                div(
                  style = "font-size: 12px; color: #666; margin-bottom: 10px;",
                  "Fill keyword values from columns in your data"
                ),
                div(
                  id = ns("auto_name_section"),
                  conditionalPanel(
                    condition = paste0("output['", ns("has_constant_columns"), "']"),
                    selectInput(
                      ns("auto_keyword"),
                      "Keyword:",
                      choices = NULL,
                      width = "100%"
                    ),
                    selectInput(
                      ns("auto_column"),
                      "From column:",
                      choices = NULL,
                      width = "100%"
                    ),
                    actionButton(
                      ns("apply_auto_name"),
                      "Apply Auto-Name",
                      icon = icon("magic"),
                      class = "btn btn-success btn-sm",
                      style = "width: 100%;"
                    )
                  ),
                  conditionalPanel(
                    condition = paste0("!output['", ns("has_constant_columns"), "']"),
                    div(
                      style = "padding: 10px; text-align: center; color: #999; font-size: 12px;",
                      "No constant-value columns detected in selected files."
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    
    # BOTTOM SECTION - Two columns for keyword configuration
    fluidRow(
      # LEFT COLUMN - Keywords
      column(
        width = 6,
        sectionBox(
          title = "2. Choose Keywords",
          div(
            class = "section-description",
            style = "margin-bottom: 10px; font-size: 13px;",
            "Select keywords to include in your filename. Drag to reorder."
          ),
          div(
            class = "keyword-option-chips",
            style = "margin-bottom: 15px;",
            lapply(c("subject", "session", "study", "task", "condition", "stimulus", "trial", "description"), function(keyword) {
              actionButton(
                ns(paste0("keyword_", keyword)), 
                keyword, 
                class = "btn btn-sm btn-primary keyword-chip"
              )
            })
          ),
          # Custom keyword inline
          div(
            style = "margin-bottom: 15px; display: flex; gap: 10px; align-items: center;",
            textInput(ns("custom_keyword_name"), NULL, 
                      placeholder = "custom keyword", width = "60%"),
            actionButton(ns("add_custom_keyword"), "Add Custom", class = "btn btn-sm btn-default")
          ),
          div(class = "section-title", style = "font-size: 13px; margin-bottom: 5px;", "Selected Keywords (drag to reorder):"),
          div(
            id = ns("selected_keywords_container"),
            class = "selected-keywords-container",
            style = "min-height: 50px; padding: 10px; background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px;",
            uiOutput(ns("selected_keywords"))
          ),
          div(
            style = "margin-top: 10px; padding: 8px; background-color: #e8f4f8; border-radius: 4px; font-size: 12px;",
            icon("info-circle", style = "color: #3498db; margin-right: 5px;"),
            "Values must contain only letters and numbers (a-z, A-Z, 0-9)"
          )
        )
      ),
      
      # RIGHT COLUMN - Values and Preview
      column(
        width = 6,
        sectionBox(
          title = "3. Keyword Values",
          div(
            class = "keyword-values-section",
            style = "min-height: 150px;",
            uiOutput(ns("keyword_value_inputs"))
          )
        ),
        
        sectionBox(
          title = "4. Filename Preview",
          div(
            class = "well well-sm filename-preview",
            style = "background-color: #f8f9fa; border: 1px solid #ced4da; padding: 15px; text-align: center; margin: 10px 0; min-height: 50px;",
            uiOutput(ns("filename_preview"))
          ),
          div(
            style = "text-align: center;",
            actionButton(ns("generate_filename"), "Use Filename for Selected File(s)", class = "btn btn-primary", style = "width: 100%;")
          )
        ),
        
        tags$div(
          id = ns("debug_output"),
          style = "display: none;",
          verbatimTextOutput(ns("debug_text"))
        )
      )
    ),
    
    commonNavigation(ns, show_back = TRUE, continue_text = "Continue to save your dataset - no files will be saved yet")
  )
}

#' Welcome Page UI Module
#'
#' @param id The module ID
#' @return UI elements for the welcome/landing page
welcomeUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Header with subtitle
    div(
      style = "text-align: center; margin-bottom: 40px;",
      h1(
        style = "font-size: 42px; font-weight: 300; color: #2c3e50; margin-bottom: 10px;",
        "Welcome to Psych-DS"
      ),
      p(
        style = "font-size: 18px; color: #7f8c8d; font-weight: 300;",
        "A standard for organizing and sharing psychological research data"
      )
    ),
    
    # About section - highlighted info box
    div(
      class = "section-box",
      style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.1); margin-bottom: 30px;",
      div(
        style = "display: flex; align-items: flex-start; gap: 20px;",
        div(
          style = "font-size: 48px; opacity: 0.9;",
          icon("cube")
        ),
        div(
          style = "flex: 1;",
          h3(style = "margin-top: 0; color: white; font-weight: 400;", "What is Psych-DS?"),
          p(
            style = "margin-bottom: 8px; line-height: 1.6;",
            "Psych-DS is a dataset organization standard: a system of very specific rules for how data (in this case, tables of rows and columns) and metadata (information about your dataset) should be organized as files and folders on a computer."
          ),
          p(
            style = "margin-bottom: 0; line-height: 1.6;",
            "Using this consistent structure for organizing research data makes it easier to share, understand, and reuse. You can adopt Psych-DS at any stage in the scientific process, from planning data collection to archiving a dataset."
          )
        )
      )
    ),
    
    # Key feature callout
    div(
      style = "background-color: #e8f4f8; border-left: 4px solid #3498db; padding: 20px; margin-bottom: 30px; border-radius: 4px;",
      div(
        style = "display: flex; align-items: center; gap: 15px;",
        icon("lock", style = "font-size: 32px; color: #3498db;"),
        div(
          p(
            style = "margin: 0; font-size: 16px; line-height: 1.6;",
            "Everything this Shiny app provides stays ", 
            strong("local to your computer"), 
            ". The app might launch in your internet browser, but no data ever leaves your machine (and the app will still work if you turn off your wifi.)"
          )
        )
      )
    ),
    
    # Validator explanation
    div(
      class = "section-box",
      style = "margin-bottom: 30px;",
      h3(
        style = "margin-top: 0; color: #2c3e50; display: flex; align-items: center; gap: 10px;",
        icon("check-circle", style = "color: #27ae60;"),
        "The Validator"
      ),
      p(
        style = "line-height: 1.6; margin-bottom: 12px;",
        "The central tool for Psych-DS is the ", 
        strong("validator"),
        ", which you can think of as spellcheck for datasets as well as the gold-standard implementation of Psych-DS. In other words, if the validator says that your directory passes, then it's a Psych-DS dataset! If there are errors or inconsistencies from the standard, the validator will try to give helpful advice about what needs to change."
      ),
      p(
        style = "line-height: 1.6; margin-bottom: 0;",
        "The validator is available in ",
        tags$a(
          href = "https://psych-ds.github.io/validator/", 
          "a few different forms", 
          target = "_blank",
          style = "color: #3498db; font-weight: 500;"
        ),
        "; this Shiny app is designed to get you started using Psych-DS and validating your own datasets. You can use each of the tabs in this app independently, or one after the other:"
      )
    ),
    
    # Tools section header
    h3(
      style = "color: #2c3e50; margin-bottom: 20px; margin-top: 40px;",
      "Tools in This App"
    ),
    
    # Tool cards in grid
    div(
      style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(280px, 1fr)); gap: 20px; margin-bottom: 40px;",
      
      # Create Dataset card
      div(
        class = "tool-card",
        style = "background-color: white; border: 1px solid #e1e8ed; border-radius: 8px; padding: 20px; transition: all 0.3s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.05); cursor: pointer;",
        onclick = "$('.sidebar-menu a[data-value=\\'create\\']').click();",
        div(
          style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
          div(
            style = "width: 40px; height: 40px; background: linear-gradient(135deg, #667eea, #764ba2); border-radius: 8px; display: flex; align-items: center; justify-content: center; color: white;",
            icon("plus-circle")
          ),
          h4(style = "margin: 0; color: #2c3e50;", "Create Dataset")
        ),
        p(
          style = "margin: 0; color: #5a6c7d; line-height: 1.6; font-size: 14px;",
          "Choose some existing data on your device. A step-by-step interface will help you build a project folder with the same data in Psych-DS format, and then save the whole directory to your machine. This will not change your original data in any way unless you choose to overwrite the directory your data came from."
        )
      ),
      
      # Validate Dataset card
      div(
        class = "tool-card",
        style = "background-color: white; border: 1px solid #e1e8ed; border-radius: 8px; padding: 20px; transition: all 0.3s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.05); cursor: pointer;",
        onclick = "$('.sidebar-menu a[data-value=\\'validate\\']').click();",
        div(
          style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
          div(
            style = "width: 40px; height: 40px; background: linear-gradient(135deg, #11998e, #38ef7d); border-radius: 8px; display: flex; align-items: center; justify-content: center; color: white;",
            icon("check-circle")
          ),
          h4(style = "margin: 0; color: #2c3e50;", "Validate Dataset")
        ),
        p(
          style = "margin: 0; color: #5a6c7d; line-height: 1.6; font-size: 14px;",
          "Check if a dataset complies with the Psych-DS standard. You can check the dataset you just made, or use this on a directory that you or someone else edited by hand."
        )
      ),
      
      # Update Dictionary card
      div(
        class = "tool-card",
        style = "background-color: white; border: 1px solid #e1e8ed; border-radius: 8px; padding: 20px; transition: all 0.3s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.05); cursor: pointer;",
        onclick = "$('.sidebar-menu a[data-value=\\'dictionary\\']').click();",
        div(
          style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
          div(
            style = "width: 40px; height: 40px; background: linear-gradient(135deg, #f093fb, #f5576c); border-radius: 8px; display: flex; align-items: center; justify-content: center; color: white;",
            icon("book")
          ),
          h4(style = "margin: 0; color: #2c3e50;", "Update Dictionary")
        ),
        p(
          style = "margin: 0; color: #5a6c7d; line-height: 1.6; font-size: 14px;",
          "Psych-DS uses a small text file to store your metadata (information about your dataset). This text file includes an entry for every variable (column) in your data. Use this page to automatically edit your metadata with variable definitions and other information, or to export a nicely formatted PDF of your data dictionary."
        )
      ),
      
      # Dataset Explorer card
      div(
        class = "tool-card",
        style = "background-color: white; border: 1px solid #e1e8ed; border-radius: 8px; padding: 20px; transition: all 0.3s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.05); cursor: pointer;",
        onclick = "$('.sidebar-menu a[data-value=\\'explorer\\']').click();",
        div(
          style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
          div(
            style = "width: 40px; height: 40px; background: linear-gradient(135deg, #4facfe, #00f2fe); border-radius: 8px; display: flex; align-items: center; justify-content: center; color: white;",
            icon("table")
          ),
          h4(style = "margin: 0; color: #2c3e50;", "Dataset Explorer")
        ),
        p(
          style = "margin: 0; color: #5a6c7d; line-height: 1.6; font-size: 14px;",
          "Because Psych-DS datasets are all structured in the same basic way, we can build tools that work on any Psych-DS dataset. Use this page to browse your data and see filters, summaries about missing data, and more."
        )
      ),
      
      # Upload to OSF card
      div(
        class = "tool-card",
        style = "background-color: white; border: 1px solid #e1e8ed; border-radius: 8px; padding: 20px; transition: all 0.3s ease; box-shadow: 0 2px 4px rgba(0,0,0,0.05); cursor: pointer;",
        onclick = "$('.sidebar-menu a[data-value=\\'upload\\']').click();",
        div(
          style = "display: flex; align-items: center; gap: 12px; margin-bottom: 12px;",
          div(
            style = "width: 40px; height: 40px; background: linear-gradient(135deg, #fa709a, #fee140); border-radius: 8px; display: flex; align-items: center; justify-content: center; color: white;",
            icon("cloud-upload")
          ),
          h4(style = "margin: 0; color: #2c3e50;", "Upload to OSF")
        ),
        p(
          style = "margin: 0; color: #5a6c7d; line-height: 1.6; font-size: 14px;",
          "Upload a Psych-DS dataset to the Open Science Framework (OSF), auto-filling all the information you've already provided about this dataset (descriptions, authors, etc.) ",
          tags$span(
            style = "display: inline-block; background-color: #fff3cd; color: #856404; padding: 2px 8px; border-radius: 3px; font-size: 12px; font-weight: 500;",
            "Beta"
          )
        )
      )
    ),
    
    # Learn More section
    div(
      class = "section-box",
      style = "background-color: #f8f9fa; border: 1px solid #dee2e6;",
      h3(
        style = "margin-top: 0; color: #2c3e50; display: flex; align-items: center; gap: 10px;",
        icon("graduation-cap", style = "color: #3498db;"),
        "Learn More"
      ),
      p(
        style = "line-height: 1.6; margin-bottom: 12px;",
        "The ",
        tags$code(
          style = "background-color: #e9ecef; padding: 2px 6px; border-radius: 3px; color: #e83e8c;",
          "psychds-r"
        ),
        " package has some standalone functions in addition to this app. You can use those functions in your own R scripts to do things like read from, write to, or validate a Psych-DS formatted directory."
      ),
      p(
        style = "line-height: 1.6; margin-bottom: 12px;",
        "Information and example vignettes are available in the ",
        tags$a(
          href = "https://psych-ds.github.io/psychds-r/", 
          "psychds-r documentation", 
          target = "_blank",
          style = "color: #3498db; font-weight: 500;"
        ),
        "."
      ),
      p(style = "line-height: 1.6; margin-bottom: 8px;", "For more information about the Psych-DS project:"),
      tags$ul(
        style = "margin-top: 8px; margin-bottom: 0;",
        tags$li(
          style = "margin-bottom: 6px;",
          icon("globe", style = "color: #3498db; margin-right: 8px;"),
          tags$a(
            href = "https://psych-ds.github.io/", 
            "Psych-DS Website", 
            target = "_blank",
            style = "color: #3498db; font-weight: 500;"
          )
        ),
        tags$li(
          style = "margin-bottom: 6px;",
          icon("book", style = "color: #3498db; margin-right: 8px;"),
          tags$a(
            href = "https://psychds-docs.readthedocs.io/en/latest/", 
            "Psych-DS Documentation", 
            target = "_blank",
            style = "color: #3498db; font-weight: 500;"
          )
        ),
        tags$li(
          icon("github", style = "color: #3498db; margin-right: 8px;"),
          tags$a(
            href = "https://github.com/psych-ds/psych-DS", 
            "Psych-DS GitHub Repository", 
            target = "_blank",
            style = "color: #3498db; font-weight: 500;"
          )
        )
      )
    ),
    
    # Add hover effects via CSS
    tags$style(HTML("
      .tool-card:hover {
        transform: translateY(-4px);
        box-shadow: 0 8px 16px rgba(0,0,0,0.1) !important;
        border-color: #3498db !important;
      }
    "))
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
      "Select a directory containing a Psych-DS dataset to validate."
    ),
    
    sectionBox(
      title = "Select Dataset",
      description = "Select a Psych-DS dataset directory to validate.",
      div(
        class = "directory-input",
        textInput(
          ns("validate_dir"),  # Added ns() wrapper
          label = NULL,
          value = "",
          placeholder = "Path to Psych-DS dataset",
          width = "100%"
        ),
        shinyDirButton(
          ns("validate_dir_select"),  # Added ns() wrapper
          label = "...",
          title = "Select a dataset directory",
          class = "browse-btn"
        )
      ),
      div(
        style = "text-align: right; margin-top: 20px;",
        actionButton(
          ns("validate_btn"),  # Added ns() wrapper
          "Validate",
          class = "continue-btn"
        )
      )
    ),
    
    sectionBox(
      title = "Validation Results",
      # Results Summary at top (hidden until validation completes)
      div(
        id = ns("validation_summary"),
        style = "display: none; margin-bottom: 20px; padding-bottom: 15px; border-bottom: 1px solid #ddd;",
        h4("Results Summary", style = "margin-top: 0;"),
        div(
          id = ns("summary_content"),
          style = "padding: 15px; border-radius: 5px; background-color: #e8f5e9;"
        )
      ),
      # Validation Progress checklist
      div(
        id = ns("validation_results_ui"),
        h4("Validation Progress", style = "margin-top: 0;"),
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
    
    # Warning box about modifying dataset_description.json
    div(
      class = "alert alert-warning",
      style = "margin-bottom: 20px; background-color: #fff3cd; border-color: #ffc107; color: #856404;",
      icon("exclamation-triangle", style = "margin-right: 8px;"),
      tags$strong("This page can modify your dataset_description.json file."),
      tags$br(),
      "Each Psych-DS dataset has a text file that stores all the metadata for your dataset. To save the information you enter on this page back to that file, click the save button at the bottom of the page."
    ),
    
    div(
      class = "section-description",
      style = "margin-bottom: 20px;",
      "Define your dataset variables in detail. This information will be stored in your dataset_description.json file as machine-readable PropertyValue objects that help others understand your data."
    ),
    
    # Validation status display (will be populated by server)
    uiOutput(ns("validation_status")),
    
    sectionBox(
      title = "Select Dataset",
      description = "Select a Psych-DS project directory to edit the data dictionary (metadata).",
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
          div(
            class = "alert alert-info",
            style = "margin-bottom: 15px; background-color: #e7f3ff; border-color: #b3d9ff; color: #004085;",
            icon("info-circle", style = "margin-right: 8px;"),
            "Please note that the details you set here are descriptive: they tell whoever is using the data what the values in that column ", 
            tags$em("should"), 
            " be. That is, if you write that a variable called height should be greater than zero and should never be missing, the Psych-DS validator will still pass your dataset even if there is a negative or missing value in the actual CSV."
          ),
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
                          "Factor (categorical)" = "categorical",
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
                      textInput(
                        ns("var_unit"),
                        label = NULL,
                        placeholder = "e.g., milliseconds, years, points, etc.",
                        width = "100%"
                      ),
                      conditionalPanel(
                        condition = paste0("input['", ns("var_type"), "'] == 'categorical'"),
                        tags$small(
                          style = "color: #6c757d; display: block; margin-top: 5px;",
                          "Unit can apply to categorical variables too (e.g., 'years' for age ranges like 0-10, 11-20, etc.)"
                        )
                      ),
                      conditionalPanel(
                        condition = paste0("input['", ns("var_type"), "'] != 'categorical' && input['", ns("var_type"), "'] != 'number' && input['", ns("var_type"), "'] != 'integer'"),
                        tags$small(
                          style = "color: #6c757d; display: block; margin-top: 5px;",
                          "Optional - leave blank if not applicable"
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
                    tags$small(
                      "List and describe the possible values this variable can take on. Use the Label column and/or Description columns if the literal values that appear in that column are not self-explanatory (e.g., a value of 1 = 'rarely' and 'rarely' is defined as less than once a week).", 
                      style = "color: #666; display: block; margin-bottom: 8px;"
                    ),
                    
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
                            label = NULL,
                            placeholder = "e.g., 1, A, true"
                          )
                        ),
                        column(
                          width = 3,
                          textInput(
                            ns("new_cat_label"),
                            label = NULL,
                            placeholder = "e.g., Group A"
                          )
                        ),
                        column(
                          width = 4,
                          textInput(
                            ns("new_cat_description"),
                            label = NULL,
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
                # Conditional constraints section - show for numeric and date types
                conditionalPanel(
                  condition = paste0("input['", ns("var_type"), "'] == 'number' || input['", ns("var_type"), "'] == 'integer' || input['", ns("var_type"), "'] == 'date' || input['", ns("var_type"), "'] == 'datetime'"),
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
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("var_type"), "'] == 'date' || input['", ns("var_type"), "'] == 'datetime'"),
                            tags$small(
                              style = "color: #6c757d; display: block; margin-top: -10px;",
                              "Format: YYYY-MM-DD"
                            )
                          )
                        ),
                        column(
                          width = 6,
                          textInput(
                            ns("var_max"),
                            "Maximum Value", 
                            placeholder = "Maximum allowed value",
                            width = "100%"
                          ),
                          conditionalPanel(
                            condition = paste0("input['", ns("var_type"), "'] == 'date' || input['", ns("var_type"), "'] == 'datetime'"),
                            tags$small(
                              style = "color: #6c757d; display: block; margin-top: -10px;",
                              "Format: YYYY-MM-DD"
                            )
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
