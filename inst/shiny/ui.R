#' Psych-DS App UI
#'
#' This file defines the main UI for the Psych-DS app.
#' It loads the modular UI components and handles the top-level UI structure.

# Load UI modules (done in global.R in actual app)
source("modules/ui_modules.R")

# Main UI definition
ui <- dashboardPage(
  skin = "blue",

  # Header
  dashboardHeader(
    title = span("psych-DS", style = "font-size: 24px;"),
    titleWidth = 200
  ),

  # Sidebar with fixed width
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      id = "sidebar",
      menuItem("Create Dataset", tabName = "create", icon = icon("plus-circle"), selected = TRUE),
      menuItem("Validate Dataset", tabName = "validate", icon = icon("check-circle")),
      menuItem("Update Dictionary", tabName = "dictionary", icon = icon("book")),
      menuItem("Dataset Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Upload to OSF", tabName = "upload", icon = icon("cloud-upload")),
      menuItem("Help", tabName = "help", icon = icon("question-circle"))
    )
  ),

  # Body
  dashboardBody(
    useShinyjs(),
    # Include the external CSS file
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = paste0("css/styles.css?v=", format(Sys.time(), "%Y%m%d%H%M%S"))),
      # Include SortableJS library
      tags$script(src = "https://cdn.jsdelivr.net/npm/sortablejs@1.14.0/Sortable.min.js")
    ),

    tabItems(
      # Create Dataset Tab
      tabItem(
        tabName = "create",
        uiOutput("create_dataset_ui")
      ),

      # Validate Dataset Tab
      tabItem(
        tabName = "validate",
        h2("Validate Dataset"),
        p("This feature will allow you to validate an existing Psych-DS dataset against the schema."),
        # Placeholder for future implementation
        div(
          class = "section-box",
          div(class = "section-title", "Select Dataset"),
          div(class = "section-description",
              "Select a Psych-DS dataset to validate."),

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
        )
      ),

      # Update Dictionary Tab
      tabItem(
        tabName = "dictionary",
        h2("Update Dictionary"),
        p("This feature will allow you to update the data dictionary of an existing Psych-DS dataset."),
        # Placeholder for future implementation
        div(
          class = "section-box",
          div(class = "section-title", "Select Dataset"),
          div(class = "section-description",
              "Select a Psych-DS dataset to update its data dictionary."),

          div(
            class = "directory-input",
            textInput(
              "dictionary_dir",
              label = NULL,
              value = "",
              placeholder = "Path to Psych-DS dataset",
              width = "100%"
            ),
            shinyDirButton(
              "dictionary_dir_select",
              label = "...",
              title = "Select a dataset directory",
              class = "browse-btn"
            )
          ),

          div(
            style = "text-align: right; margin-top: 20px;",
            actionButton(
              "dictionary_btn",
              "Continue",
              class = "continue-btn"
            )
          )
        )
      ),

      # Dataset Explorer Tab
      tabItem(
        tabName = "explorer",
        h2("Dataset Explorer"),
        p("This feature will allow you to explore and visualize Psych-DS datasets."),
        # Placeholder for future implementation
        div(
          class = "section-box",
          div(class = "section-title", "Select Dataset"),
          div(class = "section-description",
              "Select a Psych-DS dataset to explore."),

          div(
            class = "directory-input",
            textInput(
              "explorer_dir",
              label = NULL,
              value = "",
              placeholder = "Path to Psych-DS dataset",
              width = "100%"
            ),
            shinyDirButton(
              "explorer_dir_select",
              label = "...",
              title = "Select a dataset directory",
              class = "browse-btn"
            )
          ),

          div(
            style = "text-align: right; margin-top: 20px;",
            actionButton(
              "explorer_btn",
              "Explore",
              class = "continue-btn"
            )
          )
        )
      ),

      # Upload to OSF Tab
      tabItem(
        tabName = "upload",
        h2("Upload to OSF"),
        p("This feature will allow you to upload Psych-DS datasets to the Open Science Framework (OSF)."),
        # Placeholder for future implementation
        div(
          class = "section-box",
          div(class = "section-title", "Select Dataset"),
          div(class = "section-description",
              "Select a Psych-DS dataset to upload to OSF."),

          div(
            class = "directory-input",
            textInput(
              "upload_dir",
              label = NULL,
              value = "",
              placeholder = "Path to Psych-DS dataset",
              width = "100%"
            ),
            shinyDirButton(
              "upload_dir_select",
              label = "...",
              title = "Select a dataset directory",
              class = "browse-btn"
            )
          ),

          div(
            class = "section-title",
            style = "margin-top: 20px;",
            "OSF Project"
          ),
          div(class = "section-description",
              "Enter your OSF project information."),

          textInput("osf_project", "Project ID", placeholder = "OSF project ID"),
          passwordInput("osf_token", "OSF Token", placeholder = "Your OSF personal access token"),

          div(
            style = "text-align: right; margin-top: 20px;",
            actionButton(
              "upload_btn",
              "Upload",
              class = "continue-btn"
            )
          )
        )
      ),

      # Help Tab
      tabItem(
        tabName = "help",
        h2("Help"),
        div(
          class = "section-box",
          div(class = "section-title", "About Psych-DS"),
          p("The Psych-DS standard is a data organization standard for psychological datasets.
            It defines a consistent structure for organizing research data, making it easier to
            share, understand, and reuse psychological datasets."),

          div(class = "section-title", style = "margin-top: 20px;", "Using This App"),
          p("This app provides tools for creating, validating, and exploring Psych-DS datasets:"),
          tags$ul(
            tags$li(strong("Create Dataset:"), "Convert existing data to the Psych-DS format with a step-by-step interface"),
            tags$li(strong("Validate Dataset:"), "Check if datasets comply with the Psych-DS standard"),
            tags$li(strong("Update Dictionary:"), "Create and edit data dictionaries for your datasets"),
            tags$li(strong("Dataset Explorer:"), "Explore the structure and content of Psych-DS datasets"),
            tags$li(strong("Upload to OSF:"), "Upload Psych-DS datasets to the Open Science Framework (OSF)")
          ),

          div(class = "section-title", style = "margin-top: 20px;", "Resources"),
          p("For more information about the Psych-DS standard, visit the following resources:"),
          tags$ul(
            tags$li(tags$a(href = "https://psych-ds.github.io/", "Psych-DS Website", target = "_blank")),
            tags$li(tags$a(href = "https://github.com/psych-ds/psych-DS", "Psych-DS GitHub Repository", target = "_blank")),
            tags$li(tags$a(href = "https://psych-ds.github.io/psych-DS/DATA_DICTIONARY.html", "Psych-DS Data Dictionary Guide", target = "_blank"))
          )
        )
      )
    )
  )
)
