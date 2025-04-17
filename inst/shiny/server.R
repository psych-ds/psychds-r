#' Psych-DS App Server
#'
#' This file defines the server-side logic for the Psych-DS app.
#' It handles the main app flow, state management, and module coordination.

# Load server modules (done in global.R in actual app)
source("modules/server_modules.R")

# Main server function
server <- function(input, output, session) {
  # Initialize application state
  state <- init_state()

  # Handle sidebar menu selection
  observeEvent(input$sidebar, {
    if (input$sidebar == "create") {
      # If we're on create dataset tab and already in a step,
      # don't reset the step (allows navigation back to this tab)
      if (state$current_step == 0) {
        state$current_step <- 1
      }
    } else {
      # For other tabs, reset step
      state$current_step <- 0
    }
  })

  # Dynamically render the Create Dataset UI based on current step
  output$create_dataset_ui <- renderUI({
    if (state$current_step == 1) {
      step1UI("step1")
    } else if (state$current_step == 2) {
      step2UI("step2")
    } else if (state$current_step == 3) {
      step3UI("step3")
    } else {
      # Default to step 1
      step1UI("step1")
    }
  })

  # Initialize the step modules
  step1Server("step1", state, session)
  step2Server("step2", state, session)
  step3Server("step3", state, session)

  # Handle Validate Dataset tab
  volumes <- c(Home = "~")
  if (.Platform$OS.type == "windows") {
    volumes <- c(volumes, getVolumes()())
  }

  # Set up directory selection for validate tab
  shinyDirChoose(
    input,
    "validate_dir_select",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  observeEvent(input$validate_dir_select, {
    if (!is.null(input$validate_dir_select)) {
      selected_dir <- parseDirPath(volumes, input$validate_dir_select)
      updateTextInput(session, "validate_dir", value = selected_dir)
    }
  })

  observeEvent(input$validate_btn, {
    if (input$validate_dir == "") {
      showModal(modalDialog(
        title = "Error",
        "Please select a dataset directory first.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      # In a real implementation, this would validate the dataset
      # For now, just show a placeholder
      showModal(modalDialog(
        title = "Validation Results",
        div(
          p("Dataset validation would run here."),
          p("The selected directory is:"),
          p(strong(input$validate_dir))
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })

  # Handle Update Dictionary tab
  # Set up directory selection for dictionary tab
  shinyDirChoose(
    input,
    "dictionary_dir_select",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  observeEvent(input$dictionary_dir_select, {
    if (!is.null(input$dictionary_dir_select)) {
      selected_dir <- parseDirPath(volumes, input$dictionary_dir_select)
      updateTextInput(session, "dictionary_dir", value = selected_dir)
    }
  })

  observeEvent(input$dictionary_btn, {
    if (input$dictionary_dir == "") {
      showModal(modalDialog(
        title = "Error",
        "Please select a dataset directory first.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      # In a real implementation, this would load the data dictionary editor
      # For now, just show a placeholder
      showModal(modalDialog(
        title = "Update Dictionary",
        div(
          p("Data dictionary editor would open here."),
          p("The selected directory is:"),
          p(strong(input$dictionary_dir))
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })

  # Handle Dataset Explorer tab
  # Set up directory selection for explorer tab
  shinyDirChoose(
    input,
    "explorer_dir_select",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  observeEvent(input$explorer_dir_select, {
    if (!is.null(input$explorer_dir_select)) {
      selected_dir <- parseDirPath(volumes, input$explorer_dir_select)
      updateTextInput(session, "explorer_dir", value = selected_dir)
    }
  })

  observeEvent(input$explorer_btn, {
    if (input$explorer_dir == "") {
      showModal(modalDialog(
        title = "Error",
        "Please select a dataset directory first.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      # In a real implementation, this would open the dataset explorer
      # For now, just show a placeholder
      showModal(modalDialog(
        title = "Dataset Explorer",
        div(
          p("Dataset explorer would open here."),
          p("The selected directory is:"),
          p(strong(input$explorer_dir))
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })

  # Handle Upload to OSF tab
  # Set up directory selection for upload tab
  shinyDirChoose(
    input,
    "upload_dir_select",
    roots = volumes,
    session = session,
    restrictions = system.file(package = "base")
  )

  observeEvent(input$upload_dir_select, {
    if (!is.null(input$upload_dir_select)) {
      selected_dir <- parseDirPath(volumes, input$upload_dir_select)
      updateTextInput(session, "upload_dir", value = selected_dir)
    }
  })

  observeEvent(input$upload_btn, {
    if (input$upload_dir == "") {
      showModal(modalDialog(
        title = "Error",
        "Please select a dataset directory first.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else if (input$osf_project == "") {
      showModal(modalDialog(
        title = "Error",
        "Please enter an OSF project ID.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else if (input$osf_token == "") {
      showModal(modalDialog(
        title = "Error",
        "Please enter your OSF token.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    } else {
      # In a real implementation, this would upload to OSF
      # For now, just show a placeholder
      showModal(modalDialog(
        title = "Upload to OSF",
        div(
          p("Dataset would be uploaded to OSF here."),
          p("The selected directory is:"),
          p(strong(input$upload_dir)),
          p("OSF Project ID:"),
          p(strong(input$osf_project))
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }
  })
}
