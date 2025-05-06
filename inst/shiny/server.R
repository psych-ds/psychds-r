#' Psych-DS App Server
#'
#' This file defines the server-side logic for the Psych-DS app.
#' It handles the main app flow, state management, and module coordination.
source("modules/server_modules.R")

step_navigation <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Log inputs only once at initialization
    cat("Step Navigation Module initialized with ID:", id, "\n")

    # Step 1 button handler
    observeEvent(input$goto_step1, {
      cat("STEP 1 BUTTON CLICKED\n")
      # Step 1 is always accessible
      state$current_step <- 1
      cat("Updated current_step to:", state$current_step, "\n")
    })

    # Step 2 button handler
    observeEvent(input$goto_step2, {
      cat("STEP 2 BUTTON CLICKED\n")

      # Can only go to step 2 if:
      # 1. We've already been there (state$current_step >= 2), OR
      # 2. We have selected a directory and at least one data file
      can_go_to_step2 <- state$current_step >= 2 ||
        (!is.null(state$project_dir) &&
           state$project_dir != "" &&
           length(state$data_files) > 0)

      cat("Can go to step 2:", can_go_to_step2, "\n")
      cat("- current_step:", state$current_step, "\n")
      cat("- project_dir:", if(is.null(state$project_dir)) "NULL" else state$project_dir, "\n")
      cat("- data_files count:", length(state$data_files), "\n")

      if (can_go_to_step2) {
        state$current_step <- 2
        cat("Updated current_step to:", state$current_step, "\n")
      } else {
        showNotification("Complete Step 1 first (select a project directory and at least one data file)",
                         type = "warning")
        cat("Navigation to Step 2 blocked - requirements not met\n")
      }
    })

    # Step 3 button handler
    observeEvent(input$goto_step3, {
      cat("STEP 3 BUTTON CLICKED\n")

      # Can only go to step 3 if:
      # 1. We've already been there (state$current_step >= 3), OR
      # 2. We have completed step 2 (dataset name and description filled)

      # Check if dataset_info exists
      if (is.null(state$dataset_info)) {
        cat("dataset_info is NULL\n")
        dataset_info_complete <- FALSE
      } else {
        cat("dataset_info exists\n")
        cat("- name:", if(is.null(state$dataset_info$name)) "NULL" else state$dataset_info$name, "\n")
        cat("- description:", if(is.null(state$dataset_info$description)) "NULL" else "EXISTS", "\n")

        dataset_info_complete <- !is.null(state$dataset_info$name) &&
          !is.null(state$dataset_info$description) &&
          state$dataset_info$name != "" &&
          state$dataset_info$description != ""
      }

      can_go_to_step3 <- state$current_step >= 3 || dataset_info_complete

      cat("Can go to step 3:", can_go_to_step3, "\n")
      cat("- current_step:", state$current_step, "\n")
      cat("- dataset_info_complete:", dataset_info_complete, "\n")

      if (can_go_to_step3) {
        state$current_step <- 3
        cat("Updated current_step to:", state$current_step, "\n")
      } else {
        showNotification("Complete Step 2 first (provide dataset name and description)",
                         type = "warning")
        cat("Navigation to Step 3 blocked - requirements not met\n")
      }
    })
  })
}

# Main server function
server <- function(input, output, session) {
  # Initialize application state
  state <- init_state()

  # Add a reactive value to store the last create tab step
  last_create_step <- reactiveVal(1)

  # Handle sidebar menu selection
  observeEvent(input$sidebar, {
    if (input$sidebar == "create") {
      # If we're on create dataset tab, use the last remembered step
      # instead of always defaulting to 1
      if (!is.null(last_create_step()) && last_create_step() > 0) {
        state$current_step <- last_create_step()
      } else if (is.null(state$current_step) || state$current_step == 0) {
        # Only use the default step 1 if we never set a step before
        state$current_step <- 1
      }
    } else {
      # For other tabs, remember the current step before switching away
      if (!is.null(state$current_step) && state$current_step > 0) {
        last_create_step(state$current_step)
      }

      # For other tabs, set current step to 0 (no step active)
      state$current_step <- 0
    }
  })

  stepNavServer <- step_navigation("step_nav", state, session)



  # Dynamically render the Create Dataset UI based on current step
  output$create_dataset_ui <- renderUI({
    # Print to console for debugging
    cat("Rendering UI for step:", state$current_step, "\n")

    switch(as.character(state$current_step),
           "1" = step1UI("step1"),
           "2" = step2UI("step2"),
           "3" = step3UI("step3"),
           step1UI("step1"))  # Default fallback
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
