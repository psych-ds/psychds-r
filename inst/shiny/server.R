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

  # Track validator loading status
  validator_status <- reactiveVal(FALSE)

  # Observe when the validator is ready
  observeEvent(input$validator_ready, {
    if (input$validator_ready) {
      validator_status(TRUE)
      message("Validator loaded successfully and is ready to use!")
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Observe validation results
  observeEvent(input$validation_results, {
    results <- input$validation_results
    message("Received validation results")

    # Remove the loading notification if it exists
    removeNotification(id = "validate_notif")

    # Show a success message
    showNotification("Validation complete!", type = "message")

    # Store the results in state for use in the validation module
    state$validation_results <- results
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Handle validation errors
  observeEvent(input$validation_error, {
    error_msg <- input$validation_error
    warning("Validation error: ", error_msg)

    # Remove the loading notification if it exists
    removeNotification(id = "validate_notif")

    showNotification(paste("Validation error:", error_msg), type = "error")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Handle validator loading event
  observeEvent(input$validator_loaded, {
    validator_status(input$validator_loaded)
    if (input$validator_loaded) {
      message("JavaScript validator successfully loaded")
    } else {
      warning("JavaScript validator was not loaded correctly. Check validator.js file")
    }
  })

  # Function to prepare and run validation
  runValidation <- function(directory) {
    # Check if validator is loaded
    if (!validator_status()) {
      showNotification("JavaScript validator not loaded", type = "error")
      return(FALSE)
    }

    tryCatch({
      # Prepare the file tree structure expected by the validator
      # This is a simplified version - the real implementation would need to
      # recursively build a proper tree structure of the directory
      files <- list.files(directory, recursive = TRUE)
      file_tree <- list()

      # Build a simple file tree structure for testing
      for (file in files) {
        file_tree[[file]] <- list(
          type = "file",
          file = list(
            name = basename(file),
            path = file,
            text = function() {
              # This would be a promise in the real implementation
              return(readLines(file.path(directory, file), warn = FALSE))
            }
          )
        )
      }

      # Send the file tree to the JavaScript validator
      session$sendCustomMessage("run_validation", file_tree)
      return(TRUE)
    }, error = function(e) {
      showNotification(paste("Error preparing validation:", e$message), type = "error")
      return(FALSE)
    })
  }

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
  } else if (input$sidebar == "validate") {
    # For validate tab, set the validation directory to the created dataset if available
    if (!is.null(state$created_dataset_dir) && state$created_dataset_dir != "" && dir.exists(state$created_dataset_dir)) {
      updateTextInput(session, "validate_dir", value = state$created_dataset_dir)
      
      # Also reset the validation UI
      session$sendCustomMessage("reset_validation_ui", list())
    }
    
    # For tabs other than create, remember the current step before switching away
    if (!is.null(state$current_step) && state$current_step > 0) {
      last_create_step(state$current_step)
    }
    
    # Set current step to 0 (no step active)
    state$current_step <- 0
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

  # Initialize validation module
  validateServer("validate_dataset", state, session)


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
    if (!is.null(input$validate_dir) && input$validate_dir != "") {
    # Send a message to JavaScript to reset the checklist
    session$sendCustomMessage("reset_validation_ui", list())
    }
    if (!is.null(input$validate_dir_select)) {
      selected_dir <- parseDirPath(volumes, input$validate_dir_select)
      updateTextInput(session, "validate_dir", value = selected_dir)
    }
  })

  # Modify your validate_btn event handler
observeEvent(input$validate_btn, {
  if (input$validate_dir == "") {
    showModal(modalDialog(
      title = "Error",
      "Please select a dataset directory first.",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  } else {
    withProgress(message = "Validating dataset...", value = 0.1, {
      if (!dir.exists(input$validate_dir)) {
        setProgress(1)
        showNotification("Directory does not exist", type = "error")
        return()
      }
      
      setProgress(0.3, detail = "Building file tree...")
      tryCatch({
        # Build the file tree
        fileTree <- buildFileTree(input$validate_dir)
        
        # Basic validation of the tree
        if (length(fileTree) == 0) {
          setProgress(1)
          showNotification("No files found in directory", type = "error")
          return()
        }
        
        # Send to the validator
        setProgress(0.6, detail = "Running validator...")
        message("Sending file tree to validator with ", length(fileTree), " top-level entries")
        session$sendCustomMessage("run_validation", fileTree)
        
      }, error = function(e) {
        setProgress(1)
        message("Error during validation: ", e$message)
        showNotification(paste("Error during validation:", e$message), type = "error")
      })
    })
  }
})

observeEvent(input$test_validation, {
  # Create a test file tree
  testTree <- createTestFileTree()
  
  # Show a validation in progress notification
  withProgress(message = "Testing validation with basic data...", value = 0.2, {
    # Log the tree
    message("Created test file tree with ", length(testTree), " top-level entries")
    message("Test tree contains: ", paste(names(testTree), collapse = ", "))
    
    # Send the test tree for validation
    session$sendCustomMessage("run_validation", testTree)
    
    # Increment progress
    setProgress(0.8, detail = "Validation in progress...")
  })
})

observeEvent(input$validation_results, {
  if (is.null(input$validation_results)) return()
  
  results <- input$validation_results
  message("Received validation results: valid=", results$valid)
  
  # Store results in state for the validation module to use
  state$validation_results <- results
  
  # Show notification
  showNotification(
    ifelse(results$valid, 
           "Dataset is valid!", 
           "Dataset has issues - see details below"),
    type = ifelse(results$valid, "message", "warning"),
    duration = 5
  )
}, ignoreNULL = TRUE)



# Handle validation step updates
observeEvent(input$validation_step_status, {
  status_data <- input$validation_step_status
  message("Validation step status updated:", capture.output(str(status_data)))
  
  # Store step status for UI updates
  state$validation_step_status <- status_data
}, ignoreNULL = TRUE, ignoreInit = TRUE)

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
