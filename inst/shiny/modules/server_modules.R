#' Server Module Definitions
#'
#' This file contains server-side logic for the modular UI components.
#' Each module handles its own state and communicates with the global state.

#' Directory Input Server Module
#'
#' Handles directory selection and validation
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
directoryInputServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Create a reactive value to store the current path
    path_value <- reactiveVal("")
    
    # Set up directory selection
    volumes <- c(Home = "~")
    if (.Platform$OS.type == "windows") {
      volumes <- c(volumes, getVolumes()())
    }
    
    shinyDirChoose(
      input, 
      "select", 
      roots = volumes, 
      session = session,
      restrictions = system.file(package = "base")
    )
    
    # Update the project directory input when a directory is selected
    observeEvent(input$select, {
      tryCatch({
        if (!is.null(input$select)) {
          selected_dir <- parseDirPath(volumes, input$select)
          
          if (length(selected_dir) > 0 && selected_dir != "") {
            # Update input field
            updateTextInput(session, "path", value = selected_dir)
            
            # Update our reactive value
            path_value(selected_dir)
            
            # Update global state
            state$project_dir <- selected_dir
          }
        }
      }, error = function(e) {
        # Handle errors quietly
      })
    })
    
    # Also monitor manual changes to the path input
    observeEvent(input$path, {
      if (input$path != "" && input$path != path_value()) {
        # Update reactive value
        path_value(input$path)
        
        # Update global state
        state$project_dir <- input$path
      }
    })
    
    # Return reactive that provides current path
    return(path_value)
  })
}

#' File Browser Server Module
#'
#' Handles file listing and selection
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param dir_path Reactive expression that returns the directory path
#' @param session The current session object
fileBrowserServer <- function(id, state, dir_path, session) {
  moduleServer(id, function(input, output, session) {
    # Store selected files
    selected <- reactiveVal(character(0))
    
    # Handle file selection
    observeEvent(input$toggle_file, {
      file <- input$toggle_file
      
      current <- selected()
      if (file %in% current) {
        selected(setdiff(current, file))
      } else {
        selected(c(current, file))
      }
      
      # Update global state
      state$data_files <- selected()
    })
    
    # Handle directory selection
    observeEvent(input$select_dir, {
      dir_prefix <- input$select_dir
      
      # Get all files
      all_files <- list_csv_files(dir_path())
      
      # Find files in this directory
      dir_files <- all_files[startsWith(all_files, dir_prefix)]
      
      # Get current selection
      current <- selected()
      
      # Check if all directory files are already selected
      if (all(dir_files %in% current)) {
        # Deselect all files in this directory
        selected(setdiff(current, dir_files))
      } else {
        # Select all files in this directory
        selected(unique(c(current, dir_files)))
      }
      
      # Update global state
      state$data_files <- selected()
    })
    
    # Render the file browser with complete hierarchy
    output$file_container <- renderUI({
      current_dir <- dir_path()
      
      # If no directory selected
      if (is.null(current_dir) || current_dir == "") {
        return(div(
          style = "text-align: center; padding-top: 30px; color: #999;",
          "Please select a project directory first"
        ))
      }
      
      # Get CSV files
      files <- list_csv_files(current_dir)
      
      # If no files found
      if (length(files) == 0) {
        return(div(
          style = "text-align: center; padding-top: 30px; color: #999;",
          "No CSV files found in this directory"
        ))
      }
      
      # Create a hierarchical directory structure
      hierarchy <- organize_directory_hierarchy(files)
      
      # Sort directories by their full path to maintain hierarchy order
      dir_paths <- names(hierarchy)
      dir_paths <- dir_paths[order(sapply(dir_paths, function(d) {
        # Count the number of path components to sort by depth
        length(strsplit(d, "/")[[1]])
      }), dir_paths)]
      
      # Build the UI
      ui_elements <- tagList(
        # File count
        div(style = "color: #999; font-size: 12px; margin-bottom: 10px;",
            paste("Found", length(files), "CSV files"))
      )
      
      # Process directories and files
      for (dir_path in dir_paths) {
        dir_info <- hierarchy[[dir_path]]
        
        # Skip files (we'll handle them with their parent directories)
        if (dir_info$is_file) {
          next
        }
        
        # Display the directory header
        dir_indentation <- paste0(rep("&nbsp;", dir_info$level * 3), collapse = "")
        
        # Get files in this directory (with full paths for selection)
        dir_file_paths <- if (length(dir_info$files) > 0) {
          paste0(dir_path, "/", dir_info$files)
        } else {
          character(0)
        }
        
        # Check if any files in this directory are selected
        current_selection <- selected()
        dir_selected <- any(dir_file_paths %in% current_selection)
        all_selected <- length(dir_file_paths) > 0 && all(dir_file_paths %in% current_selection)
        
        # Add directory
        ui_elements <- tagAppendChild(ui_elements,
          div(
            style = paste0(
              "margin-top: 5px; font-weight: bold; cursor: pointer; ",
              if (all_selected) "color: #2196F3;" else if (dir_selected) "color: #64B5F6;" else ""
            ),
            # Directory indentation and name
            HTML(paste0(dir_indentation, dir_info$name, "/")),
            onclick = paste0("Shiny.setInputValue('", session$ns("select_dir"), "', '", 
                          dir_path, "', {priority: 'event'})")
          )
        )
        
        # Add files in this directory
        if (length(dir_info$files) > 0) {
          # Sort files alphabetically
          sorted_files <- sort(dir_info$files)
          
          for (filename in sorted_files) {
            file_path <- paste0(dir_path, "/", filename)
            is_selected <- file_path %in% current_selection
            
            ui_elements <- tagAppendChild(ui_elements,
              div(
                style = paste0(
                  "padding: 2px 5px; margin: 1px 0; cursor: pointer; ",
                  if (is_selected) "background-color: #e3f2fd; border-radius: 3px;" else ""
                ),
                # File indentation and name
                HTML(paste0(dir_indentation, "&nbsp;&nbsp;&nbsp;", filename)),
                onclick = paste0("Shiny.setInputValue('", session$ns("toggle_file"), "', '", 
                               file_path, "', {priority: 'event'})")
              )
            )
          }
        }
      }
      
      # Add root files (if any)
      root_files <- files[!grepl("/", files)]
      if (length(root_files) > 0) {
        # Sort root files alphabetically
        root_files <- sort(root_files)
        
        # Add a header for root files
        if (length(root_files) > 0) {
          ui_elements <- tagAppendChild(ui_elements,
            div(
              style = "margin-top: 8px; font-weight: bold;",
              "Root files:"
            )
          )
          
          # Add each root file
          current_selection <- selected()
          for (file in root_files) {
            is_selected <- file %in% current_selection
            
            ui_elements <- tagAppendChild(ui_elements,
              div(
                style = paste0(
                  "padding: 2px 5px; margin: 1px 0; cursor: pointer; ",
                  if (is_selected) "background-color: #e3f2fd; border-radius: 3px;" else ""
                ),
                # File indentation and name
                HTML(paste0("&nbsp;&nbsp;&nbsp;", file)),
                onclick = paste0("Shiny.setInputValue('", session$ns("toggle_file"), "', '", 
                               file, "', {priority: 'event'})")
              )
            )
          }
        }
      }
      
      return(ui_elements)
    })
    
    # Return selected files
    return(selected)
  })
}

#' Optional Directories Server Module
#'
#' Handles selection and customization of optional directories
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
optionalDirsServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Store custom directories locally
    custom_dirs <- reactiveVal(list())
    
    # Handle adding custom directories
    observeEvent(input$add, {
      if (input$custom_name != "") {
        # Format directory name
        custom_dir <- input$custom_name
        if (!grepl("/$", custom_dir)) {
          custom_dir <- paste0(custom_dir, "/")
        }
        
        # Check if directory already exists
        current <- custom_dirs()
        dir_names <- sapply(current, function(x) x$name)
        
        if (custom_dir %in% dir_names) {
          showNotification("This directory has already been added", type = "warning")
          return()
        }
        
        # Create unique ID for checkbox
        dir_id <- generate_id("dir")
        
        # Add to list of custom directories
        new_dir <- list(
          id = dir_id,
          name = custom_dir,
          selected = TRUE
        )
        
        custom_dirs(c(current, list(new_dir)))
        
        # Add checkbox to UI
        insertUI(
          selector = paste0("#", session$ns("container")),
          where = "beforeEnd",
          ui = checkboxInput(
            session$ns(dir_id),
            custom_dir,
            value = TRUE
          )
        )
        
        # Clear the input
        updateTextInput(session, "custom_name", value = "")
        
        # Update global state
        updateGlobalState()
      }
    })
    
    # Update standard directory selections in global state
    observe({
      standard_dirs <- list(
        analysis = input$dir_analysis,
        materials = input$dir_materials,
        results = input$dir_results,
        products = input$dir_products,
        documentation = input$dir_documentation
      )
      
      # Update global state for standard directories
      for (dir_name in names(standard_dirs)) {
        state$optional_dirs[[dir_name]] <- standard_dirs[[dir_name]]
      }
    })
    
    # Update custom directory selections when checkboxes change
    observe({
      # Get the current custom directories
      current <- custom_dirs()
      
      # Update selected status for each directory
      for (i in seq_along(current)) {
        dir_id <- current[[i]]$id
        if (exists(dir_id, where = input)) {
          current[[i]]$selected <- input[[dir_id]]
        }
      }
      
      # Update local state
      custom_dirs(current)
      
      # Update global state
      updateGlobalState()
    })
    
    # Update global state with custom directories
    updateGlobalState <- function() {
      current <- custom_dirs()
      
      # Filter to only selected directories and create character vector
      selected_dirs <- character(0)
      
      if (length(current) > 0) {
        for (i in seq_along(current)) {
          if (current[[i]]$selected) {
            # Strip trailing slash
            dir_name <- gsub("/$", "", current[[i]]$name)
            selected_dirs <- c(selected_dirs, dir_name)
          }
        }
      }
      
      # Update global state with character vector
      state$optional_dirs$custom <- selected_dirs
    }
    
    # Return reactive that provides all selected directories
    return(reactive({
      all_dirs <- c()
      
      # Add standard directories
      if (input$dir_analysis) all_dirs <- c(all_dirs, "analysis")
      if (input$dir_materials) all_dirs <- c(all_dirs, "materials")
      if (input$dir_results) all_dirs <- c(all_dirs, "results")
      if (input$dir_products) all_dirs <- c(all_dirs, "products")
      if (input$dir_documentation) all_dirs <- c(all_dirs, "documentation")
      
      # Add custom directories
      current <- custom_dirs()
      
      if (length(current) > 0) {
        selected_custom <- character(0)
        
        for (i in seq_along(current)) {
          if (current[[i]]$selected) {
            dir_name <- gsub("/$", "", current[[i]]$name)
            selected_custom <- c(selected_custom, dir_name)
          }
        }
        
        all_dirs <- c(all_dirs, selected_custom)
      }
      
      return(all_dirs)
    }))
  })
}

#' Step 1 Server Module
#'
#' Handles project directory and data file selection
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
step1Server <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Initialize directory input handler
    dir_path <- directoryInputServer("project_dir", state, session)
    
    # Initialize file browser with the reactive directory path
    selected_files <- fileBrowserServer("files", state, dir_path, session)
    
    # Initialize optional directories
    selected_dirs <- optionalDirsServer("opt_dirs", state, session)
    
    # Handle Continue button
    observeEvent(input$continue, {
      # Check if a valid directory is selected
      if (is.null(dir_path()) || dir_path() == "") {
        showModal(modalDialog(
          title = "Error",
          "Please select a project directory first.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else if (length(selected_files()) == 0) {
        showModal(modalDialog(
          title = "Warning",
          "No data files are selected. Do you want to continue anyway?",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("continue_no_files"), "Continue Anyway", class = "btn-warning")
          )
        ))
      } else {
        # Proceed to next step
        proceedToNextStep()
      }
    })
    
    # Handle continuing with no files
    observeEvent(input$continue_no_files, {
      removeModal()
      proceedToNextStep()
    })
    
    # Function to proceed to next step
    proceedToNextStep <- function() {
      # Update global state with current selections
      state$project_dir <- dir_path()
      state$data_files <- selected_files()
      
      # Show summary before proceeding
      dirs <- selected_dirs()
      showModal(modalDialog(
        title = "Dataset Creation - Step 1 Complete",
        div(
          p(strong("Project Directory:"), state$project_dir),
          p(strong("Selected Files:"), 
            if(length(state$data_files) > 0) paste(state$data_files, collapse = ", ") else "None"),
          p(strong("Optional Directories:"), 
            if(length(dirs) > 0) paste(dirs, collapse = ", ") else "None")
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("proceed_step2"), "Proceed to Step 2", class = "btn-primary")
        )
      ))
    }
    
    # Handle proceeding to step 2
    observeEvent(input$proceed_step2, {
      # Move to step 2
      removeModal()
      state$current_step <- 2
      
      # In the future, this will create data dictionary templates
      # and transition to Step 2
    })
  })
}