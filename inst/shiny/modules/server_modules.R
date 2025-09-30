#' Server Module Definitions
#'
#' This file contains server-side logic for the modular UI components.
#' Each module handles its own state and communicates with the global state.

#' Directory Input Server Module - Fixed Version
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
          message("Directory selected: ", selected_dir)

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
        message("Error selecting directory: ", e$message)
      })
    })

    # Also monitor manual changes to the path input
    observeEvent(input$path, {
      message("Path input changed to: ", input$path)

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

#' File Browser Server Module - Fixed Version
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
      message("Toggle file: ", file)

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
      message("Select directory: ", dir_prefix)

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
      message("Rendering file container for: ", current_dir)

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

      # Track previously rendered directories to avoid duplicates
      rendered_dirs <- character(0)

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

#' Step 1 Server Module - Fixed Version
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

    # Log directory path changes for debugging
    observe({
      path <- dir_path()
      cat("Step1: Directory path updated to:", path, "\n")
    })

    # Initialize file browser with the reactive directory path
    selected_files <- fileBrowserServer("files", state, dir_path, session)

    # Initialize optional directories
    selected_dirs <- optionalDirsServer("opt_dirs", state, session)

    # When this module is initialized, ensure we restore state if it exists
    observe({
      # Project directory is already handled by directoryInputServer
      # File selection is already handled by fileBrowserServer
      # Optional directories are already handled by optionalDirsServer

      # This is where we would add any additional stateful elements
      # specific to step 1 that aren't handled by the sub-modules
    })

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

      # Save optional directories to state
      state$optional_dirs$directories <- selected_dirs()

      # Show summary before proceeding
      dirs <- selected_dirs()

      # Create a nicer display for files
      files_html <- if(length(state$data_files) > 0) {
        tagList(
          div(
            style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; margin-top: 5px; background-color: #f8f9fa;",
            lapply(state$data_files, function(file) {
              div(
                style = "padding: 4px 0; border-bottom: 1px solid #eee;",
                icon("file"),
                span(style = "margin-left: 5px;", file)
              )
            })
          )
        )
      } else {
        "None"
      }

      # Create a nicer display for directories
      dirs_html <- if(length(dirs) > 0) {
        tagList(
          div(
            style = "max-height: 150px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; margin-top: 5px; background-color: #f8f9fa;",
            lapply(dirs, function(dir) {
              div(
                style = "padding: 4px 0; border-bottom: 1px solid #eee;",
                icon("folder"),
                span(style = "margin-left: 5px;", dir)
              )
            })
          )
        )
      } else {
        "None"
      }

      showModal(modalDialog(
        title = "Dataset Creation - Step 1 Complete",
        div(
          p(strong("Project Directory:"), state$project_dir),

          strong("Selected Files:"),
          files_html,

          strong("Optional Directories:"),
          dirs_html
        ),
        # Valid sizes are "m" (default), "s", "l", or "xl"
        size = "l",
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

      # Create data dictionary templates for selected files
      if (length(state$data_files) > 0) {
        # Only create new data dictionary if it doesn't exist yet
        if (is.null(state$data_dict) || length(state$data_dict) == 0) {
          data_dict <- list()
          for (file in state$data_files) {
            # Create full path to file
            file_path <- file.path(state$project_dir, file)

            # Extract column information if file exists
            if (file.exists(file_path)) {
              data_dict[[file]] <- extract_csv_structure(file_path)
            } else {
              # Create empty data dictionary if file doesn't exist yet
              data_dict[[file]] <- data.frame(
                name = character(0),
                type = character(0),
                description = character(0),
                stringsAsFactors = FALSE
              )
            }
          }

          # Update global state
          state$data_dict <- data_dict
        }
      }
    })
  })
}

#' Step 2 Server Module
#'
#' Handles data dictionary creation and editing
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
step2Server <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values for authors
    authors <- reactiveVal(list())
    detected_variables <- reactiveVal(data.frame())

    # IMPORTANT CHANGE: Pre-fill dataset information every time the module is rendered
    # Make sure this runs whenever needed by adding a dependency on state$current_step
    observe({
      # Adding this dependency ensures it runs when you return to this step
      step <- state$current_step

      # Initialize dataset name from state
      if (!is.null(state$dataset_info$name)) {
        updateTextInput(session, "dataset_name", value = state$dataset_info$name)
      }

      # Initialize dataset description from state
      if (!is.null(state$dataset_info$description)) {
        updateTextAreaInput(session, "dataset_description", value = state$dataset_info$description)
      }

      # Initialize authors from state if available
      if (length(authors()) == 0 && !is.null(state$dataset_info$authors) &&
          length(state$dataset_info$authors) > 0) {
        authors(state$dataset_info$authors)
      }
    })

    # Store input values immediately when they change
    observeEvent(input$dataset_name, {
      if (!is.null(input$dataset_name)) {
        state$dataset_info$name <- input$dataset_name
      }
    })

    observeEvent(input$dataset_description, {
      if (!is.null(input$dataset_description)) {
        state$dataset_info$description <- input$dataset_description
      }
    })

    # Store authors in state when they change
    observe({
      if (length(authors()) > 0) {
        state$dataset_info$authors <- authors()
      }
    })

    # On initialization, analyze selected files
    observe({
      req(state$project_dir)
      req(length(state$data_files) > 0)

      # Extract variable information from selected files
      variables_df <- extractVariableInfo(state$project_dir, state$data_files)

      # Only set if we have actual data
      if (!is.null(variables_df) && nrow(variables_df) > 0) {
        detected_variables(variables_df)
      }
    })

    # Extract variable information from files
    extractVariableInfo <- function(project_dir, file_paths) {
      # Initialize results data frame
      result <- data.frame(
        variable = character(),
        present_in = character(),
        stringsAsFactors = FALSE
      )

      # Track variables across files
      all_vars <- list()

      # Process each file
      for (file_path in file_paths) {
        full_path <- file.path(project_dir, file_path)

        if (file.exists(full_path)) {
          # Read column names from CSV
          tryCatch({
            # Read just the header row
            header <- colnames(read.csv(full_path, nrows = 1))

            # Add to the variable tracking
            for (var_name in header) {
              if (var_name %in% names(all_vars)) {
                all_vars[[var_name]] <- c(all_vars[[var_name]], file_path)  # Use full relative path
              } else {
                all_vars[[var_name]] <- file_path  # Use full relative path
              }
            }
          }, error = function(e) {
            # Handle errors reading the file
            warning(paste("Error reading", file_path, ":", e$message))
          })
        }
      }

      # Convert to data frame
      for (var_name in names(all_vars)) {
        result <- rbind(result, data.frame(
          variable = var_name,
          present_in = paste(all_vars[[var_name]], collapse = ", "),
          stringsAsFactors = FALSE
        ))
      }

      # Sort alphabetically by variable name
      if (nrow(result) > 0) {
        result <- result[order(result$variable), ]
      }

      return(result)
    }

    # Render the variables table
    output$variables_table <- DT::renderDataTable({
      vars_df <- detected_variables()

      if (is.null(vars_df) || nrow(vars_df) == 0) {
        return(DT::datatable(
          data.frame(
            Variable = "No variables detected",
            `Present In` = "Please select CSV files in Step 1",
            check.names = FALSE
          ),
          options = list(
            dom = 't',
            paging = FALSE,
            searching = FALSE,
            info = FALSE
          ),
          rownames = FALSE
        ))
      }

      # Create formatted file lists with scrollable container
      vars_df$present_in <- sapply(vars_df$present_in, function(files_str) {
        files <- strsplit(files_str, ", ")[[1]]
        file_divs <- character(length(files))

        for (i in seq_along(files)) {
          file_divs[i] <- sprintf('<div>%s</div>', files[i])
        }

        # Create scrollable container with CSS class
        paste0('<div class="file-list-scrollable">',
               paste(file_divs, collapse = ""),
               '</div>')
      })

      DT::datatable(
        vars_df,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15),
          scrollX = TRUE,
          dom = "ftip",
          columnDefs = list(
            list(className = 'dt-left', targets = "_all")
          )
        ),
        rownames = FALSE,
        selection = "none",
        escape = FALSE  # Important: allows HTML in cells
      )
    })

    # Render author list
    output$author_list <- renderUI({
      author_list <- authors()

      if (length(author_list) == 0) {
        return(
          div(
            style = "padding: 10px; text-align: center; color: #666;",
            "No authors added yet. Click 'Add New Author' below."
          )
        )
      }

      # Header row with columns for first/last name
      tagList(
        div(
          style = "display: flex; background-color: #f8f9fa; padding: 5px; border-bottom: 1px solid #ced4da;",
          div(style = "flex: 1;", strong("Given Name")),
          div(style = "flex: 1;", strong("Family Name")),
          div(style = "flex: 1;", strong("ORCID ID")),
          div(style = "flex: 0; width: 40px;", "")
        ),
        # Author rows
        lapply(seq_along(author_list), function(i) {
          author <- author_list[[i]]
          div(
            style = "display: flex; padding: 5px; border-bottom: 1px solid #ced4da;",
            div(
              style = "flex: 1;",
              author$first_name
            ),
            div(
              style = "flex: 1;",
              author$last_name
            ),
            div(
              style = "flex: 1;",
              author$orcid
            ),
            div(
              style = "flex: 0; width: 40px;",
              actionButton(
                inputId = session$ns(paste0("remove_author_", i)),
                label = NULL,
                icon = icon("trash"),
                class = "btn-sm btn-danger",
                style = "padding: 2px 6px;"
              )
            )
          )
        })
      )
    })

    # Handle adding a new author
    observeEvent(input$add_author, {
      showModal(modalDialog(
        title = "Add Author",
        # Stack fields vertically instead of flex layout
        div(
          textInput(session$ns("new_author_first"), "First Name", ""),
          textInput(session$ns("new_author_last"), "Last Name", ""),
          textInput(session$ns("new_author_orcid"), "ORCID ID (optional)",
                    placeholder = "e.g., 0000-0002-1825-0097")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("save_new_author"), "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    # Save new author
    observeEvent(input$save_new_author, {
      if (input$new_author_first != "" && input$new_author_last != "") {
        current_authors <- authors()

        # Add new author with separate first and last name
        current_authors[[length(current_authors) + 1]] <- list(
          first_name = input$new_author_first,
          last_name = input$new_author_last,
          orcid = input$new_author_orcid
        )

        # Update authors
        authors(current_authors)

        # Also update state immediately
        state$dataset_info$authors <- current_authors

        # Close modal
        removeModal()
      } else {
        showNotification("First and last name are required", type = "error")
      }
    })

    # Handle removing authors
    observe({
      author_list <- authors()

      for (i in seq_along(author_list)) {
        local({
          local_i <- i
          observeEvent(input[[paste0("remove_author_", local_i)]], {
            current_authors <- authors()

            # Remove the author at the specified index
            if (local_i <= length(current_authors)) {
              current_authors <- current_authors[-local_i]
              authors(current_authors)

              # Also update state immediately
              state$dataset_info$authors <- current_authors
            }
          })
        })
      }
    })

    # Pre-fill dataset information if it exists
    observe({
      # This will run once when the module initializes

      # Initialize authors from state if available
      if (length(authors()) == 0 && !is.null(state$dataset_info$authors) && length(state$dataset_info$authors) > 0) {
        authors(state$dataset_info$authors)
      }

      # Initialize dataset name from state if available
      if (!is.null(state$dataset_info$name)) {
        updateTextInput(session, "dataset_name", value = state$dataset_info$name)
      }

      # Initialize dataset description from state if available
      if (!is.null(state$dataset_info$description)) {
        updateTextAreaInput(session, "dataset_description", value = state$dataset_info$description)
      }
    })

    # Handle Back button
    observeEvent(input$back, {
      # No need to save metadata here since it's saved continuously

      # Confirm going back
      showModal(modalDialog(
        title = "Go Back to Step 1?",
        "Are you sure you want to go back to Step 1? Your progress in Step 2 will be saved.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_back"), "Go Back", class = "btn-warning")
        )
      ))
    })

    # Handle confirmed back action
    observeEvent(input$confirm_back, {
      removeModal()
      state$current_step <- 1
    })

    # Handle Continue button
    observeEvent(input$continue, {
      # Validate inputs
      if (is.null(input$dataset_name) || input$dataset_name == "") {
        showModal(modalDialog(
          title = "Missing Information",
          "Please enter a dataset name.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }

      if (is.null(input$dataset_description) || input$dataset_description == "") {
        showModal(modalDialog(
          title = "Missing Information",
          "Please enter a dataset description.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }

      # No need to call saveMetadata here since data is saved continuously

      # Create JSON preview
      json_preview <- createJsonPreview()

      # Show JSON preview modal
      showModal(modalDialog(
        title = "Dataset JSON Preview",
        div(
          p("Here's a preview of the dataset_description.json file that will be generated:"),

          # Use a pre tag for better code display
          tags$pre(
            class = "json-preview",
            HTML(json_preview)
          ),

          p("This JSON will be saved in your project directory when you proceed to Step 3.")
        ),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Back"),
          actionButton(session$ns("confirm_continue"), "Confirm and Continue", class = "btn-primary")
        )
      ))
    })

    # Handle confirmed continue action
    observeEvent(input$confirm_continue, {
      removeModal()
      # Move to step 3
      state$current_step <- 3
    })

    # Create JSON Preview function
    createJsonPreview <- function() {
      # Get variables
      vars_df <- detected_variables()
      if (is.null(vars_df) || nrow(vars_df) == 0) {
        variables_array <- "[]"
      } else {
        variables_list <- paste0('"', vars_df$variable, '"')
        variables_array <- paste0(
          "[\n      ",
          paste(variables_list, collapse = ",\n      "),
          "\n    ]"
        )
      }

      # Format authors in the new format
      author_list <- authors()
      if (length(author_list) == 0) {
        authors_array <- "[]"
      } else {
        author_entries <- lapply(author_list, function(author) {
          orcid_part <- if (!is.null(author$orcid) && author$orcid != "") {
            sprintf(',\n        "@id": "%s"', author$orcid)
          } else {
            ""
          }

          sprintf(
            '{\n        "@type": "Person",\n        "givenName": "%s",\n        "familyName": "%s"%s\n      }',
            author$first_name,
            author$last_name,
            orcid_part
          )
        })

        authors_array <- paste0(
          "[\n      ",
          paste(author_entries, collapse = ",\n      "),
          "\n    ]"
        )
      }

      # Create the JSON template with the correct format
      json_template <- paste0(
        "{\n",
        '    "@context": "https://schema.org/",\n',
        '    "@type": "Dataset",\n',
        sprintf('    "name": "%s",\n', gsub('"', '\\\\"', input$dataset_name)),
        sprintf('    "description": "%s",\n', gsub('"', '\\\\"', input$dataset_description)),
        '    "author": ', authors_array, ',\n',
        '    "variableMeasured": ', variables_array, '\n',
        "}"
      )

      # Simple HTML escaping for display
      json_html <- gsub("<", "&lt;", json_template)
      json_html <- gsub(">", "&gt;", json_html)

      # Manual syntax highlighting with spans
      # Mark keys
      json_html <- gsub('("@[^"]+"):', '<span class="json-key">\\1</span>:', json_html)
      json_html <- gsub('("[^@][^"]*"):',  '<span class="json-key">\\1</span>:', json_html)

      # Mark string values
      json_html <- gsub('": "([^"]*)"', '": <span class="json-string">"\\1"</span>', json_html)

      # Mark punctuation
      json_html <- gsub('\\{', '<span class="json-punctuation">{</span>', json_html)
      json_html <- gsub('\\}', '<span class="json-punctuation">}</span>', json_html)
      json_html <- gsub('\\[', '<span class="json-punctuation">[</span>', json_html)
      json_html <- gsub('\\]', '<span class="json-punctuation">]</span>', json_html)
      json_html <- gsub(',(\n)', '<span class="json-punctuation">,</span>\\1', json_html)

      return(json_html)
    }
  })
}

#' Step 3 Server Module
#'
#' Handles filename standardization and mapping
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
step3Server <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store filename mapping and configuration
    files <- reactiveVal(list())
    current_file <- reactiveVal(NULL)
    selected_keywords <- reactiveVal(list())
    file_mappings <- reactiveVal(list())

    # Add a debugging output for troubleshooting
    output$debug_text <- renderPrint({
      # Print current keywords and their values
      keywords <- selected_keywords()
      if (length(keywords) > 0) {
        lapply(keywords, function(k) {
          list(name = k$name, value = k$value, id = k$id)
        })
      } else {
        "No keywords selected"
      }
    })

    validateKeywordValue <- function(value) {
      if (is.null(value) || value == "") {
        return(list(valid = FALSE, message = "Value cannot be empty"))
      }
      
      if (!grepl("^[a-zA-Z0-9]+$", value)) {
        return(list(valid = FALSE, message = "Value must contain only letters and numbers (a-z, A-Z, 0-9). No spaces or special characters allowed."))
      }
      
      return(list(valid = TRUE, message = ""))
    }

    validateCustomKeyword <- function(keyword) {
      if (is.null(keyword) || keyword == "") {
        return(list(valid = FALSE, message = "Keyword cannot be empty"))
      }
      
      if (!grepl("^[a-z]+$", keyword)) {
        return(list(valid = FALSE, message = "Custom keywords must contain only lowercase letters (a-z). No numbers, uppercase letters, spaces, or special characters allowed."))
      }
      
      return(list(valid = TRUE, message = ""))
    }

    # Initialize with data files from state
    observe({
      req(state$project_dir)
      req(length(state$data_files) > 0)

      # Create initial file mappings
      mappings <- lapply(state$data_files, function(file) {
        list(
          original = file,
          new = "",
          keywords = list(),
          values = list()
        )
      })

      # Set the file mappings
      file_mappings(mappings)

      # Set the current file to the first one
      if (length(mappings) > 0) {
        current_file(1)
      }
    })

    # Render file mapping rows
    output$file_mapping_rows <- renderUI({
      mappings <- file_mappings()

      if (length(mappings) == 0) {
        return(div(class = "text-center text-muted", "No files selected"))
      }

      rows <- lapply(seq_along(mappings), function(i) {
        file_info <- mappings[[i]]
        is_current <- current_file() == i

        div(
          class = paste("file-mapping-row", if(is_current) "active" else "", if(i %% 2 == 0) "even" else "odd"),
          style = paste0(
            "padding: 8px 15px; border-bottom: 1px solid #eee; cursor: pointer; ",
            if(is_current) "background-color: #e3f2fd;" else if(i %% 2 == 0) "background-color: #f8f9fa;" else ""
          ),
          div(class = "row",
              div(class = "col-xs-6",
                  span(file_info$original, class = "original-filename"),
                  # Add a click handler to make this the current file
                  tags$script(paste0("$(document).on('click', '#", session$ns("file_mapping_rows"), " .file-mapping-row:eq(", i-1, ")', function() { Shiny.setInputValue('", session$ns("select_file"), "', ", i, ", {priority: 'event'}); });"))
              ),
              div(class = "col-xs-6",
                  if (file_info$new != "") {
                    span(file_info$new, class = "new-filename", style = "color: #3498db; font-weight: bold;")
                  } else {
                    if (is_current) {
                      actionButton(session$ns(paste0("generate_for_", i)), "Generate", class = "btn btn-xs btn-primary")
                    } else {
                      span("Click to configure", class = "text-muted")
                    }
                  }
              )
          )
        )
      })

      do.call(tagList, rows)
    })

    # Handle file selection
    observeEvent(input$select_file, {
      current_file(input$select_file)

      # Update selected keywords based on the current file's configuration
      mappings <- file_mappings()
      file_index <- input$select_file

      if (file_index <= length(mappings)) {
        file_info <- mappings[[file_index]]
        selected_keywords(file_info$keywords)

        # Reset all keyword value inputs
        for (keyword in file_info$keywords) {
          input_name <- paste0("keyword_value_", keyword$id)
          updateTextInput(session, input_name, value = "")
        }
      }
    })

    # Current file text
    output$current_file_text <- renderUI({
      file_index <- current_file()
      mappings <- file_mappings()

      if (is.null(file_index) || file_index > length(mappings)) {
        return(p("No file selected"))
      }

      file_info <- mappings[[file_index]]
      p(
        style = "margin: 0;",
        strong("Currently configuring:"),
        span(file_info$original, style = "font-style: italic;")
      )
    })

    # Handle keyword selection events - one for each standard keyword
    observeEvent(input$keyword_subject, {
      addKeyword("subject", "Subject")
    })

    observeEvent(input$keyword_study, {
      addKeyword("study", "Study")
    })

    observeEvent(input$keyword_session, {
      addKeyword("session", "Session")
    })

    observeEvent(input$keyword_task, {
      addKeyword("task", "Task")
    })

    observeEvent(input$keyword_condition, {
      addKeyword("condition", "Condition")
    })

    observeEvent(input$keyword_stimulus, {
      addKeyword("stimulus", "Stimulus")
    })

    observeEvent(input$keyword_trial, {
      addKeyword("trial", "Trial")
    })

    observeEvent(input$keyword_description, {
      addKeyword("description", "Description")
    })

    # Helper function to add a keyword
    addKeyword <- function(name, display) {
      keywords <- selected_keywords()

      # Only add if it doesn't already exist
      if (!any(sapply(keywords, function(k) k$name == name))) {
        # Add a unique ID for this keyword
        keyword_id <- paste0(name, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))

        keywords[[length(keywords) + 1]] <- list(
          name = name,
          display = display,
          value = "",
          id = keyword_id
        )
        selected_keywords(keywords)
        updateKeywordMapping()
      }
    }

    # Add custom keyword
    observeEvent(input$add_custom_keyword, {
      keyword_name <- input$custom_keyword_name

      if (keyword_name != "") {
        # Validate custom keyword
        validation_result <- validateCustomKeyword(keyword_name)
        
        if (!validation_result$valid) {
          # Show validation error in UI
          output$custom_keyword_validation <- renderUI({
            div(
              style = "color: #dc3545; font-size: 14px; padding: 5px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 3px;",
              icon("exclamation-triangle"), " ", validation_result$message
            )
          })
          
          # Also show notification popup
          showNotification(validation_result$message, type = "error", duration = 5)
          return()
        }
        
        # Clear validation message
        output$custom_keyword_validation <- renderUI({
          div(style = "min-height: 20px;")
        })

        keywords <- selected_keywords()

        if (!any(sapply(keywords, function(k) k$name == keyword_name))) {
          keyword_id <- paste0(keyword_name, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))

          keywords[[length(keywords) + 1]] <- list(
            name = keyword_name,
            display = keyword_name,
            value = "",
            id = keyword_id,
            custom = TRUE
          )
          selected_keywords(keywords)
          updateKeywordMapping()
          
          updateTextInput(session, "custom_keyword_name", value = "")
          showNotification("Custom keyword added successfully!", type = "message")
        } else {
          showNotification("This keyword already exists", type = "warning")
        }
      }
    })





    # Handle keyword order changes
    observeEvent(input$keyword_order, {
      # Get the keyword IDs from the sortable input
      keyword_ids <- input$keyword_order

      if (length(keyword_ids) > 0) {
        cat("Keyword order changed:", paste(keyword_ids, collapse=", "), "\n")

        # Get current keywords
        current_keywords <- selected_keywords()

        # Skip if no keywords
        if (length(current_keywords) == 0) return()

        # Create a new list with the updated order
        new_keywords <- list()

        # Create a map of id to keyword
        keyword_map <- list()
        for (i in seq_along(current_keywords)) {
          keyword <- current_keywords[[i]]
          keyword_map[[keyword$id]] <- keyword
        }

        # Then, rebuild the list in the new order
        for (id in keyword_ids) {
          if (id %in% names(keyword_map)) {
            new_keywords[[length(new_keywords) + 1]] <- keyword_map[[id]]
          }
        }

        # Update the selected keywords
        if (length(new_keywords) > 0) {
          # Verify we didn't lose any keywords
          if (length(new_keywords) == length(current_keywords)) {
            cat("Updating selected keywords with new order\n")
            selected_keywords(new_keywords)
            updateKeywordMapping()
          } else {
            cat("WARNING: New keywords list length doesn't match original (",
                length(new_keywords), " vs ", length(current_keywords), ")\n", sep="")
          }
        }
      }
    })

    observe({
      keywords <- selected_keywords()
      if (length(keywords) == 0) return()
      
      for (i in seq_along(keywords)) {
        keyword <- keywords[[i]]
        input_name <- paste0("keyword_value_", keyword$id)
        
        local({
          local_keyword <- keyword
          local_input_name <- input_name
          
          if (exists(local_input_name, where = input)) {
            observeEvent(input[[local_input_name]], {
              value <- input[[local_input_name]]
              validation_result <- validateKeywordValue(value)
              
              validation_id <- paste0("validation_", local_keyword$id)
              
              if (!validation_result$valid && !is.null(value) && value != "") {
                output[[validation_id]] <- renderUI({
                  div(
                    style = "color: #dc3545; font-size: 12px; padding: 3px; background-color: #f8d7da; border: 1px solid #f5c6cb; border-radius: 3px;",
                    icon("exclamation-triangle", style = "font-size: 10px;"), " ", validation_result$message
                  )
                })
              } else {
                output[[validation_id]] <- renderUI({
                  div(style = "min-height: 20px;")
                })
              }
            })
          }
        })
      }
    })

    # Handle keyword removal
    observeEvent(input$remove_keyword, {
      index <- input$remove_keyword
      keywords <- selected_keywords()

      if (index <= length(keywords)) {
        keywords <- keywords[-index]
        selected_keywords(keywords)
        updateKeywordMapping()
      }
    })

    # Update the current file's keyword mapping
    updateKeywordMapping <- function() {
      file_index <- current_file()
      if (is.null(file_index)) return()

      mappings <- file_mappings()
      if (file_index > length(mappings)) return()

      mappings[[file_index]]$keywords <- selected_keywords()
      file_mappings(mappings)

      # Clear the new filename since the configuration changed
      mappings[[file_index]]$new <- ""
      file_mappings(mappings)
    }

    # Modify the keyword value inputs rendering
    output$keyword_value_inputs <- renderUI({
      file_index <- current_file()
      mappings <- file_mappings()

      if (is.null(file_index) || file_index > length(mappings)) {
        return(div(
          style = "padding: 15px; text-align: center; color: #666;",
          "Select keywords above to configure values"
        ))
      }

      file_info <- mappings[[file_index]]
      keywords <- selected_keywords()

      if (length(keywords) == 0) {
        return(div(
          style = "padding: 15px; text-align: center; color: #666;",
          "Select keywords above to configure values"
        ))
      }

      # Create inputs in the same order as the keywords
      inputs <- lapply(seq_along(keywords), function(i) {
        keyword <- keywords[[i]]
        input_id <- paste0("keyword_value_", keyword$id)
        
        div(
          class = "form-group",
          style = "margin-bottom: 20px;",
          tags$label(
            class = "control-label",
            style = "color: #3498db; font-weight: bold; margin-bottom: 5px; display: block;",
            span(paste0(keyword$display, ":"))
          ),
          textInput(
            session$ns(input_id),
            NULL,
            value = "",
            placeholder = paste("Enter", tolower(keyword$display), "value (letters/numbers only)"),
            width = "100%"
          ),
          # Validation message placeholder for this input
          div(
            id = session$ns(paste0("validation_", keyword$id)),
            style = "margin-top: 5px; min-height: 20px;"
          )
        )
      })

      do.call(tagList, inputs)
    })

    output$selected_keywords <- renderUI({
      keywords <- selected_keywords()

      if (length(keywords) == 0) {
        return(div(
          style = "text-align: center; padding: 10px; color: #666;",
          "Click keywords above to add them here"
        ))
      }

      # Create a list of HTML elements for each keyword
      keyword_labels <- lapply(seq_along(keywords), function(i) {
        keyword <- keywords[[i]]

        tags$div(
          class = "keyword-chip-selected",
          id = paste0("keyword_", keyword$id),
          `data-keyword-id` = keyword$id,
          `data-keyword-name` = keyword$name,
          `data-keyword-index` = i,
          style = "display: inline-block; margin: 3px; padding: 5px 10px; background-color: #3498db; color: white; border-radius: 15px; position: relative; cursor: move;",

          # Keyword display name
          tags$span(
            style = "pointer-events: none;",
            keyword$display
          ),

          # Remove button
          tags$button(
            id = paste0("remove_keyword_", i),
            class = "remove-keyword",
            style = "background: none; border: none; color: white; opacity: 0.7; padding: 0 0 0 5px; font-size: 10px;",
            onclick = paste0("Shiny.setInputValue('", session$ns("remove_keyword"), "', ", i, ", {priority: 'event'});"),
            icon("times")
          )
        )
      })

      # Use the sortable.js function directly rather than rank_list
      tagList(
        # Container for the sortable list
        tags$div(
          id = session$ns("sortable_keywords"),  # Use session$ns here instead of ns
          class = "sortable-keywords",
          keyword_labels
        ),

        # Initialize sortable with JavaScript
        tags$script(HTML(paste0("
      $(document).ready(function() {
        // Initialize sortable
        if (typeof Sortable !== 'undefined') {
          var sortable = Sortable.create(document.getElementById('", session$ns("sortable_keywords"), "'), {  // Use session$ns here
            animation: 150,
            ghostClass: 'sortable-ghost',
            onEnd: function(evt) {
              // Get the new order
              var items = evt.to.children;
              var newOrder = [];
              for (var i = 0; i < items.length; i++) {
                newOrder.push($(items[i]).data('keyword-id'));
              }

              // Send to Shiny
              Shiny.setInputValue('", session$ns("keyword_order"), "', newOrder);  // Use session$ns here
            }
          });
        }
      });
    ")))
      )
    })

    # Monitor keyword value changes
    observe({
      file_index <- current_file()
      if (is.null(file_index)) return()

      mappings <- file_mappings()
      if (file_index > length(mappings)) return()

      keywords <- mappings[[file_index]]$keywords

      # Check for any changes in the keyword values
      updated <- FALSE

      for (i in seq_along(keywords)) {
        keyword <- keywords[[i]]
        input_name <- paste0("keyword_value_", keyword$id) # Use keyword ID instead of name

        if (exists(input_name, where = input)) {
          value <- input[[input_name]]

          if (!is.null(value) && value != keyword$value) {
            keywords[[i]]$value <- value
            updated <- TRUE
          }
        }
      }

      if (updated) {
        mappings[[file_index]]$keywords <- keywords
        file_mappings(mappings)
      }
    })

    # Preview filename
    output$filename_preview <- renderUI({
      file_index <- current_file()
      mappings <- file_mappings()

      if (is.null(file_index) || file_index > length(mappings)) {
        return(p("No file selected"))
      }

      file_info <- mappings[[file_index]]
      keywords <- file_info$keywords

      if (length(keywords) == 0) {
        return(p(
          style = "font-style: italic; color: #666; text-align: center;",
          "Add keywords to generate filename"
        ))
      }

      # Check if any keyword has an empty value
      empty_keywords <- any(sapply(keywords, function(k) k$value == ""))

      if (empty_keywords) {
        return(p(
          style = "font-style: italic; color: #666; text-align: center;",
          "Fill in all keyword values to generate filename"
        ))
      }

      # Create filename preview
      filename_parts <- c()
      for (keyword in keywords) {
        filename_parts <- c(filename_parts, paste0(keyword$name, "-", keyword$value))
      }

      # Add the file extension (use original filename extension)
      original <- file_info$original
      ext <- tools::file_ext(original)

      if (ext != "") {
        filename <- paste0(paste(filename_parts, collapse = "_"), "_data.", ext)
      } else {
        filename <- paste0(paste(filename_parts, collapse = "_"), "_data")
      }

      p(
        style = "font-weight: bold; color: #3498db; word-break: break-all; margin: 0; text-align: center;",
        filename
      )
    })

    # Generate filename button
    observeEvent(input$generate_filename, {
      cat("Generate filename button clicked\n")

      file_index <- current_file()
      if (is.null(file_index)) {
        cat("No file selected\n")
        return()
      }

      keywords <- selected_keywords()
      if (length(keywords) == 0) {
        showNotification("Please add at least one keyword", type = "warning")
        cat("No keywords selected\n")
        return()
      }

      # Collect and validate all keyword values
      keyword_values <- list()
      missing_keywords <- list()
      invalid_keywords <- list()

      for (i in seq_along(keywords)) {
        keyword <- keywords[[i]]
        input_name <- paste0("keyword_value_", keyword$id)

        if (input_name %in% names(input)) {
          value <- input[[input_name]]
          
          if (!is.null(value) && value != "") {
            # Validate the value
            validation_result <- validateKeywordValue(value)
            
            if (validation_result$valid) {
              keyword_values[[keyword$name]] <- value
              keywords[[i]]$value <- value
            } else {
              invalid_keywords[[length(invalid_keywords) + 1]] <- list(
                keyword = keyword,
                message = validation_result$message
              )
            }
          } else {
            missing_keywords[[length(missing_keywords) + 1]] <- keyword
          }
        } else {
          missing_keywords[[length(missing_keywords) + 1]] <- keyword
        }
      }

      # Check for validation errors
      if (length(invalid_keywords) > 0) {
        error_messages <- sapply(invalid_keywords, function(k) {
          paste0(k$keyword$display, ": ", k$message)
        })
        message <- paste("Please fix these validation errors:", paste(error_messages, collapse = "; "))
        showNotification(message, type = "error", duration = 8)
        return()
      }

      # Check for missing values
      if (length(missing_keywords) > 0) {
        missing_names <- sapply(missing_keywords, function(k) k$display)
        message <- paste("Please fill in values for:", paste(missing_names, collapse = ", "))
        showNotification(message, type = "warning")
        return()
      }

      # All validation passed - continue with filename generation
      filename_parts <- character(0)

      for (keyword in keywords) {
        value <- keyword_values[[keyword$name]]
        filename_part <- paste0(keyword$name, "-", value)
        filename_parts <- c(filename_parts, filename_part)
      }

      mappings <- file_mappings()
      file_info <- mappings[[file_index]]
      
      original <- file_info$original
      ext <- tools::file_ext(original)

      if (ext != "") {
        filename <- paste0(paste(filename_parts, collapse = "_"), "_data.", ext)
      } else {
        filename <- paste0(paste(filename_parts, collapse = "_"), "_data")
      }

      # Update the mapping
      updated_keywords <- keywords
      for (i in seq_along(updated_keywords)) {
        keyword_name <- updated_keywords[[i]]$name
        if (keyword_name %in% names(keyword_values)) {
          updated_keywords[[i]]$value <- keyword_values[[keyword_name]]
        }
      }

      mappings[[file_index]]$keywords <- updated_keywords
      mappings[[file_index]]$new <- filename
      file_mappings(mappings)

      selected_keywords(updated_keywords)

      showNotification("Filename generated successfully!", type = "message")
      
      auto_advance_to_next_file(file_index)
    })

    # Make sure this code is also updated to properly track values
    observe({
      # Skip if no file is selected
      file_index <- current_file()
      if (is.null(file_index)) return()

      # Get the current keywords
      keywords <- selected_keywords()
      if (length(keywords) == 0) return()

      # Check for input changes
      updated <- FALSE
      updated_keywords <- keywords

      for (i in seq_along(keywords)) {
        keyword <- keywords[[i]]
        input_name <- paste0("keyword_value_", keyword$id)

        if (exists(input_name, where = input)) {
          new_value <- input[[input_name]]

          # Only update if value is non-NULL and different
          if (!is.null(new_value) && new_value != keyword$value) {
            cat("Updating ", keyword$name, " value to '", new_value, "'\n", sep="")
            updated_keywords[[i]]$value <- new_value
            updated <- TRUE
          }
        }
      }

      # Update the keywords if any changed
      if (updated) {
        selected_keywords(updated_keywords)

        # Also update in the file mapping
        mappings <- file_mappings()
        if (file_index <= length(mappings)) {
          mappings[[file_index]]$keywords <- updated_keywords
          file_mappings(mappings)
        }
      }
    })

    # Function to auto-advance to the next unconfigured file
    auto_advance_to_next_file <- function(current_index) {
      cat("Starting auto_advance_to_next_file\n")
      cat("Current index:", current_index, "\n")

      mappings <- file_mappings()

      cat("Total mappings:", length(mappings), "\n")

      # If there's only one mapping or current index is the last mapping, don't advance
      if (length(mappings) <= 1 || current_index >= length(mappings)) {
        cat("Cannot advance - only one mapping or at last mapping\n")
        return()
      }

      # Validate inputs more robustly
      if (is.null(current_index) || current_index < 1 || current_index > length(mappings)) {
        cat("Invalid current_index, resetting to 1\n")
        current_index <- 1
      }

      # Total number of mappings
      total_mappings <- length(mappings)

      # Create a sequence of indices to check, wrapping around from current_index
      check_sequence <- c(
        seq(current_index + 1, total_mappings),  # From current index to end
        seq(1, current_index)                    # From start to current index
      )

      # Find the first unconfigured file
      for (i in check_sequence) {
        if (mappings[[i]]$new == "") {
          cat("Found unconfigured file at index", i, "\n")
          current_file(i)
          return()
        }
      }

      # If all files are configured
      cat("All files are configured\n")
    }

    # Handle generate buttons on the file mapping list
    observe({
      file_index <- current_file()
      if (is.null(file_index)) return()

      mappings <- file_mappings()
      for (i in seq_along(mappings)) {
        local({
          local_i <- i
          button_id <- paste0("generate_for_", local_i)

          if (exists(button_id, where = input)) {
            observeEvent(input[[button_id]], {
              current_file(local_i)
              # Scroll to the keywords section when a generate button is clicked
              session$sendCustomMessage("scrollToElement", list(elementId = session$ns("choose_keywords_section")))
            })
          }
        })
      }
    })

    # Handle back button
    observeEvent(input$back, {
      # Confirm going back
      showModal(modalDialog(
        title = "Go Back to Step 2?",
        "Are you sure you want to go back to Step 2? Your progress in Step 3 will be saved.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_back"), "Go Back", class = "btn-warning")
        )
      ))
    })

    # Handle confirmed back action
    observeEvent(input$confirm_back, {
      removeModal()

      # Save the file mappings to state
      state$file_mappings <- file_mappings()

      # Go back to step 2
      state$current_step <- 2
    })

    # Handle continue button
    observeEvent(input$continue, {
      # Validate that all files have been mapped
      mappings <- file_mappings()
      unmapped_files <- sapply(mappings, function(m) m$new == "")

      if (any(unmapped_files)) {
        showModal(modalDialog(
          title = "Missing Filename Mappings",
          div(
            p("Some files have not been assigned standardized filenames:"),
            tags$ul(
              lapply(which(unmapped_files), function(i) {
                tags$li(strong(mappings[[i]]$original))
              })
            ),
            p("Would you like to continue anyway?")
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_continue_unmapped"), "Continue Anyway", class = "btn-warning")
          )
        ))
      } else {
        # All files are mapped, proceed directly
        proceedToFinalStep()
      }
    })

    # Handle confirmed continue action
    observeEvent(input$confirm_continue_unmapped, {
      removeModal()
      proceedToFinalStep()
    })

    create_file_structure_html <- function(project_dir, file_mappings, optional_dirs) {
      # Create a nested list representing the file structure
      create_nested_list <- function() {
        # Root level items
        root_items <- list(
          tags$li(
            tags$span(class = "file-icon", "📄"),
            "dataset_description.json"
          )
        )

        # Add optional directories
        standard_dirs <- c("analysis", "materials", "results", "products", "documentation")
        for (dir in standard_dirs) {
          if (optional_dirs[[dir]]) {
            root_items[[length(root_items) + 1]] <- tags$li(
              tags$span(class = "folder-icon", "📁"),
              paste0(dir, "/")
            )
          }
        }

        # Add custom directories
        if (!is.null(optional_dirs$custom)) {
          for (custom_dir in optional_dirs$custom) {
            root_items[[length(root_items) + 1]] <- tags$li(
              tags$span(class = "folder-icon", "📁"),
              paste0(custom_dir, "/")
            )
          }
        }

        # Add data files
        data_files <- lapply(file_mappings(), function(mapping) {
          if (mapping$new != "") {
            tags$li(
              tags$span(class = "file-icon", "📄"),
              mapping$new
            )
          }
        })

        # Create the full structure
        tags$ul(
          class = "file-tree",
          tags$li(
            tags$span(class = "folder-icon", "📁"),
            "data/",
            tags$ul(
              lapply(data_files, function(file) file)
            )
          ),
          root_items
        )
      }

      # Return the nested list with some inline CSS for styling
      tagList(
        tags$style(HTML("
      .file-tree {
        font-family: monospace;
        line-height: 1.5;
        list-style-type: none;
        padding-left: 20px;
      }
      .file-tree ul {
        padding-left: 20px;
      }
      .file-icon, .folder-icon {
        margin-right: 5px;
      }
    ")),
        create_nested_list()
      )
    }

    proceedToFinalStep <- function() {
        # Save the file mappings to state
        state$file_mappings <- file_mappings()

        # Create a new Psych-DS compliant dataset directory
        original_project_dir <- state$project_dir
        dataset_name <- gsub("[^a-zA-Z0-9_-]", "_", tolower(state$dataset_info$name))
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        new_dataset_dir <- file.path(
          dirname(original_project_dir),
          paste0(dataset_name, "_psychds_", timestamp)
        )

        # Create the new dataset directory
        dir.create(new_dataset_dir, showWarnings = FALSE, recursive = TRUE)

        # Create data directory
        data_dir <- file.path(new_dataset_dir, "data")
        dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)

        # Copy and rename files to data directory
        for (mapping in file_mappings()) {
          if (mapping$new != "") {
            # Construct full source and destination paths
            src_path <- file.path(original_project_dir, mapping$original)
            dest_path <- file.path(data_dir, mapping$new)

            # Copy the file
            file.copy(src_path, dest_path, overwrite = FALSE)
          }
        }

        # Create optional directories at the project root
        optional_dirs <- state$optional_dirs
        standard_dirs <- c("analysis", "materials", "results", "products", "documentation")

        for (dir in standard_dirs) {
          if (optional_dirs[[dir]]) {
            dir.create(file.path(new_dataset_dir, dir), showWarnings = FALSE)
          }
        }

        # Add any custom directories at the project root
        if (!is.null(optional_dirs$custom)) {
          for (custom_dir in optional_dirs$custom) {
            dir.create(file.path(new_dataset_dir, custom_dir), showWarnings = FALSE)
          }
        }

        # Generate dataset_description.json in the project root
        dataset_info <- state$dataset_info

        # Create a comprehensive dataset description
        dataset_description <- list(
          "@context" = "https://schema.org/",
          "@type" = "Dataset",
          "name" = dataset_info$name,
          "description" = dataset_info$description,
          "author" = lapply(dataset_info$authors, function(author) {
            list(
              "@type" = "Person",
              "givenName" = author$first_name,
              "familyName" = author$last_name,
              "@id" = if(!is.null(author$orcid) && author$orcid != "") author$orcid else NULL
            )
          }),
          "variableMeasured" = do.call(c, lapply(names(state$data_dict), function(file_name) {
            file_dict <- state$data_dict[[file_name]]
            lapply(rownames(file_dict), function(var_name) {
              list(
                "@type" = "PropertyValue",
                "name" = var_name,
                "description" = file_dict[var_name, "description"]
              )
            })
          }))
        )

        # Remove NULL values
        dataset_description <- dataset_description[!sapply(dataset_description, is.null)]

        json_path <- file.path(new_dataset_dir, "dataset_description.json")
        jsonlite::write_json(dataset_description, json_path, pretty = TRUE, auto_unbox = TRUE)

        # Create file structure preview
        file_structure_preview <- create_file_structure_html(new_dataset_dir, file_mappings(), optional_dirs)

        # Update state with the new dataset directory
        state$created_dataset_dir <- new_dataset_dir

        # Show summary before proceeding
        showModal(modalDialog(
          title = "Dataset Creation Complete",
          div(
            p("Your Psych-DS dataset has been created successfully:"),
            div(
              style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f8f9fa;",
              file_structure_preview
            ),
            p("A basic version of your dataset has been copied to your Downloads folder.")
          ),
          footer = tagList(
            modalButton("Close"),
            actionButton(session$ns("validate_dataset"), "Validate Dataset", class = "btn-primary")
          ),
          size = "l"  # Using 'l' for large size
        ))
      }

    # Add new event handlers for validate and download buttons
    observeEvent(input$validate_dataset, {
      # Debug logging
      cat("Validate dataset button clicked\n")

      # Get the path to the newly created dataset
      full_dataset_dir <- state$created_dataset_dir
      downloads_dir <- path.expand("~/Downloads")
      destination_dir <- file.path(downloads_dir, basename(full_dataset_dir))

      # Render the dataset preview
      output$dataset_preview <- renderUI({
        div(
          class = "section-box",
          div(class = "section-title", "Dataset Preview"),
          div(
            style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f8f9fa;",
            create_file_structure_html(destination_dir,
                                       lapply(list.files(file.path(destination_dir, "data"), recursive = TRUE, full.names = FALSE),
                                              function(f) list(original = f, new = f)),
                                       state$optional_dirs)
          ),
          # Dataset description preview
          div(
            class = "section-title",
            style = "margin-top: 15px;",
            "Dataset Description"
          ),
          div(
            style = "border: 1px solid #ddd; padding: 10px; background-color: #f8f9fa; white-space: pre-wrap; font-family: monospace;",
            paste(readLines(file.path(destination_dir, "dataset_description.json")), collapse = "\n")
          )
        )
      })

      # Remove any existing modal
      removeModal()

      # Use shiny's session to change the active tab
      # This is a more direct approach to changing tabs
      session$sendCustomMessage("changeTab", list(tabName = "validate"))
    })

    observeEvent(input$download_dataset, {
      removeModal()

      # Get the path to the newly created dataset
      full_dataset_dir <- state$created_dataset_dir

      # Create a destination directory in the user's Downloads folder
      downloads_dir <- path.expand("~/Downloads")
      destination_dir <- file.path(downloads_dir, basename(full_dataset_dir))

      # Copy the entire directory to Downloads
      file.copy(full_dataset_dir, downloads_dir, recursive = TRUE)

      # Switch to the validate dataset tab
      updateTabItems(session, "sidebar", "validate")

      # Automatically load the newly created dataset path in the validate input
      updateTextInput(session, "validate_dir", value = destination_dir)

      # Show a notification
      showNotification(
        paste("Dataset downloaded to:", destination_dir),
        type = "message",
        duration = 5
      )
    })

    # Handle finish button
    observeEvent(input$finish, {
      removeModal()

      # Generate the dataset with standardized filenames
      showModal(modalDialog(
        title = "Success!",
        div(
          icon("check-circle", class = "text-success", style = "font-size: 48px; display: block; text-align: center; margin: 15px 0;"),
          p(style = "text-align: center; font-size: 16px;", "Your Psych-DS dataset has been created successfully."),
          hr(),
          p("The following files and directories have been created:"),
          tags$ul(
            style = "max-height: 200px; overflow-y: auto;",
            tags$li(tags$code(paste0(state$project_dir, "/data/")), style = "margin-bottom: 5px;"),
            tags$li(tags$code(paste0(state$project_dir, "/data/dataset_description.json")), style = "margin-bottom: 5px;"),
            tags$li(tags$code(paste0(state$project_dir, "/data/datapackage.json")), style = "margin-bottom: 5px;"),
            lapply(file_mappings(), function(mapping) {
              if (mapping$new != "") {
                tags$li(tags$code(paste0(state$project_dir, "/data/", mapping$new)), style = "margin-bottom: 5px;")
              } else {
                tags$li(tags$code(paste0(state$project_dir, "/data/", basename(mapping$original))), style = "margin-bottom: 5px;")
              }
            })
          )
        ),
        size = "large",
        easyClose = TRUE,
        footer = tagList(
          actionButton(ns("view_dataset"), "View Dataset", class = "btn btn-primary"),
          modalButton("Close")
        )
      ))
    })

    # Handle view dataset button
    observeEvent(input$view_dataset, {
      removeModal()
      showNotification("Dataset explorer would open here", type = "message")

      # In a real implementation, you would:
      # 1. Switch to the dataset explorer tab
      # 2. Load the newly created dataset
      # But for this prototype, we'll just show a notification
    })

    # Return reactive that provides the file mappings
    return(reactive({ file_mappings() }))
  })
}

check_validator_button <- function(id, session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$check_validator, {
      # Insert JavaScript directly into the page
      insertUI(
        selector = "body",
        where = "beforeEnd",
        ui = tags$script(HTML('
          console.log("Manual validator check requested");
          var validatorStatus = "Validator NOT found";

          if (typeof window.psychDSValidator !== "undefined") {
            validatorStatus = "VALIDATOR FOUND! Methods: " + Object.keys(window.psychDSValidator).join(", ");
            console.log(validatorStatus);
          } else {
            console.error(validatorStatus);
          }

          // Show a visible alert that users can see
          alert(validatorStatus);

          // Also try to communicate back to Shiny
          if (typeof Shiny !== "undefined") {
            Shiny.setInputValue("validator_status", validatorStatus);
          } else {
            console.error("Shiny object not available");
          }
        '))
      )

      # Also display a notification directly from R
      showNotification("Check validator executed - look for browser alert", type = "message")
    })

    # This is the handler if Shiny communication works
    observeEvent(input$validator_status, {
      showNotification(paste("Validator status:", input$validator_status),
                       type = "message",
                       duration = 10)
    })
  })
}

check_validator_available <- function() {
  shiny::insertUI(
    selector = "head",
    where = "beforeEnd",
    ui = tags$script(HTML("
      console.log('Checking validator availability...');
      if (typeof window.psychDSValidator !== 'undefined') {
        console.log('Validator is available');
        Shiny.setInputValue('validator_available', true);
      } else {
        console.error('Validator is NOT available');
        Shiny.setInputValue('validator_available', false);
      }
    "))
  )
}

validateServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Track validation status
    validation_status <- reactiveValues(
      is_validating = FALSE,
      is_complete = FALSE,
      is_valid = FALSE,
      step_statuses = list() 
    )
    
    # Define steps for the UI rendering
    validation_steps <- list(
      list(
        key = "start",
        message = list(
          imperative = "Start validation",
          pastTense = "Validation started"
        ),
        subSteps = list()
      ),
      list(
        key = "check-folder",
        message = list(
          imperative = "Find project folder",
          pastTense = "Project folder found"
        ),
        subSteps = list(
          list(
            key = "build-tree",
            message = list(
              imperative = "Crawl project folder and construct file tree",
              pastTense = "Project folder crawled and file tree constructed"
            )
          )
        )
      ),
      list(
        key = "find-metadata",
        message = list(
          imperative = "Find metadata file",
          pastTense = 'Metadata file "dataset_description.json" found in the root folder'
        ),
        subSteps = list()
      ),
      list(
        key = "find-data-dir",
        message = list(
          imperative = 'Find "data" subfolder',
          pastTense = '"data" subfolder found in the root folder'
        ),
        subSteps = list()
      ),
      list(
        key = "parse-metadata",
        message = list(
          imperative = 'Parse "dataset_description.json" metadata file',
          pastTense = 'Successfully parsed "dataset_description.json" metadata file'
        ),
        subSteps = list(
          list(
            key = "metadata-utf8",
            message = list(
              imperative = "Check metadata file for utf-8 encoding",
              pastTense = "Metadata file is utf-8 encoded"
            )
          ),
          list(
            key = "metadata-json",
            message = list(
              imperative = "Parse metadata file as JSON",
              pastTense = "Metadata file parsed successfully"
            )
          ),
          list(
            key = "metadata-jsonld",
            message = list(
              imperative = "Validate metadata file as JSON-LD",
              pastTense = "Metadata file is valid JSON-LD"
            )
          ),
          list(
            key = "metadata-fields",
            message = list(
              imperative = 'Check metadata file for required "name", "description", and "variableMeasured" fields',
              pastTense = 'Metadata file contains required "name", "description", and "variableMeasured" fields.'
            )
          ),
          list(
            key = "metadata-type",
            message = list(
              imperative = 'Check metadata file for field "@type" with value "Dataset"',
              pastTense = 'Metadata file has "@type" field with value "Dataset"'
            )
          )
        )
      ),
      list(
        key = "check-for-csv",
        message = list(
          imperative = 'Check for CSV data files in "data" subfolder',
          pastTense = 'CSV data files found in "data" subfolder'
        ),
        subSteps = list()
      ),
      list(
        key = "validate-csvs",
        message = list(
          imperative = 'Check that all CSV data files are valid',
          pastTense = 'All CSV data files are valid'
        ),
        subSteps = list(
          list(
            key = "csv-keywords",
            message = list(
              imperative = 'Check filename for keyword formatting ',
              pastTense = 'Filename uses valid keyword formatting'
            )
          ),
          list(
            key = "csv-parse",
            message = list(
              imperative = 'Parse data file as CSV',
              pastTense = 'Data file successfully parsed as CSV'
            )
          ),
          list(
            key = "csv-header",
            message = list(
              imperative = 'Check for header line',
              pastTense = 'Header line found'
            )
          ),
          list(
            key = "csv-header-repeat",
            message = list(
              imperative = 'Check for redundant column names',
              pastTense = 'No redundant column names found'
            )
          ),
          list(
            key = "csv-nomismatch",
            message = list(
              imperative = 'Check all lines for equal number of cells',
              pastTense = 'All lines have equal number of cells'
            )
          ),
          list(
            key = "csv-rowid",
            message = list(
              imperative = 'Check for any row_id columns with non-unique values',
              pastTense = 'All row_id columns have unique values'
            )
          )
        )
      ),
      list(
        key = "check-variableMeasured",
        message = list(
          imperative = 'Confirm that all column headers in CSV data files are found in "variableMeasured" metadata field',
          pastTense = 'All column headers in CSV data files were found in "variableMeasured" metadata field'
        ),
        subSteps = list()
      )
    )
    
    # Store steps for UI reference
    validation_status$steps <- validation_steps
    
    #' Custom function to print validation status in a readable format
    #' @param validation_obj The validation status object from JavaScript
    print_validation_status <- function(validation_obj) {
      cat("\n=== VALIDATION STEP UPDATE ===\n")
      
      if (is.null(validation_obj)) {
        cat("Validation object is NULL\n")
        return()
      }
      
      # Print the raw structure first
      cat("Raw object structure:\n")
      cat("- Class:", class(validation_obj), "\n")
      cat("- Length:", length(validation_obj), "\n")
      cat("- Names:", paste(names(validation_obj), collapse = ", "), "\n\n")
      
      # Check if stepStatus exists
      if ("stepStatus" %in% names(validation_obj)) {
        step_status <- validation_obj$stepStatus
        cat("stepStatus found with", length(step_status), "entries\n")
        
        # Process each step
        for (i in seq_along(step_status)) {
          step_entry <- step_status[[i]]
          
          cat("\n--- Step", i, "---\n")
          cat("Entry class:", class(step_entry), "\n")
          cat("Entry length:", length(step_entry), "\n")
          
          if (length(step_entry) >= 2) {
            # Extract step key and status
            step_key <- step_entry[[1]]
            step_info <- step_entry[[2]]
            
            cat("Step Key:", step_key, "\n")
            cat("Step Info Class:", class(step_info), "\n")
            
            # Print step info details
            if (is.list(step_info)) {
              cat("Step Info Contents:\n")
              for (prop_name in names(step_info)) {
                prop_value <- step_info[[prop_name]]
                cat("  ", prop_name, ":", prop_value, "(", class(prop_value), ")\n")
              }
              
              # Check for issue details
              if ("issue" %in% names(step_info) && !is.null(step_info$issue)) {
                cat("  Issue Details:\n")
                issue <- step_info$issue
                for (issue_prop in names(issue)) {
                  cat("    ", issue_prop, ":", issue[[issue_prop]], "\n")
                }
              }
            } else {
              cat("Step Info (non-list):", step_info, "\n")
            }
            
            # Create readable status summary
            if (is.list(step_info) && "complete" %in% names(step_info)) {
              complete <- step_info$complete
              success <- if ("success" %in% names(step_info)) step_info$success else FALSE
              
              status_icon <- if (complete) {
                if (success) "[PASS]" else "[FAIL]"
              } else "[PENDING]"
              
              cat("Summary:", status_icon, step_key, "\n")
              
              # Show any error details
              if (!is.null(step_info$issue)) {
                cat("  ERROR:", step_info$issue$reason %||% "Unknown error", "\n")
              }
            }
          } else {
            cat("Invalid step entry (length < 2)\n")
          }
        }
      } else {
        cat("No stepStatus found in validation object\n")
        cat("Available properties:", paste(names(validation_obj), collapse = ", "), "\n")
      }
      
      cat("===============================\n\n")
    }

    # Replace your existing observeEvent with this:
    observeEvent(input$validation_step_status, {
      print_validation_status(input$validation_step_status)
      
      # Continue with existing logic for updating validation_status
      if (!is.null(input$validation_step_status) && 
          !is.null(input$validation_step_status$stepStatus)) {
        
        step_updates <- input$validation_step_status$stepStatus
        
        for (i in seq_along(step_updates)) {
          step_entry <- step_updates[[i]]
          
          if (length(step_entry) >= 2) {
            step_key <- step_entry[[1]]
            step_status <- step_entry[[2]]
            
            # Update the step status in our reactive
            validation_status$step_statuses[[step_key]] <- list(
              complete = step_status$complete,
              success = step_status$success,
              issue = step_status$issue
            )
          }
        }
        
        # Start validation mode if first update
        if (!validation_status$is_validating) {
          validation_status$is_validating <- TRUE
        }
      }
    }, ignoreNULL = TRUE)
    
    # Handle validation complete events
    observeEvent(input$validation_complete, {
      message("Validation complete event received")
      validation_status$is_complete <- TRUE
      validation_status$is_valid <- TRUE
    })
    
    # Handle validation halted events
    observeEvent(input$validation_halted, {
      message("Validation halted event received")
      validation_status$is_complete <- TRUE
      validation_status$is_valid <- FALSE
    })
    
    # Handle validation results
    observeEvent(input$validation_results, {
      if (!is.null(input$validation_results)) {
        message("Validation results received")
        validation_status$is_complete <- TRUE
        validation_status$is_valid <- input$validation_results$valid
        validation_status$is_validating <- FALSE
        state$validation_results <- input$validation_results
        
        # Store the validated directory path for later use
        if (!is.null(input$validate_dir) && input$validate_dir != "") {
          state$validated_dataset_dir <- input$validate_dir
        }
      }
    })

    # Add this to your validateServer function in server_modules.R
# Replace the existing observeEvent for validation_step_status with this:

observeEvent(input$validation_step_status, {
  cat("*** STEP STATUS EVENT RECEIVED ***\n")
  
  if (!is.null(input$validation_step_status)) {
    # Convert the validation data to pretty JSON for display
    json_output <- tryCatch({
      jsonlite::toJSON(input$validation_step_status, pretty = TRUE, auto_unbox = TRUE)
    }, error = function(e) {
      paste("Error converting to JSON:", e$message)
    })
    
    # Show the validation data in a modal popup
    showModal(modalDialog(
      title = "Validation Step Status Debug",
      div(
        h4("Raw Validation Data:"),
        tags$pre(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; max-height: 400px; overflow-y: auto; font-family: monospace; font-size: 12px;",
          json_output
        ),
        hr(),
        h4("Data Structure Info:"),
        tags$ul(
          tags$li(paste("Class:", paste(class(input$validation_step_status), collapse = ", "))),
          tags$li(paste("Length:", length(input$validation_step_status))),
          tags$li(paste("Names:", paste(names(input$validation_step_status), collapse = ", "))),
          if ("stepStatus" %in% names(input$validation_step_status)) {
            tags$li(paste("stepStatus length:", length(input$validation_step_status$stepStatus)))
          }
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    # Also call the original processing logic
    if (!is.null(input$validation_step_status$stepStatus)) {
      step_updates <- input$validation_step_status$stepStatus
      
      for (i in seq_along(step_updates)) {
        step_entry <- step_updates[[i]]
        
        if (length(step_entry) >= 2) {
          step_key <- step_entry[[1]]
          step_status <- step_entry[[2]]
          
          # Update the step status in our reactive
          validation_status$step_statuses[[step_key]] <- list(
            complete = step_status$complete,
            success = step_status$success,
            issue = step_status$issue
          )
        }
      }
      
      # Start validation mode if first update
      if (!validation_status$is_validating) {
        validation_status$is_validating <- TRUE
      }
    }
  }
}, ignoreNULL = TRUE)
    
    # Render the validation UI with checklist
    output$validation_results_ui <- renderUI({
      # If validation hasn't started or is in progress, show appropriate message
      if (!validation_status$is_validating && is.null(state$validation_results)) {
        return(div(
          style = "text-align: center; padding: 20px;",
          p("Select a dataset directory and click 'Validate' to check compliance with Psych-DS standard.")
        ))
      }
      
      # Debug text to show current status
      status_text <- paste(
        "Validating:", validation_status$is_validating,
        "Complete:", validation_status$is_complete,
        "Valid:", validation_status$is_valid,
        "Steps:", length(validation_status$step_statuses)
      )
      
      # If validation is in progress or results are available, show the checklist
      checklist <- div(
        h3("Validation Progress", 
           if (validation_status$is_complete) {
             span(
               style = paste0("color: ", ifelse(validation_status$is_valid, "#4caf50", "#f44336"), "; margin-left: 10px;"),
               ifelse(validation_status$is_valid, "✓ Dataset is valid", "✗ Dataset has issues")
             )
           }
        ),
        p(style = "color: #999; font-style: italic;", status_text),
        
        # Main steps
        div(
          class = "validation-checklist",
          style = "max-height: 500px; overflow-y: auto;",
          
          # Check if steps are defined
          if (length(validation_status$steps) > 0) {
            lapply(validation_status$steps, function(step) {
              # Get status for this step
              status <- validation_status$step_statuses[[step$key]]
              
              # Default status if not found
              if (is.null(status)) {
                status <- list(complete = FALSE, success = FALSE)
              }
              
              # Determine message and icon
              message <- if (status$complete) step$message$pastTense else step$message$imperative
              icon <- if (status$complete) {
                if (status$success) "✓" else "✗"
              } else "⋯"
              icon_color <- if (status$complete) {
                if (status$success) "#4caf50" else "#f44336"
              } else "#ffc107"
              
              div(
                class = "step-item",
                style = "margin-bottom: 15px; padding: 10px; border-radius: 5px; background-color: #f9f9f9; border: 1px solid #ddd;",
                
                div(
                  class = "step-header",
                  style = "font-weight: bold; display: flex; align-items: center;",
                  span(icon, style = paste0("color: ", icon_color, "; margin-right: 10px; font-size: 18px;")),
                  message
                ),
                
                # Display issue if there is one
                if (!is.null(status$issue)) {
                  div(
                    class = "step-issue",
                    style = "margin-top: 10px; color: #f44336; padding-left: 20px;",
                    p(strong("Issue:"), status$issue$reason)
                  )
                },
                
                # Render substeps if any
                if (length(step$subSteps) > 0) {
                  div(
                    class = "substeps",
                    style = "margin-top: 10px; margin-left: 20px; padding-left: 10px; border-left: 2px solid #ddd;",
                    
                    lapply(step$subSteps, function(subStep) {
                      # Get substep status
                      subStatus <- validation_status$step_statuses[[subStep$key]]
                      
                      # Default status if not found
                      if (is.null(subStatus)) {
                        subStatus <- list(complete = FALSE, success = FALSE)
                      }
                      
                      # Determine message and icon
                      subMessage <- if (subStatus$complete) subStep$message$pastTense else subStep$message$imperative
                      subIcon <- if (subStatus$complete) {
                        if (subStatus$success) "✓" else "✗"
                      } else "⋯"
                      subIconColor <- if (subStatus$complete) {
                        if (subStatus$success) "#4caf50" else "#f44336"
                      } else "#ffc107"
                      
                      div(
                        class = "substep-item",
                        style = "margin-bottom: 8px; display: flex; align-items: flex-start;",
                        
                        span(subIcon, style = paste0("color: ", subIconColor, "; margin-right: 10px;")),
                        div(
                          style = "flex: 1;",
                          p(style = "margin: 0;", subMessage),
                          
                          # Display issue if there is one
                          if (!is.null(subStatus$issue)) {
                            div(
                              class = "substep-issue",
                              style = "margin-top: 5px; color: #f44336; font-size: 0.9em;",
                              p(strong("Issue:"), subStatus$issue$reason)
                            )
                          }
                        )
                      )
                    })
                  )
                }
              )
            })
          } else {
            div("No validation steps defined")
          }
        )
      )
      
      # If validation is complete, also show the results summary
      if (validation_status$is_complete && !is.null(state$validation_results)) {
        result <- state$validation_results
        
        # Full results with checklist and summary
        return(tagList(
          # Checklist
          div(
            class = "section-box",
            style = "margin-bottom: 20px;",
            div(class = "section-title", "Validation Process"),
            checklist
          ),
          
          # Summary
          div(
            class = "section-box",
            div(class = "section-title", "Results Summary"),
            div(
              style = paste0("padding: 15px; border-radius: 5px; background-color: ", 
                            ifelse(result$valid, "#e8f5e9", "#ffebee"), ";"),
              h3(
                style = paste0("color: ", ifelse(result$valid, "#4caf50", "#f44336"), ";"),
                ifelse(result$valid, "✓ Dataset is valid", "✗ Dataset has validation errors")
              ),
              
              # Summary details
              div(
                style = "margin-top: 15px;",
                h4("Details:"),
                tags$ul(
                  tags$li(paste0("Total files scanned: ", result$summary$totalFiles)),
                  if (!is.null(result$summary$dataTypes) && length(result$summary$dataTypes) > 0) {
                    tags$li(paste0("Data types found: ", paste(result$summary$dataTypes, collapse = ", ")))
                  },
                  tags$li(
                    span(style = paste0("color: ", ifelse(result$valid, "#4caf50", "#f44336"), "; font-weight: bold;"), 
                         ifelse(result$valid, 
                               "This dataset appears to be Psych-DS compatible", 
                               "This dataset does not appear to be Psych-DS compatible"))
                  )
                )
              ),
              
              # Only show errors section if there are any
              if (!result$valid && !is.null(result$issues) && !is.null(result$issues$errors) && length(result$issues$errors) > 0) {
                div(
                  style = "margin-top: 15px;",
                  h4("Errors:"),
                  tags$ul(
                    lapply(result$issues$errors, function(error) {
                      tags$li(
                        p(style = "font-weight: bold;", error$key),
                        p(error$reason)
                      )
                    })
                  )
                )
              }
            )
          )
        ))
      } else {
        # Just return the checklist during validation
        return(div(
          class = "section-box",
          div(class = "section-title", "Validation Progress"),
          checklist
        ))
      }
    })
    
    # Handle the validate button click
    observeEvent(input$validate_btn, {
      req(input$validate_dir)
      
      if (dir.exists(input$validate_dir)) {
        # Reset validation state
        validation_status$is_validating <- TRUE
        validation_status$is_complete <- FALSE
        validation_status$is_valid <- FALSE
        validation_status$step_statuses <- list()
        state$validation_results <- NULL
        
        # Show a loading notification
        showNotification(
          "Validating dataset... This may take a moment", 
          id = "validate_notif", 
          type = "message",
          duration = NULL
        )
        
        # Try to build the file tree and run validation
        tryCatch({
          # Build the file tree
          fileTree <- buildFileTree(input$validate_dir)
          
          # Send the file tree to the JavaScript validator
          session$sendCustomMessage("run_validation", fileTree)
        }, error = function(e) {
          removeNotification(id = "validate_notif")
          showNotification(paste("Error:", e$message), type = "error")
          validation_status$is_validating <- FALSE
        })
      } else {
        showNotification("Directory does not exist", type = "error")
      }
    })
    
    # Similar implementation for test validation button
    observeEvent(input$test_validation, {
      # Reset validation state
      validation_status$is_validating <- TRUE
      validation_status$is_complete <- FALSE
      validation_status$is_valid <- FALSE
      validation_status$step_statuses <- list()
      state$validation_results <- NULL
      
      # Show a loading notification
      showNotification(
        "Running test validation... This may take a moment", 
        id = "validate_notif", 
        type = "message",
        duration = NULL
      )
      
      # Create a test file tree and run validation
      tryCatch({
        # Create test file tree
        testTree <- createTestFileTree()
        
        # Send the test file tree to the JavaScript validator
        session$sendCustomMessage("run_validation", testTree)
      }, error = function(e) {
        removeNotification(id = "validate_notif")
        showNotification(paste("Error:", e$message), type = "error")
        validation_status$is_validating <- FALSE
      })
    })
    

    # Add debugging for all validation-related events
    observeEvent(input$validation_complete, {
      cat("VALIDATION COMPLETE EVENT - Value:", input$validation_complete, "\n")
    }, ignoreNULL = TRUE)

    observeEvent(input$validation_halted, {
      cat("VALIDATION HALTED EVENT - Value:", input$validation_halted, "\n")
    }, ignoreNULL = TRUE)

    # Debug JavaScript communication
    observeEvent(input$validate_btn, {
      cat("VALIDATE BUTTON CLICKED - Preparing validation\n")
      cat("Directory:", input$validate_dir, "\n")
      
      # Add JavaScript debugging
      session$sendCustomMessage("debug_validator", list(
        message = "Starting validation debug mode",
        timestamp = Sys.time()
      ))
    })

    # Add a test message handler to verify communication
    observeEvent(input$test_js_communication, {
      cat("Test JS communication received:", input$test_js_communication, "\n")
    }, ignoreNULL = TRUE)
  })
}

#' Data Dictionary Server Module
#'
#' Server logic for the data dictionary editor
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
dataDictionaryServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for dictionary state
    dictionary_state <- reactiveValues(
      dataset_path = NULL,
      variables = list(),
      current_variable = NULL,
      variable_data = list(),
      is_modified = FALSE,
      editing_cat_index = NULL  # Add this for tracking which categorical value is being edited
    )

    # Initialize categorical values storage for current variable
    categorical_values <- reactiveVal(list())
    
    # Track if dataset is loaded
    output$dataset_loaded <- reactive({
      !is.null(dictionary_state$dataset_path)
    })
    outputOptions(output, "dataset_loaded", suspendWhenHidden = FALSE)
    
    # Track if variable is selected  
    output$variable_selected <- reactive({
      !is.null(dictionary_state$current_variable)
    })
    outputOptions(output, "variable_selected", suspendWhenHidden = FALSE)
    
    # Display dataset info
    output$dataset_info <- renderUI({
      if (!is.null(dictionary_state$dataset_path)) {
        dataset_name <- basename(dictionary_state$dataset_path)
        variable_count <- length(dictionary_state$variables)
        
        div(
          icon("check-circle", style = "color: #28a745; margin-right: 8px;"),
          strong("Dataset loaded: "), dataset_name,
          span(style = "margin-left: 15px; color: #6c757d;",
               paste(variable_count, "variables detected"))
        )
      }
    })
    
    # Set up directory selection for modal
    volumes <- c(Home = "~")
    if (.Platform$OS.type == "windows") {
      volumes <- c(volumes, getVolumes()())
    }
    
    shinyDirChoose(
      input,
      "dataset_dir_select", 
      roots = volumes,
      session = session,
      restrictions = system.file(package = "base")
    )

    observeEvent(input$dataset_dir_select, {
      if (!is.null(input$dataset_dir_select)) {
        selected_dir <- parseDirPath(volumes, input$dataset_dir_select)
        updateTextInput(session, "dataset_dir", value = selected_dir)
      }
    })
    
    # Load dataset when button clicked
    observeEvent(input$load_dataset_btn, {
      dataset_path <- input$dataset_dir
      
      if (dataset_path == "" || !dir.exists(dataset_path)) {
        showNotification("Please select a valid dataset directory", type = "error")
        return()
      }
      
      # Check if it's a valid Psych-DS dataset
      if (!file.exists(file.path(dataset_path, "dataset_description.json"))) {
        showNotification("Selected directory does not contain dataset_description.json", type = "error")
        return()
      }
      
      if (!dir.exists(file.path(dataset_path, "data"))) {
        showNotification("Selected directory does not contain a 'data' folder", type = "error") 
        return()
      }
      
      # Load variables from CSV files
      tryCatch({
        variables <- extractVariablesFromDataset(dataset_path)
        
        dictionary_state$dataset_path <- dataset_path
        dictionary_state$variables <- variables
        dictionary_state$current_variable <- NULL
        
        removeModal()
        showNotification("Dataset loaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading dataset:", e$message), type = "error")
      })
    })

    #' Analyze a variable from a CSV file
    #' 
    #' @param csv_file Path to the CSV file
    #' @param var_name Name of the variable to analyze
    #' @return List with variable analysis results
    analyzeVariable <- function(csv_file, var_name) {
      result <- list(
        type = "string",
        unit = "",
        min_value = "",
        max_value = "",
        value_reference = "",
        categorical_values = list()  # Add this
      )
      
      tryCatch({
        # Read a sample of the data
        data <- read.csv(csv_file, nrows = 1000, stringsAsFactors = FALSE)
        
        if (!var_name %in% names(data)) {
          return(result)
        }
        
        col_data <- data[[var_name]]
        
        # Remove NA values for analysis
        col_data_clean <- col_data[!is.na(col_data)]
        
        if (length(col_data_clean) == 0) {
          return(result)
        }
        
        # Detect type
        if (is.numeric(col_data)) {
          if (all(col_data_clean == as.integer(col_data_clean))) {
            result$type <- "integer"
            result$min_value <- as.character(min(col_data_clean))
            result$max_value <- as.character(max(col_data_clean))
          } else {
            result$type <- "number"
            result$min_value <- as.character(round(min(col_data_clean), 4))
            result$max_value <- as.character(round(max(col_data_clean), 4))
          }
          
          # Detect common units based on variable name
          var_lower <- tolower(var_name)
          if (grepl("time|rt|latency", var_lower)) {
            result$unit <- "milliseconds"
          } else if (grepl("age", var_lower)) {
            result$unit <- "years"
          } else if (grepl("score", var_lower)) {
            result$unit <- "points"
          }
          
        } else if (is.logical(col_data)) {
          result$type <- "boolean"
          
        } else {
          # String type - check if categorical
          unique_vals <- unique(col_data_clean)
          
          # Check if it looks like a date first
          date_patterns <- c(
            "^\\d{4}-\\d{2}-\\d{2}$",  # YYYY-MM-DD
            "^\\d{2}/\\d{2}/\\d{4}$",  # MM/DD/YYYY
            "^\\d{2}-\\d{2}-\\d{4}$"   # DD-MM-YYYY
          )
          
          sample_values <- head(col_data_clean, 10)
          if (any(sapply(date_patterns, function(p) any(grepl(p, sample_values))))) {
            result$type <- "date"
          } else if (length(unique_vals) <= 20) {
            # This looks like a categorical variable
            result$type <- "categorical"
            result$value_reference <- paste(sort(unique_vals), collapse = "\n")
            
            # Create categorical values list with value, label, and description
            result$categorical_values <- lapply(sort(unique_vals), function(val) {
              list(
                value = as.character(val),
                label = as.character(val),  # Default label is same as value
                description = ""  # Empty description for user to fill
              )
            })
          } else {
            result$type <- "string"
          }
        }
        
      }, error = function(e) {
        warning(paste("Error analyzing variable", var_name, "in", csv_file, ":", e$message))
      })
      
      return(result)
    }
    
    # Function to auto-populate categorical values
    autoPopulateCategoricalValues <- function(var_name) {
      if (is.null(dictionary_state$dataset_path) || is.null(var_name)) return()
      
      var_info <- dictionary_state$variables[[var_name]]
      all_values <- character(0)
      
      # Read values from all files containing this variable
      withProgress(message = "Detecting categorical values...", value = 0, {
        data_dir <- file.path(dictionary_state$dataset_path, "data")
        csv_files <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
        
        for (i in seq_along(csv_files)) {
          setProgress(i / length(csv_files))
          tryCatch({
            data <- read.csv(csv_files[i], stringsAsFactors = FALSE, nrows = 1000)
            if (var_name %in% names(data)) {
              col_values <- unique(data[[var_name]][!is.na(data[[var_name]])])
              all_values <- c(all_values, as.character(col_values))
            }
          }, error = function(e) {})
        }
      })
      
      unique_values <- unique(all_values)
      
      if (length(unique_values) > 0 && length(unique_values) <= 50) {
        # Create categorical values with default labels and descriptions
        cat_values <- lapply(sort(unique_values), function(val) {
          list(
            value = val,
            label = val,  # Default label same as value
            description = ""  # Empty description for user to fill
          )
        })
        categorical_values(cat_values)
        showNotification(paste("Auto-populated", length(unique_values), "categorical values"), type = "message")
      } else if (length(unique_values) > 50) {
        showNotification("Too many unique values (>50) to auto-populate. Add manually.", type = "warning")
      }
    }
    
    # Extract variables from dataset with enhanced analysis
    extractVariablesFromDataset <- function(dataset_path) {
      data_dir <- file.path(dataset_path, "data")
      csv_files <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      
      if (length(csv_files) == 0) {
        stop("No CSV files found in data directory")
      }
      
      all_variables <- list()
      
      withProgress(message = "Analyzing dataset variables...", value = 0, {
        for (i in seq_along(csv_files)) {
          csv_file <- csv_files[i]
          rel_path <- gsub(paste0("^", dataset_path, "/"), "", csv_file)
          
          setProgress(i / length(csv_files), detail = paste("Processing", basename(csv_file)))
          
          tryCatch({
            # Read just the header first
            header <- names(read.csv(csv_file, nrows = 1))
            
            for (var_name in header) {
              if (var_name %in% names(all_variables)) {
                # Add this file to existing variable
                all_variables[[var_name]]$files <- c(all_variables[[var_name]]$files, rel_path)
              } else {
                # Create new variable entry with enhanced analysis
                var_analysis <- analyzeVariable(csv_file, var_name)
                
                all_variables[[var_name]] <- list(
                  name = var_name,
                  files = rel_path,
                  description = generateDescription(var_name, var_analysis),
                  type = var_analysis$type,
                  unit = var_analysis$unit,
                  min_value = var_analysis$min_value,
                  max_value = var_analysis$max_value,
                  value_reference = var_analysis$value_reference,
                  default_value = "",
                  source = "",
                  notes = "",
                  categorical_values = var_analysis$categorical_values,  # Include the detected values
                  required = FALSE,
                  unique = FALSE,
                  pattern = ""
                )
              }
            }
          }, error = function(e) {
            warning(paste("Could not read file:", csv_file, "-", e$message))
          })
        }
      })
      
      return(all_variables)
    }
    
    # Generate a basic description based on variable name and analysis
    generateDescription <- function(var_name, var_analysis) {
      var_lower <- tolower(var_name)
      
      # Common psychology/research variable descriptions
      if (grepl("^(participant|subject|sub)_?(id|ID|Id)", var_name)) {
        return("Unique identifier for each participant in the study")
      }
      
      if (grepl("age", var_lower)) {
        return("Age of the participant")
      }
      
      if (grepl("gender|sex", var_lower)) {
        return("Gender or biological sex of the participant")
      }
      
      if (grepl("condition|group", var_lower)) {
        return("Experimental condition or group assignment")
      }
      
      if (grepl("response_?time|rt|latency", var_lower)) {
        return("Response time or reaction time measurement")
      }
      
      if (grepl("accuracy|correct|acc", var_lower)) {
        return("Accuracy or correctness of response")
      }
      
      if (grepl("trial", var_lower)) {
        return("Trial number or trial identifier")
      }
      
      if (grepl("block", var_lower)) {
        return("Block number in the experimental design")
      }
      
      if (grepl("session", var_lower)) {
        return("Session number or session identifier")
      }
      
      if (grepl("stimulus|stim", var_lower)) {
        return("Stimulus identifier or stimulus information")
      }
      
      if (grepl("response|resp", var_lower)) {
        return("Participant response or response value")
      }
      
      if (grepl("score|rating", var_lower)) {
        return("Score or rating value")
      }
      
      if (grepl("timestamp|time", var_lower)) {
        return("Timestamp or time measurement")
      }
      
      # Generate description based on type
      type_descriptions <- switch(var_analysis$type,
        "integer" = "Numeric variable (whole numbers)",
        "number" = "Numeric variable (decimal numbers)",
        "boolean" = "Boolean variable (true/false)",
        "date" = "Date variable",
        "categorical" = "Categorical variable",
        "string" = "Text variable"
      )
      
      return(paste("Variable:", var_name, "-", type_descriptions))
    }
    
    # Render variables list
    output$variables_list <- renderUI({
      variables <- dictionary_state$variables
      search_term <- input$variable_search
      
      if (length(variables) == 0) {
        return(div(
          style = "text-align: center; padding: 50px 20px; color: #6c757d;",
          p("No variables found. Load a dataset to begin.")
        ))
      }
      
      # Filter variables based on search
      if (!is.null(search_term) && search_term != "") {
        variables <- variables[grepl(search_term, names(variables), ignore.case = TRUE)]
      }
      
      if (length(variables) == 0) {
        return(div(
          style = "text-align: center; padding: 30px 20px; color: #6c757d;",
          p("No variables match your search.")
        ))
      }
      
      # Create variable list items
      variable_items <- lapply(names(variables), function(var_name) {
        var_info <- variables[[var_name]]
        file_count <- length(var_info$files)
        is_selected <- identical(dictionary_state$current_variable, var_name)
        
        div(
          class = if (is_selected) "variable-item selected" else "variable-item",
          style = paste0(
            "padding: 12px 15px; cursor: pointer; border-bottom: 1px solid #f0f0f0; ",
            if (is_selected) "background-color: #3498db; color: white;" else "background-color: white; color: #333;",
            if (match(var_name, names(variables)) %% 2 == 0 && !is_selected) " background-color: #f8f9fa;" else ""
          ),
          onclick = paste0("Shiny.setInputValue('", session$ns("select_variable"), "', '", var_name, "', {priority: 'event'});"),
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span(var_name, style = "font-weight: 500;"),
            span(
              paste(file_count, if (file_count == 1) "file" else "files"),
              style = paste0("font-size: 12px; ", if (is_selected) "color: rgba(255,255,255,0.8);" else "color: #6c757d;")
            )
          )
        )
      })
      
      do.call(tagList, variable_items)
    })
    
    # Update the variable selection handler to include new fields:
    observeEvent(input$select_variable, {
      var_name <- input$select_variable
      
      if (!is.null(var_name) && var_name %in% names(dictionary_state$variables)) {
        dictionary_state$current_variable <- var_name
        
        # Load variable data into form
        var_info <- dictionary_state$variables[[var_name]]
        
        updateTextAreaInput(session, "var_description", value = var_info$description %||% "")
        updateSelectInput(session, "var_type", selected = var_info$type %||% "string")
        updateTextInput(session, "var_unit", value = var_info$unit %||% "")
        updateTextInput(session, "var_min", value = var_info$min_value %||% "")
        updateTextInput(session, "var_max", value = var_info$max_value %||% "")
        updateTextAreaInput(session, "var_value_reference", value = var_info$value_reference %||% "")
        updateTextInput(session, "var_default", value = var_info$default_value %||% "")
        
        # Update new fields
        updateTextInput(session, "var_source", value = var_info$source %||% "")
        updateTextAreaInput(session, "var_notes", value = var_info$notes %||% "")
        updateCheckboxInput(session, "var_required", value = var_info$required %||% FALSE)
        updateCheckboxInput(session, "var_unique", value = var_info$unique %||% FALSE)
        updateTextInput(session, "var_pattern", value = var_info$pattern %||% "")

        # Set categorical values
        if (!is.null(var_info$categorical_values) && length(var_info$categorical_values) > 0) {
          categorical_values(var_info$categorical_values)
        } else if (var_info$type == "categorical" && length(var_info$categorical_values) == 0) {
          # Auto-populate from data if switching to categorical
          autoPopulateCategoricalValues(var_name)
        } else {
          categorical_values(list())
        }
      }
    })
    
    # Add observer for when type changes to categorical
    observeEvent(input$var_type, {
      if (!is.null(input$var_type) && input$var_type == "categorical") {
        # If switching to categorical and no values exist, auto-populate
        if (length(categorical_values()) == 0 && !is.null(dictionary_state$current_variable)) {
          autoPopulateCategoricalValues(dictionary_state$current_variable)
        }
      }
    })
    
    # Render variable name header
    output$variable_name_header <- renderUI({
      if (!is.null(dictionary_state$current_variable)) {
        h3(
          dictionary_state$current_variable,
          style = "color: #333; margin: 0; font-weight: bold;"
        )
      }
    })
    
    output$file_badges_content <- renderUI({
      if (!is.null(dictionary_state$current_variable)) {
        var_info <- dictionary_state$variables[[dictionary_state$current_variable]]
        
        tagList(
          lapply(var_info$files, function(file_path) {
            span(
              basename(file_path),
              class = "badge",
              title = file_path,
              style = "background-color: #3498db; color: white; padding: 4px 8px; border-radius: 12px; font-size: 11px; cursor: help; margin: 2px;"
            )
          }),
          if (length(var_info$files) > 10) {
            div(
              style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid #ddd; font-size: 12px; color: #666; text-align: center;",
              paste("Total:", length(var_info$files), "files")
            )
          }
        )
      }
    })

    # Update the categorical values table renderer:
    output$categorical_values_table <- renderUI({
      values <- categorical_values()
      
      if (length(values) == 0) {
        return(div(
          style = "padding: 20px; text-align: center; color: #6c757d;",
          "No categorical values defined. They will be auto-populated when you select 'Categorical' as the type."
        ))
      }
      
      rows <- lapply(seq_along(values), function(i) {
        value_info <- values[[i]]
        div(
          style = "display: flex; padding: 8px; border-bottom: 1px solid #ced4da; align-items: center;",
          div(style = "flex: 2; padding-right: 10px; font-weight: 500;", value_info$value),
          div(style = "flex: 2; padding-right: 10px;", value_info$label %||% ""),
          div(style = "flex: 3; padding-right: 10px;", value_info$description %||% ""),
          div(
            style = "flex: 0; width: 80px; display: flex; gap: 5px;",
            actionButton(
              session$ns(paste0("edit_cat_", i)),
              label = NULL,
              icon = icon("edit"),
              class = "btn btn-sm btn-info",
              style = "padding: 2px 6px;"
            ),
            actionButton(
              session$ns(paste0("remove_cat_", i)),
              label = NULL,
              icon = icon("trash"),
              class = "btn btn-sm btn-danger",
              style = "padding: 2px 6px;"
            )
          )
        )
      })
      
      do.call(tagList, rows)
    })
    
    # Handle editing categorical values
    observe({
      values <- categorical_values()
      
      for (i in seq_along(values)) {
        local({
          local_i <- i
          
          # Edit button handler
          observeEvent(input[[paste0("edit_cat_", local_i)]], {
            current_values <- categorical_values()
            if (local_i <= length(current_values)) {
              value_info <- current_values[[local_i]]
              
              showModal(modalDialog(
                title = "Edit Categorical Value",
                div(
                  textInput(
                    session$ns("edit_cat_value"),
                    "Value",
                    value = value_info$value,
                    placeholder = "The actual value in the data"
                  ),
                  textInput(
                    session$ns("edit_cat_label"),
                    "Label",
                    value = value_info$label %||% "",
                    placeholder = "Human-readable label for this value"
                  ),
                  textInput(
                    session$ns("edit_cat_description"),
                    "Description",
                    value = value_info$description %||% "",
                    placeholder = "Description of what this value means"
                  )
                ),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(session$ns("save_edit_cat"), "Save", class = "btn-primary")
                ),
                easyClose = TRUE
              ))
              
              # Store which index we're editing
              dictionary_state$editing_cat_index <- local_i
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Save edited categorical value
    observeEvent(input$save_edit_cat, {
      if (!is.null(dictionary_state$editing_cat_index)) {
        current_values <- categorical_values()
        index <- dictionary_state$editing_cat_index
        
        if (index <= length(current_values)) {
          # Update the value at the index
          current_values[[index]] <- list(
            value = input$edit_cat_value,
            label = input$edit_cat_label,
            description = input$edit_cat_description
          )
          
          categorical_values(current_values)
          removeModal()
          showNotification("Categorical value updated", type = "message")
        }
      }
    })

    # Update the save variable handler to include new fields:
    observeEvent(input$save_variable, {
      if (!is.null(dictionary_state$current_variable)) {
        var_name <- dictionary_state$current_variable
        
        # Update variable data including new fields
        dictionary_state$variables[[var_name]]$description <- input$var_description %||% ""
        dictionary_state$variables[[var_name]]$type <- input$var_type %||% "string"
        dictionary_state$variables[[var_name]]$unit <- input$var_unit %||% ""
        dictionary_state$variables[[var_name]]$min_value <- input$var_min %||% ""
        dictionary_state$variables[[var_name]]$max_value <- input$var_max %||% ""
        dictionary_state$variables[[var_name]]$value_reference <- input$var_value_reference %||% ""
        dictionary_state$variables[[var_name]]$default_value <- input$var_default %||% ""
        dictionary_state$variables[[var_name]]$required <- input$var_required %||% FALSE
        dictionary_state$variables[[var_name]]$unique <- input$var_unique %||% FALSE
        dictionary_state$variables[[var_name]]$pattern <- input$var_pattern %||% ""
        dictionary_state$variables[[var_name]]$source <- input$var_source %||% ""
        dictionary_state$variables[[var_name]]$notes <- input$var_notes %||% ""
        
        # Save categorical values
        cat_values <- categorical_values()
        dictionary_state$variables[[var_name]]$categorical_values <- cat_values
        
        dictionary_state$is_modified <- TRUE
        
        showNotification(paste("Saved changes for", var_name), type = "message")
      }
    })
    
    observeEvent(input$reset_variable, {
      if (!is.null(dictionary_state$current_variable)) {
        var_name <- dictionary_state$current_variable
        var_info <- dictionary_state$variables[[var_name]]
        
        # Reset form to current saved values including new fields
        updateTextAreaInput(session, "var_description", value = var_info$description %||% "")
        updateSelectInput(session, "var_type", selected = var_info$type %||% "string")
        updateTextInput(session, "var_unit", value = var_info$unit %||% "")
        updateTextInput(session, "var_min", value = var_info$min_value %||% "")
        updateTextInput(session, "var_max", value = var_info$max_value %||% "")
        updateTextAreaInput(session, "var_value_reference", value = var_info$value_reference %||% "")
        updateTextInput(session, "var_default", value = var_info$default_value %||% "")
        updateCheckboxInput(session, "var_required", value = var_info$required %||% FALSE)
        updateCheckboxInput(session, "var_unique", value = var_info$unique %||% FALSE)
        updateTextInput(session, "var_pattern", value = var_info$pattern %||% "")
        updateTextInput(session, "var_source", value = var_info$source %||% "")
        updateTextAreaInput(session, "var_notes", value = var_info$notes %||% "")
        
        # Reset categorical values
        if (!is.null(var_info$categorical_values)) {
          categorical_values(var_info$categorical_values)
        } else {
          categorical_values(list())
        }
        
        showNotification("Reset to saved values", type = "message")
      }
    })

    # Update the add categorical value handler:
    observeEvent(input$add_cat_value, {
      if (!is.null(input$new_cat_value) && input$new_cat_value != "") {
        current_values <- categorical_values()
        
        # Check for duplicates
        existing_values <- sapply(current_values, function(x) x$value)
        if (input$new_cat_value %in% existing_values) {
          showNotification("This value already exists", type = "warning")
          return()
        }
        
        # Add new value with label
        new_value <- list(
          value = input$new_cat_value,
          label = if(is.null(input$new_cat_label) || input$new_cat_label == "") {
            input$new_cat_value  # Default label to value if not provided
          } else {
            input$new_cat_label
          },
          description = input$new_cat_description %||% ""
        )
        
        categorical_values(c(current_values, list(new_value)))
        
        # Clear inputs
        updateTextInput(session, "new_cat_value", value = "")
        updateTextInput(session, "new_cat_label", value = "")
        updateTextInput(session, "new_cat_description", value = "")
        
        showNotification("Categorical value added", type = "message")
      } else {
        showNotification("Please enter a value", type = "warning")
      }
    })
    
    # Handle removing categorical values
    observeEvent(categorical_values(), {
      values <- categorical_values()
      
      # Clear any existing observers to prevent conflicts
      if (exists("cat_observers", inherits = FALSE)) {
        lapply(cat_observers, function(obs) obs$destroy())
      }
      
      # Create new observers for current values
      cat_observers <<- list()
      
      for (i in seq_along(values)) {
        local({
          local_i <- i
          button_id <- paste0("remove_cat_", local_i)
          
          cat_observers[[local_i]] <<- observeEvent(input[[button_id]], {
            current_values <- categorical_values()
            if (local_i <= length(current_values)) {
              categorical_values(current_values[-local_i])
              showNotification("Categorical value removed", type = "message")
            }
          }, ignoreInit = TRUE)
        })
      }
    }, ignoreInit = TRUE)

    generateVariableMeasuredPreview <- function() {
  if (length(dictionary_state$variables) == 0) {
    return("[]")
  }
  
  # Build the JSON structure
  variable_measured <- lapply(names(dictionary_state$variables), function(var_name) {
    var_info <- dictionary_state$variables[[var_name]]
    
    # Build the property value object
    prop_value <- list(
      `@type` = "PropertyValue",
      name = var_name,
      description = if(nchar(var_info$description) > 0) var_info$description else NULL,
      valueType = var_info$type
    )
    
    # Add optional fields if they have values
    if (nchar(var_info$unit) > 0) {
      prop_value$unitText <- var_info$unit
    }
    
    if (nchar(var_info$min_value) > 0) {
      prop_value$minValue <- var_info$min_value
    }
    
    if (nchar(var_info$max_value) > 0) {
      prop_value$maxValue <- var_info$max_value
    }
    
    # For categorical variables, add value reference
    if (var_info$type == "categorical" && length(var_info$categorical_values) > 0) {
      prop_value$valueReference <- lapply(var_info$categorical_values, function(cat) {
        cat_obj <- list(
          value = cat$value,
          label = if(nchar(cat$label) > 0 && cat$label != cat$value) cat$label else NULL,
          description = if(nchar(cat$description) > 0) cat$description else NULL
        )
        # Remove NULL values
        cat_obj[!sapply(cat_obj, is.null)]
      })
    }
    
    # ALWAYS include validation properties (even if false)
    prop_value$required <- var_info$required %||% FALSE
    prop_value$unique <- var_info$unique %||% FALSE
    
    # Only add pattern if it has a value
    if (nchar(var_info$pattern) > 0) {
      prop_value$pattern <- var_info$pattern
    }
    
    # Remove NULL values
    prop_value[!sapply(prop_value, is.null)]
  })
  
  # Convert to JSON with pretty formatting
  json_str <- jsonlite::toJSON(
    list(variableMeasured = variable_measured), 
    pretty = TRUE, 
    auto_unbox = TRUE
  )
  
  # Extract just the variableMeasured part (remove the outer braces)
  json_str <- gsub('^\\{\\s*"variableMeasured":\\s*', '', json_str)
  json_str <- gsub('\\s*\\}$', '', json_str)
  
  # Add syntax highlighting
  json_html <- json_str
  
  # Highlight property names
  json_html <- gsub('"(@?[^"]+)":', '<span style="color: #0969da;">\"\\1\"</span>:', json_html)
  
  # Highlight string values
  json_html <- gsub(':\\s*"([^"]*)"', ': <span style="color: #0a3069;">\"\\1\"</span>', json_html)
  
  # Highlight booleans
  json_html <- gsub(':\\s*(true|false)', ': <span style="color: #cf222e;">\\1</span>', json_html)
  
  # Highlight numbers
  json_html <- gsub(':\\s*([0-9.]+)', ': <span style="color: #953800;">\\1</span>', json_html)
  
  return(json_html)
}


    observeEvent(input$continue, {
      message("Save dictionary button clicked\n")  # Debug line
      
      # Generate the variableMeasured preview
      variable_measured_preview <- generateVariableMeasuredPreview()
      
      # Show preview modal
      showModal(modalDialog(
        title = "Data Dictionary Preview",
        size = "l",
        div(
          p("Here's how your variableMeasured property will appear in dataset_description.json:"),
          
          # Preview container
          tags$pre(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; max-height: 400px; overflow-y: auto; font-family: monospace; font-size: 12px;",
            HTML(variable_measured_preview)
          ),
          
          # Summary stats
          div(
            style = "margin-top: 20px; padding: 10px; background-color: #e8f4f8; border-radius: 4px;",
            p(strong("Summary:")),
            p(paste("Total variables:", length(dictionary_state$variables))),
            p(paste("Variables with descriptions:", 
                    sum(sapply(dictionary_state$variables, function(v) nchar(v$description) > 0)))),
            p(paste("Categorical variables:", 
                    sum(sapply(dictionary_state$variables, function(v) v$type == "categorical"))))
          )
        ),
        footer = tagList(
          modalButton("Back to Editing"),
          actionButton(session$ns("confirm_save_dictionary"), "Save & Continue to Explorer", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    observeEvent(input$confirm_save_dictionary, {
      # Save the dictionary to the dataset
      if (!is.null(dictionary_state$dataset_path)) {
        tryCatch({
          # Read existing dataset_description.json
          json_path <- file.path(dictionary_state$dataset_path, "dataset_description.json")
          
          if (file.exists(json_path)) {
            dataset_desc <- jsonlite::fromJSON(json_path)
          } else {
            dataset_desc <- list()
          }
          
          # Update variableMeasured
          dataset_desc$variableMeasured <- lapply(names(dictionary_state$variables), function(var_name) {
            var_info <- dictionary_state$variables[[var_name]]
            
            prop_value <- list(
              `@type` = "PropertyValue",
              name = var_name,
              description = if(nchar(var_info$description) > 0) var_info$description else NULL,
              valueType = var_info$type
            )
            
            if (nchar(var_info$unit) > 0) prop_value$unitText <- var_info$unit
            if (nchar(var_info$min_value) > 0) prop_value$minValue <- var_info$min_value
            if (nchar(var_info$max_value) > 0) prop_value$maxValue <- var_info$max_value
            
            if (var_info$type == "categorical" && length(var_info$categorical_values) > 0) {
              prop_value$valueReference <- lapply(var_info$categorical_values, function(cat) {
                cat_obj <- list(
                  value = cat$value,
                  label = if(nchar(cat$label) > 0 && cat$label != cat$value) cat$label else NULL,
                  description = if(nchar(cat$description) > 0) cat$description else NULL
                )
                cat_obj[!sapply(cat_obj, is.null)]
              })
            }
            
            # ALWAYS include these fields
            prop_value$required <- var_info$required %||% FALSE
            prop_value$unique <- var_info$unique %||% FALSE
            
            # Only add pattern if it has a value
            if (nchar(var_info$pattern) > 0) prop_value$pattern <- var_info$pattern
            
            prop_value[!sapply(prop_value, is.null)]
          })
          
          # Write back to file
          jsonlite::write_json(dataset_desc, json_path, pretty = TRUE, auto_unbox = TRUE)
          
          # Store the dataset path in state for the explorer
          state$dictionary_dataset_dir <- dictionary_state$dataset_path
          
          removeModal()
          showNotification("Data dictionary saved successfully!", type = "success")
          
          # Navigate to dataset explorer
          session$sendCustomMessage("changeTab", list(tabName = "explorer"))
          
        }, error = function(e) {
          showNotification(paste("Error saving dictionary:", e$message), type = "error")
        })
      } else {
        showNotification("No dataset loaded", type = "error")
      }
    })
    
    # Return reactive containing dictionary state
    return(reactive({ dictionary_state }))
  })
}

#' Dataset Explorer Server Module
#'
#' @param id The module ID
#' @param state Global state reactive values  
#' @param session The current session object
datasetExplorerServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for explorer state
    explorer_state <- reactiveValues(
      dataset_path = NULL,
      csv_files = list(),
      current_file = NULL,
      current_data = NULL,
      keyword_filters = list(),
      column_filters = list(),
      available_keywords = character(0),
      available_columns = character(0)
    )
    
    # Track if dataset is loaded
    output$dataset_loaded <- reactive({
      !is.null(explorer_state$dataset_path) && length(explorer_state$csv_files) > 0
    })
    outputOptions(output, "dataset_loaded", suspendWhenHidden = FALSE)
    
    # Set up directory selection
    volumes <- c(Home = "~")
    if (.Platform$OS.type == "windows") {
      volumes <- c(volumes, getVolumes()())
    }
    
    shinyDirChoose(
      input,
      "dataset_dir_select", 
      roots = volumes,
      session = session,
      restrictions = system.file(package = "base")
    )

    extractKeywordValues <- function(keyword) {
      values <- character(0)
      
      for (file in explorer_state$csv_files) {
        filename <- basename(file)
        # Look for pattern like "keyword-value"
        pattern <- paste0(keyword, "-([^_]+)")
        matches <- regmatches(filename, regexec(pattern, filename))
        if (length(matches[[1]]) > 1) {
          values <- c(values, matches[[1]][2])
        }
      }
      
      return(unique(sort(values)))
    }
    
    observeEvent(input$dataset_dir_select, {
      if (!is.null(input$dataset_dir_select)) {
        selected_dir <- parseDirPath(volumes, input$dataset_dir_select)
        updateTextInput(session, "dataset_dir", value = selected_dir)
      }
    })
    
    # Display dataset info
    output$dataset_info <- renderUI({
      if (!is.null(explorer_state$dataset_path) && length(explorer_state$csv_files) > 0) {
        dataset_name <- basename(explorer_state$dataset_path)
        file_count <- length(explorer_state$csv_files)
        
        div(
          icon("check-circle", style = "color: #28a745; margin-right: 8px;"),
          strong("Dataset loaded: "), dataset_name,
          span(style = "margin-left: 15px; color: #6c757d;",
               paste(file_count, "CSV files found"))
        )
      } else if (!is.null(explorer_state$dataset_path)) {
        div(
          icon("exclamation-triangle", style = "color: #ffc107; margin-right: 8px;"),
          strong("Dataset path selected: "), basename(explorer_state$dataset_path),
          span(style = "margin-left: 15px; color: #dc3545;",
               "No CSV files found in data directory")
        )
      }
    })
    
    # Load dataset when button clicked
    observeEvent(input$load_dataset_btn, {
      dataset_path <- input$dataset_dir
      
      if (dataset_path == "" || !dir.exists(dataset_path)) {
        showNotification("Please select a valid dataset directory", type = "error")
        return()
      }
      
      # Check if it's a valid Psych-DS dataset
      if (!file.exists(file.path(dataset_path, "dataset_description.json"))) {
        showNotification("Selected directory does not contain dataset_description.json", type = "error")
        return()
      }
      
      if (!dir.exists(file.path(dataset_path, "data"))) {
        showNotification("Selected directory does not contain a 'data' folder", type = "error") 
        return()
      }
      
      # Load CSV files from data directory
      tryCatch({
        data_dir <- file.path(dataset_path, "data")
        # Use recursive = TRUE to find CSV files in subdirectories
        csv_files <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
        
        message("Looking for CSV files in: ", data_dir)
        message("Found files: ", paste(csv_files, collapse = ", "))
        
        if (length(csv_files) == 0) {
          # Try looking without the recursive flag first to debug
          all_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)
          message("All files in data dir: ", paste(all_files, collapse = ", "))
          
          showNotification("No CSV files found in data directory", type = "warning")
          explorer_state$dataset_path <- dataset_path
          explorer_state$csv_files <- character(0)
          return()
        }
        
        # Extract keywords from filenames
        keywords <- extractKeywordsFromFilenames(basename(csv_files))
        
        explorer_state$dataset_path <- dataset_path
        explorer_state$csv_files <- csv_files
        explorer_state$available_keywords <- keywords
        explorer_state$current_file <- csv_files[1]
        
        # Load the first file
        loadCurrentFile()
        
        # Update UI choices
        updateSelectInput(session, "keyword_select", choices = c("Select keyword..." = "", keywords))
        
        showNotification(paste("Dataset loaded successfully!", length(csv_files), "CSV files found"), type = "message")
        
      }, error = function(e) {
        message("Error in loading dataset: ", e$message)
        showNotification(paste("Error loading dataset:", e$message), type = "error")
      })
    })

    observeEvent(input$keyword_select, {
      if (!is.null(input$keyword_select) && 
          input$keyword_select != "" && 
          input$keyword_select != "(Load dataset first)") {
        
        # Extract all possible values for this keyword
        possible_values <- extractKeywordValues(input$keyword_select)
        
        # Update the selectize input with these values
        updateSelectizeInput(session, "keyword_value", 
                            choices = possible_values,
                            server = TRUE)  # Use server-side for better performance with many options
      } else {
        # Clear choices if no keyword selected
        updateSelectizeInput(session, "keyword_value", choices = character(0))
      }
    })

    observeEvent(input$column_select, {
      if (!is.null(input$column_select) && 
          input$column_select != "" && 
          input$column_select != "(Load dataset first)" &&
          !is.null(explorer_state$current_data)) {
        
        # Get unique values from the selected column across all loaded files
        # For better performance, we'll combine values from all files
        all_values <- character(0)
        
        for (file in explorer_state$csv_files) {
          tryCatch({
            temp_data <- read.csv(file, stringsAsFactors = FALSE)
            if (input$column_select %in% names(temp_data)) {
              column_values <- as.character(unique(temp_data[[input$column_select]]))
              all_values <- c(all_values, column_values[!is.na(column_values)])
            }
          }, error = function(e) {
            # Skip files that can't be read
          })
        }
        
        # Get unique sorted values
        unique_values <- unique(sort(all_values))
        
        # Limit to reasonable number if there are too many
        if (length(unique_values) > 500) {
          unique_values <- unique_values[1:500]
          showNotification("Showing first 500 unique values. Type to search for specific values.", 
                          type = "info", duration = 5)
        }
        
        # Update the selectize input
        updateSelectizeInput(session, "column_value", 
                            choices = unique_values,
                            server = TRUE)
      } else {
        # Clear choices if no column selected
        updateSelectizeInput(session, "column_value", choices = character(0))
      }
    })
    
    # Initialize the search panel UI components even before dataset is loaded
    observe({
      # Initialize keyword select with empty choices
      if (length(explorer_state$available_keywords) == 0) {
        updateSelectInput(session, "keyword_select", 
                         choices = c("Select keyword..." = "", "(Load dataset first)" = ""))
      }
      
      # Initialize column select with empty choices
      if (length(explorer_state$available_columns) == 0) {
        updateSelectInput(session, "column_select", 
                         choices = c("Select column..." = "", "(Load dataset first)" = ""))
        updateSelectInput(session, "stats_variable", 
                         choices = c("Select variable..." = "", "(Load dataset first)" = ""))
      }
    })
    
    # Extract keywords from filenames
    extractKeywordsFromFilenames <- function(filenames) {
      all_keywords <- character(0)
      
      for (filename in filenames) {
        # Remove file extension and split by underscore
        base_name <- gsub("\\.[^.]*$", "", filename)
        parts <- strsplit(base_name, "_")[[1]]
        
        # Extract keyword-value pairs
        for (part in parts) {
          if (grepl("-", part)) {
            keyword <- strsplit(part, "-")[[1]][1]
            all_keywords <- c(all_keywords, keyword)
          }
        }
      }
      
      return(unique(all_keywords))
    }
    
    # Load current file data
    loadCurrentFile <- function() {
      if (!is.null(explorer_state$current_file) && file.exists(explorer_state$current_file)) {
        tryCatch({
          message("Loading file: ", explorer_state$current_file)
          data <- read.csv(explorer_state$current_file, stringsAsFactors = FALSE)
          explorer_state$current_data <- data
          explorer_state$available_columns <- names(data)
          
          # Update column choices
          updateSelectInput(session, "column_select", 
                           choices = c("Select column..." = "", names(data)))
          updateSelectInput(session, "stats_variable", 
                           choices = c("Select variable..." = "", names(data)))
          
          message("File loaded successfully with ", nrow(data), " rows and ", ncol(data), " columns")
          
        }, error = function(e) {
          message("Error reading file: ", e$message)
          showNotification(paste("Error reading file:", e$message), type = "error")
        })
      }
    }
    
    # Add keyword filter
    observeEvent(input$add_keyword_filter, {
      if (!is.null(input$keyword_select) && input$keyword_select != "" && 
          input$keyword_select != "(Load dataset first)" &&
          !is.null(input$keyword_value) && input$keyword_value != "") {
        
        filter_key <- paste0(input$keyword_select, ":", input$keyword_value)
        
        # Check if filter already exists
        if (!filter_key %in% names(explorer_state$keyword_filters)) {
          explorer_state$keyword_filters[[filter_key]] <- list(
            keyword = input$keyword_select,
            value = input$keyword_value
          )
          
          # Clear inputs
          updateTextInput(session, "keyword_value", value = "")
          
          # Apply filters
          applyFilters()
          
          showNotification("Keyword filter added", type = "message")
        } else {
          showNotification("This filter already exists", type = "warning")
        }
      } else {
        showNotification("Please select a keyword and enter a value", type = "warning")
      }
    })
    
    # Add column filter
    observeEvent(input$add_column_filter, {
      if (!is.null(input$column_select) && input$column_select != "" && 
          input$column_select != "(Load dataset first)" &&
          !is.null(input$column_value) && input$column_value != "") {
        
        filter_key <- paste0(input$column_select, ":", input$column_value)
        
        # Check if filter already exists
        if (!filter_key %in% names(explorer_state$column_filters)) {
          explorer_state$column_filters[[filter_key]] <- list(
            column = input$column_select,
            value = input$column_value
          )
          
          # Clear inputs
          updateTextInput(session, "column_value", value = "")
          
          showNotification("Column filter added", type = "message")
        } else {
          showNotification("This filter already exists", type = "warning")
        }
      } else {
        showNotification("Please select a column and enter a value", type = "warning")
      }
    })
    
    # Apply filters to determine which files to show
    applyFilters <- function() {
      if (length(explorer_state$keyword_filters) == 0) {
        return()
      }
      
      # Filter files based on keyword filters
      matching_files <- explorer_state$csv_files
      
      for (filter in explorer_state$keyword_filters) {
        pattern <- paste0(filter$keyword, "-", filter$value)
        matching_files <- matching_files[grepl(pattern, basename(matching_files))]
      }
      
      # Load first matching file if different from current
      if (length(matching_files) > 0 && matching_files[1] != explorer_state$current_file) {
        explorer_state$current_file <- matching_files[1]
        loadCurrentFile()
      } else if (length(matching_files) == 0) {
        showNotification("No files match the current filters", type = "warning")
      }
    }
    
    # Display keyword filters
    output$keyword_filters_display <- renderUI({
      filters <- explorer_state$keyword_filters
      
      if (length(filters) == 0) {
        return(div(
          style = "color: #6c757d; font-style: italic;",
          "No keyword filters active"
        ))
      }
      
      filter_badges <- lapply(names(filters), function(filter_key) {
        filter <- filters[[filter_key]]
        span(
          style = "display: inline-block; margin: 2px; padding: 4px 8px; background-color: #3498db; color: white; border-radius: 12px; font-size: 12px;",
          paste0(filter$keyword, ': "', filter$value, '"'),
          actionButton(
            session$ns(paste0("remove_kw_", gsub("[^A-Za-z0-9]", "_", filter_key))),
            "×",
            style = "background: none; border: none; color: white; padding: 0 0 0 5px; font-size: 14px;",
            onclick = paste0("Shiny.setInputValue('", session$ns("remove_keyword_filter"), "', '", filter_key, "', {priority: 'event'});")
          )
        )
      })
      
      do.call(tagList, filter_badges)
    })
    
    # Display column filters
    output$column_filters_display <- renderUI({
      filters <- explorer_state$column_filters
      
      if (length(filters) == 0) {
        return(div(
          style = "color: #6c757d; font-style: italic;",
          "No column filters active"
        ))
      }
      
      filter_badges <- lapply(names(filters), function(filter_key) {
        filter <- filters[[filter_key]]
        span(
          style = "display: inline-block; margin: 2px; padding: 4px 8px; background-color: #e74c3c; color: white; border-radius: 12px; font-size: 12px;",
          paste0(filter$column, ': "', filter$value, '"'),
          actionButton(
            session$ns(paste0("remove_col_", gsub("[^A-Za-z0-9]", "_", filter_key))),
            "×",
            style = "background: none; border: none; color: white; padding: 0 0 0 5px; font-size: 14px;",
            onclick = paste0("Shiny.setInputValue('", session$ns("remove_column_filter"), "', '", filter_key, "', {priority: 'event'});")
          )
        )
      })
      
      do.call(tagList, filter_badges)
    })
    
    # Remove filters
    observeEvent(input$remove_keyword_filter, {
      filter_key <- input$remove_keyword_filter
      explorer_state$keyword_filters[[filter_key]] <- NULL
      applyFilters()
      showNotification("Keyword filter removed", type = "message")
    })
    
    observeEvent(input$remove_column_filter, {
      filter_key <- input$remove_column_filter
      explorer_state$column_filters[[filter_key]] <- NULL
      showNotification("Column filter removed", type = "message")
    })
    
    # File tabs
    output$file_tabs <- renderUI({
      if (length(explorer_state$csv_files) == 0) {
        return(div(
          style = "color: #6c757d; font-style: italic; padding: 10px;",
          "No CSV files loaded. Select and load a dataset above."
        ))
      }
      
      current_file <- explorer_state$current_file
      
      # Apply keyword filters to determine which files to show
      files_to_show <- explorer_state$csv_files
      for (filter in explorer_state$keyword_filters) {
        pattern <- paste0(filter$keyword, "-", filter$value)
        files_to_show <- files_to_show[grepl(pattern, basename(files_to_show))]
      }
      
      if (length(files_to_show) == 0) {
        return(div(
          style = "color: #dc3545; font-style: italic; padding: 10px;",
          "No files match current keyword filters"
        ))
      }
      
      tabs <- lapply(files_to_show, function(file) {
        filename <- basename(file)
        is_active <- identical(file, current_file)
        
        actionButton(
          session$ns(paste0("select_file_", gsub("[^A-Za-z0-9]", "_", filename))),
          filename,
          class = if (is_active) "btn btn-primary" else "btn btn-outline-secondary",
          style = "margin-right: 5px; margin-bottom: 5px;",
          onclick = paste0("Shiny.setInputValue('", session$ns("select_file"), "', '", file, "', {priority: 'event'});")
        )
      })
      
      do.call(tagList, tabs)
    })
    
    # Handle file selection
    observeEvent(input$select_file, {
      explorer_state$current_file <- input$select_file
      loadCurrentFile()
    })
    
    # Variable statistics
    output$unique_count <- renderText({
      if (!is.null(explorer_state$current_data) && 
          !is.null(input$stats_variable) && 
          input$stats_variable != "" &&
          input$stats_variable != "(Load dataset first)") {
        column_data <- explorer_state$current_data[[input$stats_variable]]
        as.character(length(unique(column_data[!is.na(column_data)])))
      } else {
        "—"
      }
    })
    
    output$total_count <- renderText({
      if (!is.null(explorer_state$current_data) && 
          !is.null(input$stats_variable) && 
          input$stats_variable != "" &&
          input$stats_variable != "(Load dataset first)") {
        column_data <- explorer_state$current_data[[input$stats_variable]]
        as.character(length(column_data[!is.na(column_data)]))
      } else {
        "—"
      }
    })
    
    # Data table
    output$data_table <- DT::renderDataTable({
      if (is.null(explorer_state$current_data)) {
        return(DT::datatable(
          data.frame(Message = "No data loaded. Select a dataset and load it using the button above."), 
          options = list(dom = 't', searching = FALSE, paging = FALSE)
        ))
      }
      
      data <- explorer_state$current_data
      
      # Apply column filters
      for (filter in explorer_state$column_filters) {
        column_name <- filter$column
        filter_value <- filter$value
        
        if (column_name %in% names(data)) {
          # Use grepl for partial matching
          data <- data[grepl(filter_value, as.character(data[[column_name]]), ignore.case = TRUE), , drop = FALSE]
        }
      }
      
      DT::datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftip"
        ),
        rownames = FALSE
      )
    }, server = TRUE)
    
    # Return reactive containing explorer state
    return(reactive({ explorer_state }))
  })
}