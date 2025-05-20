#' Server Module Definitions
#'
#' This file contains server-side logic for the modular UI components.
#' Each module handles its own state and communicates with the global state.

source(file.path(getwd(), "validation.R"))

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
          div(style = "flex: 1;", strong("First Name")),
          div(style = "flex: 1;", strong("Last Name")),
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
        # Clean up the keyword name (lowercase, replace spaces with underscores)
        keyword_name <- tolower(gsub("[^a-zA-Z0-9_]", "", gsub(" ", "_", keyword_name)))

        keywords <- selected_keywords()

        # Only add if it doesn't already exist
        if (!any(sapply(keywords, function(k) k$name == keyword_name))) {
          # Add a unique ID for this keyword
          keyword_id <- paste0(keyword_name, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))

          keywords[[length(keywords) + 1]] <- list(
            name = keyword_name,
            display = input$custom_keyword_name,
            value = "",
            id = keyword_id,
            custom = TRUE
          )
          selected_keywords(keywords)
          updateKeywordMapping()
        } else {
          showNotification("This keyword already exists", type = "warning")
        }

        # Clear the input
        updateTextInput(session, "custom_keyword_name", value = "")
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
        div(
          class = "form-group",
          style = "margin-bottom: 15px;",
          tags$label(
            class = "control-label",
            style = "color: #3498db; font-weight: bold; margin-bottom: 5px; display: block;",
            span(paste0(keyword$display, ":"))
          ),
          textInput(
            session$ns(paste0("keyword_value_", keyword$id)), # Use keyword ID instead of name
            NULL,
            value = "", # Always start with an empty string
            placeholder = paste("Enter", tolower(keyword$display), "value"),
            width = "100%"
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
      # Print log for debugging
      cat("Generate filename button clicked\n")

      # Get current file index
      file_index <- current_file()
      if (is.null(file_index)) {
        cat("No file selected\n")
        return()
      }

      # Get current keywords
      keywords <- selected_keywords()
      if (length(keywords) == 0) {
        showNotification("Please add at least one keyword", type = "warning")
        cat("No keywords selected\n")
        return()
      }

      # DIRECTLY get values from input fields
      keyword_values <- list()
      missing_keywords <- list()

      # Check each keyword for a value
      for (i in seq_along(keywords)) {
        keyword <- keywords[[i]]
        input_name <- paste0("keyword_value_", keyword$id)

        cat("Looking for input: ", input_name, "\n", sep="")

        if (input_name %in% names(input)) {
          value <- input[[input_name]]
          cat("  Found value: '", value, "'\n", sep="")

          if (!is.null(value) && value != "") {
            keyword_values[[keyword$name]] <- value
            # Update the keyword in the list with the value
            keywords[[i]]$value <- value
          } else {
            missing_keywords[[length(missing_keywords) + 1]] <- keyword
          }
        } else {
          cat("  Input not found!\n")
          missing_keywords[[length(missing_keywords) + 1]] <- keyword
        }
      }

      # Check if any values are missing
      if (length(missing_keywords) > 0) {
        missing_names <- sapply(missing_keywords, function(k) k$display)
        message <- paste("Please fill in values for:", paste(missing_names, collapse=", "))
        showNotification(message, type = "warning")
        cat(message, "\n")
        return()
      }

      # All values are present - create filename
      filename_parts <- character(0)

      for (keyword in keywords) {
        # Get the value for this keyword
        value <- keyword_values[[keyword$name]]

        # Add to filename parts
        filename_part <- paste0(keyword$name, "-", value)
        filename_parts <- c(filename_parts, filename_part)

        cat("Added part to filename: ", filename_part, "\n", sep="")
      }

      # Get mappings and file info
      mappings <- file_mappings()
      file_info <- mappings[[file_index]]

      # Add the file extension
      original <- file_info$original
      ext <- tools::file_ext(original)

      # Build the final filename
      if (ext != "") {
        filename <- paste0(paste(filename_parts, collapse = "_"), "_data.", ext)
      } else {
        filename <- paste0(paste(filename_parts, collapse = "_"), "_data")
      }

      cat("Final filename: ", filename, "\n", sep="")

      # Update the keywords with their values in the mapping
      updated_keywords <- keywords
      for (i in seq_along(updated_keywords)) {
        keyword_name <- updated_keywords[[i]]$name
        if (keyword_name %in% names(keyword_values)) {
          updated_keywords[[i]]$value <- keyword_values[[keyword_name]]
        }
      }

      # Update the mapping with updated keywords and new filename
      mappings[[file_index]]$keywords <- updated_keywords
      mappings[[file_index]]$new <- filename
      file_mappings(mappings)

      # Also update the selected keywords
      selected_keywords(updated_keywords)

      # Show success notification
      showNotification(paste("Filename generated successfully!"), type = "message")

      # Auto-advance to next unconfigured file if available
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
    
    # Simplified handler for step status updates
    observeEvent(input$validation_step_status, {
      message("Received step status update")
      
      if (!is.null(input$validation_step_status) && 
          !is.null(input$validation_step_status$stepStatus)) {
        
        # Get updates from JS
        step_updates <- input$validation_step_status$stepStatus
        
        # Process each step status
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
            
            message("Updated status for step: ", step_key)
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
      }
    })
    
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
  })
}