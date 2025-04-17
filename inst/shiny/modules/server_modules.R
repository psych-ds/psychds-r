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

      # Create data dictionary templates for selected files
      if (length(state$data_files) > 0) {
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
    # Create tabs for each file
    output$file_tabs <- renderUI({
      if (length(state$data_files) == 0) {
        return(div(
          style = "text-align: center; padding: 30px; color: #999;",
          "No data files selected. Please go back to Step 1 and select at least one data file."
        ))
      }

      # Create a tabset with a tab for each file
      tabsetPanel(
        id = session$ns("file_tabset"),
        lapply(state$data_files, function(file) {
          tabPanel(
            basename(file),
            div(
              style = "margin-top: 15px;",
              if (file %in% names(state$data_dict) && nrow(state$data_dict[[file]]) > 0) {
                renderDataDictEditor(file, state$data_dict[[file]], session$ns)
              } else {
                div(
                  style = "text-align: center; padding: 30px; color: #999;",
                  "No columns found in this file or file is not accessible."
                )
              }
            )
          )
        })
      )
    })

    # Function to render data dictionary editor for a file
    renderDataDictEditor <- function(file, dict_data, ns) {
      # Create a unique ID for this editor
      editor_id <- paste0("editor_", gsub("[^a-zA-Z0-9]", "_", file))

      # Create the editor UI
      div(
        div(class = "section-title", paste0("File: ", basename(file))),
        p(class = "section-description", "Edit the variable information below. Click 'Save Changes' when done."),

        # Data table for editing
        DTOutput(ns(editor_id)),

        # Save button
        div(
          style = "text-align: right; margin-top: 10px;",
          actionButton(ns(paste0("save_", editor_id)), "Save Changes", class = "save-btn")
        )
      )
    }

    # Create data tables for each file
    observe({
      for (file in state$data_files) {
        local({
          local_file <- file
          editor_id <- paste0("editor_", gsub("[^a-zA-Z0-9]", "_", local_file))

          if (local_file %in% names(state$data_dict) && nrow(state$data_dict[[local_file]]) > 0) {
            # Create editable data table
            output[[editor_id]] <- renderDT({
              datatable(
                state$data_dict[[local_file]],
                editable = TRUE,
                options = list(
                  pageLength = 10,
                  lengthMenu = c(5, 10, 25, 50),
                  scrollX = TRUE
                )
              )
            })

            # Handle table edits
            observeEvent(input[[paste0(editor_id, "_cell_edit")]], {
              info <- input[[paste0(editor_id, "_cell_edit")]]
              i <- info$row
              j <- info$col + 1  # column index offset
              v <- info$value

              # Update the data dictionary
              state$data_dict[[local_file]][i, j] <- v
            })

            # Handle save button
            observeEvent(input[[paste0("save_", editor_id)]], {
              showNotification(paste("Changes saved for", basename(local_file)), type = "message")
            })
          }
        })
      }
    })

    # Handle Back button
    observeEvent(input$back, {
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
      # Move to step 3
      state$current_step <- 3
    })
  })
}

#' Step 3 Server Module
#'
#' Handles dataset metadata and finalization
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
step3Server <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Initialize authors
    authors <- reactiveVal(list())

    # Add an initial author field on startup
    observe({
      if (length(authors()) == 0) {
        addAuthorField()
      }
    })

    # Function to add an author field
    addAuthorField <- function() {
      # Create a unique ID for this author
      author_id <- generate_id("author")

      # Add to the list of authors
      current <- authors()
      current[[length(current) + 1]] <- list(
        id = author_id,
        name = "",
        affiliations = ""
      )
      authors(current)

      # Add the UI element
      insertUI(
        selector = paste0("#", session$ns("authors_container")),
        where = "beforeEnd",
        ui = div(
          id = paste0("author_container_", author_id),
          style = "margin-bottom: 10px; display: flex; align-items: center;",
          div(
            style = "flex-grow: 1;",
            textInput(
              session$ns(paste0("author_name_", author_id)),
              "Name",
              width = "100%",
              placeholder = "Author's name"
            )
          ),
          div(
            style = "flex-grow: 1; margin-left: 10px;",
            textInput(
              session$ns(paste0("author_affil_", author_id)),
              "Affiliations",
              width = "100%",
              placeholder = "Author's affiliations"
            )
          ),
          div(
            style = "margin-left: 10px; margin-top: 25px;",
            if (length(authors()) > 1) {
              actionButton(
                session$ns(paste0("remove_author_", author_id)),
                label = NULL,
                icon = icon("trash"),
                class = "btn-danger remove-btn"
              )
            }
          )
        )
      )

      # Add observer for remove button if this isn't the first author
      if (length(authors()) > 1) {
        local({
          local_id <- author_id
          observeEvent(input[[paste0("remove_author_", local_id)]], {
            # Get current authors
            current <- authors()

            # Find and remove this author
            index_to_remove <- which(sapply(current, function(x) x$id == local_id))
            if (length(index_to_remove) > 0) {
              current <- current[-index_to_remove]
              authors(current)
            }

            # Remove the UI element
            removeUI(selector = paste0("#author_container_", local_id))
          })
        })
      }
    }

    # Handle add author button
    observeEvent(input$add_author, {
      addAuthorField()
    })

    # Update authors information when inputs change
    observe({
      current <- authors()

      # Update each author's information
      for (i in seq_along(current)) {
        author_id <- current[[i]]$id
        name_input <- paste0("author_name_", author_id)
        affil_input <- paste0("author_affil_", author_id)

        if (exists(name_input, where = input)) {
          current[[i]]$name <- input[[name_input]]
        }
        if (exists(affil_input, where = input)) {
          current[[i]]$affiliations <- input[[affil_input]]
        }
      }

      # Update the reactive value
      authors(current)
    })

    # Handle Back button
    observeEvent(input$back, {
      # Update global state with current values
      updateGlobalState()

      # Go back to step 2
      state$current_step <- 2
    })

    # Handle Finish button
    observeEvent(input$finish, {
      # Validate inputs
      if (is.null(input$dataset_name) || input$dataset_name == "") {
        showModal(modalDialog(
          title = "Error",
          "Please enter a dataset name.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }

      # Update global state
      updateGlobalState()

      # Show confirmation
      showModal(modalDialog(
        title = "Create Dataset",
        div(
          p("You're about to create a Psych-DS dataset with the following settings:"),
          tags$ul(
            tags$li(strong("Dataset Name:"), input$dataset_name),
            tags$li(strong("Project Directory:"), state$project_dir),
            tags$li(strong("Data Files:"),
                    if(length(state$data_files) > 0) paste(state$data_files, collapse = ", ") else "None"),
            tags$li(strong("Authors:"),
                    paste(sapply(authors(), function(a) a$name), collapse = ", ")),
            tags$li(strong("License:"), input$license)
          ),
          p("Click 'Create' to generate the Psych-DS dataset.")
        ),
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_create"), "Create", class = "btn-success")
        )
      ))
    })

    # Handle confirmed create action
    observeEvent(input$confirm_create, {
      removeModal()

      # In a real implementation, this would create the actual Psych-DS dataset
      # For this example, we'll just show a success message
      showModal(modalDialog(
        title = "Success!",
        div(
          p("Your Psych-DS dataset has been created successfully."),
          p("The following files and directories have been created:"),
          tags$ul(
            tags$li(paste0(state$project_dir, "/data/")),
            tags$li(paste0(state$project_dir, "/data/dataset_description.json")),
            tags$li(paste0(state$project_dir, "/data/datapackage.json")),
            lapply(state$data_files, function(file) {
              tags$li(paste0(state$project_dir, "/data/", basename(file)))
            })
          )
        ),
        easyClose = TRUE,
        footer = tagList(
          actionButton(session$ns("view_dataset"), "View Dataset", class = "btn-primary"),
          modalButton("Close")
        )
      ))
    })

    # Handle view dataset button
    observeEvent(input$view_dataset, {
      # In a real implementation, this would open the dataset explorer
      removeModal()
      showNotification("Dataset explorer would open here", type = "message")
    })

    # Update global state with current values
    updateGlobalState <- function() {
      # Update dataset info
      state$dataset_info$name <- input$dataset_name
      state$dataset_info$description <- input$dataset_description
      state$dataset_info$license <- input$license

      # Update authors - convert from list to character vector
      state$dataset_info$authors <- lapply(authors(), function(a) {
        paste0(a$name, if (a$affiliations != "") paste0(" (", a$affiliations, ")") else "")
      })
    }
  })
}
