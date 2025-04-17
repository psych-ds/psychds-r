#' Global configuration and shared state for the Psych-DS app
#'
#' This file contains global variables, functions, and state management
#' that will be shared across all modules of the application.
#' It's loaded before both ui.R and server.R.

# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(DT)

#' Initialize Reactive Value Store
#'
#' This reactive values object will serve as our "global state"
#' for the application, making it easier to share data between
#' different modules and screens.
#'
#' @return A reactiveValues object that will store app state
init_state <- function() {
  reactiveValues(
    # Current step in the dataset creation workflow
    current_step = 1,
    
    # Project settings
    project_dir = NULL,           # Selected project directory path
    data_files = list(),          # List of data files selected for inclusion
    optional_dirs = list(         # Optional directories to create
      analysis = TRUE,
      materials = TRUE,
      results = FALSE,
      products = FALSE,
      documentation = FALSE,
      custom = character(0)       # Custom directories added by user
    )
  )
}

#' Check if a directory path is valid
#'
#' @param dir_path Character string of directory path to check
#' @return Logical indicating if the directory exists and is accessible
is_valid_dir <- function(dir_path) {
  tryCatch({
    dir.exists(dir_path)
  }, error = function(e) {
    FALSE
  })
}

#' List all CSV files in a directory (recursively)
#'
#' @param dir_path Character string of directory path to scan
#' @param recursive Logical indicating whether to search recursively
#' @return List of CSV files (relative paths from dir_path)
list_csv_files <- function(dir_path, recursive = TRUE) {
  if (!is_valid_dir(dir_path)) {
    return(character(0))
  }
  
  tryCatch({
    # Get relative paths directly (most reliable approach)
    rel_paths <- list.files(dir_path, recursive = recursive, pattern = "\\.csv$",
                            ignore.case = TRUE, full.names = FALSE)
    return(rel_paths)
  }, error = function(e) {
    return(character(0))
  })
}

#' Generate unique ID for UI elements
#'
#' @param prefix Prefix to add to the ID
#' @return Character string with unique ID
generate_id <- function(prefix = "id") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
}

#' Create a hierarchical directory structure from file paths
#'
#' @param files Character vector of file paths
#' @return A list with directory structure information
organize_directory_hierarchy <- function(files) {
  # Initialize the result
  result <- list()
  
  for (file in files) {
    # Split the path into components
    parts <- strsplit(file, "/")[[1]]
    
    # Process each level of the directory hierarchy
    if (length(parts) > 0) {
      current_path <- ""
      
      # Process each directory in the path
      for (i in 1:(length(parts) - 1)) {
        # Build the path to this level
        if (current_path == "") {
          current_path <- parts[i]
        } else {
          current_path <- paste(current_path, parts[i], sep = "/")
        }
        
        # Create an entry for this directory if it doesn't exist
        if (!current_path %in% names(result)) {
          result[[current_path]] <- list(
            name = parts[i],
            level = i,
            is_file = FALSE,
            files = character(0)
          )
        }
      }
      
      # Handle the file itself
      if (length(parts) > 1) {
        # File within a directory
        dir_path <- paste(parts[1:(length(parts) - 1)], collapse = "/")
        filename <- parts[length(parts)]
        
        # Add file to its directory
        if (!dir_path %in% names(result)) {
          # Create the directory entry if it doesn't exist
          result[[dir_path]] <- list(
            name = parts[length(parts) - 1],
            level = length(parts) - 1,
            is_file = FALSE,
            files = character(0)
          )
        }
        
        # Add this file to the directory's files
        result[[dir_path]]$files <- c(result[[dir_path]]$files, filename)
      } else {
        # File in root directory
        result[[file]] <- list(
          name = file,
          level = 0,
          is_file = TRUE,
          dir_path = ""
        )
      }
    }
  }
  
  return(result)
}