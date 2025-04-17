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
library(jsonlite)
library(tools)
library(utils)

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
      custom = list()             # Custom directories added by user
    ),

    # Data dictionary information
    data_dict = list(),           # Data dictionary for selected files

    # Dataset metadata
    dataset_info = list(
      name = NULL,
      description = NULL,
      authors = list(),
      license = "CC-BY-4.0",
      version = "1.0.0"
    ),

    # Error and notification tracking
    errors = list(),
    notifications = list()
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

#' List all CSV files in a directory (recursively) - Fixed Version
#'
#' @param dir_path Character string of directory path to scan
#' @param recursive Logical indicating whether to search recursively
#' @return List of CSV files (relative paths from dir_path)
#' List all CSV files in a directory (recursively) - Fixed rel_paths issue
#'
#' @param dir_path Character string of directory path to scan
#' @param recursive Logical indicating whether to search recursively
#' @return List of CSV files (relative paths from dir_path)
list_csv_files <- function(dir_path, recursive = TRUE) {
  # Debug output
  message("Listing CSV files in: ", dir_path)

  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    message("Directory does not exist or is not accessible")
    return(character(0))
  }

  tryCatch({
    # Get all files (with full paths)
    all_files <- list.files(dir_path, recursive = recursive, full.names = TRUE)
    message("Found ", length(all_files), " total files")

    # Filter for CSV files (case insensitive)
    csv_files <- all_files[grepl("\\.csv$", all_files, ignore.case = TRUE)]
    message("Found ", length(csv_files), " CSV files")

    # Get JUST the relative paths without using regex
    # This is the most reliable approach
    rel_paths <- list.files(dir_path, recursive = recursive, pattern = "\\.csv$",
                            ignore.case = TRUE, full.names = FALSE)

    message("Relative paths (first 3): ", paste(head(rel_paths, 3), collapse=", "),
            ifelse(length(rel_paths) > 3, "...", ""))

    return(rel_paths)
  }, error = function(e) {
    message("Error listing files: ", e$message)
    return(character(0))
  })
}

#' Extract column information from a CSV file
#'
#' @param file_path Path to the CSV file
#' @return Data frame with column information
extract_csv_structure <- function(file_path) {
  tryCatch({
    # Read the first few rows to determine column types
    data <- utils::read.csv(file_path, nrows = 100, stringsAsFactors = FALSE)

    # Create column info data frame
    column_info <- data.frame(
      name = names(data),
      type = sapply(data, function(x) class(x)[1]),
      description = "",
      unique_values = sapply(data, function(x) length(unique(x))),
      min_value = sapply(data, function(x) {
        if (is.numeric(x)) min(x, na.rm = TRUE) else NA
      }),
      max_value = sapply(data, function(x) {
        if (is.numeric(x)) max(x, na.rm = TRUE) else NA
      }),
      na_count = sapply(data, function(x) sum(is.na(x))),
      stringsAsFactors = FALSE
    )

    return(column_info)
  }, error = function(e) {
    # Return empty data frame if there's an error
    return(data.frame())
  })
}

#' Create a basic dataset_description.json template
#'
#' @param dataset_info List containing dataset information
#' @return List object representing the dataset description
create_dataset_description_template <- function(dataset_info) {
  template <- list(
    Name = dataset_info$name,
    BIDSVersion = "1.0.0-rc1",
    Description = dataset_info$description,
    License = dataset_info$license,
    Authors = dataset_info$authors,
    Acknowledgements = dataset_info$acknowledgements,
    HowToAcknowledge = dataset_info$how_to_acknowledge,
    Funding = dataset_info$funding,
    ReferencesAndLinks = dataset_info$references_and_links,
    DatasetDOI = dataset_info$dataset_doi
  )

  # Remove NULL values
  template[sapply(template, is.null)] <- NULL

  return(template)
}

#' Generate unique ID for UI elements
#'
#' @param prefix Prefix to add to the ID
#' @return Character string with unique ID
generate_id <- function(prefix = "id") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
}

js_code <- "
Shiny.addCustomMessageHandler('refreshUI', function(message) {
  // Force a redraw by slightly resizing elements
  $('.file-browser').each(function() {
    var $this = $(this);
    var w = $this.width();
    $this.width(w+1);
    setTimeout(function() { $this.width(w); }, 50);
  });
});
"

# Helper function to create a hierarchical structure from file paths
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
