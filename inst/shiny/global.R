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

#' Correct Recursive File Tree Builder
#'
#' Recursively builds a nested file tree that persists changes to the main list.
#'
#' @param directory Path to the dataset directory
#' @return List representing the file tree
buildFileTree <- function(directory) {
  # Normalize and verify the directory path
  directory <- normalizePath(directory, mustWork = TRUE)
  message("Building file tree for: ", directory)

  # Helper to read file content - only for allowed text file types
  readFileText <- function(path) {
    # Only attempt to read contents for CSV and JSON files
    file_ext <- tolower(tools::file_ext(path))
    if (file_ext %in% c("csv", "json", "md", "txt")) {
      tryCatch({
        paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      }, error = function(e) {
        message("Error reading file: ", path, " - ", e$message)
        ""
      })
    } else {
      # For non-text files, return empty string
      message("Skipping content read for non-text file: ", path)
      ""
    }
  }

  # Recursive helper to insert file into nested list
  insertIntoTree <- function(tree, parts, fileInfo) {
    part <- parts[[1]]
    if (length(parts) == 1) {
      # We're at the file level
      tree[[part]] <- list(
        type = "file",
        file = fileInfo
      )
      return(tree)
    } else {
      # We're at a directory level
      if (is.null(tree[[part]])) {
        tree[[part]] <- list(
          type = "directory",
          contents = list()
        )
      }
      tree[[part]]$contents <- insertIntoTree(tree[[part]]$contents, parts[-1], fileInfo)
      return(tree)
    }
  }

  fileTree <- list()

  allFiles <- list.files(directory, recursive = TRUE, full.names = TRUE)
  message("Found ", length(allFiles), " files")

  for (filePath in allFiles) {
    if (dir.exists(filePath)) next

    relPath <- sub(paste0("^", directory, "/?"), "", filePath)
    relPath <- gsub("\\\\", "/", relPath)
    parts <- strsplit(relPath, "/")[[1]]

    fileName <- parts[[length(parts)]]

    fileInfo <- list(
      name = fileName,
      path = paste0("/", relPath),
      text = readFileText(filePath)
    )

    fileTree <- insertIntoTree(fileTree, parts, fileInfo)

    message("Added file: ", fileName, " to path: ", paste(parts[-length(parts)], collapse = "/"))
  }

  message("✅ File tree built with ", length(fileTree), " top-level entries")
  return(fileTree)
}

#' Summarize a directory's contents
#'
#' @param contents Directory contents
#' @return Named list with counts of files and directories
summarizeDirectory <- function(contents) {
  files <- 0
  dirs <- 0
  totalItems <- 0
  
  # Recursive helper function
  countItems <- function(items) {
    if (is.null(items) || length(items) == 0) return()
    
    for (name in names(items)) {
      item <- items[[name]]
      if (identical(item$type, "directory")) {
        dirs <<- dirs + 1
        totalItems <<- totalItems + 1
        countItems(item[["contents"]])
      } else if (identical(item$type, "file")) {
        files <<- files + 1
        totalItems <<- totalItems + 1
      }
    }
  }
  
  # Count items
  countItems(contents)
  
  # Return summary
  return(list(
    files = files,
    dirs = dirs,
    total = totalItems
  ))
}

#' Print a comprehensive visualization of the file tree
#'
#' @param tree File tree structure
#' @param prefix Prefix for indentation (used in recursion)
#' @param isLast Whether the current item is the last in its list (used in recursion)
#' @return None, prints to console
printFileTree <- function(tree, prefix = "", isLast = TRUE) {
  if (is.null(tree) || length(tree) == 0) return()
  
  # Get all names and iterate
  names <- names(tree)
  for (i in seq_along(names)) {
    name <- names[i]
    item <- tree[[name]]
    isLastItem <- (i == length(names))
    
    # Print the current item
    cat(prefix)
    if (isLast && isLastItem) {
      cat("└── ")
      newPrefix <- paste0(prefix, "    ")
    } else {
      cat("├── ")
      newPrefix <- paste0(prefix, "│   ")
    }
    
    if (identical(item$type, "directory")) {
      cat(crayon::blue(name), "\n")
      # Recursively print contents
      printFileTree(item[["contents"]], newPrefix, isLastItem)
    } else {
      cat(name, "\n")
    }
  }
}

#' Validate the file tree structure
#'
#' @param fileTree The file tree to validate
#' @return Logical indicating if the structure is valid
validateFileTree <- function(fileTree) {
  # Count files and directories
  fileCount <- 0
  dirCount <- 0
  nestedFileCount <- 0
  
  # Helper function to recursively count items
  countItems <- function(node) {
    if (is.null(node) || length(node) == 0) return()
    
    for (name in names(node)) {
      item <- node[[name]]
      if (identical(item$type, "directory")) {
        dirCount <<- dirCount + 1
        # Recursively count items in the directory
        countItems(item[["contents"]])
      } else if (identical(item$type, "file")) {
        if (identical(node, fileTree)) {
          fileCount <<- fileCount + 1
        } else {
          nestedFileCount <<- nestedFileCount + 1
        }
      }
    }
  }
  
  # Count everything
  countItems(fileTree)
  
  # Print results
  cat("File Tree Validation Results:\n")
  cat("----------------------------\n")
  cat("Top-level directories:", dirCount, "\n")
  cat("Top-level files:", fileCount, "\n")
  cat("Nested files:", nestedFileCount, "\n")
  cat("Total items:", dirCount + fileCount + nestedFileCount, "\n\n")
  
  # Show first level of tree
  cat("Top-level structure:\n")
  for (name in names(fileTree)) {
    item <- fileTree[[name]]
    if (identical(item$type, "directory")) {
      itemCount <- length(item[["contents"]])
      cat(sprintf("📁 %s (%d items)\n", name, itemCount))
    } else {
      cat(sprintf("📄 %s\n", name))
    }
  }
  
  # Return TRUE if structure looks good
  return(dirCount > 0 && (fileCount + nestedFileCount) > 0)
}

createTestFileTree <- function() {
  # This time using string values instead of functions
  fileTree <- list(
    `dataset_description.json` = list(
      type = "file",
      file = list(
        name = "dataset_description.json",
        path = "/dataset_description.json",
        text = '{"name":"Test Dataset","description":"A test dataset","@type":"Dataset","@context":"https://schema.org","variableMeasured":[{"name":"id"},{"name":"value"}]}'
      )
    ),
    
    `README.md` = list(
      type = "file",
      file = list(
        name = "README.md",
        path = "/README.md",
        text = '# Test Dataset\nThis is a test dataset for validation.'
      )
    ),
    
    `CHANGES.md` = list(
      type = "file",
      file = list(
        name = "CHANGES.md",
        path = "/CHANGES.md",
        text = '# Changes\n- Initial version'
      )
    ),
    
    `data` = list(
      type = "directory",
      contents = list(
        `sub-01_task-test_data.csv` = list(
          type = "file",
          file = list(
            name = "sub-01_task-test_data.csv",
            path = "/data/sub-01_task-test_data.csv",
            text = 'id,value\n1,10\n2,20'
          )
        )
      )
    ),
    
    `analysis` = list(
      type = "directory",
      contents = list()
    ),
    
    `results` = list(
      type = "directory",
      contents = list()
    ),
    
    `materials` = list(
      type = "directory",
      contents = list()
    ),
    
    `documentation` = list(
      type = "directory",
      contents = list()
    )
  )
  
  return(fileTree)
}