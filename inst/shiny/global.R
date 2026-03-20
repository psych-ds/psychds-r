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

#' List all data files (CSV and TSV) in a directory (recursively)
#'
#' @param dir_path Character string of directory path to scan
#' @param recursive Logical indicating whether to search recursively
#' @param verbose Logical; if TRUE prints progress messages to the console.
#'   Default is FALSE.
#' @return List of data files (relative paths from dir_path)
list_data_files <- function(dir_path, recursive = TRUE, verbose = FALSE) {
  if (verbose) message("Listing data files in: ", dir_path)

  # Ensure the directory exists
  if (!dir.exists(dir_path)) {
    if (verbose) message("Directory does not exist or is not accessible")
    return(character(0))
  }

  tryCatch({
    # Get all files (with full paths)
    all_files <- list.files(dir_path, recursive = recursive, full.names = TRUE)
    if (verbose) message("Found ", length(all_files), " total files")

    # Filter for CSV and TSV files (case insensitive)
    data_files <- all_files[grepl("\\.(csv|tsv)$", all_files, ignore.case = TRUE)]
    if (verbose) message("Found ", length(data_files), " data files (CSV/TSV)")

    # Get JUST the relative paths without using regex
    # This is the most reliable approach
    rel_paths <- list.files(dir_path, recursive = recursive, pattern = "\\.(csv|tsv)$",
                            ignore.case = TRUE, full.names = FALSE)

    if (verbose) {
      message("Relative paths (first 3): ", paste(head(rel_paths, 3), collapse=", "),
              ifelse(length(rel_paths) > 3, "...", ""))
    }

    return(rel_paths)
  }, error = function(e) {
    if (verbose) message("Error listing files: ", e$message)
    return(character(0))
  })
}

#' Extract column information from a data file (CSV or TSV)
#'
#' @param file_path Path to the data file
#' @return Data frame with column information
extract_data_structure <- function(file_path) {
  tryCatch({
    # Read the first few rows to determine column types
    # Detect separator based on file extension
    file_ext <- tolower(tools::file_ext(file_path))
    sep <- if (file_ext == "tsv") "\t" else ","
    data <- utils::read.csv(file_path, nrows = 100, stringsAsFactors = FALSE, sep = sep)

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

#' Read a data file (CSV or TSV) with appropriate separator
#'
#' Detects the file type from extension and uses the correct separator.
#' Drop-in replacement for read.csv that handles both CSV and TSV.
#'
#' @param file_path Path to the data file
#' @param ... Additional arguments passed to read.csv/read.delim
#' @return Data frame
read_data_file <- function(file_path, ...) {
  file_ext <- tolower(tools::file_ext(file_path))
  sep <- if (file_ext == "tsv") "\t" else ","
  utils::read.csv(file_path, sep = sep, ...)
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

#' Enhanced File Tree Builder that includes empty directories
#'
#' Recursively builds a nested file tree that includes both files and directories,
#' even if directories are empty.
#'
#' @param directory Path to the dataset directory
#' @param verbose Logical; if TRUE prints progress messages to the console.
#'   Default is FALSE.
#' @return List representing the file tree
buildFileTree <- function(directory, verbose = FALSE) {
  directory <- normalizePath(directory, mustWork = TRUE)
  if (verbose) message("Building file tree for: ", directory)
  
  # Get all files first
  all_files <- list.files(directory, recursive = TRUE, full.names = TRUE)
  all_dirs <- list.dirs(directory, recursive = TRUE, full.names = TRUE)
  all_dirs <- all_dirs[all_dirs != directory]
  
  total_files <- length(all_files)
  if (verbose) message("Found ", total_files, " files to process")
  
  # Helper to check binary files
  isLikelyBinary <- function(path) {
    tryCatch({
      con <- file(path, "rb")
      bytes <- readBin(con, "raw", n = 1024)
      close(con)
      return(any(bytes == as.raw(0)))
    }, error = function(e) TRUE)
  }
  
  # Read file content
  readFileText <- function(path) {
    file_ext <- tolower(tools::file_ext(path))
    
    # Skip known binary types
    if (file_ext %in% c("xlsx", "xls", "pdf", "zip", "mp4", "jpg", "png")) {
      return("")
    }
    
    # Read CSV files completely
    if (file_ext == "csv" || file_ext == "tsv") {
      tryCatch({
        # Read the entire CSV file
        content <- readLines(path, warn = FALSE, encoding = "UTF-8")
        
        # Check for encoding issues
        if (any(is.na(content))) {
          # Try latin1 if UTF-8 fails
          content <- readLines(path, warn = FALSE, encoding = "latin1")
        }
        
        return(paste(content, collapse = "\n"))
        
      }, error = function(e) {
        if (verbose) message("Error reading data file: ", basename(path), " - ", e$message)
        return(paste0("ERROR: ", e$message))
      })
    }
    
    # Read other text files
    if (file_ext %in% c("json", "md", "txt")) {
      if (isLikelyBinary(path)) {
        return("ERROR_BINARY_FILE")
      }
      
      tryCatch({
        content <- readLines(path, warn = FALSE, encoding = "UTF-8")
        return(paste(content, collapse = "\n"))
      }, error = function(e) {
        return(paste0("ERROR: ", e$message))
      })
    }
    
    return("")
  }
  
  # Helper to build tree structure
  insertIntoTree <- function(tree, parts, itemInfo) {
    if (length(parts) == 1) {
      tree[[parts[1]]] <- itemInfo
    } else {
      if (is.null(tree[[parts[1]]])) {
        tree[[parts[1]]] <- list(type = "directory", contents = list())
      }
      tree[[parts[1]]]$contents <- insertIntoTree(
        tree[[parts[1]]]$contents, 
        parts[-1], 
        itemInfo
      )
    }
    return(tree)
  }
  
  # Initialize tree
  fileTree <- list()
  
  # Add all directories first
  for (dirPath in all_dirs) {
    relPath <- sub(paste0("^", directory, "/?"), "", dirPath)
    relPath <- gsub("\\\\", "/", relPath)
    parts <- strsplit(relPath, "/")[[1]]
    
    fileTree <- insertIntoTree(fileTree, parts, list(
      type = "directory",
      contents = list()
    ))
  }
  
  # Process files with progress bar
  if (total_files > 10) {
    # Use progress bar for larger datasets
    withProgress(message = 'Reading dataset files', value = 0, {
      
      for (i in seq_along(all_files)) {
        filePath <- all_files[i]
        
        if (!dir.exists(filePath)) {
          # Parse file path
          relPath <- sub(paste0("^", directory, "/?"), "", filePath)
          relPath <- gsub("\\\\", "/", relPath)
          parts <- strsplit(relPath, "/")[[1]]
          fileName <- parts[[length(parts)]]
          
          # Read file content (this reads the ENTIRE file)
          fileContent <- readFileText(filePath)
          
          # Create file entry
          fileInfo <- list(
            type = "file",
            file = list(
              name = fileName,
              path = paste0("/", relPath),
              text = fileContent
            )
          )
          
          # Add to tree
          fileTree <- insertIntoTree(fileTree, parts, fileInfo)
        }
        
        # Update progress every 5 files or at the end
        if (i %% 5 == 0 || i == total_files) {
          setProgress(
            value = i / total_files,
            detail = paste('Processing file', i, 'of', total_files)
          )
          
          # CRITICAL: Allow Shiny to process other events
          # This prevents the UI from freezing
          shinyjs::delay(1, {})  # 1ms delay
          
          # Alternative if shinyjs not available:
          # Sys.sleep(0.001)
        }
      }
    })
  } else {
    # For small datasets, just process without progress bar
    for (filePath in all_files) {
      if (!dir.exists(filePath)) {
        relPath <- sub(paste0("^", directory, "/?"), "", filePath)
        relPath <- gsub("\\\\", "/", relPath)
        parts <- strsplit(relPath, "/")[[1]]
        fileName <- parts[[length(parts)]]
        
        fileContent <- readFileText(filePath)
        
        fileInfo <- list(
          type = "file",
          file = list(
            name = fileName,
            path = paste0("/", relPath),
            text = fileContent
          )
        )
        
        fileTree <- insertIntoTree(fileTree, parts, fileInfo)
      }
    }
  }
  
  if (verbose) message("Successfully processed all ", total_files, " files")
  
  # Clean up memory for large datasets
  if (total_files > 50) {
    gc()
  }
  
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

