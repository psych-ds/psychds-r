#' Check if a directory is a valid Psych-DS dataset
#'
#' @param dir_path Path to the directory to check
#' @return Logical indicating if the directory is a valid Psych-DS dataset
#' @keywords internal
is_valid_psych_ds_dir <- function(dir_path) {
  # Check if the directory exists
  if (!dir.exists(dir_path)) {
    return(FALSE)
  }

  # Check for required files
  req_files <- c(
    file.path(dir_path, "dataset_description.json"),
    file.path(dir_path, "datapackage.json")
  )

  all(file.exists(req_files))
}

#' Create a Psych-DS directory structure
#'
#' @param base_dir Path to the base directory
#' @param opt_dirs Optional directories to create
#' @return Invisible NULL
#' @keywords internal
create_psych_ds_structure <- function(base_dir, opt_dirs = NULL) {
  # Create the data directory if it doesn't exist
  data_dir <- file.path(base_dir, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  # Create optional directories
  if (!is.null(opt_dirs) && length(opt_dirs) > 0) {
    for (dir in opt_dirs) {
      dir_path <- file.path(base_dir, dir)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
    }
  }

  invisible(NULL)
}

#' Create a dataset_description.json file
#'
#' @param dir_path Path to the directory where the file will be created
#' @param dataset_info List containing dataset information
#' @return Invisible NULL
#' @keywords internal
create_dataset_description <- function(dir_path, dataset_info) {
  # Default template
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
    DatasetDOI = dataset_info$dataset_doi,
    Version = dataset_info$version
  )

  # Remove NULL values
  template <- template[!sapply(template, is.null)]

  # Create JSON file
  json_path <- file.path(dir_path, "dataset_description.json")

  # If jsonlite is installed, use it to write the JSON file
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(template, json_path, pretty = TRUE, auto_unbox = TRUE)
  } else {
    # Fallback to basic JSON creation if jsonlite is not available
    json_content <- "{\n"

    # Add each field
    fields <- names(template)
    for (i in seq_along(fields)) {
      field <- fields[i]
      value <- template[[field]]

      # Format value based on type
      if (is.character(value)) {
        formatted_value <- paste0('"', gsub('"', '\\"', value), '"')
      } else if (is.list(value)) {
        if (length(value) == 0) {
          formatted_value <- "[]"
        } else {
          # Simple array formatting
          items <- sapply(value, function(item) {
            if (is.character(item)) {
              paste0('"', gsub('"', '\\"', item), '"')
            } else {
              as.character(item)
            }
          })
          formatted_value <- paste0("[", paste(items, collapse = ", "), "]")
        }
      } else {
        formatted_value <- as.character(value)
      }

      # Add to JSON string
      json_content <- paste0(
        json_content,
        '  "', field, '": ', formatted_value,
        if (i < length(fields)) ",\n" else "\n"
      )
    }

    json_content <- paste0(json_content, "}\n")

    # Write to file
    writeLines(json_content, json_path)
  }

  invisible(NULL)
}

#' Create a datapackage.json file
#'
#' @param dir_path Path to the directory where the file will be created
#' @param files List of data files
#' @param dataset_info List containing dataset information
#' @return Invisible NULL
#' @keywords internal
create_datapackage <- function(dir_path, files, dataset_info) {
  # Generate resources for each file
  resources <- lapply(files, function(file) {
    list(
      name = tools::file_path_sans_ext(basename(file)),
      path = basename(file),
      format = "csv",
      mediatype = "text/csv",
      encoding = "utf-8"
    )
  })

  # Create datapackage template
  template <- list(
    name = dataset_info$name,
    title = dataset_info$title,
    description = dataset_info$description,
    version = "1.0.0",
    licenses = list(
      list(
        name = dataset_info$license,
        path = "https://opensource.org/licenses/LICENSE",
        title = dataset_info$license
      )
    ),
    contributors = lapply(dataset_info$authors, function(author) {
      list(
        title = author,
        role = "author"
      )
    }),
    resources = resources
  )

  # Remove NULL values
  template <- template[!sapply(template, is.null)]

  # Create JSON file
  json_path <- file.path(dir_path, "datapackage.json")
  jsonlite::write_json(template, json_path, pretty = TRUE, auto_unbox = TRUE)

  invisible(NULL)
}

#' Copy data files to the Psych-DS data directory
#'
#' @param files Vector of file paths to copy
#' @param dest_dir Destination directory
#' @return Character vector of copied file paths
#' @keywords internal
copy_data_files <- function(files, dest_dir) {
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }

  copied_files <- character(length(files))

  for (i in seq_along(files)) {
    file_path <- files[i]
    if (file.exists(file_path)) {
      dest_file <- file.path(dest_dir, basename(file_path))
      file.copy(file_path, dest_file, overwrite = TRUE)
      copied_files[i] <- dest_file
    }
  }

  copied_files
}

#' Extract CSV file structure for dictionary creation
#'
#' @param file_path Path to the CSV file
#' @return Data frame with column information
#' @keywords internal
extract_csv_structure <- function(file_path) {
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

  column_info
}

#' Generate a data dictionary from CSV files
#'
#' @param files Vector of CSV file paths
#' @return List of data frames, one for each file
#' @keywords internal
generate_data_dictionary <- function(files) {
  dict_list <- list()

  for (file in files) {
    if (file.exists(file)) {
      dict_list[[basename(file)]] <- extract_csv_structure(file)
    }
  }

  dict_list
}

#' Validate a Psych-DS Dataset
#'
#' Validates a dataset directory against the full 'Psych-DS' specification
#' using the same validator used by the 'psychds-validator' command-line tool.
#'
#' @param dir_path Path to the 'Psych-DS' dataset directory to validate.
#' @param json Logical. If \code{TRUE}, output results as JSON instead of
#'   formatted text. Default is \code{FALSE}.
#' @param verbose Logical. If \code{TRUE}, show verbose output including
#'   warnings. Default is \code{FALSE}.
#' @param use_events Logical. If \code{TRUE}, show live checklist progress
#'   output. Default is \code{FALSE}.
#'
#' @return Invisibly returns the exit status. Called for its side effect
#'   of printing validation results to the console.
#'
#' @details
#' Requires 'Node.js' to be installed and available on the system PATH.
#' Install 'Node.js' from \url{https://nodejs.org}.
#'
#' @examples
#' \dontrun{
#' # Standard text output
#' validate_dataset("path/to/my-study")
#'
#' # JSON output for programmatic use
#' validate_dataset("path/to/my-study", json = TRUE)
#'
#' # Verbose with warnings
#' validate_dataset("path/to/my-study", verbose = TRUE)
#'
#' # Live checklist
#' validate_dataset("path/to/my-study", use_events = TRUE)
#' }
#' @export
validate_dataset <- function(dir_path,
                             json       = FALSE,
                             verbose    = FALSE,
                             use_events = FALSE) {

  node <- Sys.which("node")
  if (!nchar(node)) {
    stop(
      "validate_dataset() requires Node.js to be installed.\n",
      "Install it from https://nodejs.org\n",
      "Then restart R and try again."
    )
  }

  dir_path <- normalizePath(dir_path, mustWork = TRUE)

  validator_script <- system.file(
    "node", "validate.js",
    package = "psychds"
  )
  if (!nchar(validator_script)) {
    stop("Bundled validator script not found. Is the psychds package installed correctly?")
  }

  # Build CLI args the same way the npm CLI does
   args <- c(shQuote(validator_script), shQuote(dir_path))
  if (json)       args <- c(args, "--json")
  if (verbose)    args <- c(args, "--verbose")
  if (use_events) args <- c(args, "--useEvents")

  status <- system2(
    node,
    args   = args,
    stdout = "",
    stderr = ""
  )

  invisible(status)
}


# Build a file tree structure matching what the JS validator expects.
# Mirrors the logic of buildFileTree() in inst/shiny/global.R but
# runs entirely in R without a Shiny session.
#' @keywords internal
.build_validation_tree <- function(dir_path) {

  insert_node <- function(tree, parts, info) {
    if (length(parts) == 0) return(tree)
    key <- parts[[1]]
    if (length(parts) == 1) {
      tree[[key]] <- info
    } else {
      if (is.null(tree[[key]])) {
        tree[[key]] <- list(type = "directory", name = key, contents = list())
      }
      tree[[key]]$contents <- insert_node(tree[[key]]$contents, parts[-1], info)
    }
    tree
  }

  all_files <- list.files(dir_path, recursive = TRUE,
                          full.names = FALSE, all.files = FALSE)
  all_dirs  <- list.dirs(dir_path,  recursive = TRUE,
                         full.names = FALSE)
  all_dirs  <- all_dirs[nchar(all_dirs) > 0]

  tree <- list()

  # Insert directories first
  for (d in all_dirs) {
    parts <- strsplit(d, .Platform$file.sep, fixed = TRUE)[[1]]
    tree <- insert_node(tree, parts,
                        list(type = "directory", name = parts[length(parts)],
                             contents = list()))
  }

  # Insert files with content for JSON/text files
  for (f in all_files) {
    full <- file.path(dir_path, f)
    parts <- strsplit(f, .Platform$file.sep, fixed = TRUE)[[1]]
    ext <- tolower(tools::file_ext(f))

    file_info <- list(
      type    = "file",
      name    = basename(f),
      path    = f,
      content = if (ext %in% c("json", "csv", "tsv", "txt")) {
        tryCatch(paste(readLines(full, warn = FALSE), collapse = "\n"),
                 error = function(e) "")
      } else {
        ""
      }
    )
    tree <- insert_node(tree, parts, file_info)
  }

  list(type = "directory", name = basename(dir_path), contents = tree)
}


# Parse a raw validation result list into clean R structure.
#' @keywords internal
.parse_validation_result <- function(raw, verbose = FALSE) {

  errors   <- character()
  warnings <- character()
  steps    <- list()

  step_status <- raw$stepStatus

  if (!is.null(step_status)) {
    for (entry in step_status) {
      # Each entry is a two-element list: [step_key, step_info]
      if (length(entry) < 2) next
      key  <- entry[[1]]
      info <- entry[[2]]

      complete <- isTRUE(info$complete)
      success  <- isTRUE(info$success)
      issue    <- info$issue  # NULL if no issue

      steps[[key]] <- list(
        complete = complete,
        success  = success,
        issue    = issue
      )

      if (complete && !success && !is.null(issue)) {
        reason <- issue$reason %||% "Unknown error"
        errors <- c(errors, paste0("[", key, "] ", reason))
      }

      if (verbose) {
        status_label <- if (!complete) "SKIP"
                        else if (success) "PASS"
                        else "FAIL"
        message(sprintf("  %-6s %s", status_label, key))
        if (!success && !is.null(issue)) {
          message("         ", issue$reason %||% "")
        }
      }
    }
  }

  valid <- isTRUE(raw$valid) && length(errors) == 0

  if (verbose) {
    message(if (valid) "\nResult: VALID" else "\nResult: INVALID")
  }

  list(
    valid        = valid,
    errors       = errors,
    warnings     = warnings,
    step_results = steps
  )
}

#' Check and Load Package Dependencies
#' 
#' This function provides robust package management following CRAN policies.
#' It checks versions, handles conflicts, and provides clear user feedback.
#' 
#' @param min_versions Named list of minimum required package versions
#' @param recommended_versions Named list of recommended package versions  
#' @param startup_mode Character: "strict", "recommended", or "minimal"
#' @return Logical indicating success
#' @export
check_dependencies <- function(
  min_versions = NULL,
  recommended_versions = NULL, 
  startup_mode = getOption("psychds.startup_mode", "recommended")
) {
  
  # Define minimum required versions (absolutely necessary for app to function)
  if (is.null(min_versions)) {
    min_versions <- list(
      shiny = "1.7.0",           # Minimum for modern JS handling
      shinydashboard = "0.7.0",   # Basic dashboard functionality
      shinyjs = "2.0.0",          # JavaScript integration
      shinyFiles = "0.9.0",       # File system access
      DT = "0.20",                # DataTables functionality
      jsonlite = "1.7.0",         # JSON parsing
      sortable = "0.4.0"          # Drag-and-drop support
    )
  }
  
  # Define recommended versions (known to work well together)
  if (is.null(recommended_versions)) {
    recommended_versions <- list(
      shiny = "1.8.0",
      shinydashboard = "0.7.2", 
      shinyjs = "2.1.0",
      shinyFiles = "0.9.3",
      DT = "0.31",
      jsonlite = "1.8.0",
      sortable = "0.5.0",
      zip = "2.2.0",
      pointblank = "0.11.0",
      osfr = "0.2.9"
    )
  }
  
  # Core packages that must be present
  core_packages <- c("shiny", "shinydashboard", "shinyjs", "shinyFiles", 
                     "DT", "jsonlite", "tools", "utils")
  
  # Optional packages that enhance functionality
  optional_packages <- c("sortable", "zip", "pointblank", "osfr")
  
  # Initialize status tracking
  status <- list(
    missing_core = character(),
    missing_optional = character(),
    version_conflicts = character(),
    warnings = character(),
    success = TRUE
  )
  
  # ---- Step 1: Check Core Package Availability ----
  message("Checking package dependencies for psychds...")
  
  for (pkg in core_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      status$missing_core <- c(status$missing_core, pkg)
      status$success <- FALSE
    }
  }
  
  # ---- Step 2: Check Optional Package Availability ----
  for (pkg in optional_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      status$missing_optional <- c(status$missing_optional, pkg)
    }
  }
  
  # ---- Step 3: Version Checking (if packages are available) ----
  installed_packages <- core_packages[!core_packages %in% status$missing_core]
  
  for (pkg in installed_packages) {
    current_version <- tryCatch(
      utils::packageVersion(pkg),
      error = function(e) NULL
    )
    
    if (!is.null(current_version)) {
      # Check against minimum version
      if (pkg %in% names(min_versions)) {
        min_ver <- package_version(min_versions[[pkg]])
        if (current_version < min_ver) {
          status$version_conflicts <- c(
            status$version_conflicts,
            sprintf("%s (have %s, need >= %s)", pkg, current_version, min_ver)
          )
          status$success <- FALSE
        }
      }
      
      # Check against recommended version (warning only)
      if (startup_mode == "recommended" && pkg %in% names(recommended_versions)) {
        rec_ver <- package_version(recommended_versions[[pkg]])
        if (current_version < rec_ver) {
          status$warnings <- c(
            status$warnings,
            sprintf("%s: version %s is older than recommended %s", 
                   pkg, current_version, rec_ver)
          )
        }
      }
    }
  }
  
  # ---- Step 4: Check for Known Conflicts ----
  status <- check_known_conflicts(status)
  
  # ---- Step 5: Report Results ----
  report_dependency_status(status, startup_mode)
  
  # ---- Step 6: Offer Installation Help (if needed) ----
  if (!status$success && interactive()) {
    offer_installation_help(status, min_versions)
  }
  
  return(status$success)
}

#' Check for Known Package Conflicts
#' @param status Current status list
#' @return Updated status list
#' @noRd
check_known_conflicts <- function(status) {
  # Check for shiny namespace conflicts
  if ("package:shiny" %in% search()) {
    loaded_shiny_ver <- utils::packageVersion("shiny")
    # Check if miniUI or other packages might cause conflicts
    conflicting_pkgs <- c("miniUI", "manipulateWidget", "colourpicker")
    loaded_conflicts <- intersect(
      gsub("package:", "", search()),
      conflicting_pkgs
    )
    
    if (length(loaded_conflicts) > 0) {
      status$warnings <- c(
        status$warnings,
        sprintf("Potential conflict: %s is loaded and may interfere with Shiny %s",
               paste(loaded_conflicts, collapse = ", "), loaded_shiny_ver)
      )
    }
  }
  
  # Check for DT/crosstalk version compatibility
  if (requireNamespace("DT", quietly = TRUE) && 
      requireNamespace("crosstalk", quietly = TRUE)) {
    dt_ver <- utils::packageVersion("DT")
    ct_ver <- utils::packageVersion("crosstalk")
    
    # Known incompatible combinations
    if (dt_ver >= "0.30" && ct_ver < "1.2.0") {
      status$warnings <- c(
        status$warnings,
        "DT >= 0.30 requires crosstalk >= 1.2.0 for full compatibility"
      )
    }
  }
  
  return(status)
}

#' Report Dependency Check Results
#' @param status Status list from check_dependencies
#' @param mode Startup mode
#' @noRd
report_dependency_status <- function(status, mode) {
  
  # Use cli package for nice output if available, otherwise basic messages
  use_cli <- requireNamespace("cli", quietly = TRUE)
  
  if (length(status$missing_core) > 0) {
    msg <- sprintf("Missing required packages: %s", 
                  paste(status$missing_core, collapse = ", "))
    if (use_cli) {
      cli::cli_alert_danger(msg)
    } else {
      message("ERROR: ", msg)
    }
  }
  
  if (length(status$missing_optional) > 0) {
    msg <- sprintf("Missing optional packages (some features may be unavailable): %s",
                  paste(status$missing_optional, collapse = ", "))
    if (use_cli) {
      cli::cli_alert_warning(msg)
    } else {
      message("WARNING: ", msg)
    }
  }
  
  if (length(status$version_conflicts) > 0) {
    msg <- "Package version conflicts:"
    if (use_cli) {
      cli::cli_alert_danger(msg)
      for (conflict in status$version_conflicts) {
        cli::cli_alert_danger(paste0("  ", conflict))
      }
    } else {
      message("ERROR: ", msg)
      for (conflict in status$version_conflicts) {
        message("  - ", conflict)
      }
    }
  }
  
  if (length(status$warnings) > 0 && mode != "minimal") {
    if (use_cli) {
      cli::cli_alert_info("Package compatibility notes:")
      for (warning in status$warnings) {
        cli::cli_alert_info(paste0("  ", warning))
      }
    } else {
      message("INFO: Package compatibility notes:")
      for (warning in status$warnings) {
        message("  - ", warning)
      }
    }
  }
  
  if (status$success) {
    msg <- "All required dependencies satisfied"
    if (use_cli) {
      cli::cli_alert_success(msg)
    } else {
      message("SUCCESS: ", msg)
    }
  }
}

#' Offer Installation Help
#' @param status Status list
#' @param min_versions Minimum version requirements
#' @noRd
offer_installation_help <- function(status, min_versions) {
  
  cat("\n")
  message("=== Installation Instructions ===")
  
  # Combine missing and outdated packages
  packages_to_install <- c(status$missing_core)
  
  # Add packages with version conflicts
  if (length(status$version_conflicts) > 0) {
    # Extract package names from version conflict messages
    conflict_pkgs <- gsub(" \\(.*", "", status$version_conflicts)
    packages_to_install <- c(packages_to_install, conflict_pkgs)
  }
  
  packages_to_install <- unique(packages_to_install)
  
  if (length(packages_to_install) > 0) {
    message("\nTo install missing/outdated packages, run:")
    cat(sprintf('install.packages(c(%s))\n', 
               paste0('"', packages_to_install, '"', collapse = ", ")))
    
    message("\nOr for specific versions from CRAN archives:")
    for (pkg in packages_to_install) {
      if (pkg %in% names(min_versions)) {
        cat(sprintf('# For %s >= %s:\n', pkg, min_versions[[pkg]]))
        cat(sprintf('remotes::install_version("%s", version = "%s")\n', 
                   pkg, min_versions[[pkg]]))
      }
    }
  }
  
  if (length(status$missing_optional) > 0) {
    message("\nOptional packages for full functionality:")
    cat(sprintf('install.packages(c(%s))\n',
               paste0('"', status$missing_optional, '"', collapse = ", ")))
  }
  
  message("\nAfter installation, restart R and try again:")
  message('.rs.restartR() # In RStudio')
  message('# OR')
  message('q("no") # Then restart R')
}

#' Safe Package Loading with Conflict Resolution
#' 
#' Loads packages while handling potential conflicts
#' @param package_name Name of package to load
#' @param required_version Minimum version required (optional)
#' @param unload_conflicts Attempt to unload conflicting packages
#' @return Logical indicating success
#' @noRd
safe_load_package <- function(package_name, 
                              required_version = NULL,
                              unload_conflicts = FALSE) {
  
  # Check if package is available
  if (!requireNamespace(package_name, quietly = TRUE)) {
    return(FALSE)
  }
  
  # Check version if specified
  if (!is.null(required_version)) {
    current_ver <- utils::packageVersion(package_name)
    if (current_ver < package_version(required_version)) {
      message(sprintf("Package %s version %s is older than required %s",
                     package_name, current_ver, required_version))
      return(FALSE)
    }
  }
  
  # Check if already loaded
  pkg_search_name <- paste0("package:", package_name)
  if (pkg_search_name %in% search()) {
    # Already loaded - check if it's the right version
    if (!is.null(required_version)) {
      loaded_ver <- utils::packageVersion(package_name)
      if (loaded_ver < package_version(required_version)) {
        if (unload_conflicts) {
          # Try to unload and reload
          tryCatch({
            detach(pkg_search_name, unload = TRUE, character.only = TRUE)
            library(package_name, character.only = TRUE)
            return(TRUE)
          }, error = function(e) {
            message(sprintf("Could not reload %s: %s", package_name, e$message))
            return(FALSE)
          })
        } else {
          return(FALSE)
        }
      }
    }
    return(TRUE)  # Already loaded with acceptable version
  }
  
  # Try to load the package
  tryCatch({
    library(package_name, character.only = TRUE)
    TRUE
  }, error = function(e) {
    message(sprintf("Failed to load %s: %s", package_name, e$message))
    FALSE
  })
}

#' Initialize Shiny App with Robust Package Management
#' 
#' Main entry point that ensures all dependencies are met
#' @param app_dir Directory containing the Shiny app
#' @param ... Additional arguments passed to shiny::runApp
#' @export
run_app_safe <- function(app_dir = system.file("app", package = "psychds"),
                        ...) {
  
  # ---- Pre-flight Checks ----
  
  # 1. Check R version
  r_version <- getRversion()
  if (r_version < "4.0.0") {
    warning(sprintf("R version %s detected. This app is tested with R >= 4.0.0", r_version))
  }
  
  # 2. Check if we're in RStudio and version
  in_rstudio <- Sys.getenv("RSTUDIO") == "1"
  if (in_rstudio) {
    rs_version <- tryCatch(
      rstudioapi::versionInfo()$version,
      error = function(e) NULL
    )
    
    if (!is.null(rs_version) && rs_version < "1.4") {
      warning("Old RStudio version detected. Some features may not work properly.")
      message("Consider updating RStudio or running in external browser.")
    }
  }
  
  # 3. Check dependencies
  if (!check_dependencies()) {
    stop("Dependency check failed. Please install required packages and try again.")
  }
  
  # 4. Load packages in correct order with conflict handling
  package_load_order <- c(
    "shiny",           # Load first
    "shinydashboard",  # Before shinyjs
    "shinyjs",         # Before custom JS
    "shinyFiles",
    "DT",
    "jsonlite",
    "tools",
    "utils"
  )
  
  # Optional packages (don't fail if not available)
  optional_packages <- c("sortable", "zip", "pointblank", "osfr")
  
  message("Loading required packages...")
  for (pkg in package_load_order) {
    if (!safe_load_package(pkg)) {
      stop(sprintf("Failed to load required package: %s", pkg))
    }
  }
  
  # Load optional packages (with warnings if missing)
  for (pkg in optional_packages) {
    if (!safe_load_package(pkg)) {
      message(sprintf("Optional package %s not available. Some features may be limited.", pkg))
    }
  }
  
  # 5. Set recommended options, restoring originals on exit
  old_opts <- options(
    shiny.maxRequestSize = 100 * 1024^2,  # 100MB upload limit
    shiny.sanitize.errors = FALSE,        # Show detailed errors during development
    shiny.reactlog = FALSE                # Disable reactlog unless debugging
  )
  on.exit(options(old_opts), add = TRUE)
  
  # 6. Browser handling for RStudio viewer issues
  if (in_rstudio) {
    # Check if we should force external browser
    force_browser <- getOption("psychds.force_browser", FALSE)
    
    # Detect potential viewer issues
    if (rs_version < "2023.06.0" || force_browser) {
      message("Opening in external browser for better compatibility...")
      old_browser_opt <- options(shiny.launch.browser = TRUE)
      on.exit(options(old_browser_opt), add = TRUE)
    }
  }
  
  # 7. Check app directory exists
  if (!dir.exists(app_dir)) {
    stop(sprintf("App directory not found: %s", app_dir))
  }
  
  # Check for required app files
  required_files <- c("ui.R", "server.R", "global.R")
  missing_files <- required_files[!file.exists(file.path(app_dir, required_files))]
  
  if (length(missing_files) > 0) {
    stop(sprintf("Missing required app files: %s", 
                paste(missing_files, collapse = ", ")))
  }
  
  # 8. Run the app with error handling
  message("Starting psychds Shiny application...")
  
  tryCatch({
    shiny::runApp(app_dir, ...)
  }, error = function(e) {
    message("\n=== Application failed to start ===")
    message("Error: ", e$message)
    message("\nTroubleshooting steps:")
    message("1. Restart R: .rs.restartR() or q('no')")
    message("2. Update packages: update.packages(ask = FALSE)")
    message("3. Force browser mode: options(psychds.force_browser = TRUE)")
    message("4. Check package versions: psychds::check_dependencies()")
    stop("Application startup failed", call. = FALSE)
  })
}

