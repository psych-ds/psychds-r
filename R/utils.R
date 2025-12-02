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

#' Validate a Psych-DS dataset
#'
#' @param dir_path Path to the Psych-DS dataset directory
#' @return List containing validation results
#' @keywords internal
validate_dataset <- function(dir_path) {
  # Placeholder for validation logic
  # In a real implementation, this would check:
  # 1. Required files exist
  # 2. JSON files validate against schemas
  # 3. CSV files match the data dictionary

  results <- list(
    valid = is_valid_psych_ds_dir(dir_path),
    messages = character(0),
    warnings = character(0),
    errors = character(0)
  )

  # If not valid, add error message
  if (!results$valid) {
    results$errors <- c(results$errors, "Directory is not a valid Psych-DS dataset")
  }

  results
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
      RStudio.Version()$version,
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
  
  # 5. Set recommended options
  options(
    shiny.maxRequestSize = 100 * 1024^2,  # 100MB upload limit
    shiny.sanitize.errors = FALSE,        # Show detailed errors during development
    shiny.reactlog = FALSE                # Disable reactlog unless debugging
  )
  
  # 6. Browser handling for RStudio viewer issues
  if (in_rstudio) {
    # Check if we should force external browser
    force_browser <- getOption("psychds.force_browser", FALSE)
    
    # Detect potential viewer issues
    if (rs_version < "2023.06.0" || force_browser) {
      message("Opening in external browser for better compatibility...")
      options(shiny.launch.browser = TRUE)
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

check_pdf_capabilities <- function() {
  
  capabilities <- list(
    has_rmarkdown = FALSE,
    has_tinytex = FALSE,
    has_system_latex = FALSE,
    has_pagedown = FALSE,
    can_generate_pdf = FALSE,
    messages = character()
  )
  
  # Check for rmarkdown
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    capabilities$has_rmarkdown <- TRUE
  } else {
    capabilities$messages <- c(capabilities$messages,
      "Package 'rmarkdown' not installed (needed for formatted output)")
  }
  
  # Check for LaTeX
  if (Sys.which("pdflatex") != "") {
    capabilities$has_system_latex <- TRUE
    
    # Check if it's TinyTeX specifically
    if (requireNamespace("tinytex", quietly = TRUE)) {
      if (tinytex::is_tinytex()) {
        capabilities$has_tinytex <- TRUE
      }
    }
  } else {
    capabilities$messages <- c(capabilities$messages,
      "LaTeX not found (needed for PDF generation)")
  }
  
  # Check for pagedown (alternative PDF method)
  if (requireNamespace("pagedown", quietly = TRUE)) {
    capabilities$has_pagedown <- TRUE
  }
  
  # Determine if we can generate PDFs
  capabilities$can_generate_pdf <- 
    capabilities$has_rmarkdown && 
    (capabilities$has_system_latex || capabilities$has_pagedown)
  
  return(capabilities)
}

#' Setup PDF Generation Capabilities
#' 
#' Interactive function to help users set up PDF generation
#' @param force Logical. Force setup even if already capable
#' @export
setup_pdf_generation <- function(force = FALSE) {
  
  caps <- check_pdf_capabilities()
  
  if (caps$can_generate_pdf && !force) {
    message("✓ PDF generation is already set up!")
    
    if (caps$has_tinytex) {
      message("  Using: TinyTeX")
    } else if (caps$has_system_latex) {
      message("  Using: System LaTeX")
    } else if (caps$has_pagedown) {
      message("  Using: pagedown (Chrome-based)")
    }
    
    return(invisible(TRUE))
  }
  
  # Interactive setup
  message("Setting up PDF generation capabilities for psychds...")
  message("")
  
  # Step 1: rmarkdown
  if (!caps$has_rmarkdown) {
    message("Step 1: Installing rmarkdown package...")
    
    if (interactive()) {
      response <- readline("Install rmarkdown? (y/n): ")
      if (tolower(response) == "y") {
        install.packages("rmarkdown")
        caps$has_rmarkdown <- TRUE
      }
    } else {
      message("Run: install.packages('rmarkdown')")
    }
  } else {
    message("✓ Step 1: rmarkdown already installed")
  }
  
  # Step 2: PDF engine
  if (!caps$has_system_latex && !caps$has_pagedown) {
    message("")
    message("Step 2: Choose PDF generation method:")
    message("  1. TinyTeX (recommended, ~100MB, auto-manages LaTeX packages)")
    message("  2. pagedown (uses Chrome, no LaTeX needed)")
    message("  3. Skip (use HTML output only)")
    
    if (interactive()) {
      choice <- readline("Enter choice (1/2/3): ")
      
      if (choice == "1") {
        # Install TinyTeX
        message("Installing TinyTeX...")
        
        if (!requireNamespace("tinytex", quietly = TRUE)) {
          install.packages("tinytex")
        }
        
        # Install TinyTeX distribution
        tinytex::install_tinytex()
        
        # Verify installation
        if (tinytex::is_tinytex()) {
          message("✓ TinyTeX installed successfully!")
          caps$has_tinytex <- TRUE
          caps$has_system_latex <- TRUE
        } else {
          message("⚠ TinyTeX installation may have failed. Try manually:")
          message("  tinytex::install_tinytex()")
        }
        
      } else if (choice == "2") {
        # Install pagedown
        message("Installing pagedown...")
        install.packages("pagedown")
        caps$has_pagedown <- TRUE
        message("✓ pagedown installed successfully!")
        
      } else {
        message("Skipping PDF setup. HTML output will be available.")
      }
      
    } else {
      message("For PDF generation, run one of:")
      message("  Option 1: tinytex::install_tinytex()")
      message("  Option 2: install.packages('pagedown')")
    }
    
  } else if (caps$has_tinytex) {
    message("✓ Step 2: TinyTeX already installed")
  } else if (caps$has_system_latex) {
    message("✓ Step 2: System LaTeX detected")
  } else if (caps$has_pagedown) {
    message("✓ Step 2: pagedown already installed")
  }
  
  # Final check
  caps <- check_pdf_capabilities()
  
  message("")
  if (caps$can_generate_pdf) {
    message("✅ PDF generation setup complete!")
  } else {
    message("⚠ PDF generation not fully configured.")
    message("  HTML output will be available instead.")
  }
  
  return(invisible(caps$can_generate_pdf))
}