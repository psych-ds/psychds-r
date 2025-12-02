#' Psych-DS Shiny App
#'
#' Main entry point for the Psych-DS Shiny application.
#' This file uses minimal package loading to avoid conflicts.
#'
#' IMPORTANT: This file should NOT auto-install packages per CRAN policy.
#' Package installation must be explicitly requested by the user.

# ================================================================================
# Startup Configuration
# ================================================================================

# Check if we're running as a package or standalone
is_package_mode <- function() {
  # Check if we're running from an installed package
  !is.null(tryCatch(
    system.file(package = "psychds"),
    error = function(e) NULL
  )) && system.file(package = "psychds") != ""
}

# ================================================================================
# Dependency Checking (No Auto-Install)
# ================================================================================

# Define required packages and their minimum versions
required_packages <- list(
  shiny = "1.7.0",
  shinydashboard = "0.7.0",
  shinyjs = "2.0.0",
  shinyFiles = "0.9.0",
  DT = "0.20",
  jsonlite = "1.7.0"
)

# Optional packages that enhance functionality
optional_packages <- list(
  sortable = "0.4.0",
  zip = "2.0.0",
  pointblank = "0.10.0"
)

# Base R packages that should always be available
base_packages <- c("tools", "utils", "stats", "methods")

# ================================================================================
# Check Dependencies Function
# ================================================================================

check_app_dependencies <- function() {
  all_ok <- TRUE
  missing_packages <- character()
  version_issues <- character()
  optional_missing <- character()
  
  # Check required packages
  for (pkg_name in names(required_packages)) {
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg_name)
      all_ok <- FALSE
    } else {
      # Check version
      current_ver <- utils::packageVersion(pkg_name)
      required_ver <- required_packages[[pkg_name]]
      
      if (current_ver < required_ver) {
        version_issues <- c(version_issues, 
                          sprintf("%s (have %s, need >= %s)", 
                                 pkg_name, current_ver, required_ver))
        all_ok <- FALSE
      }
    }
  }
  
  # Check optional packages (just warn, don't fail)
  for (pkg_name in names(optional_packages)) {
    if (!requireNamespace(pkg_name, quietly = TRUE)) {
      optional_missing <- c(optional_missing, pkg_name)
    }
  }
  
  # Report issues
  if (!all_ok) {
    message("\n========================================")
    message("  DEPENDENCY CHECK FAILED")
    message("========================================\n")
    
    if (length(missing_packages) > 0) {
      message("Missing required packages:")
      for (pkg in missing_packages) {
        message("  - ", pkg)
      }
      message("\nTo install missing packages, run:")
      message('install.packages(c(', 
             paste0('"', missing_packages, '"', collapse = ", "), '))\n')
    }
    
    if (length(version_issues) > 0) {
      message("Package version issues:")
      for (issue in version_issues) {
        message("  - ", issue)
      }
      message("\nTo update packages, run:")
      message('update.packages(ask = FALSE)\n')
    }
    
    message("After installing/updating, restart R and try again:")
    message('  - In RStudio: Session -> Restart R')
    message('  - Or run: .rs.restartR()\n')
    
    stop("Cannot start app due to missing dependencies", call. = FALSE)
  }
  
  # Warn about optional packages
  if (length(optional_missing) > 0) {
    message("\nNote: Optional packages not installed (some features may be unavailable):")
    for (pkg in optional_missing) {
      message("  - ", pkg)
    }
    message("To enable all features, consider installing:")
    message('install.packages(c(', 
           paste0('"', optional_missing, '"', collapse = ", "), '))\n')
  }
  
  return(TRUE)
}

# ================================================================================
# Safe Package Loading
# ================================================================================

safe_library <- function(package_name, required = TRUE) {
  # Try to load the package
  success <- suppressPackageStartupMessages(
    requireNamespace(package_name, quietly = TRUE)
  )
  
  if (success) {
    # Check if already attached to avoid conflicts
    if (!paste0("package:", package_name) %in% search()) {
      suppressPackageStartupMessages(
        library(package_name, character.only = TRUE)
      )
    }
    return(TRUE)
  } else if (required) {
    stop(sprintf("Required package '%s' could not be loaded", package_name), 
         call. = FALSE)
  } else {
    message(sprintf("Optional package '%s' not available", package_name))
    return(FALSE)
  }
}

# ================================================================================
# Main Execution
# ================================================================================

# Only run if this is the main file being sourced
if (!interactive() || !exists("PSYCHDS_LOADING", envir = .GlobalEnv)) {
  
  # Set flag to prevent recursive loading
  assign("PSYCHDS_LOADING", TRUE, envir = .GlobalEnv)
  on.exit(rm("PSYCHDS_LOADING", envir = .GlobalEnv))
  
  tryCatch({
    # Step 1: Check dependencies (no auto-install)
    check_app_dependencies()
    
    # Step 2: Load packages in specific order to minimize conflicts
    message("Loading packages...")
    
    # Core packages (required)
    safe_library("shiny", required = TRUE)
    safe_library("shinydashboard", required = TRUE)
    safe_library("shinyjs", required = TRUE)
    safe_library("shinyFiles", required = TRUE)
    safe_library("DT", required = TRUE)
    safe_library("jsonlite", required = TRUE)
    
    # Base R packages (should always be available)
    safe_library("tools", required = TRUE)
    safe_library("utils", required = TRUE)
    
    # Optional packages (don't fail if missing)
    has_sortable <- safe_library("sortable", required = FALSE)
    has_zip <- safe_library("zip", required = FALSE)
    has_pointblank <- safe_library("pointblank", required = FALSE)
    
    # Store feature availability in options for the app to check
    options(
      psychds.has_sortable = has_sortable,
      psychds.has_zip = has_zip,
      psychds.has_pointblank = has_pointblank
    )
    
    # Step 3: Source app components
    message("Loading app components...")
    
    # Check if files exist
    required_files <- c("global.R", "ui.R", "server.R")
    for (file in required_files) {
      if (!file.exists(file)) {
        stop(sprintf("Required file '%s' not found. Please ensure you're in the app directory.", file),
             call. = FALSE)
      }
    }
    
    # Source in correct order
    source("global.R", local = TRUE)
    source("ui.R", local = TRUE)
    source("server.R", local = TRUE)
    
    # Step 4: Configure Shiny options
    options(
      shiny.maxRequestSize = 100 * 1024^2,  # 100MB upload limit
      shiny.sanitize.errors = FALSE         # Show detailed errors during development
    )
    
    # Step 5: Handle RStudio viewer issues
    if (Sys.getenv("RSTUDIO") == "1") {
      # Check RStudio version if possible
      rs_version <- tryCatch({
        if (requireNamespace("rstudioapi", quietly = TRUE)) {
          rstudioapi::versionInfo()$version
        } else {
          NULL
        }
      }, error = function(e) NULL)
      
      # Warn about potential viewer issues
      if (!is.null(rs_version) && rs_version < "2023.06.0") {
        message("\n========================================")
        message("  RSTUDIO VIEWER COMPATIBILITY NOTE")
        message("========================================")
        message("Your RStudio version may have issues with the app viewer.")
        message("If buttons are unresponsive or content doesn't load:")
        message("  1. Click 'Open in Browser' button in the viewer")
        message("  2. Or run: options(shiny.launch.browser = TRUE)")
        message("========================================\n")
      }
    }
    
    # Step 6: Launch the application
    message("Starting Psych-DS application...")
    shinyApp(ui = ui, server = server)
    
  }, error = function(e) {
    # Clean error reporting
    message("\n========================================")
    message("  APPLICATION STARTUP ERROR")
    message("========================================")
    message("Error: ", conditionMessage(e))
    message("\nTroubleshooting steps:")
    message("  1. Check all files are present (global.R, ui.R, server.R)")
    message("  2. Ensure you're in the correct directory")
    message("  3. Restart R and try again: .rs.restartR()")
    message("  4. Check package versions with sessionInfo()")
    message("========================================\n")
    stop("Failed to start application", call. = FALSE)
  })
}
