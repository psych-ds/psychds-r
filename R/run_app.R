#' Run the Psych-DS Shiny Application
#'
#' Launches the interactive Shiny application for creating, editing, and
#' validating Psych-DS compliant datasets.
#'
#' @param launch.browser Logical or function. Should the app be opened in a
#'   browser? Defaults to `TRUE` in interactive sessions.
#' @param port Integer. Port number for the application. If `NULL`, a random
#'   available port is used.
#' @param host Character. Host IP address. Defaults to `"127.0.0.1"` (localhost).
#' @param startup_mode Character. One of `"strict"`, `"recommended"`, or
#'   `"minimal"`. Controls dependency checking strictness:
#'   \itemize{
#'     \item `"strict"`: Requires all packages at minimum versions
#'     \item `"recommended"`: Warns about outdated packages but continues
#'     \item `"minimal"`: Only checks for critical dependencies
#'   }
#' @param force_browser Logical. Force opening in external browser. Recommended
#'   for RStudio users experiencing viewer issues.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return This function does not return a value. It launches the Shiny
#'   application and blocks until the app is closed.
#'
#' @details
#' The application provides a guided interface for:
#' \itemize{
#'   \item Selecting project directories and data files
#'   \item Adding dataset metadata
#'   \item Standardizing filenames with Psych-DS keywords
#'   \item Generating data dictionaries
#'   \item Validating datasets against the Psych-DS specification
#'   \item Uploading to the Open Science Framework (OSF)
#' }
#'
#' @seealso [check_psychds_deps()] for verifying dependencies before running.
#'
#' @examples
#' \dontrun{
#' # Standard launch
#' run_psych_ds_app()
#'
#' # Force external browser (if RStudio viewer has issues)
#' run_psych_ds_app(force_browser = TRUE)
#'
#' # Minimal checking for faster startup
#' run_psych_ds_app(startup_mode = "minimal")
#'
#' # Specific port
#' run_psych_ds_app(port = 3838)
#' }
#'
#' @export
#' @import shiny
#' @importFrom utils packageVersion
#' 

run_psych_ds_app <- function(
  launch.browser = getOption("shiny.launch.browser", interactive()),
  port = getOption("shiny.port"),
  host = getOption("shiny.host", "127.0.0.1"),
  startup_mode = getOption("psychds.startup_mode", "recommended"),
  force_browser = getOption("psychds.force_browser", FALSE),
  ...
) {
  
  # Store original options to restore on exit
  original_options <- options()
  on.exit({
    # Restore only the options we changed
    options(
      shiny.launch.browser = original_options$shiny.launch.browser,
      psychds.startup_mode = original_options$psychds.startup_mode,
      psychds.force_browser = original_options$psychds.force_browser
    )
  })
  
  # Set options for this session
  options(
    psychds.startup_mode = startup_mode,
    psychds.force_browser = force_browser
  )
  
  # Override browser launching if forced
  if (force_browser) {
    launch.browser <- TRUE
  }
  
  # Find app directory (handle both package and development scenarios)
  app_dir <- system.file("shiny", package = "psychds")
  
  if (app_dir == "") {
    # Development mode - look in inst/shiny
    if (file.exists("inst/shiny/app.R")) {
      app_dir <- "inst/shiny"
      message("Running in development mode from: ", app_dir)
    } else if (file.exists("app.R")) {
      app_dir <- getwd()
      message("Running in development mode from: ", app_dir)
    } else {
      stop("Cannot find psychds app files. Please ensure the package is properly installed.")
    }
  }
  
  # Run with comprehensive checking
  run_app_safe(
    app_dir = app_dir,
    launch.browser = launch.browser,
    port = port,
    host = host,
    ...
  )
}

#' Check psychds Dependencies
#'
#' Verifies that all required and optional package dependencies are installed
#' and meet minimum version requirements.
#'
#' @param detailed Logical. If `TRUE`, displays detailed information about
#'   installed package versions, R environment, and PDF generation capabilities.
#'   Default is `FALSE`.
#' @param check_pdf Logical. If `TRUE` (and `detailed = TRUE`), also checks

#'   PDF generation capabilities. Default is `TRUE`.
#'
#' @return Invisibly returns `TRUE` if all required dependencies are satisfied,
#'   `FALSE` otherwise.
#'
#' @details
#' This function checks for:
#' \itemize{
#'   \item Required packages: shiny, shinydashboard, shinyjs, shinyFiles, DT, jsonlite
#'   \item Optional packages: sortable, zip, pointblank, osfr
#'   \item Minimum version requirements for each package
#'   \item PDF generation capabilities (rmarkdown, TinyTeX, pagedown)
#' }
#'
#' If dependencies are missing, the function provides installation instructions.
#'
#' @seealso [setup_pdf_generation()] for setting up PDF capabilities.
#'
#' @examples
#' # Quick check
#' check_psychds_deps()
#'
#' # Detailed check with all version info
#' check_psychds_deps(detailed = TRUE)
#'
#' @export

check_psychds_deps <- function(install_missing = interactive()) {
  
  # Core required packages
  required_packages <- c(
    "shiny", "shinydashboard", "shinyjs", "shinyFiles",
    "DT", "jsonlite", "jsonvalidate", 
    "dplyr", "tidyr", "readr"
  )
  
  # Optional but recommended
  recommended_packages <- c(
    "httr",       # For OSF upload
    "rmarkdown",  # For better formatting
    "knitr"       # For documentation
  )
  
  # PDF generation packages
  pdf_packages <- c(
    "tinytex",    # For LaTeX-based PDF
    "pagedown"    # For Chrome-based PDF
  )
  
  message("Checking psychds dependencies...\n")
  
  # Check required packages
  missing_required <- character()
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_required <- c(missing_required, pkg)
      message("✗ ", pkg, " - MISSING (required)")
    } else {
      ver <- packageVersion(pkg)
      message("✓ ", pkg, " (", ver, ")")
    }
  }
  
  # Check recommended packages
  message("\nRecommended packages:")
  missing_recommended <- character()
  for (pkg in recommended_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_recommended <- c(missing_recommended, pkg)
      message("○ ", pkg, " - not installed (optional)")
    } else {
      ver <- packageVersion(pkg)
      message("✓ ", pkg, " (", ver, ")")
    }
  }
  
  # Check PDF capabilities
  message("\nPDF Generation:")
  has_latex <- Sys.which("pdflatex") != ""
  has_tinytex <- requireNamespace("tinytex", quietly = TRUE) && 
                 tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)
  has_pagedown <- requireNamespace("pagedown", quietly = TRUE)
  
  if (has_tinytex) {
    message("✓ TinyTeX installed")
  } else if (has_latex) {
    message("✓ System LaTeX available")
  } else if (has_pagedown) {
    message("✓ pagedown available (HTML to PDF)")
  } else {
    message("○ No PDF generation available")
    message("  Run setup_pdf_generation() to enable")
  }
  
  # Summary and installation
  if (length(missing_required) > 0) {
    message("\n⚠ Missing required packages: ", 
            paste(missing_required, collapse = ", "))
    
    if (install_missing) {
      response <- readline("Install missing required packages? [Y/n]: ")
      if (tolower(response) != "n") {
        install.packages(missing_required)
        message("✓ Required packages installed!")
      }
    } else {
      message("\nTo install missing packages, run:")
      message('install.packages(c("', 
              paste(missing_required, collapse = '", "'), '"))')
    }
  } else {
    message("\n✓ All required dependencies are installed!")
  }
  
  if (length(missing_recommended) > 0 && install_missing) {
    response <- readline("\nInstall recommended packages? [y/N]: ")
    if (tolower(response) == "y") {
      install.packages(missing_recommended)
    }
  }
  
  invisible(list(
    missing_required = missing_required,
    missing_recommended = missing_recommended,
    pdf_available = has_latex || has_pagedown
  ))
}