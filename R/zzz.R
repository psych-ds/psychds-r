# ================================================================================
# Package Startup Configuration (zzz.R)
# This file contains .onLoad and .onAttach functions that run when package loads
# Following CRAN best practices for package startup
# ================================================================================

#' Package Startup Message
#' @noRd
.onAttach <- function(libname, pkgname) {
  
  # Only show messages in interactive sessions
  if (!interactive()) {
    return(invisible())
  }
  
  # Get package version
  version <- utils::packageDescription(pkgname, fields = "Version")
  
  # Basic welcome message
  packageStartupMessage(
    "psychds version ", version, "\n",
    "Type 'run_psych_ds_app()' to start the application\n",
    "Type 'check_psychds_deps()' to verify dependencies"
  )
  
  # Check for potential issues and provide warnings
  startup_check()
  
  invisible()
}

#' Package Load Configuration
#' @noRd
.onLoad <- function(libname, pkgname) {
  
  # Set default options that can be overridden by user
  op <- options()
  op.psychds <- list(
    psychds.startup_mode = "recommended",
    psychds.force_browser = FALSE,
    psychds.max_file_size = 100,  # MB
    psychds.validation_level = "standard",
    psychds.debug = FALSE
  )
  
  # Only set options that haven't been set already
  toset <- !(names(op.psychds) %in% names(op))
  if (any(toset)) options(op.psychds[toset])
  
  # Register S3 methods if any
  # registerS3method(...)
  
  invisible()
}

#' Startup Environment Check (MODIFIED VERSION)
#' @keywords internal
startup_check <- function() {
  
  # Don't run checks if explicitly disabled
  if (isTRUE(getOption("psychds.skip_startup_check", FALSE))) {
    return(invisible())
  }
  
  warnings <- character()
  
  # 1. Check R version
  if (getRversion() < "4.0.0") {
    warnings <- c(warnings, 
                 sprintf("R version %s detected. psychds requires R >= 4.0.0 for full functionality.",
                        getRversion()))
  }
  
  # 2. Check if in RStudio and version
  if (Sys.getenv("RSTUDIO") == "1") {
    rs_version <- tryCatch({
      if (requireNamespace("rstudioapi", quietly = TRUE)) {
        rstudioapi::versionInfo()$version
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (!is.null(rs_version) && rs_version < "2023.06.0") {
      warnings <- c(warnings,
                   "Older RStudio version detected. If the app viewer has issues, use: run_psych_ds_app(force_browser = TRUE)")
    }
  }
  
  # 3. Quick check for critical missing packages (don't load them)
  critical_packages <- c("shiny", "shinydashboard", "shinyjs", "DT")
  missing <- character()
  
  for (pkg in critical_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing <- c(missing, pkg)
    }
  }
  
  if (length(missing) > 0) {
    warnings <- c(warnings,
                 sprintf("Missing required packages: %s. Run check_psychds_deps() for installation help.",
                        paste(missing, collapse = ", ")))
  }
  
  # 4. Check for known problematic package combinations
  if (requireNamespace("shiny", quietly = TRUE) && 
      requireNamespace("miniUI", quietly = TRUE)) {
    shiny_ver <- tryCatch(utils::packageVersion("shiny"), error = function(e) NULL)
    miniUI_ver <- tryCatch(utils::packageVersion("miniUI"), error = function(e) NULL)
    
    if (!is.null(shiny_ver) && !is.null(miniUI_ver)) {
      if (shiny_ver >= "1.8.0" && miniUI_ver < "0.1.1.1") {
        warnings <- c(warnings,
                     "Potential conflict detected between shiny and miniUI versions. Consider updating miniUI.")
      }
    }
  }
  
  # ============================================================================
  
  # Display warnings if any
  if (length(warnings) > 0) {
    packageStartupMessage(
      "\n", 
      paste(strwrap(paste("Note:", warnings), width = 70), collapse = "\n"),
      "\n"
    )
  } else {
    packageStartupMessage("\u2713 All dependencies satisfied")
  }
  
  invisible()
}

#' Clean up on package unload
#' @noRd
.onUnload <- function(libpath) {
  # Clean up any temporary files or connections if needed
  # This is good practice for CRAN compliance
  
  # Remove package options
  op <- options()
  op.psychds <- op[grep("^psychds\\.", names(op))]
  if (length(op.psychds) > 0) {
    # Set them to NULL to remove
    options(stats::setNames(vector("list", length(op.psychds)), names(op.psychds)))
  }
  
  invisible()
}
