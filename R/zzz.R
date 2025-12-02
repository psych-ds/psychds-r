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
    psychds.debug = FALSE,
    # ADD THIS NEW OPTION
    psychds.pdf_check_done = FALSE  # Track if we've checked PDF this session
  )
  
  # Only set options that haven't been set already
  toset <- !(names(op.psychds) %in% names(op))
  if (any(toset)) options(op.psychds[toset])
  
  # Register S3 methods if any
  # registerS3method(...)
  
  invisible()
}

#' Startup Environment Check (MODIFIED VERSION)
#' @noRd
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
  # 5. NEW: Check PDF generation capabilities (only once per session)
  # ============================================================================
  if (!isTRUE(getOption("psychds.pdf_check_done", FALSE))) {
    
    # Simple check - don't load the full function, just do basic detection
    pdf_available <- FALSE
    pdf_method <- NULL
    
    # Check for LaTeX
    if (Sys.which("pdflatex") != "") {
      pdf_available <- TRUE
      
      # Check if it's TinyTeX
      if (requireNamespace("tinytex", quietly = TRUE)) {
        if (tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)) {
          pdf_method <- "TinyTeX"
        } else {
          pdf_method <- "System LaTeX"
        }
      } else {
        pdf_method <- "System LaTeX"
      }
    }
    
    # Check for pagedown as alternative
    if (!pdf_available && requireNamespace("pagedown", quietly = TRUE)) {
      pdf_available <- TRUE
      pdf_method <- "pagedown (HTML to PDF)"
    }
    
    # Check if rmarkdown is available (needed for nice formatting)
    has_rmarkdown <- requireNamespace("rmarkdown", quietly = TRUE)
    
    # Add appropriate message
    if (!pdf_available) {
      # Only show message if user might want PDF generation
      # Don't overwhelm with messages on every startup
      warnings <- c(warnings,
                   "PDF generation not available. Run setup_pdf_generation() to enable.")
    } else if (!has_rmarkdown) {
      warnings <- c(warnings,
                   sprintf("Using %s for PDFs. Install 'rmarkdown' for better formatting.", pdf_method))
    }
    
    # Mark that we've done the PDF check this session
    options(psychds.pdf_check_done = TRUE)
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
    # ============================================================================
    # NEW: Show success message including PDF status
    # ============================================================================
    if (isTRUE(getOption("psychds.pdf_check_done", FALSE))) {
      # We already checked, so we can show status
      
      # Do a quick re-check for the success message
      if (Sys.which("pdflatex") != "") {
        packageStartupMessage("✓ All dependencies satisfied (PDF generation available)")
      } else if (requireNamespace("pagedown", quietly = TRUE)) {
        packageStartupMessage("✓ All dependencies satisfied (PDF via pagedown)")
      } else {
        packageStartupMessage("✓ Core dependencies satisfied (HTML output only)")
      }
    }
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
    options(setNames(vector("list", length(op.psychds)), names(op.psychds)))
  }
  
  invisible()
}

# ================================================================================
# NEW: Additional helper function for manual PDF setup
# This makes it easy for users to set up PDF generation
# ================================================================================

#' Setup PDF Generation Capabilities
#' 
#' Interactive setup wizard for PDF generation.
#' This function is called if PDF generation is not available.
#' 
#' @param method Character: "tinytex", "pagedown", or "auto"
#' @export
setup_pdf_generation <- function(method = "auto") {
  
  # Check current status
  has_latex <- Sys.which("pdflatex") != ""
  has_tinytex <- requireNamespace("tinytex", quietly = TRUE) && 
                 tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)
  has_pagedown <- requireNamespace("pagedown", quietly = TRUE)
  has_rmarkdown <- requireNamespace("rmarkdown", quietly = TRUE)
  
  # If everything is already set up
  if (has_latex && has_rmarkdown) {
    message("✓ PDF generation is already configured!")
    if (has_tinytex) {
      message("  Using: TinyTeX")
    } else {
      message("  Using: System LaTeX")
    }
    return(invisible(TRUE))
  }
  
  message("=== Setting up PDF generation for psychds ===\n")
  
  # Step 1: Ensure rmarkdown is installed
  if (!has_rmarkdown) {
    message("Step 1: Installing rmarkdown package...")
    install.packages("rmarkdown")
    message("✓ rmarkdown installed\n")
  } else {
    message("✓ Step 1: rmarkdown already installed\n")
  }
  
  # Step 2: Set up PDF engine
  if (!has_latex && !has_pagedown) {
    
    if (method == "auto" && interactive()) {
      message("Step 2: Choose PDF generation method:\n")
      message("  1. TinyTeX (recommended, ~100MB)")
      message("     Pros: Best quality, handles complex formatting")
      message("     Cons: Requires download\n")
      
      message("  2. pagedown (alternative, uses Chrome)")
      message("     Pros: No LaTeX needed, quick setup")
      message("     Cons: Requires Chrome/Chromium\n")
      
      choice <- readline("Enter choice (1 or 2): ")
      method <- if (choice == "2") "pagedown" else "tinytex"
    }
    
    if (method == "tinytex") {
      message("\nInstalling TinyTeX...")
      message("This will download ~100MB and may take a few minutes.\n")
      
      if (!requireNamespace("tinytex", quietly = TRUE)) {
        install.packages("tinytex")
      }
      
      tinytex::install_tinytex()
      
      # Verify
      if (tinytex::is_tinytex()) {
        message("\n✓ TinyTeX installed successfully!")
      } else {
        message("\n⚠ TinyTeX installation may have issues.")
        message("Try running: tinytex::reinstall_tinytex()")
      }
      
    } else if (method == "pagedown") {
      message("\nInstalling pagedown...")
      install.packages("pagedown")
      message("✓ pagedown installed successfully!")
      message("\nNote: pagedown requires Chrome or Chromium browser.")
    }
    
  } else if (has_latex) {
    message("✓ Step 2: LaTeX already available\n")
  } else if (has_pagedown) {
    message("✓ Step 2: pagedown already available\n")
  }
  
  message("\n=== Setup complete! ===")
  message("You can now generate PDF data dictionaries.")
  message("\nTest it with: run_psych_ds_app()")
  
  # Update the session option so we don't check again
  options(psychds.pdf_check_done = TRUE)
  
  invisible(TRUE)
}