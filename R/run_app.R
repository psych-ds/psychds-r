#' Run the Psych-DS Shiny Application
#'
#' This function launches the Shiny application that allows users to create,
#' validate, and explore Psych-DS datasets. The application provides a user-friendly
#' interface for working with the Psych-DS standard.
#'
#' @param ... Additional arguments to pass to \code{shiny::runApp()}
#'
#' @return A Shiny application object
#' @export
#'
#' @importFrom shiny runApp
#'
#' @examples
#' if (interactive()) {
#'   run_psych_ds_app()
#' }
run_psych_ds_app <- function(...) {
  app_dir <- system.file("shiny", package = "psychds")
  if (app_dir == "") {
    stop("Could not find app directory. Try reinstalling the package.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, ...)
}
