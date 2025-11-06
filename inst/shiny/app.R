#' Psych-DS Shiny App
#'
#' Main entry point for the Psych-DS Shiny application.
#' This file loads all components and starts the app.

required_packages <- c("shiny", "shinydashboard", "shinyjs", "shinyFiles",
                       "DT", "jsonlite", "tools", "utils", "sortable",
                       "zip", "pointblank", "osfr")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste("Installing required package:", pkg))
    install.packages(pkg)
  }
}


# Load required packages
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)
library(DT)
library(jsonlite)
library(tools)
library(utils)
library(sortable)
library(zip)
library(pointblank)
library(osfr)

# Load global configuration and helper functions
source("global.R")

# Load UI definition
source("ui.R")

# Load server definition
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
