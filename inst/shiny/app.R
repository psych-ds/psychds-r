#' Psych-DS Shiny App
#'
#' Main entry point for the Psych-DS Shiny application.
#' This file loads all components and starts the app.

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

# Load global configuration and helper functions
source("global.R")

# Load UI definition
source("ui.R")

# Load server definition
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
