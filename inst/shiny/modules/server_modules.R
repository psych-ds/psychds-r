

#' Server Module Definitions
#'
#' This file contains server-side logic for the modular UI components.
#' Each module handles its own state and communicates with the global state.

# =============================================================================
# HTML Data Dictionary Generator
# Generates professional, print-ready HTML data dictionaries
# =============================================================================

#' Null-safe accessor operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x) || (is.character(x) && length(x) == 1 && x == "")) y else x
}

#' Generate HTML Data Dictionary
#'
#' Creates a professional, print-ready HTML data dictionary document from
#' variable metadata.
#'
#' @param dictionary_data A list containing:
#'   \describe{
#'     \item{variables}{Named list of variable definitions. Each variable can have:
#'       `type`, `description`, `unit`, `min_value`, `max_value`, `required`,
#'       `unique`, `pattern`, `categorical_values`, `statistics`}
#'     \item{missing_values}{Character vector of global missing value codes}
#'   }
#' @param dataset_info A list containing dataset metadata:
#'   \describe{
#'     \item{name}{Dataset name}
#'     \item{description}{Dataset description}
#'     \item{version}{Version string}
#'     \item{author}{List of author information}
#'   }
#' @param output_file Character. Path for the output HTML file. If `NULL`,
#'   a temporary file is created.
#' @param include_stats Logical. Whether to include summary statistics in
#'   the output. Default is `TRUE`.
#'
#' @return Character string containing the path to the generated HTML file.
#'
#' @details
#' The generated HTML document includes:
#' \itemize{
#'   \item Professional styling optimized for both screen and print
#'   \item Table of contents with navigation links
#'   \item Dataset overview with summary statistics
#'   \item Detailed variable definitions with constraints
#'   \item Categorical value tables
#'   \item Summary statistics (if available and requested)
#' }
#'
#' The HTML file can be converted to PDF by opening in a web browser and
#' using the browser's print function (Ctrl+P / Cmd+P).
#'
#' @examples
#' \dontrun{
#' # Define variables
#' dict_data <- list(
#'   variables = list(
#'     participant_id = list(
#'       type = "string",
#'       description = "Unique participant identifier",
#'       required = TRUE,
#'       unique = TRUE
#'     ),
#'     age = list(
#'       type = "integer",
#'       description = "Participant age in years",
#'       unit = "years",
#'       min_value = 18,
#'       max_value = 100
#'     )
#'   ),
#'   missing_values = c("NA", "-999")
#' )
#'
#' # Dataset info
#' info <- list(
#'   name = "My Study",
#'   description = "A psychological study"
#' )
#'
#' # Generate dictionary
#' generate_html_dictionary(dict_data, info, "my_dictionary.html")
#' }
#'
#' @export

generate_html_dictionary <- function(dictionary_data, 
                                     dataset_info = NULL, 
                                     output_file = NULL,
                                     include_stats = TRUE) {
  
  if (is.null(output_file)) {
    output_file <- file.path(tempdir(), 
                             paste0("data_dictionary_", format(Sys.Date(), "%Y%m%d"), ".html"))
  }
  
  if (!grepl("\\.html$", output_file, ignore.case = TRUE)) {
    output_file <- gsub("\\.[^.]+$", ".html", output_file)
    if (!grepl("\\.html$", output_file)) {
      output_file <- paste0(output_file, ".html")
    }
  }
  
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  html_content <- generate_html_content(dictionary_data, dataset_info, include_stats)
  writeLines(html_content, output_file, useBytes = TRUE)
  message("HTML dictionary generated: ", output_file)
  return(output_file)
}

#' Generate HTML Content
#' @noRd
generate_html_content <- function(dictionary_data, dataset_info, include_stats) {
  
  css <- get_dictionary_css()
  
  html_parts <- c('<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Data Dictionary', 
  if (!is.null(dataset_info$name)) paste0(" - ", escape_html(dataset_info$name)) else "",
  '</title>
  <style>', css, '</style>
</head>
<body>
  <header class="document-header">
    <div class="header-content">
      <h1 class="document-title">Data Dictionary</h1>')
  
  if (!is.null(dataset_info$name)) {
    html_parts <- c(html_parts, '<h2 class="dataset-name">', escape_html(dataset_info$name), '</h2>')
  }
  
  html_parts <- c(html_parts, '<p class="generation-date">Generated on ', 
                  format(Sys.Date(), "%B %d, %Y"), '</p>
    </div>
  </header>
  <main class="container">')
  
  html_parts <- c(html_parts, generate_toc_html(dictionary_data, dataset_info))
  html_parts <- c(html_parts, generate_overview_html(dictionary_data, dataset_info))
  
  if (!is.null(dictionary_data$missing_values) && length(dictionary_data$missing_values) > 0) {
    html_parts <- c(html_parts, generate_missing_values_html(dictionary_data$missing_values))
  }
  
  html_parts <- c(html_parts, generate_variables_html(dictionary_data$variables, include_stats))
  
  html_parts <- c(html_parts, '
  </main>
  <footer class="document-footer">
    <div class="footer-content">
      <p>Generated following the <a href="https://psych-ds.github.io/" target="_blank">Psych-DS Standard</a></p>
      <p class="generator-info">Created with the psychds R package</p>
    </div>
  </footer>
</body>
</html>')
  
  return(paste(html_parts, collapse = "\n"))
}

#' Get CSS Styles
#' @noRd
get_dictionary_css <- function() {
'
:root {
  --primary: #2c3e50;
  --secondary: #3498db;
  --accent: #1abc9c;
  --text: #333;
  --muted: #6c757d;
  --surface: #f8f9fa;
  --border: #e9ecef;
}

*, *::before, *::after { box-sizing: border-box; }

body {
  font-family: "Segoe UI", -apple-system, BlinkMacSystemFont, Arial, sans-serif;
  font-size: 14px;
  line-height: 1.6;
  color: var(--text);
  background: #fff;
  margin: 0;
  padding: 0;
}

h1, h2, h3, h4 {
  font-weight: 600;
  line-height: 1.3;
  margin-top: 0;
  color: var(--primary);
}

h2 {
  font-size: 1.4rem;
  border-bottom: 2px solid var(--secondary);
  padding-bottom: 0.5rem;
  margin-bottom: 1.5rem;
}

code {
  font-family: "SF Mono", Consolas, monospace;
  font-size: 0.9em;
  background: var(--surface);
  padding: 0.2em 0.4em;
  border-radius: 4px;
  color: #c7254e;
}

a { color: var(--secondary); text-decoration: none; }
a:hover { text-decoration: underline; }

.document-header {
  background: linear-gradient(135deg, var(--primary) 0%, #34495e 100%);
  color: white;
  padding: 2.5rem 2rem;
  text-align: center;
}

.document-header h1, .document-header h2 {
  color: white;
  border: none;
}

.document-title {
  font-size: 2.2rem;
  font-weight: 300;
  margin-bottom: 0.5rem;
}

.dataset-name {
  font-size: 1.3rem;
  font-weight: 400;
  opacity: 0.95;
  margin-bottom: 0.75rem;
}

.generation-date {
  font-size: 0.85rem;
  opacity: 0.8;
  margin: 0;
}

.container {
  max-width: 900px;
  margin: 0 auto;
  padding: 2rem;
}

.section {
  margin-bottom: 2.5rem;
}

.section-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  margin-bottom: 1.25rem;
}

.section-icon {
  width: 32px;
  height: 32px;
  background: var(--secondary);
  border-radius: 50%;
  display: flex;
  align-items: center;
  justify-content: center;
  color: white;
  font-size: 1rem;
}

.toc {
  background: var(--surface);
  border-radius: 8px;
  padding: 1.25rem 1.5rem;
  margin-bottom: 2rem;
}

.toc h2 {
  font-size: 1rem;
  margin-bottom: 0.75rem;
  border: none;
  padding: 0;
}

.toc-list {
  list-style: none;
  padding: 0;
  margin: 0;
  columns: 2;
  column-gap: 2rem;
}

.toc-list li {
  margin-bottom: 0.4rem;
  break-inside: avoid;
}

.toc-number {
  color: var(--secondary);
  font-weight: 600;
  margin-right: 0.5rem;
}

.overview-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
  gap: 1rem;
  margin-bottom: 1.5rem;
}

.stat-card {
  background: var(--surface);
  border-radius: 8px;
  padding: 1.25rem;
  text-align: center;
  border-left: 4px solid var(--secondary);
}

.stat-value {
  font-size: 1.8rem;
  font-weight: 700;
  color: var(--secondary);
  line-height: 1;
}

.stat-label {
  font-size: 0.8rem;
  color: var(--muted);
  margin-top: 0.4rem;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.description-box {
  background: var(--surface);
  border-radius: 8px;
  padding: 1.25rem;
  margin-bottom: 1rem;
}

.info-list {
  list-style: none;
  padding: 0;
  margin: 0;
}

.info-list li {
  padding: 0.6rem 0;
  border-bottom: 1px solid var(--border);
  display: flex;
  gap: 1rem;
}

.info-list li:last-child { border-bottom: none; }

.info-label {
  font-weight: 600;
  min-width: 110px;
  color: var(--muted);
}

.missing-values-list {
  display: flex;
  flex-wrap: wrap;
  gap: 0.5rem;
  margin-top: 0.75rem;
}

.missing-value-badge {
  background: #fff3cd;
  border: 1px solid #ffc107;
  color: #856404;
  padding: 0.3rem 0.7rem;
  border-radius: 20px;
  font-family: monospace;
  font-size: 0.85rem;
}

.variable-card {
  background: white;
  border: 1px solid var(--border);
  border-radius: 8px;
  margin-bottom: 1.25rem;
  overflow: hidden;
  box-shadow: 0 2px 4px rgba(0,0,0,0.08);
  page-break-inside: avoid;
}

.variable-header {
  background: linear-gradient(to right, var(--surface), white);
  padding: 0.9rem 1.25rem;
  border-bottom: 1px solid var(--border);
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.variable-name {
  font-family: "SF Mono", Consolas, monospace;
  font-size: 1.05rem;
  font-weight: 600;
  color: var(--primary);
}

.variable-type-badge {
  background: var(--secondary);
  color: white;
  padding: 0.2rem 0.65rem;
  border-radius: 20px;
  font-size: 0.7rem;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}

.variable-body {
  padding: 1.25rem;
}

.variable-description {
  font-size: 0.95rem;
  margin-bottom: 1.25rem;
  padding-bottom: 0.9rem;
  border-bottom: 1px dashed var(--border);
}

.badge-container {
  display: flex;
  gap: 0.4rem;
  flex-wrap: wrap;
  margin-bottom: 0.9rem;
}

.badge {
  font-size: 0.65rem;
  padding: 0.2rem 0.45rem;
  border-radius: 4px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  font-weight: 600;
}

.badge-required { background: #dc3545; color: white; }
.badge-unique { background: #6f42c1; color: white; }

.property-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
  gap: 0.75rem;
  margin-bottom: 1.25rem;
}

.property-item {
  background: var(--surface);
  padding: 0.6rem 0.9rem;
  border-radius: 6px;
}

.property-label {
  font-size: 0.7rem;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  color: var(--muted);
  margin-bottom: 0.2rem;
}

.property-value {
  font-weight: 600;
  color: var(--primary);
}

.property-value.monospace {
  font-family: "SF Mono", Consolas, monospace;
  font-size: 0.85em;
}

.categorical-section { margin-top: 0.9rem; }

.categorical-section h4 {
  font-size: 0.85rem;
  color: var(--muted);
  margin-bottom: 0.6rem;
}

.categorical-table {
  width: 100%;
  border-collapse: collapse;
  font-size: 0.85rem;
}

.categorical-table th,
.categorical-table td {
  padding: 0.6rem 0.9rem;
  text-align: left;
  border-bottom: 1px solid var(--border);
}

.categorical-table th {
  background: var(--surface);
  font-weight: 600;
  color: var(--muted);
  text-transform: uppercase;
  font-size: 0.7rem;
  letter-spacing: 0.5px;
}

.categorical-table td:first-child {
  font-family: monospace;
  font-weight: 600;
  color: var(--secondary);
}

.categorical-table tr:last-child td { border-bottom: none; }

.stats-section {
  margin-top: 0.9rem;
  padding-top: 0.9rem;
  border-top: 1px solid var(--border);
}

.stats-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(90px, 1fr));
  gap: 0.6rem;
}

.stats-item {
  text-align: center;
  padding: 0.5rem;
  background: var(--surface);
  border-radius: 6px;
}

.stats-item .stats-value {
  font-size: 1rem;
  font-weight: 700;
  color: var(--primary);
}

.stats-item .stats-label {
  font-size: 0.65rem;
  color: var(--muted);
  text-transform: uppercase;
}

.document-footer {
  background: var(--surface);
  padding: 1.5rem;
  text-align: center;
  margin-top: 2rem;
  border-top: 1px solid var(--border);
}

.document-footer p {
  margin: 0.4rem 0;
  color: var(--muted);
  font-size: 0.85rem;
}

.generator-info { font-size: 0.75rem !important; opacity: 0.7; }

@media print {
  body { font-size: 11pt; }
  .document-header {
    background: var(--primary) !important;
    -webkit-print-color-adjust: exact;
    print-color-adjust: exact;
    padding: 1.25rem;
  }
  .container { max-width: none; padding: 0; }
  .toc { page-break-after: always; }
  .variable-card { box-shadow: none; border: 1px solid #ddd; }
  .stat-card {
    border-left-color: var(--secondary) !important;
    -webkit-print-color-adjust: exact;
    print-color-adjust: exact;
  }
  a[href]::after { content: none; }
}

@media (max-width: 768px) {
  .document-header { padding: 1.5rem 1rem; }
  .document-title { font-size: 1.6rem; }
  .container { padding: 1rem; }
  .toc-list { columns: 1; }
  .overview-grid { grid-template-columns: 1fr 1fr; }
  .property-grid { grid-template-columns: 1fr; }
}
'
}

#' Generate Table of Contents
#' @noRd
generate_toc_html <- function(dictionary_data, dataset_info) {
  html <- '<nav class="toc">
    <h2>📑 Contents</h2>
    <ol class="toc-list">
      <li><a href="#overview"><span class="toc-number">1.</span> Overview</a></li>'
  
  if (!is.null(dictionary_data$missing_values) && length(dictionary_data$missing_values) > 0) {
    html <- paste0(html, '<li><a href="#missing-values"><span class="toc-number">2.</span> Missing Values</a></li>')
  }
  
  html <- paste0(html, '<li><a href="#variables"><span class="toc-number">3.</span> Variables</a></li>')
  
  if (!is.null(dictionary_data$variables)) {
    var_names <- names(dictionary_data$variables)
    for (i in seq_along(var_names)) {
      var_id <- make_id(var_names[i])
      html <- paste0(html, '<li><a href="#var-', var_id, '"><span class="toc-number">3.', i, '</span> ', 
                     escape_html(var_names[i]), '</a></li>')
    }
  }
  
  html <- paste0(html, '</ol></nav>')
  return(html)
}

#' Generate Overview Section
#' @noRd
generate_overview_html <- function(dictionary_data, dataset_info) {
  n_vars <- length(dictionary_data$variables)
  var_types <- sapply(dictionary_data$variables, function(v) v$type %||% "unspecified")
  type_counts <- table(var_types)
  required_count <- sum(sapply(dictionary_data$variables, function(v) isTRUE(v$required)))
  categorical_count <- sum(sapply(dictionary_data$variables, function(v) 
    !is.null(v$categorical_values) && length(v$categorical_values) > 0))
  
  html <- '<section class="section" id="overview">
    <div class="section-header">
      <div class="section-icon">📊</div>
      <h2>Dataset Overview</h2>
    </div>'
  
  if (!is.null(dataset_info$description) && dataset_info$description != "") {
    html <- paste0(html, '<div class="description-box"><p>', 
                   escape_html(dataset_info$description), '</p></div>')
  }
  
  html <- paste0(html, '<div class="overview-grid">
      <div class="stat-card">
        <div class="stat-value">', n_vars, '</div>
        <div class="stat-label">Variables</div>
      </div>')
  
  if (required_count > 0) {
    html <- paste0(html, '<div class="stat-card">
        <div class="stat-value">', required_count, '</div>
        <div class="stat-label">Required</div>
      </div>')
  }
  
  if (categorical_count > 0) {
    html <- paste0(html, '<div class="stat-card">
        <div class="stat-value">', categorical_count, '</div>
        <div class="stat-label">Categorical</div>
      </div>')
  }
  
  html <- paste0(html, '<div class="stat-card">
        <div class="stat-value">', length(type_counts), '</div>
        <div class="stat-label">Data Types</div>
      </div>
    </div>')
  
  if (!is.null(dataset_info)) {
    html <- paste0(html, '<ul class="info-list">')
    if (!is.null(dataset_info$name)) {
      html <- paste0(html, '<li><span class="info-label">Dataset</span><span>', 
                     escape_html(dataset_info$name), '</span></li>')
    }
    if (!is.null(dataset_info$version)) {
      html <- paste0(html, '<li><span class="info-label">Version</span><span>', 
                     escape_html(dataset_info$version), '</span></li>')
    }
    if (!is.null(dataset_info$author) && length(dataset_info$author) > 0) {
      author_names <- sapply(dataset_info$author, function(a) {
        if (!is.null(a$givenName) && !is.null(a$familyName)) {
          paste(a$givenName, a$familyName)
        } else if (!is.null(a$name)) { a$name } else { "Unknown" }
      })
      html <- paste0(html, '<li><span class="info-label">Authors</span><span>', 
                     escape_html(paste(author_names, collapse = ", ")), '</span></li>')
    }
    html <- paste0(html, '</ul>')
  }
  
  html <- paste0(html, '</section>')
  return(html)
}

#' Generate Missing Values Section
#' @noRd
generate_missing_values_html <- function(missing_values) {
  html <- '<section class="section" id="missing-values">
    <div class="section-header">
      <div class="section-icon">⚠️</div>
      <h2>Global Missing Value Codes</h2>
    </div>
    <p>These codes represent missing values across all variables:</p>
    <div class="missing-values-list">'
  
  for (mv in missing_values) {
    html <- paste0(html, '<span class="missing-value-badge">', 
                   escape_html(as.character(mv)), '</span>')
  }
  
  html <- paste0(html, '</div></section>')
  return(html)
}

#' Generate Variables Section
#' @noRd
generate_variables_html <- function(variables, include_stats) {
  if (is.null(variables) || length(variables) == 0) {
    return('<section class="section" id="variables">
      <h2>Variable Definitions</h2>
      <p>No variables defined.</p>
    </section>')
  }
  
  html <- '<section class="section" id="variables">
    <div class="section-header">
      <div class="section-icon">📋</div>
      <h2>Variable Definitions</h2>
    </div>'
  
  var_names <- names(variables)
  for (i in seq_along(var_names)) {
    html <- paste0(html, generate_variable_card_html(var_names[i], variables[[var_names[i]]], 
                                                     i, make_id(var_names[i]), include_stats))
  }
  
  html <- paste0(html, '</section>')
  return(html)
}

#' Generate Single Variable Card
#' @noRd
generate_variable_card_html <- function(var_name, var_info, index, var_id, include_stats) {
  var_type <- var_info$type %||% "string"
  
  html <- paste0('<article class="variable-card" id="var-', var_id, '">
      <div class="variable-header">
        <span class="variable-name">', escape_html(var_name), '</span>
        <span class="variable-type-badge">', escape_html(var_type), '</span>
      </div>
      <div class="variable-body">')
  
  # Badges
  badges <- character()
  if (isTRUE(var_info$required)) badges <- c(badges, '<span class="badge badge-required">Required</span>')
  if (isTRUE(var_info$unique)) badges <- c(badges, '<span class="badge badge-unique">Unique</span>')
  if (length(badges) > 0) {
    html <- paste0(html, '<div class="badge-container">', paste(badges, collapse = ""), '</div>')
  }
  
  # Description
  if (!is.null(var_info$description) && var_info$description != "") {
    html <- paste0(html, '<p class="variable-description">', escape_html(var_info$description), '</p>')
  }
  
  # Properties
  props <- list()
  if (!is.null(var_info$display_name) && var_info$display_name != "") props[["Display Name"]] <- var_info$display_name
  if (!is.null(var_info$unit) && var_info$unit != "") props[["Unit"]] <- var_info$unit
  if (!is.null(var_info$min_value) && var_info$min_value != "") props[["Minimum"]] <- list(v = var_info$min_value, mono = TRUE)
  if (!is.null(var_info$max_value) && var_info$max_value != "") props[["Maximum"]] <- list(v = var_info$max_value, mono = TRUE)
  if (!is.null(var_info$pattern) && var_info$pattern != "") props[["Pattern"]] <- list(v = var_info$pattern, mono = TRUE)
  if (!is.null(var_info$default_value) && var_info$default_value != "") props[["Default"]] <- list(v = var_info$default_value, mono = TRUE)
  if (!is.null(var_info$source) && var_info$source != "") props[["Source"]] <- var_info$source
  
  if (length(props) > 0) {
    html <- paste0(html, '<div class="property-grid">')
    for (pn in names(props)) {
      pv <- props[[pn]]
      if (is.list(pv)) {
        val <- escape_html(as.character(pv$v))
        cls <- if (isTRUE(pv$mono)) ' monospace' else ''
      } else {
        val <- escape_html(as.character(pv))
        cls <- ''
      }
      html <- paste0(html, '<div class="property-item">
            <div class="property-label">', escape_html(pn), '</div>
            <div class="property-value', cls, '">', val, '</div>
          </div>')
    }
    html <- paste0(html, '</div>')
  }
  
  # Categorical values
  if (!is.null(var_info$categorical_values) && length(var_info$categorical_values) > 0) {
    html <- paste0(html, '<div class="categorical-section">
          <h4>Allowed Values</h4>
          <table class="categorical-table">
            <thead><tr><th>Value</th><th>Label</th><th>Description</th></tr></thead>
            <tbody>')
    for (cv in var_info$categorical_values) {
      v <- cv$value %||% ""
      l <- cv$label %||% v
      d <- cv$description %||% ""
      html <- paste0(html, '<tr><td>', escape_html(as.character(v)), '</td><td>', 
                     escape_html(as.character(l)), '</td><td>', escape_html(as.character(d)), '</td></tr>')
    }
    html <- paste0(html, '</tbody></table></div>')
  }
  
  # Statistics
  if (include_stats && !is.null(var_info$statistics)) {
    s <- var_info$statistics
    html <- paste0(html, '<div class="stats-section"><h4>Statistics</h4><div class="stats-grid">')
    if (!is.null(s$n)) html <- paste0(html, '<div class="stats-item"><div class="stats-value">', s$n, '</div><div class="stats-label">N</div></div>')
    if (!is.null(s$missing)) html <- paste0(html, '<div class="stats-item"><div class="stats-value">', s$missing, '</div><div class="stats-label">Missing</div></div>')
    if (!is.null(s$mean)) html <- paste0(html, '<div class="stats-item"><div class="stats-value">', round(s$mean, 2), '</div><div class="stats-label">Mean</div></div>')
    if (!is.null(s$sd)) html <- paste0(html, '<div class="stats-item"><div class="stats-value">', round(s$sd, 2), '</div><div class="stats-label">SD</div></div>')
    if (!is.null(s$min)) html <- paste0(html, '<div class="stats-item"><div class="stats-value">', s$min, '</div><div class="stats-label">Min</div></div>')
    if (!is.null(s$max)) html <- paste0(html, '<div class="stats-item"><div class="stats-value">', s$max, '</div><div class="stats-label">Max</div></div>')
    html <- paste0(html, '</div></div>')
  }
  
  # Notes
  if (!is.null(var_info$notes) && var_info$notes != "") {
    html <- paste0(html, '<div class="notes-section"><h4>Notes</h4><p>', escape_html(var_info$notes), '</p></div>')
  }
  
  html <- paste0(html, '</div></article>')
  return(html)
}

#' Escape HTML
#' @noRd
escape_html <- function(text) {
  if (is.null(text) || length(text) == 0) return("")
  text <- as.character(text)
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&#39;", text, fixed = TRUE)
  return(text)
}

#' Make valid HTML ID
#' @noRd
make_id <- function(text) {
  id <- tolower(text)
  id <- gsub("[^a-z0-9]+", "-", id)
  id <- gsub("^-|-$", "", id)
  if (grepl("^[0-9]", id)) id <- paste0("v-", id)
  return(id)
}

#' Main entry point for dictionary generation
#' @param dictionary_data The dictionary data
#' @param dataset_info Dataset metadata  
#' @param output_file Output file path
#' @param include_stats Include statistics
#' @param format Ignored (always HTML)
#' @return List with status and file path
generate_dictionary_html <- function(dictionary_data, 
                                     dataset_info = NULL,
                                     output_file = NULL,
                                     include_stats = TRUE,
                                     format = "html") {
  
  if (is.null(output_file)) {
    output_file <- paste0("data_dictionary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  }
  
  if (!grepl("\\.html$", output_file, ignore.case = TRUE)) {
    output_file <- gsub("\\.[^.]+$", ".html", output_file)
    if (!grepl("\\.html$", output_file)) output_file <- paste0(output_file, ".html")
  }
  
  result <- tryCatch({
    generated_file <- generate_html_dictionary(
      dictionary_data = dictionary_data,
      dataset_info = dataset_info,
      output_file = output_file,
      include_stats = include_stats
    )
    list(success = TRUE, file = generated_file, format = "html", method = "HTML Document")
  }, error = function(e) {
    list(success = FALSE, error = e$message, format = "html", method = NULL)
  })
  
  return(result)
}

#' Directory Input Server Module - Fixed Version
#'
#' Handles directory selection and validation
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
directoryInputServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Create a reactive value to store the current path
    path_value <- reactiveVal("")

    # Set up directory selection
    volumes <- c(Home = "~")
    if (.Platform$OS.type == "windows") {
      volumes <- c(volumes, getVolumes()())
    }

    shinyDirChoose(
      input,
      "select",
      roots = volumes,
      session = session,
      restrictions = system.file(package = "base")
    )

    # Update the project directory input when a directory is selected
    observeEvent(input$select, {
      tryCatch({
        if (!is.null(input$select)) {
          selected_dir <- parseDirPath(volumes, input$select)
          message("Directory selected: ", selected_dir)

          if (length(selected_dir) > 0 && selected_dir != "") {
            # Update input field
            updateTextInput(session, "path", value = selected_dir)

            # Update our reactive value
            path_value(selected_dir)

            # Update global state
            state$project_dir <- selected_dir
          }
        }
      }, error = function(e) {
        message("Error selecting directory: ", e$message)
      })
    })

    # Also monitor manual changes to the path input
    observeEvent(input$path, {
      message("Path input changed to: ", input$path)

      if (input$path != "" && input$path != path_value()) {
        # Update reactive value
        path_value(input$path)

        # Update global state
        state$project_dir <- input$path
      }
    })

    # Return reactive that provides current path
    return(path_value)
  })
}

#' File Browser Server Module - Fixed Version
#'
#' Handles file listing and selection
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param dir_path Reactive expression that returns the directory path
#' @param session The current session object
fileBrowserServer <- function(id, state, dir_path, session) {
  moduleServer(id, function(input, output, session) {
    # Store selected files
    selected <- reactiveVal(character(0))

    # Handle file selection
    observeEvent(input$toggle_file, {
      file <- input$toggle_file
      message("Toggle file: ", file)

      current <- selected()
      if (file %in% current) {
        selected(setdiff(current, file))
      } else {
        selected(c(current, file))
      }

      # Update global state
      state$data_files <- selected()
    })

    # Handle directory selection
    observeEvent(input$select_dir, {
      dir_prefix <- input$select_dir
      message("Select directory: ", dir_prefix)

      # Get all files
      all_files <- list_csv_files(dir_path())

      # Find files in this directory
      dir_files <- all_files[startsWith(all_files, dir_prefix)]

      # Get current selection
      current <- selected()

      # Check if all directory files are already selected
      if (all(dir_files %in% current)) {
        # Deselect all files in this directory
        selected(setdiff(current, dir_files))
      } else {
        # Select all files in this directory
        selected(unique(c(current, dir_files)))
      }

      # Update global state
      state$data_files <- selected()
    })

    # Render the file browser with complete hierarchy
    output$file_container <- renderUI({
      current_dir <- dir_path()
      message("Rendering file container for: ", current_dir)

      # If no directory selected
      if (is.null(current_dir) || current_dir == "") {
        return(div(
          style = "text-align: center; padding-top: 30px; color: #999;",
          "Please select a project directory first"
        ))
      }

      # Get CSV files
      files <- list_csv_files(current_dir)

      # If no files found
      if (length(files) == 0) {
        return(div(
          style = "text-align: center; padding-top: 30px; color: #999;",
          "No CSV files found in this directory"
        ))
      }

      # Create a hierarchical directory structure
      hierarchy <- organize_directory_hierarchy(files)

      # Sort directories by their full path to maintain hierarchy order
      dir_paths <- names(hierarchy)
      dir_paths <- dir_paths[order(sapply(dir_paths, function(d) {
        # Count the number of path components to sort by depth
        length(strsplit(d, "/")[[1]])
      }), dir_paths)]

      # Build the UI
      ui_elements <- tagList(
        # File count
        div(style = "color: #999; font-size: 12px; margin-bottom: 10px;",
            paste("Found", length(files), "CSV files"))
      )

      # Track previously rendered directories to avoid duplicates
      rendered_dirs <- character(0)

      # Process directories and files
      for (dir_path in dir_paths) {
        dir_info <- hierarchy[[dir_path]]

        # Skip files (we'll handle them with their parent directories)
        if (dir_info$is_file) {
          next
        }

        # Display the directory header
        dir_indentation <- paste0(rep("&nbsp;", dir_info$level * 3), collapse = "")

        # Get files in this directory (with full paths for selection)
        dir_file_paths <- if (length(dir_info$files) > 0) {
          paste0(dir_path, "/", dir_info$files)
        } else {
          character(0)
        }

        # Check if any files in this directory are selected
        current_selection <- selected()
        dir_selected <- any(dir_file_paths %in% current_selection)
        all_selected <- length(dir_file_paths) > 0 && all(dir_file_paths %in% current_selection)

        # Add directory
        ui_elements <- tagAppendChild(ui_elements,
                                      div(
                                        style = paste0(
                                          "margin-top: 5px; font-weight: bold; cursor: pointer; ",
                                          if (all_selected) "color: #2196F3;" else if (dir_selected) "color: #64B5F6;" else ""
                                        ),
                                        # Directory indentation and name
                                        HTML(paste0(dir_indentation, dir_info$name, "/")),
                                        onclick = paste0("Shiny.setInputValue('", session$ns("select_dir"), "', '",
                                                         dir_path, "', {priority: 'event'})")
                                      )
        )

        # Add files in this directory
        if (length(dir_info$files) > 0) {
          # Sort files alphabetically
          sorted_files <- sort(dir_info$files)

          for (filename in sorted_files) {
            file_path <- paste0(dir_path, "/", filename)
            is_selected <- file_path %in% current_selection

            ui_elements <- tagAppendChild(ui_elements,
                                          div(
                                            style = paste0(
                                              "padding: 2px 5px; margin: 1px 0; cursor: pointer; ",
                                              if (is_selected) "background-color: #e3f2fd; border-radius: 3px;" else ""
                                            ),
                                            # File indentation and name
                                            HTML(paste0(dir_indentation, "&nbsp;&nbsp;&nbsp;", filename)),
                                            onclick = paste0("Shiny.setInputValue('", session$ns("toggle_file"), "', '",
                                                             file_path, "', {priority: 'event'})")
                                          )
            )
          }
        }
      }

      # Add root files (if any)
      root_files <- files[!grepl("/", files)]
      if (length(root_files) > 0) {
        # Sort root files alphabetically
        root_files <- sort(root_files)

        # Add a header for root files
        if (length(root_files) > 0) {
          ui_elements <- tagAppendChild(ui_elements,
                                        div(
                                          style = "margin-top: 8px; font-weight: bold;",
                                          "Root files:"
                                        )
          )

          # Add each root file
          current_selection <- selected()
          for (file in root_files) {
            is_selected <- file %in% current_selection

            ui_elements <- tagAppendChild(ui_elements,
                                          div(
                                            style = paste0(
                                              "padding: 2px 5px; margin: 1px 0; cursor: pointer; ",
                                              if (is_selected) "background-color: #e3f2fd; border-radius: 3px;" else ""
                                            ),
                                            # File indentation and name
                                            HTML(paste0("&nbsp;&nbsp;&nbsp;", file)),
                                            onclick = paste0("Shiny.setInputValue('", session$ns("toggle_file"), "', '",
                                                             file, "', {priority: 'event'})")
                                          )
            )
          }
        }
      }

      return(ui_elements)
    })

    # Return selected files
    return(selected)
  })
}

#' Optional Directories Server Module
#'
#' Handles selection and customization of optional directories
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
optionalDirsServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Store custom directories locally
    custom_dirs <- reactiveVal(list())

    # Handle adding custom directories
    observeEvent(input$add, {
      if (input$custom_name != "") {
        # Format directory name
        custom_dir <- input$custom_name
        if (!grepl("/$", custom_dir)) {
          custom_dir <- paste0(custom_dir, "/")
        }

        # Check if directory already exists
        current <- custom_dirs()
        dir_names <- sapply(current, function(x) x$name)

        if (custom_dir %in% dir_names) {
          showNotification("This directory has already been added", type = "warning")
          return()
        }

        # Create unique ID for checkbox
        dir_id <- generate_id("dir")

        # Add to list of custom directories
        new_dir <- list(
          id = dir_id,
          name = custom_dir,
          selected = TRUE
        )

        custom_dirs(c(current, list(new_dir)))

        # Add checkbox to UI
        insertUI(
          selector = paste0("#", session$ns("container")),
          where = "beforeEnd",
          ui = checkboxInput(
            session$ns(dir_id),
            custom_dir,
            value = TRUE
          )
        )

        # Clear the input
        updateTextInput(session, "custom_name", value = "")

        # Update global state
        updateGlobalState()
      }
    })

    # Update standard directory selections in global state
    observe({
      standard_dirs <- list(
        analysis = input$dir_analysis,
        materials = input$dir_materials,
        results = input$dir_results,
        products = input$dir_products,
        documentation = input$dir_documentation
      )

      # Update global state for standard directories
      for (dir_name in names(standard_dirs)) {
        state$optional_dirs[[dir_name]] <- standard_dirs[[dir_name]]
      }
    })

    # Update custom directory selections when checkboxes change
    observe({
      # Get the current custom directories
      current <- custom_dirs()

      # Update selected status for each directory
      for (i in seq_along(current)) {
        dir_id <- current[[i]]$id
        if (exists(dir_id, where = input)) {
          current[[i]]$selected <- input[[dir_id]]
        }
      }

      # Update local state
      custom_dirs(current)

      # Update global state
      updateGlobalState()
    })

    # Update global state with custom directories
    updateGlobalState <- function() {
      current <- custom_dirs()

      # Filter to only selected directories and create character vector
      selected_dirs <- character(0)

      if (length(current) > 0) {
        for (i in seq_along(current)) {
          if (current[[i]]$selected) {
            # Strip trailing slash
            dir_name <- gsub("/$", "", current[[i]]$name)
            selected_dirs <- c(selected_dirs, dir_name)
          }
        }
      }

      # Update global state with character vector
      state$optional_dirs$custom <- selected_dirs
    }

    # Return reactive that provides all selected directories
    return(reactive({
      all_dirs <- c()

      # Add standard directories
      if (input$dir_analysis) all_dirs <- c(all_dirs, "analysis")
      if (input$dir_materials) all_dirs <- c(all_dirs, "materials")
      if (input$dir_results) all_dirs <- c(all_dirs, "results")
      if (input$dir_products) all_dirs <- c(all_dirs, "products")
      if (input$dir_documentation) all_dirs <- c(all_dirs, "documentation")

      # Add custom directories
      current <- custom_dirs()

      if (length(current) > 0) {
        selected_custom <- character(0)

        for (i in seq_along(current)) {
          if (current[[i]]$selected) {
            dir_name <- gsub("/$", "", current[[i]]$name)
            selected_custom <- c(selected_custom, dir_name)
          }
        }

        all_dirs <- c(all_dirs, selected_custom)
      }

      return(all_dirs)
    }))
  })
}

#' Step 1 Server Module - Fixed Version
#'
#' Handles project directory and data file selection
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
step1Server <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Initialize directory input handler
    dir_path <- directoryInputServer("project_dir", state, session)

    # Log directory path changes for debugging
    observe({
      path <- dir_path()
      cat("Step1: Directory path updated to:", path, "\n")
    })

    # Initialize file browser with the reactive directory path
    selected_files <- fileBrowserServer("files", state, dir_path, session)

    # Initialize optional directories
    selected_dirs <- optionalDirsServer("opt_dirs", state, session)

    # Handle Continue button
    observeEvent(input$continue, {
      # Check if a valid directory is selected
      if (is.null(dir_path()) || dir_path() == "") {
        showModal(modalDialog(
          title = "Error",
          "Please select a project directory first.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else if (length(selected_files()) == 0) {
        showModal(modalDialog(
          title = "Warning",
          "No data files are selected. Do you want to continue anyway?",
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("continue_no_files"), "Continue Anyway", class = "btn-warning")
          )
        ))
      } else {
        # Proceed to next step
        proceedToNextStep()
      }
    })

    # Handle continuing with no files
    observeEvent(input$continue_no_files, {
      removeModal()
      proceedToNextStep()
    })

    # Function to proceed to next step
    proceedToNextStep <- function() {
      # Update global state with current selections
      state$project_dir <- dir_path()
      state$data_files <- selected_files()

      # Save optional directories to state
      state$optional_dirs$directories <- selected_dirs()

      # Show summary before proceeding
      dirs <- selected_dirs()

      # Create a nicer display for files
      files_html <- if(length(state$data_files) > 0) {
        tagList(
          div(
            style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; margin-top: 5px; background-color: #f8f9fa;",
            lapply(state$data_files, function(file) {
              div(
                style = "padding: 4px 0; border-bottom: 1px solid #eee;",
                icon("file"),
                span(style = "margin-left: 5px;", file)
              )
            })
          )
        )
      } else {
        "None"
      }

      # Create a nicer display for directories
      dirs_html <- if(length(dirs) > 0) {
        tagList(
          div(
            style = "max-height: 150px; overflow-y: auto; border: 1px solid #ddd; padding: 8px; margin-top: 5px; background-color: #f8f9fa;",
            lapply(dirs, function(dir) {
              div(
                style = "padding: 4px 0; border-bottom: 1px solid #eee;",
                icon("folder"),
                span(style = "margin-left: 5px;", dir)
              )
            })
          )
        )
      } else {
        "None"
      }

      showModal(modalDialog(
        title = "Dataset Creation - Step 1 Complete",
        div(
          p(strong("Project Directory:"), state$project_dir),

          strong("Selected Files:"),
          files_html,

          strong("Optional Directories:"),
          dirs_html
        ),
        # Valid sizes are "m" (default), "s", "l", or "xl"
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("proceed_step2"), "Proceed to Step 2", class = "btn-primary")
        )
      ))
    }

    # Handle proceeding to step 2
    observeEvent(input$proceed_step2, {
      # Move to step 2
      removeModal()
      state$current_step <- 2

      # Create data dictionary templates for selected files
      if (length(state$data_files) > 0) {
        # Only create new data dictionary if it doesn't exist yet
        if (is.null(state$data_dict) || length(state$data_dict) == 0) {
          data_dict <- list()
          for (file in state$data_files) {
            # Create full path to file
            file_path <- file.path(state$project_dir, file)

            # Extract column information if file exists
            if (file.exists(file_path)) {
              data_dict[[file]] <- extract_csv_structure(file_path)
            } else {
              # Create empty data dictionary if file doesn't exist yet
              data_dict[[file]] <- data.frame(
                name = character(0),
                type = character(0),
                description = character(0),
                stringsAsFactors = FALSE
              )
            }
          }

          # Update global state
          state$data_dict <- data_dict
        }
      }
    })
  })
}

#' Step 2 Server Module
#'
#' Handles data dictionary creation and editing
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
step2Server <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values for authors
    authors <- reactiveVal(list())
    detected_variables <- reactiveVal(data.frame())

    observe({
      # Adding this dependency ensures it runs when you return to this step
      step <- state$current_step

      # Initialize dataset name from state
      if (!is.null(state$dataset_info$name)) {
        updateTextInput(session, "dataset_name", value = state$dataset_info$name)
      }

      # Initialize dataset description from state
      if (!is.null(state$dataset_info$description)) {
        updateTextAreaInput(session, "dataset_description", value = state$dataset_info$description)
      }

      # Initialize authors from state if available
      if (length(authors()) == 0 && !is.null(state$dataset_info$authors) &&
          length(state$dataset_info$authors) > 0) {
        authors(state$dataset_info$authors)
      }
    })

    # Store input values immediately when they change
    observeEvent(input$dataset_name, {
      if (!is.null(input$dataset_name)) {
        state$dataset_info$name <- input$dataset_name
      }
    })

    observeEvent(input$dataset_description, {
      if (!is.null(input$dataset_description)) {
        state$dataset_info$description <- input$dataset_description
      }
    })

    # Store authors in state when they change
    observe({
      if (length(authors()) > 0) {
        state$dataset_info$authors <- authors()
      }
    })
    

    # On initialization, analyze selected files
    observe({
      req(state$project_dir)
      req(length(state$data_files) > 0)

      # Extract variable information from selected files
      variables_df <- extractVariableInfo(state$project_dir, state$data_files)

      # Only set if we have actual data
      if (!is.null(variables_df) && nrow(variables_df) > 0) {
        detected_variables(variables_df)
      }
    })

    

    # Extract variable information from files
    extractVariableInfo <- function(project_dir, file_paths) {
      # Initialize results data frame
      result <- data.frame(
        variable = character(),
        present_in = character(),
        stringsAsFactors = FALSE
      )

      # Track variables across files
      all_vars <- list()

      # Process each file
      for (file_path in file_paths) {
        full_path <- file.path(project_dir, file_path)

        if (file.exists(full_path)) {
          # Read column names from CSV
          tryCatch({
            # Read just the header row
            header <- colnames(read.csv(full_path, nrows = 1))

            # Add to the variable tracking
            for (var_name in header) {
              if (var_name %in% names(all_vars)) {
                all_vars[[var_name]] <- c(all_vars[[var_name]], file_path)  # Use full relative path
              } else {
                all_vars[[var_name]] <- file_path  # Use full relative path
              }
            }
          }, error = function(e) {
            # Handle errors reading the file
            warning(paste("Error reading", file_path, ":", e$message))
          })
        }
      }

      # Convert to data frame
      for (var_name in names(all_vars)) {
        result <- rbind(result, data.frame(
          variable = var_name,
          present_in = paste(all_vars[[var_name]], collapse = ", "),
          stringsAsFactors = FALSE
        ))
      }

      # Sort alphabetically by variable name
      if (nrow(result) > 0) {
        result <- result[order(result$variable), ]
      }

      return(result)
    }

    # Render the variables table
    output$variables_table <- DT::renderDataTable({
      vars_df <- detected_variables()

      if (is.null(vars_df) || nrow(vars_df) == 0) {
        return(DT::datatable(
          data.frame(
            Variable = "No variables detected",
            `Present In` = "Please select CSV files in Step 1",
            check.names = FALSE
          ),
          options = list(
            dom = 't',
            paging = FALSE,
            searching = FALSE,
            info = FALSE
          ),
          rownames = FALSE
        ))
      }

      # Create formatted file lists with scrollable container
      vars_df$present_in <- sapply(vars_df$present_in, function(files_str) {
        files <- strsplit(files_str, ", ")[[1]]
        file_divs <- character(length(files))

        for (i in seq_along(files)) {
          file_divs[i] <- sprintf('<div>%s</div>', files[i])
        }

        # Create scrollable container with CSS class
        paste0('<div class="file-list-scrollable">',
               paste(file_divs, collapse = ""),
               '</div>')
      })

      DT::datatable(
        vars_df,
        options = list(
          pageLength = 5,
          lengthMenu = c(5, 10, 15),
          scrollX = TRUE,
          dom = "ftip",
          columnDefs = list(
            list(className = 'dt-left', targets = "_all")
          )
        ),
        rownames = FALSE,
        selection = "none",
        escape = FALSE  # Important: allows HTML in cells
      )
    })

    # Render author list
    output$author_list <- renderUI({
      author_list <- authors()

      if (length(author_list) == 0) {
        return(
          div(
            style = "padding: 10px; text-align: center; color: #666;",
            "No authors added yet. Click 'Add New Author' below."
          )
        )
      }

      # Header row with columns for first/last name
      tagList(
        div(
          style = "display: flex; background-color: #f8f9fa; padding: 5px; border-bottom: 1px solid #ced4da;",
          div(style = "flex: 1;", strong("Given Name")),
          div(style = "flex: 1;", strong("Family Name")),
          div(style = "flex: 1;", strong("ORCID ID")),
          div(style = "flex: 0; width: 40px;", "")
        ),
        # Author rows
        lapply(seq_along(author_list), function(i) {
          author <- author_list[[i]]
          div(
            style = "display: flex; padding: 5px; border-bottom: 1px solid #ced4da;",
            div(
              style = "flex: 1;",
              author$first_name
            ),
            div(
              style = "flex: 1;",
              author$last_name
            ),
            div(
              style = "flex: 1;",
              author$orcid
            ),
            div(
              style = "flex: 0; width: 40px;",
              actionButton(
                inputId = session$ns(paste0("remove_author_", i)),
                label = NULL,
                icon = icon("trash"),
                class = "btn-sm btn-danger",
                style = "padding: 2px 6px;"
              )
            )
          )
        })
      )
    })

    # Handle adding a new author
    observeEvent(input$add_author, {
      showModal(modalDialog(
        title = "Add Author",
        # Stack fields vertically instead of flex layout
        div(
          textInput(session$ns("new_author_first"), "Given Name", ""),
          textInput(session$ns("new_author_last"), "Family Name", ""),
          textInput(session$ns("new_author_orcid"), "ORCID ID (optional)",
                    placeholder = "e.g., 0000-0002-1825-0097")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("save_new_author"), "Add", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    # Save new author
    observeEvent(input$save_new_author, {
      if (input$new_author_first != "" && input$new_author_last != "") {
        current_authors <- authors()

        # Add new author with separate first and last name
        current_authors[[length(current_authors) + 1]] <- list(
          first_name = input$new_author_first,
          last_name = input$new_author_last,
          orcid = input$new_author_orcid
        )

        # Update authors
        authors(current_authors)

        # Also update state immediately
        state$dataset_info$authors <- current_authors

        # Close modal
        removeModal()
      } else {
        showNotification("First and last name are required", type = "error")
      }
    })

    # Handle removing authors
    observe({
      author_list <- authors()

      for (i in seq_along(author_list)) {
        local({
          local_i <- i
          observeEvent(input[[paste0("remove_author_", local_i)]], {
            current_authors <- authors()

            # Remove the author at the specified index
            if (local_i <= length(current_authors)) {
              current_authors <- current_authors[-local_i]
              authors(current_authors)

              # Also update state immediately
              state$dataset_info$authors <- current_authors
            }
          })
        })
      }
    })

    # Pre-fill dataset information if it exists
    observe({

      # Initialize authors from state if available
      if (length(authors()) == 0 && !is.null(state$dataset_info$authors) && length(state$dataset_info$authors) > 0) {
        authors(state$dataset_info$authors)
      }

      # Initialize dataset name from state if available
      if (!is.null(state$dataset_info$name)) {
        updateTextInput(session, "dataset_name", value = state$dataset_info$name)
      }

      # Initialize dataset description from state if available
      if (!is.null(state$dataset_info$description)) {
        updateTextAreaInput(session, "dataset_description", value = state$dataset_info$description)
      }
    })

    # Handle Back button
    observeEvent(input$back, {

      # Confirm going back
      showModal(modalDialog(
        title = "Go Back to Step 1?",
        "Are you sure you want to go back to Step 1? Your progress in Step 2 will be saved.",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_back"), "Go Back", class = "btn-warning")
        )
      ))
    })

    # Handle confirmed back action
    observeEvent(input$confirm_back, {
      removeModal()
      state$current_step <- 1
    })

    # Handle Continue button
    observeEvent(input$continue, {
      # Validate inputs
      if (is.null(input$dataset_name) || input$dataset_name == "") {
        showModal(modalDialog(
          title = "Missing Information",
          "Please enter a dataset name.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }

      if (is.null(input$dataset_description) || input$dataset_description == "") {
        showModal(modalDialog(
          title = "Missing Information",
          "Please enter a dataset description.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return()
      }

      # Create JSON preview
      json_preview <- createJsonPreview()

      # Show JSON preview modal
      showModal(modalDialog(
        title = "Dataset JSON Preview",
        div(
          p("Here's a preview of the dataset_description.json file that will be generated:"),

          # Use a pre tag for better code display
          tags$pre(
            class = "json-preview",
            HTML(json_preview)
          ),

          p("This JSON will be saved in your project directory when you proceed to Step 3.")
        ),
        size = "l",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Back"),
          actionButton(session$ns("confirm_continue"), "Confirm and Continue", class = "btn-primary")
        )
      ))
    })

    # Handle confirmed continue action
    observeEvent(input$confirm_continue, {
      removeModal()
      # Move to step 3
      state$current_step <- 3
    })

    # Create JSON Preview function
    createJsonPreview <- function() {
      # Get variables
      vars_df <- detected_variables()
      if (is.null(vars_df) || nrow(vars_df) == 0) {
        variables_array <- "[]"
      } else {
        variables_list <- paste0('"', vars_df$variable, '"')
        variables_array <- paste0(
          "[\n      ",
          paste(variables_list, collapse = ",\n      "),
          "\n    ]"
        )
      }

      # Format authors in the new format
      author_list <- authors()
      if (length(author_list) == 0) {
        authors_array <- "[]"
      } else {
        author_entries <- lapply(author_list, function(author) {
          orcid_part <- if (!is.null(author$orcid) && author$orcid != "") {
            sprintf(',\n        "@id": "%s"', author$orcid)
          } else {
            ""
          }

          sprintf(
            '{\n        "@type": "Person",\n        "givenName": "%s",\n        "familyName": "%s"%s\n      }',
            author$first_name,
            author$last_name,
            orcid_part
          )
        })

        authors_array <- paste0(
          "[\n      ",
          paste(author_entries, collapse = ",\n      "),
          "\n    ]"
        )
      }

      # Create the JSON template with the correct format
      json_template <- paste0(
        "{\n",
        '    "@context": "https://schema.org/",\n',
        '    "@type": "Dataset",\n',
        sprintf('    "name": "%s",\n', gsub('"', '\\\\"', input$dataset_name)),
        sprintf('    "description": "%s",\n', gsub('"', '\\\\"', input$dataset_description)),
        '    "author": ', authors_array, ',\n',
        '    "variableMeasured": ', variables_array, '\n',
        "}"
      )

      # Simple HTML escaping for display
      json_html <- gsub("<", "&lt;", json_template)
      json_html <- gsub(">", "&gt;", json_html)

      # Manual syntax highlighting with spans
      # Mark keys
      json_html <- gsub('("@[^"]+"):', '<span class="json-key">\\1</span>:', json_html)
      json_html <- gsub('("[^@][^"]*"):',  '<span class="json-key">\\1</span>:', json_html)

      # Mark string values
      json_html <- gsub('": "([^"]*)"', '": <span class="json-string">"\\1"</span>', json_html)

      # Mark punctuation
      json_html <- gsub('\\{', '<span class="json-punctuation">{</span>', json_html)
      json_html <- gsub('\\}', '<span class="json-punctuation">}</span>', json_html)
      json_html <- gsub('\\[', '<span class="json-punctuation">[</span>', json_html)
      json_html <- gsub('\\]', '<span class="json-punctuation">]</span>', json_html)
      json_html <- gsub(',(\n)', '<span class="json-punctuation">,</span>\\1', json_html)

      return(json_html)
    }
  })
}

#' Step 3 Server Module - Complete Working Version
#'
#' Handles filename standardization and mapping
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
step3Server <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {

    # Initialize file system volumes for directory selection
    volumes <- c(Home = "~")
    if (.Platform$OS.type == "windows") {
      volumes <- c(volumes, getVolumes()())
    }

    # Reactive values for managing file mapping state
    files <- reactiveVal(list())
    current_file <- reactiveVal(NULL)
    selected_keywords <- reactiveVal(list())
    file_mappings <- reactiveVal(list())
    selected_files <- reactiveVal(integer(0))
    constant_columns <- reactiveVal(list())
    ui_trigger <- reactiveVal(0)
    initialized <- reactiveVal(FALSE)

    # Debug output for keyword troubleshooting
    output$debug_text <- renderPrint({
      keywords <- selected_keywords()
      if (length(keywords) > 0) {
        lapply(keywords, function(k) {
          list(name = k$name, value = k$value, id = k$id)
        })
      } else {
        "No keywords selected"
      }
    })

    #
    # Validates keyword values to ensure they meet naming requirements
    # @param value The keyword value to validate
    # @return List with valid (boolean) and message (string)
    #
    validateKeywordValue <- function(value) {
      if (is.null(value) || value == "") {
        return(list(valid = FALSE, message = "Value cannot be empty"))
      }
      
      if (!grepl("^[a-zA-Z0-9]+$", value)) {
        return(list(valid = FALSE, message = "Value must contain only letters and numbers (a-z, A-Z, 0-9). No spaces or special characters allowed."))
      }
      
      return(list(valid = TRUE, message = ""))
    }

    #
    # Validates custom keyword names
    # @param keyword The keyword name to validate
    # @return List with valid (boolean) and message (string)
    #
    validateCustomKeyword <- function(keyword) {
      if (is.null(keyword) || keyword == "") {
        return(list(valid = FALSE, message = "Keyword cannot be empty"))
      }
      
      if (!grepl("^[a-z]+$", keyword)) {
        return(list(valid = FALSE, message = "Custom keywords must contain only lowercase letters (a-z). No numbers, uppercase letters, spaces, or special characters allowed."))
      }
      
      return(list(valid = TRUE, message = ""))
    }

    # Initialize module with data files from parent state
    observe({
      req(state$project_dir)
      req(length(state$data_files) > 0)
      
      # Prevent re-initialization
      if (initialized()) {
        return()
      }
      
      message("STEP3 INIT: Project dir: ", state$project_dir)
      message("STEP3 INIT: Number of data files: ", length(state$data_files))

      # Create initial mappings for each data file
      mappings <- lapply(state$data_files, function(file) {
        list(
          original = file,
          new = "",
          keywords = list(),
          values = list(),
          partial_keywords = list()
        )
      })

      file_mappings(mappings)
      message("GEN: Saved mappings back to reactive")

      # Verify save immediately
      immediate_check <- file_mappings()
      message("GEN: Immediate check - File 1 new name: '", immediate_check[[1]]$new, "'")
      
      # Analyze files for columns with constant values (for auto-naming)
      analyzeConstantColumns()

      # Set initial file selection
      if (length(mappings) > 0) {
        current_file(1)
        message("STEP3 INIT: Set current file to index 1")
      }
      
      initialized(TRUE)
    })

    # Handle returning to step 3 from other steps
    observeEvent(state$current_step, {
      if (state$current_step == 3 && !initialized()) {
        initialized(FALSE)
      }
    })

    #
    # Analyzes CSV files to find columns with constant values
    # These columns can be used for automatic keyword-based naming
    #
    analyzeConstantColumns <- function() {
      mappings <- file_mappings()
      constant_cols <- list()
      
      withProgress(message = "Analyzing columns for auto-naming...", value = 0, {
        for (i in seq_along(mappings)) {
          file_path <- file.path(state$project_dir, mappings[[i]]$original)
          
          if (file.exists(file_path)) {
            tryCatch({
              # Read sample of data to find constant columns
              data <- read.csv(file_path, stringsAsFactors = FALSE, nrows = 1000)
              
              const_cols <- list()
              
              for (col_name in names(data)) {
                col_data <- data[[col_name]]
                non_na <- col_data[!is.na(col_data) & col_data != ""]
                
                # Column is constant if all non-NA values are identical
                if (length(non_na) > 0 && length(unique(non_na)) == 1) {
                  const_value <- as.character(non_na[1])
                  const_cols[[col_name]] <- list(
                    column = col_name,
                    value = const_value
                  )
                }
              }
              
              constant_cols[[i]] <- const_cols
              
            }, error = function(e) {
              constant_cols[[i]] <- list()
            })
          } else {
            constant_cols[[i]] <- list()
          }
          
          setProgress(i / length(mappings))
        }
      })
      
      constant_columns(constant_cols)
    }
    
    # Configure directory selection widget
    shinyDirChoose(
      input,
      "save_dataset_dir_select",
      roots = volumes,
      session = session
    )

    observeEvent(input$save_dataset_dir_select, {
      if (!is.null(input$save_dataset_dir_select) && !is.integer(input$save_dataset_dir_select)) {
        selected_dir <- parseDirPath(volumes, input$save_dataset_dir_select)
        if (length(selected_dir) > 0 && selected_dir != "") {
          updateTextInput(session, "save_dataset_dir", value = selected_dir)
        }
      }
    })
    
    output$save_full_path <- renderText({
      req(input$save_dataset_name, input$save_dataset_dir)
      if (input$save_dataset_name != "" && input$save_dataset_dir != "") {
        file.path(input$save_dataset_dir, input$save_dataset_name)
      } else {
        "Please enter a dataset name"
      }
    })

    # Display count of selected files
    output$selected_count_text <- renderUI({
      count <- length(selected_files())
      if (count > 0) {
        div(
          style = "color: #2196F3; font-weight: 500;",
          paste(count, if(count == 1) "file" else "files", "selected")
        )
      } else {
        div(
          style = "color: #999; font-style: italic;",
          "No files selected"
        )
      }
    })
    
    # Reactive flag for file selection state
    output$has_selection <- reactive({
      length(selected_files()) > 0
    })
    outputOptions(output, "has_selection", suspendWhenHidden = FALSE)
    
    # Check if selected files have constant columns
    output$has_constant_columns <- reactive({
      selected <- selected_files()
      file_indices <- if (length(selected) > 0) selected else c(current_file())
      
      const_cols <- constant_columns()
      
      for (idx in file_indices) {
        if (!is.null(idx) && length(const_cols) >= idx && length(const_cols[[idx]]) > 0) {
          return(TRUE)
        }
      }
      return(FALSE)
    })
    outputOptions(output, "has_constant_columns", suspendWhenHidden = FALSE)
    
    # Update auto-naming dropdown options based on selected files
    observe({
      selected <- selected_files()
      file_indices <- if (length(selected) > 0) selected else c(current_file())
      
      if (length(file_indices) == 0) return()
      
      const_cols <- constant_columns()
      
      # Collect constant columns across all selected files
      all_const_cols <- list()
      for (idx in file_indices) {
        if (length(const_cols) >= idx && length(const_cols[[idx]]) > 0) {
          for (col_name in names(const_cols[[idx]])) {
            if (!(col_name %in% names(all_const_cols))) {
              all_const_cols[[col_name]] <- TRUE
            }
          }
        }
      }
      
      # Update column dropdown
      if (length(all_const_cols) > 0) {
        col_choices <- setNames(names(all_const_cols), names(all_const_cols))
        updateSelectInput(session, "auto_column", choices = col_choices)
        
        # Build keyword choices from standard and custom keywords
        standard_keywords <- c("subject", "session", "study", "task", "condition", 
                              "stimulus", "trial", "description")
        
        keywords <- selected_keywords()
        custom_keywords <- character(0)
        if (length(keywords) > 0) {
          custom_keywords <- sapply(keywords[sapply(keywords, function(k) {
            !is.null(k$custom) && k$custom
          })], function(k) k$name)
        }
        
        all_keywords <- c(standard_keywords, custom_keywords)
        keyword_choices <- setNames(all_keywords, all_keywords)
        
        updateSelectInput(session, "auto_keyword", choices = keyword_choices)
      }
    })
    
    # Apply auto-naming to selected files
    observeEvent(input$apply_auto_name, {
      keyword_name <- input$auto_keyword
      column_name <- input$auto_column
      
      if (is.null(keyword_name) || is.null(column_name)) {
        showNotification("Please select both a keyword and a column", type = "warning")
        return()
      }
      
      selected <- selected_files()
      file_indices <- if (length(selected) > 0) selected else c(current_file())
      
      if (length(file_indices) == 0) {
        showNotification("No files selected", type = "warning")
        return()
      }
      
      const_cols <- constant_columns()
      mappings <- file_mappings()
      
      success_count <- 0
      error_count <- 0
      filenames_generated <- 0
      
      message("AUTO-NAME: Processing ", length(file_indices), " files")
      message("AUTO-NAME: Keyword: ", keyword_name, ", Column: ", column_name)
      
      withProgress(message = "Applying auto-naming...", value = 0, {
        for (i in seq_along(file_indices)) {
          file_idx <- file_indices[i]
          setProgress(i / length(file_indices), 
                      detail = paste("File", i, "of", length(file_indices)))
          
          if (file_idx > length(mappings) || file_idx > length(const_cols)) {
            error_count <- error_count + 1
            next
          }
          
          if (length(const_cols[[file_idx]]) == 0 || 
              !(column_name %in% names(const_cols[[file_idx]]))) {
            error_count <- error_count + 1
            next
          }
          
          col_info <- const_cols[[file_idx]][[column_name]]
          keyword_value <- as.character(col_info$value)
          
          if (is.null(keyword_value) || keyword_value == "" || is.na(keyword_value)) {
            error_count <- error_count + 1
            next
          }
          
          message("AUTO-NAME: File ", file_idx, " - value: ", keyword_value)
          
          # Update or add keyword
          file_keywords <- mappings[[file_idx]]$keywords
          if (length(file_keywords) == 0) {
            file_keywords <- list()
          }
          
          keyword_found <- FALSE
          for (j in seq_along(file_keywords)) {
            if (file_keywords[[j]]$name == keyword_name) {
              file_keywords[[j]]$value <- keyword_value
              keyword_found <- TRUE
              break
            }
          }
          
          if (!keyword_found) {
            keyword_id <- paste0(keyword_name, "_auto_f", file_idx, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
            file_keywords <- c(file_keywords, list(list(
              name = keyword_name,
              display = keyword_name,
              value = keyword_value,
              id = keyword_id,
              custom = !(keyword_name %in% c("subject", "session", "study", "task", 
                                            "condition", "stimulus", "trial", "description"))
            )))
          }
          
          # Generate filename if all keywords complete
          filename_parts <- character(0)
          all_filled <- TRUE
          
          for (kw in file_keywords) {
            if (is.null(kw$value) || kw$value == "") {
              all_filled <- FALSE
              break
            }
            filename_parts <- c(filename_parts, paste0(kw$name, "-", kw$value))
          }
          
          # Save keywords
          mappings[[file_idx]]$keywords <- file_keywords
          mappings[[file_idx]]$partial_keywords <- file_keywords
          
          # Generate filename if ready
          if (all_filled && length(filename_parts) > 0) {
            original <- mappings[[file_idx]]$original
            ext <- tools::file_ext(original)
            
            filename <- if (ext != "") {
              paste0(paste(filename_parts, collapse = "_"), "_data.", ext)
            } else {
              paste0(paste(filename_parts, collapse = "_"), "_data")
            }
            
            mappings[[file_idx]]$new <- filename
            filenames_generated <- filenames_generated + 1
            
            message("AUTO-NAME: Generated filename: ", filename)
          } else {
            message("AUTO-NAME: Incomplete keywords for file ", file_idx)
          }
          
          success_count <- success_count + 1
        }
      })
      
      message("AUTO-NAME: Completed. Success: ", success_count, ", Errors: ", error_count, ", Filenames: ", filenames_generated)
      
      file_mappings(mappings)
      ui_trigger(ui_trigger() + 1)
      
      # Update current file display if affected
      current_idx <- current_file()
      if (!is.null(current_idx) && current_idx %in% file_indices) {
        selected_keywords(mappings[[current_idx]]$keywords)
      }
      
      # Show notification
      if (filenames_generated > 0) {
        showNotification(
          paste("Successfully generated", filenames_generated, "filenames!"),
          type = "message",
          duration = 5
        )
      } else if (success_count > 0) {
        showNotification(
          paste("Applied keyword to", success_count, "files (filenames will generate when all keywords filled)"),
          type = "warning",
          duration = 5
        )
      } else {
        showNotification("No files were processed. Check that column exists in selected files.", 
                        type = "error", duration = 5)
      }
    })

    # Render the file mapping interface with subdirectory display
    output$file_mapping_rows <- renderUI({
      trigger_value <- ui_trigger()
      message("RENDER file_mapping_rows - trigger value: ", trigger_value)
      
      mappings <- file_mappings()
      selected <- selected_files()
      
      message("RENDER: Number of mappings: ", length(mappings))
      
      if (length(mappings) == 0) {
        return(div(class = "text-center text-muted", "No files selected"))
      }

      # Log first few files for debugging
      for (i in 1:min(3, length(mappings))) {
        message("  File ", i, " - original: ", mappings[[i]]$original, " - new: ", mappings[[i]]$new)
      }

      rows <- lapply(seq_along(mappings), function(i) {
        file_info <- mappings[[i]]
        is_current <- current_file() == i
        is_selected <- i %in% selected

        # Extract subdirectory path for display
        original_dir <- dirname(file_info$original)
        display_new_path <- if (!is.null(file_info$new) && file_info$new != "") {
          if (original_dir != "." && original_dir != "data") {
            # Include subdirectory in display
            file.path(original_dir, file_info$new)
          } else {
            file_info$new
          }
        } else {
          NULL
        }

        div(
          class = paste("file-mapping-row", 
                      if(is_current) "active" else "", 
                      if(i %% 2 == 0) "even" else "odd"),
          style = paste0(
            "padding: 8px 15px; border-bottom: 1px solid #eee; cursor: pointer; ",
            "display: flex; align-items: center; ",
            if(is_current) "background-color: #e3f2fd; border-left: 4px solid #2196F3;" 
            else if(i %% 2 == 0) "background-color: #f8f9fa;" 
            else ""
          ),
          
          # Multi-select checkbox
          div(
            style = "width: 30px; flex-shrink: 0;",
            checkboxInput(
              session$ns(paste0("select_", i)),
              label = NULL,
              value = is_selected
            )
          ),
          
          # File information
          div(
            class = "row",
            style = "flex: 1; margin: 0;",
            onclick = paste0("Shiny.setInputValue('", session$ns("select_file"), "', ", i, ", {priority: 'event'});"),
            div(class = "col-xs-6",
                span(file_info$original, class = "original-filename")
            ),
            div(class = "col-xs-6",
              if (!is.null(display_new_path)) {
                span(display_new_path, class = "new-filename", style = "color: #3498db; font-weight: bold;")
              } else if (length(file_info$partial_keywords) > 0) {
                span(
                  paste(length(file_info$partial_keywords), "keywords applied"),
                  style = "color: #ff9800; font-style: italic;"
                )
              } else {
                if (is_current) {
                  actionButton(session$ns(paste0("generate_for_", i)), "Generate", 
                              class = "btn btn-xs btn-primary")
                } else {
                  span("Click to configure", class = "text-muted")
                }
              }
            )
          )
        )
      })

      do.call(tagList, rows)
    })

    # Select all files
    observeEvent(input$select_all_files, {
      mappings <- file_mappings()
      all_indices <- seq_along(mappings)
      selected_files(all_indices)
      
      for (i in all_indices) {
        updateCheckboxInput(session, paste0("select_", i), value = TRUE)
      }
      
      showNotification(paste("Selected all", length(all_indices), "files"), type = "message")
    })

    # Deselect all files
    observeEvent(input$deselect_all_files, {
      mappings <- file_mappings()
      all_indices <- seq_along(mappings)
      selected_files(integer(0))
      
      for (i in all_indices) {
        updateCheckboxInput(session, paste0("select_", i), value = FALSE)
      }
      
      showNotification("Deselected all files", type = "message")
    })

    # Set up individual checkbox observers
    observe({
      mappings <- file_mappings()
      
      for (i in seq_along(mappings)) {
        local({
          local_i <- i
          checkbox_id <- paste0("select_", local_i)
          
          observeEvent(input[[checkbox_id]], {
            current_selected <- selected_files()
            
            if (input[[checkbox_id]]) {
              if (!local_i %in% current_selected) {
                selected_files(c(current_selected, local_i))
              }
            } else {
              selected_files(setdiff(current_selected, local_i))
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Handle file selection and keyword restoration
    observeEvent(input$select_file, {
      new_file_index <- input$select_file
      message("FILE SELECT: Switching to file index: ", new_file_index)
      
      current_file(new_file_index)

      mappings <- file_mappings()

      if (new_file_index <= length(mappings)) {
        file_info <- mappings[[new_file_index]]
        
        message("FILE SELECT: File ", new_file_index, " has ", length(file_info$keywords), " keywords")
        
        # Restore keywords for this file
        keywords_to_restore <- NULL
        
        if (length(file_info$keywords) > 0) {
          keywords_to_restore <- file_info$keywords
          message("FILE SELECT: Restoring from keywords field")
        } else if (length(file_info$partial_keywords) > 0) {
          keywords_to_restore <- file_info$partial_keywords
          message("FILE SELECT: Restoring from partial_keywords field")
        }
        
        if (!is.null(keywords_to_restore)) {
          message("FILE SELECT: Setting ", length(keywords_to_restore), " keywords")
          selected_keywords(keywords_to_restore)
          
          # Update input fields with keyword values
          for (keyword in keywords_to_restore) {
            if (!is.null(keyword$value) && keyword$value != "") {
              input_name <- paste0("keyword_value_", keyword$id)
              updateTextInput(session, input_name, value = keyword$value)
            }
          }
        } else {
          message("FILE SELECT: No keywords to restore")
          selected_keywords(list())
        }
      }
    })

    # Keyword selection handlers
    observeEvent(input$keyword_subject, { addKeyword("subject", "subject") })
    observeEvent(input$keyword_study, { addKeyword("study", "study") })
    observeEvent(input$keyword_session, { addKeyword("session", "session") })
    observeEvent(input$keyword_task, { addKeyword("task", "task") })
    observeEvent(input$keyword_condition, { addKeyword("condition", "condition") })
    observeEvent(input$keyword_stimulus, { addKeyword("stimulus", "stimulus") })
    observeEvent(input$keyword_trial, { addKeyword("trial", "trial") })
    observeEvent(input$keyword_description, { addKeyword("description", "description") })

    # Add a keyword to the current configuration
    addKeyword <- function(name, display) {
      keywords <- selected_keywords()
      
      message("ADD KEYWORD: Adding '", name, "'")

      # Check for duplicates
      if (!any(sapply(keywords, function(k) k$name == name))) {
        keyword_id <- paste0(name, "_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))

        keywords[[length(keywords) + 1]] <- list(
          name = name,
          display = display,
          value = "",
          id = keyword_id
        )
        selected_keywords(keywords)
        updateKeywordMapping()
        message("ADD KEYWORD: Added successfully. Total keywords: ", length(keywords))
      }
    }

    # Save current keyword configuration to file mapping
    updateKeywordMapping <- function() {
      file_index <- current_file()
      if (is.null(file_index)) return()

      mappings <- file_mappings()
      if (file_index > length(mappings)) return()

      message("UPDATE MAPPING: Saving keywords for file ", file_index)
      
      mappings[[file_index]]$keywords <- selected_keywords()
      mappings[[file_index]]$partial_keywords <- selected_keywords()
      mappings[[file_index]]$new <- ""  # Clear filename since config changed
      file_mappings(mappings)
    }

    # Generate standardized filename from keywords
    observeEvent(input$generate_filename, {
      message("\n=== GENERATE FILENAME CLICKED ===")

      file_index <- current_file()
      message("GEN: Current file index: ", file_index)
      
      if (is.null(file_index)) {
        showNotification("No file selected", type = "warning")
        return()
      }

      keywords <- selected_keywords()
      message("GEN: Number of keywords: ", length(keywords))
      
      if (length(keywords) == 0) {
        showNotification("Please add at least one keyword", type = "warning")
        return()
      }

      # Validate and collect keyword values
      keyword_values <- list()
      all_filled <- TRUE
      updated_keywords <- keywords
      
      for (i in seq_along(keywords)) {
        keyword <- keywords[[i]]
        input_name <- paste0("keyword_value_", keyword$id)
        value <- input[[input_name]]
        
        message("GEN: Keyword '", keyword$name, "' - Input: ", input_name, " - Value: '", value, "'")
        
        if (is.null(value) || value == "") {
          all_filled <- FALSE
          message("GEN: Missing value for keyword: ", keyword$name)
        } else {
          # Validate value format
          validation_result <- validateKeywordValue(value)
          if (!validation_result$valid) {
            showNotification(validation_result$message, type = "error")
            return()
          }
          keyword_values[[keyword$name]] <- value
          updated_keywords[[i]]$value <- value
        }
      }
      
      if (!all_filled) {
        showNotification("Please fill in all keyword values", type = "warning")
        return()
      }

      # Build filename from keyword-value pairs
      filename_parts <- character(0)
      for (i in seq_along(updated_keywords)) {
        kw <- updated_keywords[[i]]
        filename_parts <- c(filename_parts, paste0(kw$name, "-", kw$value))
      }
      
      mappings <- isolate(file_mappings())
      message("GEN: Retrieved ", length(mappings), " mappings")
      
      if (file_index > length(mappings)) {
        message("GEN ERROR: file_index (", file_index, ") > mappings length (", length(mappings), ")")
        return()
      }
      
      original <- mappings[[file_index]]$original
      ext <- tools::file_ext(original)
      
      filename <- if (ext != "") {
        paste0(paste(filename_parts, collapse = "_"), "_data.", ext)
      } else {
        paste0(paste(filename_parts, collapse = "_"), "_data")
      }
      
      message("GEN: Generated filename: ", filename)
      
      # Save mapping with keywords and new filename
      mappings[[file_index]]$keywords <- updated_keywords
      mappings[[file_index]]$partial_keywords <- updated_keywords
      mappings[[file_index]]$new <- filename
      
      file_mappings(mappings)
      message("GEN: Saved mappings back to reactive")
      
      selected_keywords(updated_keywords)
      message("GEN: Updated selected_keywords with values")
      
      # Trigger UI refresh
      old_trigger <- isolate(ui_trigger())
      ui_trigger(old_trigger + 1)
      message("GEN: Incremented ui_trigger from ", old_trigger, " to ", old_trigger + 1)
      
      # Verify save
      verification <- isolate(file_mappings())
      message("GEN VERIFY: File ", file_index, " new name: '", verification[[file_index]]$new, "'")
      
      showNotification("Filename generated successfully!", type = "message")
      
      # Auto-advance to next unconfigured file
      message("GEN: Attempting to advance to next file...")
      auto_advance_to_next_file(file_index)
      
      message("=== END GENERATE FILENAME ===\n")
    })

    # Auto-advance to next unconfigured file
    auto_advance_to_next_file <- function(current_index) {
      message("AUTO-ADVANCE: Starting from index ", current_index)

      mappings <- file_mappings()
      message("AUTO-ADVANCE: Total files: ", length(mappings))

      if (length(mappings) <= 1 || current_index >= length(mappings)) {
        message("AUTO-ADVANCE: Cannot advance - at end or only one file")
        return()
      }

      if (is.null(current_index) || current_index < 1 || current_index > length(mappings)) {
        message("AUTO-ADVANCE: Invalid index, resetting to 1")
        current_index <- 1
      }

      total_mappings <- length(mappings)

      # Search for next unconfigured file
      check_sequence <- c(
        seq(current_index + 1, total_mappings),
        seq(1, current_index)
      )

      for (i in check_sequence) {
        if (mappings[[i]]$new == "") {
          message("AUTO-ADVANCE: Found unconfigured file at index ", i)
          current_file(i)
          session$sendInputMessage("select_file", list(value = i, priority = "event"))
          return()
        }
      }

      message("AUTO-ADVANCE: All files are configured")
    }

    # UI output components
    output$current_file_text <- renderUI({
      file_index <- current_file()
      mappings <- file_mappings()

      if (is.null(file_index) || file_index > length(mappings)) {
        return(p("No file selected"))
      }

      file_info <- mappings[[file_index]]
      p(
        style = "margin: 0;",
        strong("Currently configuring:"),
        span(file_info$original, style = "font-style: italic;")
      )
    })

    output$selected_keywords <- renderUI({
      keywords <- selected_keywords()

      if (length(keywords) == 0) {
        return(div(
          style = "text-align: center; padding: 10px; color: #666;",
          "Click keywords above to add them here"
        ))
      }

      keyword_labels <- lapply(seq_along(keywords), function(i) {
        keyword <- keywords[[i]]

        tags$div(
          class = "keyword-chip-selected",
          id = paste0("keyword_", keyword$id),
          `data-keyword-id` = keyword$id,
          `data-keyword-name` = keyword$name,
          `data-keyword-index` = i,
          style = "display: inline-block; margin: 3px; padding: 5px 10px; background-color: #3498db; color: white; border-radius: 15px; position: relative; cursor: move;",

          tags$span(
            style = "pointer-events: none;",
            keyword$display
          ),

          tags$button(
            id = paste0("remove_keyword_", i),
            class = "remove-keyword",
            style = "background: none; border: none; color: white; opacity: 0.7; padding: 0 0 0 5px; font-size: 10px;",
            onclick = paste0("Shiny.setInputValue('", session$ns("remove_keyword"), "', ", i, ", {priority: 'event'});"),
            icon("times")
          )
        )
      })

      tagList(
        tags$div(
          id = session$ns("sortable_keywords"),
          class = "sortable-keywords",
          keyword_labels
        ),

        tags$script(HTML(paste0("
      $(document).ready(function() {
        if (typeof Sortable !== 'undefined') {
          var sortable = Sortable.create(document.getElementById('", session$ns("sortable_keywords"), "'), {
            animation: 150,
            ghostClass: 'sortable-ghost',
            onEnd: function(evt) {
              var items = evt.to.children;
              var newOrder = [];
              for (var i = 0; i < items.length; i++) {
                newOrder.push($(items[i]).data('keyword-id'));
              }
              Shiny.setInputValue('", session$ns("keyword_order"), "', newOrder);
            }
          });
        }
      });
    ")))
      )
    })

    output$keyword_value_inputs <- renderUI({
      file_index <- current_file()
      mappings <- file_mappings()

      if (is.null(file_index) || file_index > length(mappings)) {
        return(div(
          style = "padding: 15px; text-align: center; color: #666;",
          "Select keywords above to configure values"
        ))
      }

      file_info <- mappings[[file_index]]
      keywords <- selected_keywords()

      if (length(keywords) == 0) {
        return(div(
          style = "padding: 15px; text-align: center; color: #666;",
          "Select keywords above to configure values"
        ))
      }

      inputs <- lapply(seq_along(keywords), function(i) {
        keyword <- keywords[[i]]
        input_id <- paste0("keyword_value_", keyword$id)
        
        initial_value <- if (!is.null(keyword$value) && keyword$value != "") {
          keyword$value
        } else {
          ""
        }
        
        div(
          class = "form-group",
          style = "margin-bottom: 20px;",
          tags$label(
            class = "control-label",
            style = "color: #3498db; font-weight: bold; margin-bottom: 5px; display: block;",
            span(paste0(keyword$display, ":"))
          ),
          textInput(
            session$ns(input_id),
            NULL,
            value = initial_value,
            placeholder = paste("Enter", tolower(keyword$display), "value (letters/numbers only)"),
            width = "100%"
          ),
          div(
            id = session$ns(paste0("validation_", keyword$id)),
            style = "margin-top: 5px; min-height: 20px;"
          )
        )
      })

      do.call(tagList, inputs)
    })

    # Continue to next step
    observeEvent(input$continue, {
      mappings <- file_mappings()
      unmapped_files <- sapply(mappings, function(m) m$new == "")

      if (any(unmapped_files)) {
        showModal(modalDialog(
          title = "Missing Filename Mappings",
          div(
            p("Some files have not been assigned standardized filenames:"),
            tags$ul(
              lapply(which(unmapped_files), function(i) {
                tags$li(strong(mappings[[i]]$original))
              })
            ),
            p("Would you like to continue anyway?")
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(session$ns("confirm_continue_unmapped"), "Continue Anyway", class = "btn-warning")
          )
        ))
      } else {
        proceedToFinalStep()
      }
    })

    observeEvent(input$confirm_continue_unmapped, {
      removeModal()
      proceedToFinalStep()
    })

    # Create HTML preview of file structure
    create_file_structure_html <- function(project_dir, file_mappings, optional_dirs) {
      create_nested_list <- function() {
        root_items <- list(
          tags$li(
            tags$span(class = "file-icon", "📄"),
            "dataset_description.json"
          )
        )

        # Add optional directories
        standard_dirs <- c("analysis", "materials", "results", "products", "documentation")
        for (dir in standard_dirs) {
          if (optional_dirs[[dir]]) {
            root_items[[length(root_items) + 1]] <- tags$li(
              tags$span(class = "folder-icon", "📁"),
              paste0(dir, "/")
            )
          }
        }

        # Add custom directories
        if (!is.null(optional_dirs$custom)) {
          for (custom_dir in optional_dirs$custom) {
            root_items[[length(root_items) + 1]] <- tags$li(
              tags$span(class = "folder-icon", "📁"),
              paste0(custom_dir, "/")
            )
          }
        }

        # Build data directory structure with subdirectories
        data_structure <- list()
        
        for (mapping in file_mappings()) {
          if (mapping$new != "") {
            # Parse the original path to get subdirectory structure
            file_parts <- strsplit(mapping$original, "/")[[1]]
            
            if (length(file_parts) > 2) {
              # File is in subdirectory
              subdir_path <- paste(file_parts[2:(length(file_parts)-1)], collapse = "/")
              full_path <- paste0(subdir_path, "/", mapping$new)
            } else {
              # File is directly in data/
              full_path <- mapping$new
            }
            
            data_structure[[length(data_structure) + 1]] <- tags$li(
              tags$span(class = "file-icon", "📄"),
              full_path
            )
          }
        }

        # Create the full structure
        tags$ul(
          class = "file-tree",
          tags$li(
            tags$span(class = "folder-icon", "📁"),
            "data/",
            tags$ul(data_structure)
          ),
          root_items
        )
      }

      tagList(
        tags$style(HTML("
      .file-tree {
        font-family: monospace;
        line-height: 1.5;
        list-style-type: none;
        padding-left: 20px;
      }
      .file-tree ul {
        padding-left: 20px;
      }
      .file-icon, .folder-icon {
        margin-right: 5px;
      }
    ")),
        create_nested_list()
      )
    }

    proceedToFinalStep <- function() {
      state$file_mappings <- file_mappings()
      
      dataset_name <- gsub("[^a-zA-Z0-9_-]", "_", tolower(state$dataset_info$name))
      suggested_name <- paste0(dataset_name, "_psychds")
      
      # Create preview HTML with proper subdirectory structure
      preview_html <- tagList(
        div(
          style = "margin-top: 15px; margin-bottom: 15px;",
          tags$label("Preview of Dataset Structure", style = "font-weight: bold; display: block; margin-bottom: 8px;"),
          div(
            style = "max-height: 250px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f8f9fa; border-radius: 4px;",
            create_file_structure_html(NULL, file_mappings(), state$optional_dirs)
          )
        )
      )
      
      showModal(modalDialog(
        title = "Choose Dataset Location",
        size = "m",
        
        div(
          class = "alert alert-info",
          style = "margin-bottom: 20px;",
          icon("info-circle", style = "margin-right: 8px;"),
          "Your new Psych-DS dataset will be created as a self-contained folder. ",
          "Choose where to save it and what to name it."
        ),
        
        div(
          style = "margin-bottom: 20px;",
          tags$label("Dataset Name", style = "font-weight: bold; margin-bottom: 8px; display: block;"),
          textInput(
            session$ns("save_dataset_name"),
            label = NULL,
            value = suggested_name,
            placeholder = "my_dataset_psychds",
            width = "100%"
          ),
          tags$small(
            style = "color: #6c757d;",
            "Use only letters, numbers, hyphens, and underscores"
          )
        ),
        
        div(
          style = "margin-bottom: 20px;",
          tags$label("Save Location", style = "font-weight: bold; margin-bottom: 8px; display: block;"),
          div(
            class = "directory-input",
            textInput(
              session$ns("save_dataset_dir"),
              label = NULL,
              value = path.expand("~/Downloads"),
              placeholder = "Choose destination folder",
              width = "100%"
            ),
            shinyDirButton(
              session$ns("save_dataset_dir_select"),
              label = "...",
              title = "Select destination folder",
              class = "browse-btn"
            )
          ),
          tags$small(
            style = "color: #6c757d;",
            "The dataset folder will be created inside this location"
          )
        ),
        
        preview_html,
        
        div(
          class = "alert",
          style = "background-color: #fff3cd; border: 1px solid #ffc107; margin-top: 20px;",
          icon("exclamation-triangle", style = "margin-right: 8px;"),
          tags$strong("Full path: "),
          textOutput(session$ns("save_full_path"), inline = TRUE)
        ),
        
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton(session$ns("confirm_save_location"), "Create Dataset", class = "btn-primary")
        )
      ))
    }

    #
    # Creates the dataset with proper subdirectory structure preservation
    # This is the FIX for the subdirectory issue
    #
    observeEvent(input$confirm_save_location, {
      cat("Create Dataset button clicked\n")
      
      # Validate inputs
      if (is.null(input$save_dataset_name) || input$save_dataset_name == "") {
        showNotification("Please enter a dataset name", type = "error")
        return()
      }
      
      if (is.null(input$save_dataset_dir) || input$save_dataset_dir == "") {
        showNotification("Please select a save location", type = "error")
        return()
      }
      
      if (!grepl("^[a-zA-Z0-9_-]+$", input$save_dataset_name)) {
        showNotification("Dataset name can only contain letters, numbers, hyphens, and underscores", type = "error")
        return()
      }
      
      new_dataset_dir <- file.path(input$save_dataset_dir, input$save_dataset_name)
      
      if (dir.exists(new_dataset_dir)) {
        showNotification("A folder with this name already exists at this location. Please choose a different name.", type = "error")
        return()
      }
      
      tryCatch({
        # Create main dataset directory
        dir.create(new_dataset_dir, showWarnings = FALSE, recursive = TRUE)
        
        # Create data directory
        data_dir <- file.path(new_dataset_dir, "data")
        dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
        
        # Copy files with preserved subdirectory structure
        original_project_dir <- state$project_dir
        files_copied <- 0
        
        for (mapping in file_mappings()) {
          if (mapping$new != "") {
            # Parse original path to extract subdirectory
            file_parts <- strsplit(mapping$original, "/")[[1]]
            
            # Determine destination path with subdirectory
            if (length(file_parts) > 2) {
              # File has subdirectories under data/
              subdir_path <- paste(file_parts[2:(length(file_parts)-1)], collapse = "/")
              
              # Create subdirectory in destination
              dest_subdir <- file.path(data_dir, subdir_path)
              if (!dir.exists(dest_subdir)) {
                dir.create(dest_subdir, recursive = TRUE, showWarnings = FALSE)
                message("Created subdirectory: ", dest_subdir)
              }
              
              dest_path <- file.path(dest_subdir, mapping$new)
            } else {
              # File goes directly in data/
              dest_path <- file.path(data_dir, mapping$new)
            }
            
            # Copy the file
            src_path <- file.path(original_project_dir, mapping$original)
            
            if (file.exists(src_path)) {
              file.copy(src_path, dest_path, overwrite = FALSE)
              files_copied <- files_copied + 1
              message("Copied: ", src_path, " -> ", dest_path)
            } else {
              message("Warning: Source file not found: ", src_path)
            }
          }
        }
        
        message("Total files copied: ", files_copied)
        
        # Create optional directories
        optional_dirs <- state$optional_dirs
        standard_dirs <- c("analysis", "materials", "results", "products", "documentation")
        
        for (dir in standard_dirs) {
          if (optional_dirs[[dir]]) {
            dir.create(file.path(new_dataset_dir, dir), showWarnings = FALSE)
          }
        }
        
        # Create custom directories
        if (!is.null(optional_dirs$custom)) {
          for (custom_dir in optional_dirs$custom) {
            dir.create(file.path(new_dataset_dir, custom_dir), showWarnings = FALSE)
          }
        }
        
        # Generate dataset_description.json
        dataset_info <- state$dataset_info
        
        dataset_description <- list(
          "@context" = "https://schema.org/",
          "@type" = "Dataset",
          "name" = dataset_info$name,
          "description" = dataset_info$description,
          "author" = lapply(dataset_info$authors, function(author) {
            list(
              "@type" = "Person",
              "givenName" = author$first_name,
              "familyName" = author$last_name,
              "@id" = if(!is.null(author$orcid) && author$orcid != "") author$orcid else ""
            )
          }),
          "variableMeasured" = do.call(c, lapply(names(state$data_dict), function(file_name) {
            file_dict <- state$data_dict[[file_name]]
            lapply(rownames(file_dict), function(var_name) {
              list(
                "@type" = "PropertyValue",
                "name" = var_name,
                "description" = file_dict[var_name, "description"]
              )
            })
          }))
        )
        
        dataset_description <- dataset_description[!sapply(dataset_description, is.null)]
        
        json_path <- file.path(new_dataset_dir, "dataset_description.json")
        jsonlite::write_json(dataset_description, json_path, pretty = TRUE, auto_unbox = TRUE)
        
        state$created_dataset_dir <- new_dataset_dir
        
        removeModal()
        
        # Show success modal
        file_structure_preview <- create_file_structure_html(new_dataset_dir, file_mappings, optional_dirs)
        
        showModal(modalDialog(
          title = "Dataset Created Successfully!",
          div(
            class = "alert alert-success",
            style = "margin-bottom: 20px;",
            icon("check-circle", style = "margin-right: 8px; font-size: 20px;"),
            tags$strong("Your Psych-DS dataset has been created at:"),
            tags$br(),
            tags$code(new_dataset_dir, style = "font-size: 14px;")
          ),
          div(
            style = "max-height: 300px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f8f9fa;",
            file_structure_preview
          ),
          footer = tagList(
            modalButton("Close"),
            actionButton(session$ns("validate_dataset"), "Validate Dataset", class = "btn-primary")
          ),
          size = "l",
          easyClose = TRUE
        ))
        
      }, error = function(e) {
        showNotification(paste("Error creating dataset:", e$message), type = "error")
      })
    })

    # Validate created dataset
    observeEvent(input$validate_dataset, {
      cat("Validate dataset button clicked\n")

      full_dataset_dir <- state$created_dataset_dir
      downloads_dir <- path.expand("~/Downloads")
      destination_dir <- file.path(downloads_dir, basename(full_dataset_dir))

      output$dataset_preview <- renderUI({
        div(
          class = "section-box",
          div(class = "section-title", "Dataset Preview"),
          div(
            style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f8f9fa;",
            create_file_structure_html(destination_dir,
                                       lapply(list.files(file.path(destination_dir, "data"), recursive = TRUE, full.names = FALSE),
                                              function(f) list(original = f, new = f)),state$optional_dirs)
          ),
          div(
            class = "section-title",
            style = "margin-top: 15px;",
            "Dataset Description"
          ),
          div(
            style = "border: 1px solid #ddd; padding: 10px; background-color: #f8f9fa; white-space: pre-wrap; font-family: monospace;",
            paste(readLines(file.path(destination_dir, "dataset_description.json")), collapse = "\n")
          )
        )
      })

      removeModal()
      session$sendCustomMessage("changeTab", list(tabName = "validate"))
    })

    # Return reactive file mappings
    return(reactive({ file_mappings() }))
  })
}

check_validator_button <- function(id, session) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$check_validator, {
      # Insert JavaScript directly into the page
      insertUI(
        selector = "body",
        where = "beforeEnd",
        ui = tags$script(HTML('
          console.log("Manual validator check requested");
          var validatorStatus = "Validator NOT found";

          if (typeof window.psychDSValidator !== "undefined") {
            validatorStatus = "VALIDATOR FOUND! Methods: " + Object.keys(window.psychDSValidator).join(", ");
            console.log(validatorStatus);
          } else {
            console.error(validatorStatus);
          }

          // Show a visible alert that users can see
          alert(validatorStatus);

          // Also try to communicate back to Shiny
          if (typeof Shiny !== "undefined") {
            Shiny.setInputValue("validator_status", validatorStatus);
          } else {
            console.error("Shiny object not available");
          }
        '))
      )

      # Also display a notification directly from R
      showNotification("Check validator executed - look for browser alert", type = "message")
    })

    # This is the handler if Shiny communication works
    observeEvent(input$validator_status, {
      showNotification(paste("Validator status:", input$validator_status),
                       type = "message",
                       duration = 10)
    })
  })
}

check_validator_available <- function() {
  shiny::insertUI(
    selector = "head",
    where = "beforeEnd",
    ui = tags$script(HTML("
      console.log('Checking validator availability...');
      if (typeof window.psychDSValidator !== 'undefined') {
        console.log('Validator is available');
        Shiny.setInputValue('validator_available', true);
      } else {
        console.error('Validator is NOT available');
        Shiny.setInputValue('validator_available', false);
      }
    "))
  )
}

validateServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    # Track validation status
    validation_status <- reactiveValues(
      is_validating = FALSE,
      is_complete = FALSE,
      is_valid = FALSE,
      step_statuses = list() 
    )
    
    # Define steps for the UI rendering
    validation_steps <- list(
      list(
        key = "start",
        message = list(
          imperative = "Start validation",
          pastTense = "Validation started"
        ),
        subSteps = list()
      ),
      list(
        key = "check-folder",
        message = list(
          imperative = "Find project folder",
          pastTense = "Project folder found"
        ),
        subSteps = list(
          list(
            key = "build-tree",
            message = list(
              imperative = "Crawl project folder and construct file tree",
              pastTense = "Project folder crawled and file tree constructed"
            )
          )
        )
      ),
      list(
        key = "find-metadata",
        message = list(
          imperative = "Find metadata file",
          pastTense = 'Metadata file "dataset_description.json" found in the root folder'
        ),
        subSteps = list()
      ),
      list(
        key = "find-data-dir",
        message = list(
          imperative = 'Find "data" subfolder',
          pastTense = '"data" subfolder found in the root folder'
        ),
        subSteps = list()
      ),
      list(
        key = "parse-metadata",
        message = list(
          imperative = 'Parse "dataset_description.json" metadata file',
          pastTense = 'Successfully parsed "dataset_description.json" metadata file'
        ),
        subSteps = list(
          list(
            key = "metadata-utf8",
            message = list(
              imperative = "Check metadata file for utf-8 encoding",
              pastTense = "Metadata file is utf-8 encoded"
            )
          ),
          list(
            key = "metadata-json",
            message = list(
              imperative = "Parse metadata file as JSON",
              pastTense = "Metadata file parsed successfully"
            )
          ),
          list(
            key = "metadata-jsonld",
            message = list(
              imperative = "Validate metadata file as JSON-LD",
              pastTense = "Metadata file is valid JSON-LD"
            )
          ),
          list(
            key = "metadata-fields",
            message = list(
              imperative = 'Check metadata file for required "name", "description", and "variableMeasured" fields',
              pastTense = 'Metadata file contains required "name", "description", and "variableMeasured" fields.'
            )
          ),
          list(
            key = "metadata-type",
            message = list(
              imperative = 'Check metadata file for field "@type" with value "Dataset"',
              pastTense = 'Metadata file has "@type" field with value "Dataset"'
            )
          )
        )
      ),
      list(
        key = "check-for-csv",
        message = list(
          imperative = 'Check for CSV data files in "data" subfolder',
          pastTense = 'CSV data files found in "data" subfolder'
        ),
        subSteps = list()
      ),
      list(
        key = "validate-csvs",
        message = list(
          imperative = 'Check that all CSV data files are valid',
          pastTense = 'All CSV data files are valid'
        ),
        subSteps = list(
          list(
            key = "csv-keywords",
            message = list(
              imperative = 'Check filename for keyword formatting ',
              pastTense = 'Filename uses valid keyword formatting'
            )
          ),
          list(
            key = "csv-parse",
            message = list(
              imperative = 'Parse data file as CSV',
              pastTense = 'Data file successfully parsed as CSV'
            )
          ),
          list(
            key = "csv-header",
            message = list(
              imperative = 'Check for header line',
              pastTense = 'Header line found'
            )
          ),
          list(
            key = "csv-header-repeat",
            message = list(
              imperative = 'Check for redundant column names',
              pastTense = 'No redundant column names found'
            )
          ),
          list(
            key = "csv-nomismatch",
            message = list(
              imperative = 'Check all lines for equal number of cells',
              pastTense = 'All lines have equal number of cells'
            )
          ),
          list(
            key = "csv-rowid",
            message = list(
              imperative = 'Check for any row_id columns with non-unique values',
              pastTense = 'All row_id columns have unique values'
            )
          )
        )
      ),
      list(
        key = "check-variableMeasured",
        message = list(
          imperative = 'Confirm that all column headers in CSV data files are found in "variableMeasured" metadata field',
          pastTense = 'All column headers in CSV data files were found in "variableMeasured" metadata field'
        ),
        subSteps = list()
      )
    )
    
    # Store steps for UI reference
    validation_status$steps <- validation_steps
    
    #' Custom function to print validation status in a readable format
    #' @param validation_obj The validation status object from JavaScript
    print_validation_status <- function(validation_obj) {
      cat("\n=== VALIDATION STEP UPDATE ===\n")
      
      if (is.null(validation_obj)) {
        cat("Validation object is NULL\n")
        return()
      }
      
      # Print the raw structure first
      cat("Raw object structure:\n")
      cat("- Class:", class(validation_obj), "\n")
      cat("- Length:", length(validation_obj), "\n")
      cat("- Names:", paste(names(validation_obj), collapse = ", "), "\n\n")
      
      # Check if stepStatus exists
      if ("stepStatus" %in% names(validation_obj)) {
        step_status <- validation_obj$stepStatus
        cat("stepStatus found with", length(step_status), "entries\n")
        
        # Process each step
        for (i in seq_along(step_status)) {
          step_entry <- step_status[[i]]
          
          cat("\n--- Step", i, "---\n")
          cat("Entry class:", class(step_entry), "\n")
          cat("Entry length:", length(step_entry), "\n")
          
          if (length(step_entry) >= 2) {
            # Extract step key and status
            step_key <- step_entry[[1]]
            step_info <- step_entry[[2]]
            
            cat("Step Key:", step_key, "\n")
            cat("Step Info Class:", class(step_info), "\n")
            
            # Print step info details
            if (is.list(step_info)) {
              cat("Step Info Contents:\n")
              for (prop_name in names(step_info)) {
                prop_value <- step_info[[prop_name]]
                cat("  ", prop_name, ":", prop_value, "(", class(prop_value), ")\n")
              }
              
              # Check for issue details
              if ("issue" %in% names(step_info) && !is.null(step_info$issue)) {
                cat("  Issue Details:\n")
                issue <- step_info$issue
                for (issue_prop in names(issue)) {
                  cat("    ", issue_prop, ":", issue[[issue_prop]], "\n")
                }
              }
            } else {
              cat("Step Info (non-list):", step_info, "\n")
            }
            
            # Create readable status summary
            if (is.list(step_info) && "complete" %in% names(step_info)) {
              complete <- step_info$complete
              success <- if ("success" %in% names(step_info)) step_info$success else FALSE
              
              status_icon <- if (complete) {
                if (success) "[PASS]" else "[FAIL]"
              } else "[PENDING]"
              
              cat("Summary:", status_icon, step_key, "\n")
              
              # Show any error details
              if (!is.null(step_info$issue)) {
                cat("  ERROR:", step_info$issue$reason %||% "Unknown error", "\n")
              }
            }
          } else {
            cat("Invalid step entry (length < 2)\n")
          }
        }
      } else {
        cat("No stepStatus found in validation object\n")
        cat("Available properties:", paste(names(validation_obj), collapse = ", "), "\n")
      }
      
      cat("===============================\n\n")
    }

    # Replace your existing observeEvent with this:
    observeEvent(input$validation_step_status, {
      print_validation_status(input$validation_step_status)
      
      # Continue with existing logic for updating validation_status
      if (!is.null(input$validation_step_status) && 
          !is.null(input$validation_step_status$stepStatus)) {
        
        step_updates <- input$validation_step_status$stepStatus
        
        for (i in seq_along(step_updates)) {
          step_entry <- step_updates[[i]]
          
          if (length(step_entry) >= 2) {
            step_key <- step_entry[[1]]
            step_status <- step_entry[[2]]
            
            # Update the step status in our reactive
            validation_status$step_statuses[[step_key]] <- list(
              complete = step_status$complete,
              success = step_status$success,
              issue = step_status$issue
            )
          }
        }
        
        # Start validation mode if first update
        if (!validation_status$is_validating) {
          validation_status$is_validating <- TRUE
        }
      }
    }, ignoreNULL = TRUE)
    
    # Handle validation complete events
    observeEvent(input$validation_complete, {
      message("Validation complete event received")
      validation_status$is_complete <- TRUE
      validation_status$is_valid <- TRUE
    })
    
    # Handle validation halted events
    observeEvent(input$validation_halted, {
      message("Validation halted event received")
      validation_status$is_complete <- TRUE
      validation_status$is_valid <- FALSE
    })
    
    # Handle validation results
    observeEvent(input$validation_results, {
      if (!is.null(input$validation_results)) {
        message("Validation results received")
        validation_status$is_complete <- TRUE
        validation_status$is_valid <- input$validation_results$valid
        validation_status$is_validating <- FALSE
        state$validation_results <- input$validation_results
        
        # Store the validated directory path for later use
        if (!is.null(input$validate_dir) && input$validate_dir != "") {
          state$validated_dataset_dir <- input$validate_dir
        }
      }
    })

    # Add this to your validateServer function in server_modules.R
    # Replace the existing observeEvent for validation_step_status with this:

    observeEvent(input$validation_step_status, {
      cat("*** STEP STATUS EVENT RECEIVED ***\n")
      
      if (!is.null(input$validation_step_status)) {
        # Convert the validation data to pretty JSON for display
        json_output <- tryCatch({
          jsonlite::toJSON(input$validation_step_status, pretty = TRUE, auto_unbox = TRUE)
        }, error = function(e) {
          paste("Error converting to JSON:", e$message)
        })
        
        # Show the validation data in a modal popup
        showModal(modalDialog(
          title = "Validation Step Status Debug",
          div(
            h4("Raw Validation Data:"),
            tags$pre(
              style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; max-height: 400px; overflow-y: auto; font-family: monospace; font-size: 12px;",
              json_output
            ),
            hr(),
            h4("Data Structure Info:"),
            tags$ul(
              tags$li(paste("Class:", paste(class(input$validation_step_status), collapse = ", "))),
              tags$li(paste("Length:", length(input$validation_step_status))),
              tags$li(paste("Names:", paste(names(input$validation_step_status), collapse = ", "))),
              if ("stepStatus" %in% names(input$validation_step_status)) {
                tags$li(paste("stepStatus length:", length(input$validation_step_status$stepStatus)))
              }
            )
          ),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        # Also call the original processing logic
        if (!is.null(input$validation_step_status$stepStatus)) {
          step_updates <- input$validation_step_status$stepStatus
          
          for (i in seq_along(step_updates)) {
            step_entry <- step_updates[[i]]
            
            if (length(step_entry) >= 2) {
              step_key <- step_entry[[1]]
              step_status <- step_entry[[2]]
              
              # Update the step status in our reactive
              validation_status$step_statuses[[step_key]] <- list(
                complete = step_status$complete,
                success = step_status$success,
                issue = step_status$issue
              )
            }
          }
          
          # Start validation mode if first update
          if (!validation_status$is_validating) {
            validation_status$is_validating <- TRUE
          }
        }
      }
    }, ignoreNULL = TRUE)
    
    # Render the validation UI with checklist
    output$validation_results_ui <- renderUI({
      # If validation hasn't started or is in progress, show appropriate message
      if (!validation_status$is_validating && is.null(state$validation_results)) {
        return(div(
          style = "text-align: center; padding: 20px;",
          p("Select a dataset directory and click 'Validate' to check compliance with Psych-DS standard.")
        ))
      }
      
      # Debug text to show current status
      status_text <- paste(
        "Validating:", validation_status$is_validating,
        "Complete:", validation_status$is_complete,
        "Valid:", validation_status$is_valid,
        "Steps:", length(validation_status$step_statuses)
      )
      
      # If validation is in progress or results are available, show the checklist
      checklist <- div(
        h3("Validation Progress", 
           if (validation_status$is_complete) {
             span(
               style = paste0("color: ", ifelse(validation_status$is_valid, "#4caf50", "#f44336"), "; margin-left: 10px;"),
               ifelse(validation_status$is_valid, "✓ Dataset is valid", "✗ Dataset has issues")
             )
           }
        ),
        p(style = "color: #999; font-style: italic;", status_text),
        
        # Main steps
        div(
          class = "validation-checklist",
          style = "max-height: 500px; overflow-y: auto;",
          
          # Check if steps are defined
          if (length(validation_status$steps) > 0) {
            lapply(validation_status$steps, function(step) {
              # Get status for this step
              status <- validation_status$step_statuses[[step$key]]
              
              # Default status if not found
              if (is.null(status)) {
                status <- list(complete = FALSE, success = FALSE)
              }
              
              # Determine message and icon
              message <- if (status$complete) step$message$pastTense else step$message$imperative
              icon <- if (status$complete) {
                if (status$success) "✓" else "✗"
              } else "⋯"
              icon_color <- if (status$complete) {
                if (status$success) "#4caf50" else "#f44336"
              } else "#ffc107"
              
              div(
                class = "step-item",
                style = "margin-bottom: 15px; padding: 10px; border-radius: 5px; background-color: #f9f9f9; border: 1px solid #ddd;",
                
                div(
                  class = "step-header",
                  style = "font-weight: bold; display: flex; align-items: center;",
                  span(icon, style = paste0("color: ", icon_color, "; margin-right: 10px; font-size: 18px;")),
                  message
                ),
                
                # Display issue if there is one
                if (!is.null(status$issue)) {
                  div(
                    class = "step-issue",
                    style = "margin-top: 10px; color: #f44336; padding-left: 20px;",
                    p(strong("Issue:"), status$issue$reason)
                  )
                },
                
                # Render substeps if any
                if (length(step$subSteps) > 0) {
                  div(
                    class = "substeps",
                    style = "margin-top: 10px; margin-left: 20px; padding-left: 10px; border-left: 2px solid #ddd;",
                    
                    lapply(step$subSteps, function(subStep) {
                      # Get substep status
                      subStatus <- validation_status$step_statuses[[subStep$key]]
                      
                      # Default status if not found
                      if (is.null(subStatus)) {
                        subStatus <- list(complete = FALSE, success = FALSE)
                      }
                      
                      # Determine message and icon
                      subMessage <- if (subStatus$complete) subStep$message$pastTense else subStep$message$imperative
                      subIcon <- if (subStatus$complete) {
                        if (subStatus$success) "✓" else "✗"
                      } else "⋯"
                      subIconColor <- if (subStatus$complete) {
                        if (subStatus$success) "#4caf50" else "#f44336"
                      } else "#ffc107"
                      
                      div(
                        class = "substep-item",
                        style = "margin-bottom: 8px; display: flex; align-items: flex-start;",
                        
                        span(subIcon, style = paste0("color: ", subIconColor, "; margin-right: 10px;")),
                        div(
                          style = "flex: 1;",
                          p(style = "margin: 0;", subMessage),
                          
                          # Display issue if there is one
                          if (!is.null(subStatus$issue)) {
                            div(
                              class = "substep-issue",
                              style = "margin-top: 5px; color: #f44336; font-size: 0.9em;",
                              p(strong("Issue:"), subStatus$issue$reason)
                            )
                          }
                        )
                      )
                    })
                  )
                }
              )
            })
          } else {
            div("No validation steps defined")
          }
        )
      )
      
      # If validation is complete, also show the results summary
      if (validation_status$is_complete && !is.null(state$validation_results)) {
        result <- state$validation_results
        
        # Full results with checklist and summary
        return(tagList(
          # Checklist
          div(
            class = "section-box",
            style = "margin-bottom: 20px;",
            div(class = "section-title", "Validation Process"),
            checklist
          ),
          
          # Summary
          div(
            class = "section-box",
            div(class = "section-title", "Results Summary"),
            div(
              style = paste0("padding: 15px; border-radius: 5px; background-color: ", 
                            ifelse(result$valid, "#e8f5e9", "#ffebee"), ";"),
              h3(
                style = paste0("color: ", ifelse(result$valid, "#4caf50", "#f44336"), ";"),
                ifelse(result$valid, "✓ Dataset is valid", "✗ Dataset has validation errors")
              ),
              
              # Summary details
              div(
                style = "margin-top: 15px;",
                h4("Details:"),
                tags$ul(
                  tags$li(paste0("Total files scanned: ", result$summary$totalFiles)),
                  if (!is.null(result$summary$dataTypes) && length(result$summary$dataTypes) > 0) {
                    tags$li(paste0("Data types found: ", paste(result$summary$dataTypes, collapse = ", ")))
                  },
                  tags$li(
                    span(style = paste0("color: ", ifelse(result$valid, "#4caf50", "#f44336"), "; font-weight: bold;"), 
                         ifelse(result$valid, 
                               "This dataset appears to be Psych-DS compatible", 
                               "This dataset does not appear to be Psych-DS compatible"))
                  )
                )
              ),
              
              # Only show errors section if there are any
              if (!result$valid && !is.null(result$issues) && !is.null(result$issues$errors) && length(result$issues$errors) > 0) {
                div(
                  style = "margin-top: 15px;",
                  h4("Errors:"),
                  tags$ul(
                    lapply(result$issues$errors, function(error) {
                      tags$li(
                        p(style = "font-weight: bold;", error$key),
                        p(error$reason)
                      )
                    })
                  )
                )
              }
            )
          )
        ))
      } else {
        # Just return the checklist during validation
        return(div(
          class = "section-box",
          div(class = "section-title", "Validation Progress"),
          checklist
        ))
      }
    })

    
    
    # Handle the validate button click
    observeEvent(input$validate_btn, {
      req(input$validate_dir)
      
      if (dir.exists(input$validate_dir)) {
        # Reset validation state
        validation_status$is_validating <- TRUE
        validation_status$is_complete <- FALSE
        validation_status$is_valid <- FALSE
        validation_status$step_statuses <- list()
        state$validation_results <- NULL
        
        # Show a loading notification
        showNotification(
          "Validating dataset... This may take a moment", 
          id = "validate_notif", 
          type = "message",
          duration = NULL
        )
        
        # Try to build the file tree and run validation
        tryCatch({
          # Build the file tree
          fileTree <- buildFileTree(input$validate_dir)
          
          # Send the file tree to the JavaScript validator
          session$sendCustomMessage("run_validation", fileTree)
        }, error = function(e) {
          removeNotification(id = "validate_notif")
          showNotification(paste("Error:", e$message), type = "error")
          validation_status$is_validating <- FALSE
        })
      } else {
        showNotification("Directory does not exist", type = "error")
      }
    })
    
    # Similar implementation for test validation button
    observeEvent(input$test_validation, {
      # Reset validation state
      validation_status$is_validating <- TRUE
      validation_status$is_complete <- FALSE
      validation_status$is_valid <- FALSE
      validation_status$step_statuses <- list()
      state$validation_results <- NULL
      
      # Show a loading notification
      showNotification(
        "Running test validation... This may take a moment", 
        id = "validate_notif", 
        type = "message",
        duration = NULL
      )
      
      # Create a test file tree and run validation
      tryCatch({
        # Create test file tree
        testTree <- createTestFileTree()
        
        # Send the test file tree to the JavaScript validator
        session$sendCustomMessage("run_validation", testTree)
      }, error = function(e) {
        removeNotification(id = "validate_notif")
        showNotification(paste("Error:", e$message), type = "error")
        validation_status$is_validating <- FALSE
      })
    })
    

    # Add debugging for all validation-related events
    observeEvent(input$validation_complete, {
      cat("VALIDATION COMPLETE EVENT - Value:", input$validation_complete, "\n")
    }, ignoreNULL = TRUE)

    observeEvent(input$validation_halted, {
      cat("VALIDATION HALTED EVENT - Value:", input$validation_halted, "\n")
    }, ignoreNULL = TRUE)

    # Debug JavaScript communication
    observeEvent(input$validate_btn, {
      cat("VALIDATE BUTTON CLICKED - Preparing validation\n")
      cat("Directory:", input$validate_dir, "\n")
      
      # Add JavaScript debugging
      session$sendCustomMessage("debug_validator", list(
        message = "Starting validation debug mode",
        timestamp = Sys.time()
      ))
    })

    # Add a test message handler to verify communication
    observeEvent(input$test_js_communication, {
      cat("Test JS communication received:", input$test_js_communication, "\n")
    }, ignoreNULL = TRUE)
  })
}

#' Data Dictionary Server Module
#'
#' Server logic for the data dictionary editor
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
dataDictionaryServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for dictionary state
    dictionary_state <- reactiveValues(
      dataset_path = NULL,
      variables = list(),
      current_variable = NULL,
      variable_data = list(),
      is_modified = FALSE,
      editing_cat_index = NULL,  # Add this for tracking which categorical value is being edited
      missing_values = c("NA", "N/A", "null", "NULL", "-999", "missing")
    )

    # Initialize categorical values storage for current variable
    categorical_values <- reactiveVal(list())

    # Function to save current variable state
    saveCurrentVariable <- function() {
      if (!is.null(dictionary_state$current_variable)) {
        var_name <- dictionary_state$current_variable
        
        # Update variable data including new fields
        dictionary_state$variables[[var_name]]$description <- input$var_description %||% ""
        dictionary_state$variables[[var_name]]$type <- input$var_type %||% "string"
        dictionary_state$variables[[var_name]]$unit <- input$var_unit %||% ""
        
        # Only save min/max if checkbox is enabled
        if (input$var_use_minmax %||% FALSE) {
          dictionary_state$variables[[var_name]]$min_value <- input$var_min %||% ""
          dictionary_state$variables[[var_name]]$max_value <- input$var_max %||% ""
        } else {
          dictionary_state$variables[[var_name]]$min_value <- ""
          dictionary_state$variables[[var_name]]$max_value <- ""
        }
        
        dictionary_state$variables[[var_name]]$value_reference <- input$var_value_reference %||% ""
        dictionary_state$variables[[var_name]]$default_value <- input$var_default %||% ""
        dictionary_state$variables[[var_name]]$required <- input$var_required %||% FALSE
        dictionary_state$variables[[var_name]]$unique <- input$var_unique %||% FALSE
        dictionary_state$variables[[var_name]]$pattern <- input$var_pattern %||% ""
        dictionary_state$variables[[var_name]]$source <- input$var_source %||% ""
        dictionary_state$variables[[var_name]]$notes <- input$var_notes %||% ""
        
        # Save categorical values
        cat_values <- categorical_values()
        dictionary_state$variables[[var_name]]$categorical_values <- cat_values
        
        dictionary_state$is_modified <- TRUE
      }
    }

    observeEvent(input$remove_missing_value, {
      index <- input$remove_missing_value
      current_values <- dictionary_state$missing_values
      
      if (!is.null(index) && index > 0 && index <= length(current_values)) {
        dictionary_state$missing_values <- current_values[-index]
        showNotification("Missing value code removed", type = "message")
      }
    }, ignoreInit = TRUE)
    
    # Track if dataset is loaded
    output$dataset_loaded <- reactive({
      !is.null(dictionary_state$dataset_path)
    })
    outputOptions(output, "dataset_loaded", suspendWhenHidden = FALSE)
    
    # Track if variable is selected  
    output$variable_selected <- reactive({
      !is.null(dictionary_state$current_variable)
    })
    outputOptions(output, "variable_selected", suspendWhenHidden = FALSE)

    output$missing_values_table <- renderUI({
      values <- dictionary_state$missing_values
      
      if (length(values) == 0) {
        return(div(
          style = "padding: 20px; text-align: center; color: #6c757d;",
          "No missing value codes defined. Add common codes like NA, -999, etc."
        ))
      }
      
      rows <- lapply(seq_along(values), function(i) {
        div(
          style = "display: flex; padding: 8px; border-bottom: 1px solid #ced4da; align-items: center;",
          div(style = "flex: 3; padding-right: 10px; font-family: monospace;", values[i]),
          div(
            style = "flex: 0; width: 60px;",
            actionButton(
              session$ns(paste0("remove_missing_", i)),
              label = NULL,
              icon = icon("trash"),
              class = "btn btn-sm btn-danger",
              style = "padding: 2px 6px;",
              onclick = paste0("Shiny.setInputValue('", session$ns("remove_missing_value"), "', ", i, ", {priority: 'event'});")
            )
          )
        )
      })
      
      do.call(tagList, rows)
    })
    
    # Add missing value
    observeEvent(input$add_missing_value, {
      if (!is.null(input$new_missing_value) && input$new_missing_value != "") {
        current_values <- dictionary_state$missing_values
        
        # Check for duplicates
        if (input$new_missing_value %in% current_values) {
          showNotification("This missing value code already exists", type = "warning")
          return()
        }
        
        # Add new value
        dictionary_state$missing_values <- c(current_values, input$new_missing_value)
        
        # Clear input
        updateTextInput(session, "new_missing_value", value = "")
        
        showNotification("Missing value code added", type = "message")
      } else {
        showNotification("Please enter a value", type = "warning")
      }
    })
    
    # Handler for Generate Human-Readable Dictionary button
    observeEvent(input$generate_dictionary, {
      req(dictionary_state$dataset_path)
      
      ns <- session$ns
      
      showModal(modalDialog(
        title = "Generate Data Dictionary",
        size = "m",
        
        tags$div(
          tags$p("Generate a professional, printable data dictionary with all variable 
                  definitions, descriptions, and metadata."),
          tags$br(),
          tags$div(
            class = "alert alert-info",
            style = "margin-bottom: 15px;",
            icon("file-alt", style = "margin-right: 8px;"),
            tags$strong("Format: "), "HTML Document",
            tags$br(),
            tags$small("Open in any browser • Print to PDF with Ctrl+P / Cmd+P")
          )
        ),
        
        checkboxInput(
          inputId = ns("include_stats"),
          label = "Include summary statistics (if available)",
          value = TRUE
        ),
        
        checkboxInput(
          inputId = ns("include_missing"),
          label = "Include global missing value codes",
          value = TRUE
        ),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(
            inputId = ns("confirm_generate"), 
            label = "Generate Dictionary", 
            class = "btn-primary",
            icon = icon("file-alt")
          )
        )
      ))
    })

    observeEvent(input$setup_pdf, {
      removeModal()
      
      showModal(modalDialog(
        title = "Setup PDF Generation",
        size = "m",
        
        tags$div(
          tags$h4("To enable PDF generation, run this in your R console:"),
          tags$pre(
            style = "background-color: #f5f5f5; padding: 10px; border-radius: 4px;",
            "# Recommended: Install TinyTeX (lightweight LaTeX)\n",
            "psychds::setup_pdf_generation()\n\n",
            "# Or manually:\n",
            "install.packages('tinytex')\n",
            "tinytex::install_tinytex()"
          ),
          tags$hr(),
          tags$p("After installation, restart your R session and try again."),
          tags$p("Alternative: You can always generate HTML and print to PDF from your browser.")
        ),
        
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
    })

# Handler for confirming generation
observeEvent(input$confirm_generate, {
  ns <- session$ns
  removeModal()

  saveCurrentVariable()
  
  prog_id <- showNotification(
    "Generating data dictionary...", 
    duration = NULL, 
    type = "message",
    closeButton = FALSE
  )
  
  dict_data <- list(
    variables = dictionary_state$variables,
    missing_values = if(isTRUE(input$include_missing)) dictionary_state$missing_values else NULL
  )
  
  dataset_info <- NULL
  desc_file <- file.path(dictionary_state$dataset_path, "dataset_description.json")
  if (file.exists(desc_file)) {
    tryCatch({
      dataset_info <- jsonlite::fromJSON(desc_file, simplifyVector = FALSE)
    }, error = function(e) {})
  }
  
  output_file <- file.path(
    dictionary_state$dataset_path,
    paste0("data_dictionary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
  )
  
  result <- generate_dictionary_html(
    dictionary_data = dict_data,
    dataset_info = dataset_info,
    output_file = output_file,
    include_stats = input$include_stats
  )
  
  removeNotification(prog_id)
  
  if (result$success) {
    showModal(modalDialog(
      title = "Success!",
      size = "m",
      
      tags$div(
        tags$div(
          style = "text-align: center; margin-bottom: 20px;",
          icon("check-circle", style = "font-size: 48px; color: #28a745;")
        ),
        tags$h4("Dictionary Generated!", style = "text-align: center; color: #28a745;"),
        tags$hr(),
        tags$p("Saved to:"),
        tags$div(
          style = "background-color: #f8f9fa; padding: 12px; border-radius: 6px; 
                   margin: 15px 0; word-break: break-all;",
          tags$code(style = "font-size: 12px;", result$file)
        ),
        tags$div(
          class = "alert alert-info",
          style = "margin-top: 15px;",
          icon("lightbulb", style = "margin-right: 8px;"),
          "Open in browser, then press ", tags$kbd("Ctrl+P"), " / ", 
          tags$kbd("Cmd+P"), " to save as PDF."
        )
      ),
      
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  } else {
    showNotification(
      paste("Failed to generate dictionary:", result$error),
      type = "error",
      duration = 10
    )
  }
})









  #' Smart PDF Dictionary Generator
  #' 
  #' Automatically selects the best available method for PDF generation:
  #' 1. Try TinyTeX/LaTeX if available (best quality)
  #' 2. Fall back to pagedown if available (good quality, no LaTeX)
  #' 3. Fall back to HTML if neither available
  #' 
  #' @param dictionary_data The dictionary data
  #' @param dataset_info Dataset metadata
  #' @param output_file Output file path
  #' @param include_stats Include statistics
  #' @param format Requested format ("pdf", "html", "auto")
  #' @return List with status and file path
  generate_dictionary <- function(dictionary_data, 
                                      dataset_info = NULL,
                                      output_file = NULL,
                                      include_stats = TRUE,
                                      format = "auto") {
    
    # Check capabilities
    caps <- check_pdf_capabilities()
    
    # Determine actual format based on capabilities
    actual_format <- format
    method_used <- NULL
    
    if (format == "pdf" || format == "auto") {
      if (caps$has_rmarkdown && caps$has_system_latex) {
        # Best option: Use LaTeX (either TinyTeX or system)
        actual_format <- "pdf"
        method_used <- if (caps$has_tinytex) "TinyTeX" else "System LaTeX"
        
      } else if (caps$has_pagedown) {
        # Good option: Use pagedown
        actual_format <- "pdf"
        method_used <- "pagedown (Chrome-based)"
        
      } else {
        # Fallback: HTML only
        actual_format <- "html"
        method_used <- "HTML (PDF not available)"
        
        if (format == "pdf") {
          # User specifically requested PDF but we can't do it
          message("PDF generation not available, creating HTML instead")
        }
      }
    } else {
      actual_format <- "html"
      method_used <- "HTML (requested)"
    }
    
    # Set output file with correct extension
    if (is.null(output_file)) {
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      extension <- if (actual_format == "pdf") ".pdf" else ".html"
      output_file <- paste0("data_dictionary_", timestamp, extension)
    } else {
      # Ensure correct extension
      if (actual_format == "pdf" && !grepl("\\.pdf$", output_file)) {
        output_file <- gsub("\\.[^.]+$", ".pdf", output_file)
      } else if (actual_format == "html" && !grepl("\\.html$", output_file)) {
        output_file <- gsub("\\.[^.]+$", ".html", output_file)
      }
    }
    
    # Generate based on selected method
    result <- tryCatch({
      
      if (actual_format == "pdf" && method_used == "pagedown (Chrome-based)") {
        # Use pagedown method
        message("Generating PDF using pagedown...")
        
        generated_file <- generate_pdf_dictionary_pagedown(
          dictionary_data = dictionary_data,
          dataset_info = dataset_info,
          output_file = output_file,
          include_stats = include_stats
        )
        
      } else if (actual_format == "pdf" && caps$has_rmarkdown) {
        # Use standard rmarkdown/LaTeX method
        message(paste0("Generating PDF using ", method_used, "..."))
        
        generated_file <- generate_pdf_dictionary(
          dictionary_data = dictionary_data,
          dataset_info = dataset_info,
          output_file = output_file,
          include_stats = include_stats
        )
        
      } else if (caps$has_rmarkdown) {
        # Generate nice HTML using rmarkdown
        message("Generating HTML document using rmarkdown...")
        
        # Force HTML output in the standard generator
        html_file <- gsub("\\.pdf$", ".html", output_file)
        generated_file <- generate_pdf_dictionary(
          dictionary_data = dictionary_data,
          dataset_info = dataset_info,
          output_file = html_file,
          include_stats = include_stats
        )
        
      } else {
        # Use simple HTML generator
        message("Generating HTML document...")
        
        generated_file <- generate_html_dictionary_simple(
          dictionary_data = dictionary_data,
          output_file = output_file
        )
      }
      
      list(
        success = TRUE,
        file = generated_file,
        format = actual_format,
        method = method_used
      )
      
    }, error = function(e) {
      # If primary method fails, try fallback
      if (actual_format == "pdf") {
        message("PDF generation failed, falling back to HTML...")
        
        # Try simple HTML as last resort
        tryCatch({
          generated_file <- generate_html_dictionary_simple(
            dictionary_data = dictionary_data,
            output_file = gsub("\\.pdf$", ".html", output_file)
          )
          
          list(
            success = TRUE,
            file = generated_file,
            format = "html",
            method = "HTML (fallback)",
            warning = paste("PDF generation failed:", e$message)
          )
          
        }, error = function(e2) {
          list(
            success = FALSE,
            error = e2$message,
            format = NULL,
            method = NULL
          )
        })
        
      } else {
        list(
          success = FALSE,
          error = e$message,
          format = actual_format,
          method = method_used
        )
      }
    })
    
    return(result)
  }

    # Handler for download button (appears in success modal)
    output$download_dict <- downloadHandler(
      filename = function() {
        if (input$output_format == "pdf") {
          paste0("data_dictionary_", format(Sys.Date(), "%Y%m%d"), ".pdf")
        } else {
          paste0("data_dictionary_", format(Sys.Date(), "%Y%m%d"), ".html")
        }
      },
      content = function(file) {
        # The file has already been generated, just copy it
        generated_file <- if (input$output_format == "pdf") {
          list.files(dictionary_state$dataset_path, 
                    pattern = "data_dictionary.*\\.pdf$", 
                    full.names = TRUE)[1]
        } else {
          list.files(dictionary_state$dataset_path, 
                    pattern = "data_dictionary.*\\.html$", 
                    full.names = TRUE)[1]
        }
        
        if (file.exists(generated_file)) {
          file.copy(generated_file, file, overwrite = TRUE)
        }
      }
    )

    # Handler for "Generate Another Format" button
    observeEvent(input$generate_another, {
      removeModal()
      # Trigger the generate dialog again
      shinyjs::click("generate_dictionary")
    })

    # Handler for "Try HTML Instead" button (shown on PDF failure)
    observeEvent(input$try_html_instead, {
      removeModal()
      
      # Update format to simple HTML and regenerate
      updateRadioButtons(session, "output_format", selected = "simple_html")
      
      # Trigger generation with HTML format
      shinyjs::delay(100, {
        shinyjs::click("confirm_generate")
      })
    })
    
    # Display dataset info
    output$dataset_info <- renderUI({
      if (!is.null(dictionary_state$dataset_path)) {
        dataset_name <- basename(dictionary_state$dataset_path)
        variable_count <- length(dictionary_state$variables)
        
        div(
          icon("check-circle", style = "color: #28a745; margin-right: 8px;"),
          strong("Dataset loaded: "), dataset_name,
          span(style = "margin-left: 15px; color: #6c757d;",
               paste(variable_count, "variables detected"))
        )
      }
    })
    
    # Set up directory selection for modal
    volumes <- c(Home = "~")
    if (.Platform$OS.type == "windows") {
      volumes <- c(volumes, getVolumes()())
    }
    
    shinyDirChoose(
      input,
      "dataset_dir_select", 
      roots = volumes,
      session = session,
      restrictions = system.file(package = "base")
    )

    observeEvent(input$dataset_dir_select, {
      if (!is.null(input$dataset_dir_select)) {
        selected_dir <- parseDirPath(volumes, input$dataset_dir_select)
        if (length(selected_dir) > 0 && selected_dir != "") {
          updateTextInput(session, "dataset_dir", value = selected_dir)
        }
      }
    })
    
    # Load dataset when button clicked
    observeEvent(input$load_dataset_btn, {
      dataset_path <- input$dataset_dir
      
      if (dataset_path == "" || !dir.exists(dataset_path)) {
        showNotification("Please select a valid dataset directory", type = "error")
        return()
      }
      
      # Check if it's a valid Psych-DS dataset
      if (!file.exists(file.path(dataset_path, "dataset_description.json"))) {
        showNotification("Selected directory does not contain dataset_description.json", type = "error")
        return()
      }
      
      if (!dir.exists(file.path(dataset_path, "data"))) {
        showNotification("Selected directory does not contain a 'data' folder", type = "error") 
        return()
      }
      
      # Load variables from CSV files
      tryCatch({
        variables <- extractVariablesFromDataset(dataset_path)
        
        dictionary_state$dataset_path <- dataset_path
        dictionary_state$variables <- variables
        dictionary_state$current_variable <- NULL
        
        removeModal()
        showNotification("Dataset loaded successfully!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error loading dataset:", e$message), type = "error")
      })
    })

    #' Analyze a variable from a CSV file
    #' 
    #' @param csv_file Path to the CSV file
    #' @param var_name Name of the variable to analyze
    #' @return List with variable analysis results
    analyzeVariable <- function(csv_file, var_name) {
      result <- list(
        type = "string",
        unit = "",
        min_value = "",
        max_value = "",
        categorical_values = list(),
        required = FALSE,
        unique = FALSE,
        pattern = ""
      )
      
      tryCatch({
        # Read data
        data <- read.csv(csv_file, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A", "null", "NULL"))
        
        if (!var_name %in% names(data)) return(result)
        
        col_data <- data[[var_name]]
        col_clean <- col_data[!is.na(col_data)]
        
        if (length(col_clean) == 0) return(result)
        
        # Completeness & uniqueness
        result$required <- (length(col_clean) / length(col_data)) > 0.95
        result$unique <- length(unique(col_clean)) == length(col_clean)
        
        var_lower <- tolower(var_name)
        
        # Type detection
        if (is.numeric(col_data)) {
          unique_vals <- unique(col_clean)
          n_unique <- length(unique_vals)
          ratio <- n_unique / length(col_clean)
          
          # Detect if this is actually a categorical variable coded as numbers
          is_likely_categorical <- FALSE
          
          # Check 1: Binary (0/1, 1/2, etc.) - very likely categorical
          if (n_unique == 2) {
            is_likely_categorical <- TRUE
          }
          
          # Check 2: Small set of consecutive integers (0-6, 1-5, etc.) with suggestive name
          if (n_unique <= 10 && all(unique_vals == floor(unique_vals))) {
            # Check if values are consecutive or near-consecutive
            sorted_vals <- sort(unique_vals)
            range_size <- max(sorted_vals) - min(sorted_vals) + 1
            
            if (range_size <= n_unique * 1.5) {  # Allow some gaps
              # Check for suggestive variable names
              categorical_indicators <- c(
                "gender", "sex", "group", "condition", "category", "type", "class",
                "level", "grade", "rating", "scale", "response", "choice", "option",
                "status", "state", "code", "id"
              )
              
              if (any(sapply(categorical_indicators, function(x) grepl(x, var_lower)))) {
                is_likely_categorical <- TRUE
              }
              
              # Also check if ratio suggests categorical
              if (ratio < 0.05 || n_unique <= 7) {
                is_likely_categorical <- TRUE
              }
            }
          }
          
          # Check 3: Low uniqueness ratio regardless of values
          if (ratio < 0.02 && n_unique <= 15) {
            is_likely_categorical <- TRUE
          }
          
          if (is_likely_categorical) {
            result$type <- "categorical"
            result$categorical_values <- lapply(sort(unique_vals), function(v) {
              list(value = as.character(v), label = as.character(v), description = "")
            })
          } else {
            # True numeric variable
            result$type <- if (all(col_clean == floor(col_clean))) "integer" else "number"
            result$min_value <- as.character(min(col_clean))
            result$max_value <- as.character(max(col_clean))
            result$unit <- inferUnit(var_name, mean(col_clean, na.rm = TRUE))
          }
          
        } else {
          # String type - check if categorical
          unique_vals <- unique(col_clean)
          n_unique <- length(unique_vals)
          ratio <- n_unique / length(col_clean)
          
          # Boolean detection for string data
          if (n_unique == 2) {
            vals_lower <- tolower(unique_vals)
            boolean_pairs <- list(
              c("true", "false"), c("yes", "no"), c("y", "n"), 
              c("t", "f"), c("1", "0"), c("male", "female"),
              c("m", "f")
            )
            
            if (any(sapply(boolean_pairs, function(pair) all(sort(vals_lower) == sort(pair))))) {
              result$type <- "boolean"
              result$categorical_values <- lapply(sort(unique_vals), function(v) {
                list(value = as.character(v), label = as.character(v), description = "")
              })
              return(result)
            }
          }
          
          # Categorical if: few unique values OR low uniqueness ratio
          if (n_unique <= 20 && (ratio < 0.05 || n_unique <= 10)) {
            result$type <- "categorical"
            result$categorical_values <- lapply(sort(unique_vals), function(v) {
              list(value = as.character(v), label = as.character(v), description = "")
            })
          } else {
            result$type <- "string"
          }
        }
        
      }, error = function(e) {
        warning(paste("Error analyzing", var_name, ":", e$message))
      })
      
      return(result)
    }

    # Simple helper for unit inference
    inferUnit <- function(var_name, mean_val) {
      var_lower <- tolower(var_name)
      if (grepl("time|rt|latency", var_lower)) {
        return(if (mean_val > 100) "milliseconds" else "seconds")
      } else if (grepl("age", var_lower)) {
        return("years")
      } else if (grepl("score|rating", var_lower)) {
        return("points")
      }
      return("")
    }

    # Simple helper for unit inference
    inferUnit <- function(var_name, mean_val) {
      var_lower <- tolower(var_name)
      if (grepl("time|rt|latency", var_lower)) {
        return(if (mean_val > 100) "milliseconds" else "seconds")
      } else if (grepl("age", var_lower)) {
        return("years")
      } else if (grepl("score|rating", var_lower)) {
        return("points")
      }
      return("")
    }

    # Simple helper for unit inference
    inferUnit <- function(var_name, mean_val) {
      var_lower <- tolower(var_name)
      if (grepl("time|rt|latency", var_lower)) {
        return(if (mean_val > 100) "milliseconds" else "seconds")
      } else if (grepl("age", var_lower)) {
        return("years")
      } else if (grepl("score|rating", var_lower)) {
        return("points")
      }
      return("")
    }
    # Function to auto-populate categorical values
    autoPopulateCategoricalValues <- function(var_name) {
      if (is.null(dictionary_state$dataset_path) || is.null(var_name)) return()
      
      var_info <- dictionary_state$variables[[var_name]]
      all_values <- character(0)
      
      # Read values from all files containing this variable
      withProgress(message = "Detecting categorical values...", value = 0, {
        data_dir <- file.path(dictionary_state$dataset_path, "data")
        csv_files <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
        
        for (i in seq_along(csv_files)) {
          setProgress(i / length(csv_files))
          tryCatch({
            data <- read.csv(csv_files[i], stringsAsFactors = FALSE, nrows = 1000)
            if (var_name %in% names(data)) {
              col_values <- unique(data[[var_name]][!is.na(data[[var_name]])])
              all_values <- c(all_values, as.character(col_values))
            }
          }, error = function(e) {})
        }
      })
      
      unique_values <- unique(all_values)
      
      if (length(unique_values) > 0 && length(unique_values) <= 50) {
        # Create categorical values with default labels and descriptions
        cat_values <- lapply(sort(unique_values), function(val) {
          list(
            value = val,
            label = val,  # Default label same as value
            description = ""  # Empty description for user to fill
          )
        })
        categorical_values(cat_values)
        showNotification(paste("Auto-populated", length(unique_values), "categorical values"), type = "message")
      } else if (length(unique_values) > 50) {
        showNotification("Too many unique values (>50) to auto-populate. Add manually.", type = "warning")
      }
    }
    
    # Extract variables from dataset with enhanced analysis
    extractVariablesFromDataset <- function(dataset_path) {
      data_dir <- file.path(dataset_path, "data")
      csv_files <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      
      if (length(csv_files) == 0) {
        stop("No CSV files found in data directory")
      }
      
      all_variables <- list()
      
      withProgress(message = "Analyzing dataset variables...", value = 0, {
        for (i in seq_along(csv_files)) {
          csv_file <- csv_files[i]
          rel_path <- gsub(paste0("^", dataset_path, "/"), "", csv_file)
          
          setProgress(i / length(csv_files), detail = paste("Processing", basename(csv_file)))
          
          tryCatch({
            # Read just the header first
            header <- names(read.csv(csv_file, nrows = 1))
            
            for (var_name in header) {
              if (var_name %in% names(all_variables)) {
                # Add this file to existing variable
                all_variables[[var_name]]$files <- c(all_variables[[var_name]]$files, rel_path)
              } else {
                # Create new variable entry with enhanced analysis
                var_analysis <- analyzeVariable(csv_file, var_name)
                
                all_variables[[var_name]] <- list(
                  name = var_name,
                  files = rel_path,
                  description = generateDescription(var_name, var_analysis),
                  type = var_analysis$type,
                  unit = var_analysis$unit,
                  min_value = var_analysis$min_value,
                  max_value = var_analysis$max_value,
                  value_reference = var_analysis$value_reference,
                  default_value = "",
                  source = "",
                  notes = "",
                  categorical_values = var_analysis$categorical_values,  # Include the detected values
                  required = FALSE,
                  unique = FALSE,
                  pattern = ""
                )
              }
            }
          }, error = function(e) {
            warning(paste("Could not read file:", csv_file, "-", e$message))
          })
        }
      })
      
      return(all_variables)
    }
    
    # Generate a basic description based on variable name and analysis
    generateDescription <- function(var_name, var_analysis) {
      var_lower <- tolower(var_name)
      
      # Common psychology/research variable descriptions
      if (grepl("^(participant|subject|sub)_?(id|ID|Id)", var_name)) {
        return("Unique identifier for each participant in the study")
      }
      
      if (grepl("age", var_lower)) {
        return("Age of the participant")
      }
      
      if (grepl("gender|sex", var_lower)) {
        return("Gender or biological sex of the participant")
      }
      
      if (grepl("condition|group", var_lower)) {
        return("Experimental condition or group assignment")
      }
      
      if (grepl("response_?time|rt|latency", var_lower)) {
        return("Response time or reaction time measurement")
      }
      
      if (grepl("accuracy|correct|acc", var_lower)) {
        return("Accuracy or correctness of response")
      }
      
      if (grepl("trial", var_lower)) {
        return("Trial number or trial identifier")
      }
      
      if (grepl("block", var_lower)) {
        return("Block number in the experimental design")
      }
      
      if (grepl("session", var_lower)) {
        return("Session number or session identifier")
      }
      
      if (grepl("stimulus|stim", var_lower)) {
        return("Stimulus identifier or stimulus information")
      }
      
      if (grepl("response|resp", var_lower)) {
        return("Participant response or response value")
      }
      
      if (grepl("score|rating", var_lower)) {
        return("Score or rating value")
      }
      
      if (grepl("timestamp|time", var_lower)) {
        return("Timestamp or time measurement")
      }
      
      # Generate description based on type
      type_descriptions <- switch(var_analysis$type,
        "integer" = "Numeric variable (whole numbers)",
        "number" = "Numeric variable (decimal numbers)",
        "boolean" = "Boolean variable (true/false)",
        "date" = "Date variable",
        "categorical" = "Categorical variable",
        "string" = "Text variable"
      )
      
      return(paste("Variable:", var_name, "-", type_descriptions))
    }
    
    # Render variables list
    output$variables_list <- renderUI({
      variables <- dictionary_state$variables
      search_term <- input$variable_search
      
      if (length(variables) == 0) {
        return(div(
          style = "text-align: center; padding: 50px 20px; color: #6c757d;",
          p("No variables found. Load a dataset to begin.")
        ))
      }
      
      # Filter variables based on search
      if (!is.null(search_term) && search_term != "") {
        variables <- variables[grepl(search_term, names(variables), ignore.case = TRUE)]
      }
      
      if (length(variables) == 0) {
        return(div(
          style = "text-align: center; padding: 30px 20px; color: #6c757d;",
          p("No variables match your search.")
        ))
      }
      
      # Create variable list items
      variable_items <- lapply(names(variables), function(var_name) {
        var_info <- variables[[var_name]]
        file_count <- length(var_info$files)
        is_selected <- identical(dictionary_state$current_variable, var_name)
        
        div(
          class = if (is_selected) "variable-item selected" else "variable-item",
          style = paste0(
            "padding: 12px 15px; cursor: pointer; border-bottom: 1px solid #f0f0f0; ",
            if (is_selected) "background-color: #3498db; color: white;" else "background-color: white; color: #333;",
            if (match(var_name, names(variables)) %% 2 == 0 && !is_selected) " background-color: #f8f9fa;" else ""
          ),
          onclick = paste0("Shiny.setInputValue('", session$ns("select_variable"), "', '", var_name, "', {priority: 'event'});"),
          
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            span(var_name, style = "font-weight: 500;"),
            span(
              paste(file_count, if (file_count == 1) "file" else "files"),
              style = paste0("font-size: 12px; ", if (is_selected) "color: rgba(255,255,255,0.8);" else "color: #6c757d;")
            )
          )
        )
      })
      
      do.call(tagList, variable_items)
    })
    
    # Update the variable selection handler to include new fields:
    observeEvent(input$select_variable, {
      # Save current variable before switching
      saveCurrentVariable()

      var_name <- input$select_variable
      
      if (!is.null(var_name) && var_name %in% names(dictionary_state$variables)) {
        dictionary_state$current_variable <- var_name
        
        # Load variable data into form
        var_info <- dictionary_state$variables[[var_name]]
        
        updateTextAreaInput(session, "var_description", value = var_info$description %||% "")
        updateSelectInput(session, "var_type", selected = var_info$type %||% "string")
        updateTextInput(session, "var_unit", value = var_info$unit %||% "")
        updateTextInput(session, "var_min", value = var_info$min_value %||% "")
        updateTextInput(session, "var_max", value = var_info$max_value %||% "")
        updateTextAreaInput(session, "var_value_reference", value = var_info$value_reference %||% "")
        updateTextInput(session, "var_default", value = var_info$default_value %||% "")

        updateCheckboxInput(session, "var_use_minmax", value = FALSE)
        
        # Update new fields
        updateTextInput(session, "var_source", value = var_info$source %||% "")
        updateTextAreaInput(session, "var_notes", value = var_info$notes %||% "")
        updateCheckboxInput(session, "var_required", value = var_info$required %||% FALSE)
        updateCheckboxInput(session, "var_unique", value = var_info$unique %||% FALSE)
        updateTextInput(session, "var_pattern", value = var_info$pattern %||% "")

        # Set categorical values
        if (!is.null(var_info$categorical_values) && length(var_info$categorical_values) > 0) {
          categorical_values(var_info$categorical_values)
        } else if (var_info$type == "categorical" && length(var_info$categorical_values) == 0) {
          # Auto-populate from data if switching to categorical
          autoPopulateCategoricalValues(var_name)
        } else {
          categorical_values(list())
        }
      }
    })
    
    # Add observer for when type changes to categorical
    observeEvent(input$var_type, {
      if (!is.null(input$var_type) && input$var_type == "categorical") {
        # If switching to categorical and no values exist, auto-populate
        if (length(categorical_values()) == 0 && !is.null(dictionary_state$current_variable)) {
          autoPopulateCategoricalValues(dictionary_state$current_variable)
        }
      }
    })
    
    # Render variable name header
    output$variable_name_header <- renderUI({
      if (!is.null(dictionary_state$current_variable)) {
        h3(
          dictionary_state$current_variable,
          style = "color: #333; margin: 0; font-weight: bold;"
        )
      }
    })
    
    output$file_badges_content <- renderUI({
      if (!is.null(dictionary_state$current_variable)) {
        var_info <- dictionary_state$variables[[dictionary_state$current_variable]]
        
        tagList(
          lapply(var_info$files, function(file_path) {
            span(
              basename(file_path),
              class = "badge",
              title = file_path,
              style = "background-color: #3498db; color: white; padding: 4px 8px; border-radius: 12px; font-size: 11px; cursor: help; margin: 2px;"
            )
          }),
          if (length(var_info$files) > 10) {
            div(
              style = "margin-top: 8px; padding-top: 8px; border-top: 1px solid #ddd; font-size: 12px; color: #666; text-align: center;",
              paste("Total:", length(var_info$files), "files")
            )
          }
        )
      }
    })

    # Update the categorical values table renderer:
    output$categorical_values_table <- renderUI({
      values <- categorical_values()
      
      if (length(values) == 0) {
        return(div(
          style = "padding: 20px; text-align: center; color: #6c757d;",
          "No categorical values defined. They will be auto-populated when you select 'Categorical' as the type."
        ))
      }
      
      rows <- lapply(seq_along(values), function(i) {
        value_info <- values[[i]]
        div(
          style = "display: flex; padding: 8px; border-bottom: 1px solid #ced4da; align-items: center;",
          div(style = "flex: 2; padding-right: 10px; font-weight: 500;", value_info$value),
          div(style = "flex: 2; padding-right: 10px;", value_info$label %||% ""),
          div(style = "flex: 3; padding-right: 10px;", value_info$description %||% ""),
          div(
            style = "flex: 0; width: 80px; display: flex; gap: 5px;",
            actionButton(
              session$ns(paste0("edit_cat_", i)),
              label = NULL,
              icon = icon("edit"),
              class = "btn btn-sm btn-info",
              style = "padding: 2px 6px;"
            ),
            actionButton(
              session$ns(paste0("remove_cat_", i)),
              label = NULL,
              icon = icon("trash"),
              class = "btn btn-sm btn-danger",
              style = "padding: 2px 6px;",
              onclick = paste0("Shiny.setInputValue('", session$ns("remove_cat_value"), "', ", i, ", {priority: 'event'});")
            )
          )
        )
      })
      
      do.call(tagList, rows)
    })
    
    # Handle editing categorical values
    observe({
      values <- categorical_values()
      
      for (i in seq_along(values)) {
        local({
          local_i <- i
          
          # Edit button handler
          observeEvent(input[[paste0("edit_cat_", local_i)]], {
            current_values <- categorical_values()
            if (local_i <= length(current_values)) {
              value_info <- current_values[[local_i]]
              
              showModal(modalDialog(
                title = "Edit Categorical Value",
                div(
                  textInput(
                    session$ns("edit_cat_value"),
                    "Value",
                    value = value_info$value,
                    placeholder = "The actual value in the data"
                  ),
                  textInput(
                    session$ns("edit_cat_label"),
                    "Label",
                    value = value_info$label %||% "",
                    placeholder = "Human-readable label for this value"
                  ),
                  textInput(
                    session$ns("edit_cat_description"),
                    "Description",
                    value = value_info$description %||% "",
                    placeholder = "Description of what this value means"
                  )
                ),
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton(session$ns("save_edit_cat"), "Save", class = "btn-primary")
                ),
                easyClose = TRUE
              ))
              
              # Store which index we're editing
              dictionary_state$editing_cat_index <- local_i
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Save edited categorical value
    observeEvent(input$save_edit_cat, {
      if (!is.null(dictionary_state$editing_cat_index)) {
        current_values <- categorical_values()
        index <- dictionary_state$editing_cat_index
        
        if (index <= length(current_values)) {
          # Update the value at the index
          current_values[[index]] <- list(
            value = input$edit_cat_value,
            label = input$edit_cat_label,
            description = input$edit_cat_description
          )
          
          categorical_values(current_values)
          removeModal()
          showNotification("Categorical value updated", type = "message")
        }
      }
    })

    

    # Update the add categorical value handler:
    observeEvent(input$add_cat_value, {
      if (!is.null(input$new_cat_value) && input$new_cat_value != "") {
        current_values <- categorical_values()
        
        # Check for duplicates
        existing_values <- sapply(current_values, function(x) x$value)
        if (input$new_cat_value %in% existing_values) {
          showNotification("This value already exists", type = "warning")
          return()
        }
        
        # Add new value with label
        new_value <- list(
          value = input$new_cat_value,
          label = if(is.null(input$new_cat_label) || input$new_cat_label == "") {
            input$new_cat_value  # Default label to value if not provided
          } else {
            input$new_cat_label
          },
          description = input$new_cat_description %||% ""
        )
        
        categorical_values(c(current_values, list(new_value)))
        
        # Clear inputs
        updateTextInput(session, "new_cat_value", value = "")
        updateTextInput(session, "new_cat_label", value = "")
        updateTextInput(session, "new_cat_description", value = "")
        
        showNotification("Categorical value added", type = "message")
      } else {
        showNotification("Please enter a value", type = "warning")
      }
    })
    
    # Handle removing categorical values
    observeEvent(input$remove_cat_value, {
      index <- input$remove_cat_value
      current_values <- categorical_values()
      
      if (!is.null(index) && index > 0 && index <= length(current_values)) {
        categorical_values(current_values[-index])
        showNotification("Categorical value removed", type = "message")
      }
    }, ignoreInit = TRUE)

    generateFullDatasetDescriptionPreview <- function() {
      # Read existing dataset_description.json to get name, description, authors
      json_path <- file.path(dictionary_state$dataset_path, "dataset_description.json")
      
      dataset_info <- list(
        name = "Dataset Name",
        description = "Dataset Description",
        authors = list()
      )
      
      if (file.exists(json_path)) {
        tryCatch({
          existing <- jsonlite::fromJSON(json_path)
          if (!is.null(existing$name)) dataset_info$name <- existing$name
          if (!is.null(existing$description)) dataset_info$description <- existing$description
          if (!is.null(existing$author)) dataset_info$authors <- existing$author
        }, error = function(e) {})
      }
      
      # Build variableMeasured array
      global_missing_values <- dictionary_state$missing_values
      
      variable_measured <- lapply(names(dictionary_state$variables), function(var_name) {
        var_info <- dictionary_state$variables[[var_name]]
        
        prop_value <- list(
          `@type` = "PropertyValue",
          name = var_name,
          description = if(nchar(var_info$description) > 0) var_info$description else NULL,
          valueType = var_info$type
        )
        
        if (length(global_missing_values) > 0) {
          prop_value$missingValueCodes <- global_missing_values
        }
        
        if (nchar(var_info$unit) > 0) prop_value$unitText <- var_info$unit
        if (nchar(var_info$min_value) > 0) prop_value$minValue <- var_info$min_value
        if (nchar(var_info$max_value) > 0) prop_value$maxValue <- var_info$max_value
        
        if (var_info$type == "categorical" && length(var_info$categorical_values) > 0) {
          prop_value$valueReference <- lapply(var_info$categorical_values, function(cat) {
            cat_obj <- list(
              value = cat$value,
              label = if(nchar(cat$label) > 0 && cat$label != cat$value) cat$label else NULL,
              description = if(nchar(cat$description) > 0) cat$description else NULL
            )
            cat_obj[!sapply(cat_obj, is.null)]
          })
        }
        
        prop_value$required <- var_info$required %||% FALSE
        prop_value$unique <- var_info$unique %||% FALSE
        if (nchar(var_info$pattern) > 0) prop_value$pattern <- var_info$pattern
        
        prop_value[!sapply(prop_value, is.null)]
      })
      
      # Build complete dataset_description
      full_json <- list(
        `@context` = "https://schema.org/",
        `@type` = "Dataset",
        name = dataset_info$name,
        description = dataset_info$description,
        author = dataset_info$authors,
        variableMeasured = variable_measured
      )
      
      # Convert to JSON string
      json_str <- jsonlite::toJSON(full_json, pretty = TRUE, auto_unbox = TRUE)
      
      # Add syntax highlighting
      json_html <- json_str
      json_html <- gsub('"(@?[^"]+)":', '<span style="color: #0969da;">\"\\1\"</span>:', json_html)
      json_html <- gsub(':\\s*"([^"]*)"', ': <span style="color: #0a3069;">\"\\1\"</span>', json_html)
      json_html <- gsub(':\\s*(true|false)', ': <span style="color: #cf222e;">\\1</span>', json_html)
      json_html <- gsub(':\\s*([0-9.]+)', ': <span style="color: #953800;">\\1</span>', json_html)
      
      return(json_html)
    }


    observeEvent(input$continue, {
      message("Save dictionary button clicked\n")
      
      # Save current variable before proceeding
      saveCurrentVariable()
      
      # Generate the FULL dataset_description.json preview
      full_json_preview <- generateFullDatasetDescriptionPreview()
      
      # Default save path
      default_save_path <- file.path(dictionary_state$dataset_path, "dataset_description.json")
      
      # Show preview modal with overwrite warning and path selector
      showModal(modalDialog(
        title = "Review Complete Dataset Description",
        size = "l",
        
        div(
          class = "alert alert-warning",
          style = "margin-bottom: 20px;",
          icon("exclamation-triangle", style = "margin-right: 8px;"),
          tags$strong("Important: "),
          "By default, this will overwrite the existing dataset_description.json file. ",
          "You can change the save location below if you want to preserve the original."
        ),
        
        div(
          p("Here's the complete dataset_description.json that will be saved:"),
          
          # Preview container
          tags$pre(
            style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; max-height: 300px; overflow-y: auto; font-family: monospace; font-size: 12px;",
            HTML(full_json_preview)
          ),
          
          # Summary stats
          div(
            style = "margin-top: 15px; padding: 10px; background-color: #e8f4f8; border-radius: 4px;",
            p(strong("Summary:")),
            p(paste("Total variables:", length(dictionary_state$variables))),
            p(paste("Variables with descriptions:", 
                    sum(sapply(dictionary_state$variables, function(v) nchar(v$description) > 0)))),
            p(paste("Categorical variables:", 
                    sum(sapply(dictionary_state$variables, function(v) v$type == "categorical")))),
            p(paste("Missing value codes defined:", length(dictionary_state$missing_values)))
          ),
          
          # Save location selector
          div(
            style = "margin-top: 20px;",
            tags$label("Save Location", style = "font-weight: bold; margin-bottom: 8px; display: block;"),
            div(
              class = "directory-input",
              textInput(
                session$ns("dictionary_save_path"),
                label = NULL,
                value = default_save_path,
                placeholder = "Path to save dataset_description.json",
                width = "100%"
              ),
              shinySaveButton(
                session$ns("dictionary_save_select"),
                label = "...",
                title = "Choose save location",
                filetype = list(json = "json"),
                class = "browse-btn"
              )
            ),
            tags$small(
              style = "color: #6c757d; display: block; margin-top: 5px;",
              "Change this path if you want to save to a different location without overwriting the original"
            )
          )
        ),
        
        footer = tagList(
          modalButton("Back to Editing"),
          actionButton(session$ns("confirm_save_dictionary"), "Save & Continue to Explorer", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    })

    shinyFileSave(
      input,
      "dictionary_save_select",
      roots = volumes,
      session = session,
      filetypes = c("json"),
      restrictions = system.file(package = "base")
    )

    observeEvent(input$dictionary_save_select, {
      if (!is.integer(input$dictionary_save_select)) {
        file_selected <- parseSavePath(volumes, input$dictionary_save_select)
        if (length(file_selected$datapath) > 0) {
          save_path <- as.character(file_selected$datapath)
          # Ensure it has .json extension
          if (!grepl("\\.json$", save_path)) {
            save_path <- paste0(save_path, ".json")
          }
          updateTextInput(session, "dictionary_save_path", value = save_path)
        }
      }
    })

    observeEvent(input$confirm_save_dictionary, {
      # Use the custom save path from the input
      json_path <- input$dictionary_save_path
      
      if (is.null(json_path) || json_path == "") {
        showNotification("Please specify a save location", type = "error")
        return()
      }
      
      # Get the directory path from the file path
      save_dir <- dirname(json_path)
      
      if (!dir.exists(save_dir)) {
        showNotification("The directory does not exist. Please choose a valid location.", type = "error")
        return()
      }
      
      tryCatch({
        # Read existing dataset_description.json for base info
        original_json_path <- file.path(dictionary_state$dataset_path, "dataset_description.json")
        
        if (file.exists(original_json_path)) {
          dataset_desc <- jsonlite::fromJSON(original_json_path)
        } else {
          dataset_desc <- list()
        }
        
        # Get global missing values
        global_missing_values <- dictionary_state$missing_values
        
        # Update variableMeasured with missing values in each PropertyValue
        dataset_desc$variableMeasured <- lapply(names(dictionary_state$variables), function(var_name) {
          var_info <- dictionary_state$variables[[var_name]]
          
          prop_value <- list(
            `@type` = "PropertyValue",
            name = var_name,
            description = if(nchar(var_info$description) > 0) var_info$description else NULL,
            valueType = var_info$type
          )
          
          # Add missing value codes to each variable
          if (length(global_missing_values) > 0) {
            prop_value$missingValueCodes <- global_missing_values
          }
          
          if (nchar(var_info$unit) > 0) prop_value$unitText <- var_info$unit
          if (nchar(var_info$min_value) > 0) prop_value$minValue <- var_info$min_value
          if (nchar(var_info$max_value) > 0) prop_value$maxValue <- var_info$max_value
          
          if (var_info$type == "categorical" && length(var_info$categorical_values) > 0) {
            prop_value$valueReference <- lapply(var_info$categorical_values, function(cat) {
              cat_obj <- list(
                value = cat$value,
                label = if(nchar(cat$label) > 0 && cat$label != cat$value) cat$label else NULL,
                description = if(nchar(cat$description) > 0) cat$description else NULL
              )
              cat_obj[!sapply(cat_obj, is.null)]
            })
          }
          
          # ALWAYS include these fields
          prop_value$required <- var_info$required %||% FALSE
          prop_value$unique <- var_info$unique %||% FALSE
          
          # Only add pattern if it has a value
          if (nchar(var_info$pattern) > 0) prop_value$pattern <- var_info$pattern
          
          prop_value[!sapply(prop_value, is.null)]
        })
        
        # Write to the specified path
        jsonlite::write_json(dataset_desc, json_path, pretty = TRUE, auto_unbox = TRUE)
        
        # Store the dataset path in state for the explorer
        state$dictionary_dataset_dir <- dictionary_state$dataset_path
        
        removeModal()
        showNotification(paste("Data dictionary saved successfully to:", json_path), type = "message")
        
        # Navigate to dataset explorer
        session$sendCustomMessage("changeTab", list(tabName = "explorer"))
        
      }, error = function(e) {
        showNotification(paste("Error saving dictionary:", e$message), type = "error")
      })
    })
    
    # Return reactive containing dictionary state
    return(reactive({ dictionary_state }))
  })
}

#' Dataset Explorer Server Module
#'
#' @param id The module ID
#' @param state Global state reactive values  
#' @param session The current session object
datasetExplorerServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values for explorer state
    explorer_state <- reactiveValues(
      dataset_path = NULL,
      csv_files = list(),
      current_file = NULL,
      current_data = NULL,
      keyword_filters = list(),
      column_filters = list(),
      available_keywords = character(0),
      available_columns = character(0)
    )
    
    # Track if dataset is loaded
    output$dataset_loaded <- reactive({
      !is.null(explorer_state$dataset_path) && length(explorer_state$csv_files) > 0
    })
    outputOptions(output, "dataset_loaded", suspendWhenHidden = FALSE)
    
    # Set up directory selection
    volumes <- c(Home = "~")
    if (.Platform$OS.type == "windows") {
      volumes <- c(volumes, getVolumes()())
    }
    
    shinyDirChoose(
      input,
      "dataset_dir_select", 
      roots = volumes,
      session = session,
      restrictions = system.file(package = "base")
    )

    extractKeywordValues <- function(keyword) {
      values <- character(0)
      
      for (file in explorer_state$csv_files) {
        filename <- basename(file)
        # Look for pattern like "keyword-value"
        pattern <- paste0(keyword, "-([^_]+)")
        matches <- regmatches(filename, regexec(pattern, filename))
        if (length(matches[[1]]) > 1) {
          values <- c(values, matches[[1]][2])
        }
      }
      
      return(unique(sort(values)))
    }
    
    observeEvent(input$dataset_dir_select, {
      if (!is.null(input$dataset_dir_select)) {
        selected_dir <- parseDirPath(volumes, input$dataset_dir_select)
        if (length(selected_dir) > 0 && selected_dir != "") {
          updateTextInput(session, "dataset_dir", value = selected_dir)
        }
      }
    })
    
    # Display dataset info
    output$dataset_info <- renderUI({
      if (!is.null(explorer_state$dataset_path) && length(explorer_state$csv_files) > 0) {
        dataset_name <- basename(explorer_state$dataset_path)
        file_count <- length(explorer_state$csv_files)
        
        div(
          icon("check-circle", style = "color: #28a745; margin-right: 8px;"),
          strong("Dataset loaded: "), dataset_name,
          span(style = "margin-left: 15px; color: #6c757d;",
               paste(file_count, "CSV files found"))
        )
      } else if (!is.null(explorer_state$dataset_path)) {
        div(
          icon("exclamation-triangle", style = "color: #ffc107; margin-right: 8px;"),
          strong("Dataset path selected: "), basename(explorer_state$dataset_path),
          span(style = "margin-left: 15px; color: #dc3545;",
               "No CSV files found in data directory")
        )
      }
    })
    
    # Load dataset when button clicked
    observeEvent(input$load_dataset_btn, {
      dataset_path <- input$dataset_dir
      
      if (dataset_path == "" || !dir.exists(dataset_path)) {
        showNotification("Please select a valid dataset directory", type = "error")
        return()
      }
      
      # Check if it's a valid Psych-DS dataset
      if (!file.exists(file.path(dataset_path, "dataset_description.json"))) {
        showNotification("Selected directory does not contain dataset_description.json", type = "error")
        return()
      }
      
      if (!dir.exists(file.path(dataset_path, "data"))) {
        showNotification("Selected directory does not contain a 'data' folder", type = "error") 
        return()
      }
      
      # Load CSV files from data directory
      tryCatch({
        data_dir <- file.path(dataset_path, "data")
        # Use recursive = TRUE to find CSV files in subdirectories
        csv_files <- list.files(data_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
        
        message("Looking for CSV files in: ", data_dir)
        message("Found files: ", paste(csv_files, collapse = ", "))
        
        if (length(csv_files) == 0) {
          # Try looking without the recursive flag first to debug
          all_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)
          message("All files in data dir: ", paste(all_files, collapse = ", "))
          
          showNotification("No CSV files found in data directory", type = "warning")
          explorer_state$dataset_path <- dataset_path
          explorer_state$csv_files <- character(0)
          return()
        }
        
        # Extract keywords from filenames
        keywords <- extractKeywordsFromFilenames(basename(csv_files))
        
        explorer_state$dataset_path <- dataset_path
        explorer_state$csv_files <- csv_files
        explorer_state$available_keywords <- keywords
        explorer_state$current_file <- csv_files[1]
        
        # Load the first file
        loadCurrentFile()
        
        # Update UI choices
        updateSelectInput(session, "keyword_select", choices = c("Select keyword..." = "", keywords))
        
        showNotification(paste("Dataset loaded successfully!", length(csv_files), "CSV files found"), type = "message")
        
      }, error = function(e) {
        message("Error in loading dataset: ", e$message)
        showNotification(paste("Error loading dataset:", e$message), type = "error")
      })
    })

    observeEvent(input$keyword_select, {
      if (!is.null(input$keyword_select) && 
          input$keyword_select != "" && 
          input$keyword_select != "(Load dataset first)") {
        
        # Extract all possible values for this keyword
        possible_values <- extractKeywordValues(input$keyword_select)
        
        # Update the selectize input with these values
        updateSelectizeInput(session, "keyword_value", 
                            choices = possible_values,
                            server = TRUE)  # Use server-side for better performance with many options
      } else {
        # Clear choices if no keyword selected
        updateSelectizeInput(session, "keyword_value", choices = character(0))
      }
    })

    observeEvent(input$column_select, {
      if (!is.null(input$column_select) && 
          input$column_select != "" && 
          input$column_select != "(Load dataset first)" &&
          !is.null(explorer_state$current_data)) {
        
        # Get unique values from the selected column across all loaded files
        all_values <- character(0)
        
        for (file in explorer_state$csv_files) {
          tryCatch({
            temp_data <- read.csv(file, stringsAsFactors = FALSE)
            if (input$column_select %in% names(temp_data)) {
              column_values <- as.character(temp_data[[input$column_select]])
              # Keep blank values explicitly
              all_values <- c(all_values, column_values)
            }
          }, error = function(e) {
            # Skip files that can't be read
          })
        }
        
        # Get unique sorted values - include blanks
        unique_values <- unique(all_values)
        # Separate blanks and non-blanks
        blank_values <- unique_values[unique_values == "" | is.na(unique_values)]
        non_blank_values <- unique_values[unique_values != "" & !is.na(unique_values)]
        
        # Sort non-blank values
        non_blank_values <- sort(non_blank_values)
        
        # Combine with blanks at the top, using a display label
        if (length(blank_values) > 0) {
          # Add a labeled blank option
          unique_values <- c("(blank)" = "", non_blank_values)
        } else {
          unique_values <- non_blank_values
        }
        
        # Limit to reasonable number if there are too many
        if (length(unique_values) > 500) {
          unique_values <- unique_values[1:500]
          showNotification("Showing first 500 unique values. Type to search for specific values.", 
                          type = "info", duration = 5)
        }
        
        # Update the selectize input
        updateSelectizeInput(session, "column_value", 
                            choices = unique_values,
                            server = TRUE)
      } else {
        # Clear choices if no column selected
        updateSelectizeInput(session, "column_value", choices = character(0))
      }
    })
    
    # Initialize the search panel UI components even before dataset is loaded
    observe({
      # Initialize keyword select with empty choices
      if (length(explorer_state$available_keywords) == 0) {
        updateSelectInput(session, "keyword_select", 
                         choices = c("Select keyword..." = "", "(Load dataset first)" = ""))
      }
      
      # Initialize column select with empty choices
      if (length(explorer_state$available_columns) == 0) {
        updateSelectInput(session, "column_select", 
                         choices = c("Select column..." = "", "(Load dataset first)" = ""))
        updateSelectInput(session, "stats_variable", 
                         choices = c("Select variable..." = "", "(Load dataset first)" = ""))
      }
    })
    
    # Extract keywords from filenames
    extractKeywordsFromFilenames <- function(filenames) {
      all_keywords <- character(0)
      
      for (filename in filenames) {
        # Remove file extension and split by underscore
        base_name <- gsub("\\.[^.]*$", "", filename)
        parts <- strsplit(base_name, "_")[[1]]
        
        # Extract keyword-value pairs
        for (part in parts) {
          if (grepl("-", part)) {
            keyword <- strsplit(part, "-")[[1]][1]
            all_keywords <- c(all_keywords, keyword)
          }
        }
      }
      
      return(unique(all_keywords))
    }
    
    # Load current file data
    loadCurrentFile <- function() {
      if (!is.null(explorer_state$current_file) && file.exists(explorer_state$current_file)) {
        tryCatch({
          message("Loading file: ", explorer_state$current_file)
          data <- read.csv(explorer_state$current_file, stringsAsFactors = FALSE)
          explorer_state$current_data <- data
          explorer_state$available_columns <- names(data)
          
          # Update column choices
          updateSelectInput(session, "column_select", 
                           choices = c("Select column..." = "", names(data)))
          updateSelectInput(session, "stats_variable", 
                           choices = c("Select variable..." = "", names(data)))
          
          message("File loaded successfully with ", nrow(data), " rows and ", ncol(data), " columns")
          
        }, error = function(e) {
          message("Error reading file: ", e$message)
          showNotification(paste("Error reading file:", e$message), type = "error")
        })
      }
    }
    
    # Add keyword filter
    observeEvent(input$add_keyword_filter, {
      if (!is.null(input$keyword_select) && input$keyword_select != "" && 
          input$keyword_select != "(Load dataset first)" &&
          !is.null(input$keyword_value) && input$keyword_value != "") {
        
        filter_key <- paste0(input$keyword_select, ":", input$keyword_value)
        
        # Check if filter already exists
        if (!filter_key %in% names(explorer_state$keyword_filters)) {
          explorer_state$keyword_filters[[filter_key]] <- list(
            keyword = input$keyword_select,
            value = input$keyword_value
          )
          
          # Clear inputs
          updateTextInput(session, "keyword_value", value = "")
          
          # Apply filters
          applyFilters()
          
          showNotification("Keyword filter added", type = "message")
        } else {
          showNotification("This filter already exists", type = "warning")
        }
      } else {
        showNotification("Please select a keyword and enter a value", type = "warning")
      }
    })
    
    # Add column filter
    observeEvent(input$add_column_filter, {
      if (!is.null(input$column_select) && input$column_select != "" && 
          input$column_select != "(Load dataset first)" &&
          !is.null(input$column_value)) {  # Remove the check for input$column_value != ""
        
        filter_key <- paste0(input$column_select, ":", input$column_value)
        
        # Check if filter already exists
        if (!filter_key %in% names(explorer_state$column_filters)) {
          explorer_state$column_filters[[filter_key]] <- list(
            column = input$column_select,
            value = input$column_value
          )
          
          # Clear inputs
          updateSelectizeInput(session, "column_value", selected = character(0))
          
          showNotification("Column filter added", type = "message")
        } else {
          showNotification("This filter already exists", type = "warning")
        }
      } else {
        showNotification("Please select a column and enter a value", type = "warning")
      }
    })
    
    # Apply filters to determine which files to show
    applyFilters <- function() {
      if (length(explorer_state$keyword_filters) == 0) {
        return()
      }
      
      # Filter files based on keyword filters
      matching_files <- explorer_state$csv_files
      
      for (filter in explorer_state$keyword_filters) {
        pattern <- paste0(filter$keyword, "-", filter$value)
        matching_files <- matching_files[grepl(pattern, basename(matching_files))]
      }
      
      # Load first matching file if different from current
      if (length(matching_files) > 0 && matching_files[1] != explorer_state$current_file) {
        explorer_state$current_file <- matching_files[1]
        loadCurrentFile()
      } else if (length(matching_files) == 0) {
        showNotification("No files match the current filters", type = "warning")
      }
    }
    
    # Display keyword filters
    output$keyword_filters_display <- renderUI({
      filters <- explorer_state$keyword_filters
      
      if (length(filters) == 0) {
        return(div(
          style = "color: #6c757d; font-style: italic;",
          "No keyword filters active"
        ))
      }
      
      filter_badges <- lapply(names(filters), function(filter_key) {
        filter <- filters[[filter_key]]
        span(
          style = "display: inline-block; margin: 2px; padding: 4px 8px; background-color: #3498db; color: white; border-radius: 12px; font-size: 12px;",
          paste0(filter$keyword, ': "', filter$value, '"'),
          actionButton(
            session$ns(paste0("remove_kw_", gsub("[^A-Za-z0-9]", "_", filter_key))),
            "×",
            style = "background: none; border: none; color: white; padding: 0 0 0 5px; font-size: 14px;",
            onclick = paste0("Shiny.setInputValue('", session$ns("remove_keyword_filter"), "', '", filter_key, "', {priority: 'event'});")
          )
        )
      })
      
      do.call(tagList, filter_badges)
    })
    
    # Display column filters
    output$column_filters_display <- renderUI({
      filters <- explorer_state$column_filters
      
      if (length(filters) == 0) {
        return(div(
          style = "color: #6c757d; font-style: italic;",
          "No column filters active"
        ))
      }
      
      filter_badges <- lapply(names(filters), function(filter_key) {
        filter <- filters[[filter_key]]
        span(
          style = "display: inline-block; margin: 2px; padding: 4px 8px; background-color: #e74c3c; color: white; border-radius: 12px; font-size: 12px;",
          paste0(filter$column, ': "', filter$value, '"'),
          actionButton(
            session$ns(paste0("remove_col_", gsub("[^A-Za-z0-9]", "_", filter_key))),
            "×",
            style = "background: none; border: none; color: white; padding: 0 0 0 5px; font-size: 14px;",
            onclick = paste0("Shiny.setInputValue('", session$ns("remove_column_filter"), "', '", filter_key, "', {priority: 'event'});")
          )
        )
      })
      
      do.call(tagList, filter_badges)
    })
    
    # Remove filters
    observeEvent(input$remove_keyword_filter, {
      filter_key <- input$remove_keyword_filter
      explorer_state$keyword_filters[[filter_key]] <- NULL
      applyFilters()
      showNotification("Keyword filter removed", type = "message")
    })
    
    observeEvent(input$remove_column_filter, {
      filter_key <- input$remove_column_filter
      explorer_state$column_filters[[filter_key]] <- NULL
      showNotification("Column filter removed", type = "message")
    })
    
    # File tabs
    output$file_tabs <- renderUI({
      if (length(explorer_state$csv_files) == 0) {
        return(div(
          style = "color: #6c757d; font-style: italic; padding: 10px;",
          "No CSV files loaded. Select and load a dataset above."
        ))
      }
      
      current_file <- explorer_state$current_file
      
      # Apply keyword filters to determine which files to show
      files_to_show <- explorer_state$csv_files
      for (filter in explorer_state$keyword_filters) {
        pattern <- paste0(filter$keyword, "-", filter$value)
        files_to_show <- files_to_show[grepl(pattern, basename(files_to_show))]
      }
      
      if (length(files_to_show) == 0) {
        return(div(
          style = "color: #dc3545; font-style: italic; padding: 10px;",
          "No files match current keyword filters"
        ))
      }
      
      tabs <- lapply(files_to_show, function(file) {
        filename <- basename(file)
        is_active <- identical(file, current_file)
        
        actionButton(
          session$ns(paste0("select_file_", gsub("[^A-Za-z0-9]", "_", filename))),
          filename,
          class = if (is_active) "btn btn-primary" else "btn btn-outline-secondary",
          style = "margin-right: 5px; margin-bottom: 5px;",
          onclick = paste0("Shiny.setInputValue('", session$ns("select_file"), "', '", file, "', {priority: 'event'});")
        )
      })
      
      do.call(tagList, tabs)
    })
    
    # Handle file selection
    observeEvent(input$select_file, {
      explorer_state$current_file <- input$select_file
      loadCurrentFile()
    })
    

    # Render variable statistics based on type
    output$variable_statistics <- renderUI({
      if (is.null(input$stats_variable) || 
          input$stats_variable == "" ||
          input$stats_variable == "(Load dataset first)") {
        return(div(
          style = "padding: 20px; text-align: center; color: #6c757d;",
          "Select a variable to view statistics"
        ))
      }
      
      # Get all files that match current keyword filters
      files_to_analyze <- explorer_state$csv_files
      
      # Apply keyword filters to determine which files to include
      if (length(explorer_state$keyword_filters) > 0) {
        for (filter in explorer_state$keyword_filters) {
          pattern <- paste0(filter$keyword, "-", filter$value)
          files_to_analyze <- files_to_analyze[grepl(pattern, basename(files_to_analyze))]
        }
      }
      
      if (length(files_to_analyze) == 0) {
        return(div(
          style = "padding: 20px; text-align: center; color: #dc3545;",
          "No files match current filters"
        ))
      }
      
      # Aggregate data from all matching files
      all_column_data <- list()
      
      for (file in files_to_analyze) {
        tryCatch({
          file_data <- read.csv(file, stringsAsFactors = FALSE)
          
          if (input$stats_variable %in% names(file_data)) {
            # Apply column filters to this file's data
            filtered_data <- file_data
            
            for (col_filter in explorer_state$column_filters) {
              col_name <- col_filter$column
              filter_value <- col_filter$value
              
              if (col_name %in% names(filtered_data)) {
                if (is.null(filter_value) || filter_value == "") {
                  # Filter for blank/empty values
                  filtered_data <- filtered_data[is.na(filtered_data[[col_name]]) | 
                                                filtered_data[[col_name]] == "", , drop = FALSE]
                } else {
                  # Filter for matching values
                  filtered_data <- filtered_data[grepl(filter_value, 
                                                      as.character(filtered_data[[col_name]]), 
                                                      ignore.case = TRUE), , drop = FALSE]
                }
              }
            }
            
            # Add this file's filtered data to aggregation
            all_column_data <- c(all_column_data, list(filtered_data[[input$stats_variable]]))
          }
        }, error = function(e) {
          # Skip files that can't be read
        })
      }
      
      if (length(all_column_data) == 0) {
        return(div(
          style = "padding: 20px; text-align: center; color: #dc3545;",
          "Selected variable not found in any matching files"
        ))
      }
      
      # Combine all data
      column_data <- unlist(all_column_data)
      
      # Rest of the statistics calculation remains the same...
      # FIXED: Better NA detection - treat empty strings as NA too
      is_missing <- is.na(column_data) | column_data == "" | trimws(as.character(column_data)) == ""
      non_na_data <- column_data[!is_missing]
      na_count <- sum(is_missing)
      total_count <- length(column_data)
      
      # Add check for empty data after removing NAs
      if (length(non_na_data) == 0) {
        return(div(
          style = "padding: 20px; text-align: center; color: #856404; background-color: #fff3cd; border-radius: 4px;",
          icon("exclamation-triangle", style = "margin-right: 8px;"),
          "This variable has no non-missing values"
        ))
      }
      
      # Load metadata if available
      metadata <- NULL
      if (!is.null(explorer_state$dataset_path)) {
        json_path <- file.path(explorer_state$dataset_path, "dataset_description.json")
        if (file.exists(json_path)) {
          tryCatch({
            dataset_desc <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
            if (!is.null(dataset_desc$variableMeasured)) {
              # Find the matching variable - case-insensitive search as backup
              for (var_meta in dataset_desc$variableMeasured) {
                if (!is.null(var_meta$name) && var_meta$name == input$stats_variable) {
                  metadata <- var_meta
                  break
                }
              }
            }
          }, error = function(e) {
            message("Error loading metadata: ", e$message)
          })
        }
      }

      # Debug output
      message("=== DEBUG: Variable '", input$stats_variable, "' ===\n", sep = "")
      message("Metadata found:", !is.null(metadata), "\n")
      if (!is.null(metadata)) {
        message("valueType in metadata:", metadata$valueType, "\n")
      }
      message("is.numeric(column_data):", is.numeric(column_data), "\n")

      # Detect variable type from metadata or data
      var_type <- if (!is.null(metadata) && !is.null(metadata$valueType)) {
        metadata$valueType
      } else if (is.numeric(column_data)) {
        "number"
      } else {
        "string"
      }

      message("Final var_type:", var_type, "\n")
      message("================\n")
      
      # Base statistics (always shown)
      base_stats <- div(
        style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 15px;",
        
        div(
          style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; text-align: center;",
          div("Total", style = "font-size: 12px; color: #6c757d;"),
          div(as.character(total_count), style = "font-weight: bold; font-size: 18px;")
        ),
        
        div(
          style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; text-align: center;",
          div("Unique", style = "font-size: 12px; color: #6c757d;"),
          div(as.character(length(unique(non_na_data))), style = "font-weight: bold; font-size: 18px;")
        ),
        
        div(
          style = "padding: 10px; background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 4px; text-align: center;",
          div("Missing", style = "font-size: 12px; color: #856404;"),
          div(paste0(na_count, " (", round(100 * na_count / total_count, 1), "%)"), 
              style = "font-weight: bold; font-size: 18px; color: #856404;")
        ),
        
        div(
          style = "padding: 10px; background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; text-align: center;",
          div("Valid", style = "font-size: 12px; color: #6c757d;"),
          div(as.character(length(non_na_data)), style = "font-weight: bold; font-size: 18px;")
        )
      )
      
      # Type-specific statistics - add more safety checks
      type_stats <- if (var_type %in% c("number", "integer") && is.numeric(column_data) && length(non_na_data) > 0) {
        # Numeric statistics
        tryCatch({
          div(
            tags$hr(style = "margin: 15px 0;"),
            div(strong("Numeric Statistics"), style = "margin-bottom: 10px; color: #333;"),
            div(
              style = "display: grid; grid-template-columns: 1fr 1fr; gap: 10px;",
              
              div(
                style = "padding: 8px; background-color: #e3f2fd; border-radius: 4px;",
                div("Mean", style = "font-size: 11px; color: #1565c0;"),
                div(round(mean(non_na_data, na.rm = TRUE), 3), style = "font-weight: bold; color: #1565c0;")
              ),
              
              div(
                style = "padding: 8px; background-color: #e3f2fd; border-radius: 4px;",
                div("Median", style = "font-size: 11px; color: #1565c0;"),
                div(round(median(non_na_data, na.rm = TRUE), 3), style = "font-weight: bold; color: #1565c0;")
              ),
              
              div(
                style = "padding: 8px; background-color: #e3f2fd; border-radius: 4px;",
                div("Std Dev", style = "font-size: 11px; color: #1565c0;"),
                div(if(length(non_na_data) > 1) round(sd(non_na_data, na.rm = TRUE), 3) else "N/A", 
                    style = "font-weight: bold; color: #1565c0;")
              ),
              
              div(
                style = "padding: 8px; background-color: #e3f2fd; border-radius: 4px;",
                div("Range", style = "font-size: 11px; color: #1565c0;"),
                div(paste(round(min(non_na_data, na.rm = TRUE), 2), "to", 
                          round(max(non_na_data, na.rm = TRUE), 2)), 
                    style = "font-weight: bold; color: #1565c0; font-size: 11px;")
              )
            )
          )
        }, error = function(e) {
          div(
            tags$hr(style = "margin: 15px 0;"),
            div(
              style = "padding: 10px; background-color: #ffebee; border-radius: 4px; color: #c62828;",
              "Error calculating numeric statistics: ", e$message
            )
          )
        })
      } else if ((var_type == "categorical" || length(unique(non_na_data)) <= 20) && length(non_na_data) > 0) {
        # Categorical statistics - FIXED: Better handling of sparse data
        tryCatch({
          # Convert to character first to avoid factor issues
          char_data <- as.character(non_na_data)
          # Remove any remaining empty strings
          char_data <- char_data[char_data != "" & !is.na(char_data)]
          
          if (length(char_data) == 0) {
            return(div(
              tags$hr(style = "margin: 15px 0;"),
              div(
                style = "padding: 10px; background-color: #fff3cd; border-radius: 4px; color: #856404;",
                "No valid categorical values to display"
              )
            ))
          }
          
          value_counts <- table(char_data)
          value_counts <- sort(value_counts, decreasing = TRUE)
          
          div(
            tags$hr(style = "margin: 15px 0;"),
            div(strong("Value Counts"), style = "margin-bottom: 10px; color: #333;"),
            div(
              style = "max-height: 200px; overflow-y: auto; background-color: white; border: 1px solid #dee2e6; border-radius: 4px; padding: 5px;",
              lapply(names(value_counts), function(val) {
                count <- value_counts[[val]]
                pct <- round(100 * count / length(char_data), 1)
                
                div(
                  style = "padding: 5px 8px; border-bottom: 1px solid #f0f0f0; display: flex; justify-content: space-between; align-items: center;",
                  span(val, style = "font-family: monospace; font-size: 13px;"),
                  span(
                    paste0(count, " (", pct, "%)"),
                    style = "font-size: 12px; color: #6c757d; font-weight: 500;"
                  )
                )
              })
            )
          )
        }, error = function(e) {
          div(
            tags$hr(style = "margin: 15px 0;"),
            div(
              style = "padding: 10px; background-color: #ffebee; border-radius: 4px; color: #c62828;",
              "Error calculating categorical statistics: ", e$message
            )
          )
        })
      } else {
        # String/other types - show sample values
        tryCatch({
          # Get sample values (up to 20 unique examples)
          char_data <- as.character(non_na_data)
          char_data <- char_data[char_data != "" & !is.na(char_data)]
          
          if (length(char_data) > 0) {
            # Get up to 20 unique samples
            unique_samples <- unique(char_data)
            samples_to_show <- head(unique_samples, 20)
            
            div(
              tags$hr(style = "margin: 15px 0;"),
              div(strong("Sample Values"), style = "margin-bottom: 10px; color: #333;"),
              div(
                style = "max-height: 200px; overflow-y: auto; background-color: white; border: 1px solid #dee2e6; border-radius: 4px; padding: 8px;",
                lapply(samples_to_show, function(val) {
                  div(
                    style = "padding: 6px 8px; margin-bottom: 4px; background-color: #f8f9fa; border-radius: 3px; font-family: monospace; font-size: 13px; word-wrap: break-word;",
                    val
                  )
                }),
                if (length(unique_samples) > 20) {
                  div(
                    style = "padding: 8px; margin-top: 8px; text-align: center; color: #6c757d; font-size: 12px; border-top: 1px solid #dee2e6;",
                    paste("Showing 20 of", length(unique_samples), "unique values")
                  )
                }
              )
            )
          } else {
            div(
              tags$hr(style = "margin: 15px 0;"),
              div(
                style = "padding: 10px; background-color: #f8f9fa; border-radius: 4px; text-align: center; color: #6c757d;",
                "No valid text values to display"
              )
            )
          }
        }, error = function(e) {
          div(
            tags$hr(style = "margin: 15px 0;"),
            div(
              style = "padding: 10px; background-color: #ffebee; border-radius: 4px; color: #c62828;",
              "Error displaying sample values: ", e$message
            )
          )
        })
      }
      
      tagList(
        base_stats,
        type_stats
      )
    })
    
    # Data table
    output$data_table <- DT::renderDataTable({
      if (is.null(explorer_state$current_data)) {
        return(DT::datatable(
          data.frame(Message = "No data loaded. Select a dataset and load it using the button above."), 
          options = list(dom = 't', searching = FALSE, paging = FALSE)
        ))
      }
      
      data <- explorer_state$current_data
      
      # Apply column filters
      for (filter in explorer_state$column_filters) {
        column_name <- filter$column
        filter_value <- filter$value
        
        if (column_name %in% names(data)) {
          # Handle blank/empty string filters specially
          if (is.null(filter_value) || filter_value == "") {
            # Filter for blank/empty values
            data <- data[is.na(data[[column_name]]) | data[[column_name]] == "", , drop = FALSE]
          } else {
            # Use grepl for partial matching on non-blank values
            data <- data[grepl(filter_value, as.character(data[[column_name]]), ignore.case = TRUE), , drop = FALSE]
          }
        }
      }
      
      DT::datatable(
        data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "ftip"
        ),
        rownames = FALSE
      )
    }, server = TRUE)
    
    # Return reactive containing explorer state
    return(reactive({ explorer_state }))
  })
}


#' OSF Upload Server Module
#'
#' @param id The module ID
#' @param state Global state reactive values
#' @param session The current session object
osfUploadServer <- function(id, state, session) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values
    osf_state <- reactiveValues(
      authenticated = FALSE,
      user_info = NULL,
      projects = list(),
      dataset_valid = FALSE,
      dataset_info = NULL,
      upload_status = NULL
    )
    
    # Output flags for conditional panels
    output$authenticated <- reactive({ osf_state$authenticated })
    outputOptions(output, "authenticated", suspendWhenHidden = FALSE)
    
    output$dataset_valid <- reactive({ osf_state$dataset_valid })
    outputOptions(output, "dataset_valid", suspendWhenHidden = FALSE)
    
    output$ready_to_upload <- reactive({ 
      osf_state$authenticated && osf_state$dataset_valid && 
      ((!is.null(input$project_select) && input$project_select != "") ||
       (!is.null(input$project_id_manual) && input$project_id_manual != "") ||
       (input$project_option == "new" && !is.null(input$new_project_title) && input$new_project_title != ""))
    })
    outputOptions(output, "ready_to_upload", suspendWhenHidden = FALSE)
    
    # Directory selection
    volumes <- c(Home = "~")
    if (.Platform$OS.type == "windows") {
      volumes <- c(volumes, getVolumes()())
    }
    
    shinyDirChoose(
      input,
      "dataset_dir_select",
      roots = volumes,
      session = session
    )
    
    observeEvent(input$dataset_dir_select, {
      if (!is.null(input$dataset_dir_select)) {
        selected_dir <- parseDirPath(volumes, input$dataset_dir_select)
        if (length(selected_dir) > 0 && selected_dir != "") {
          updateTextInput(session, "dataset_dir", value = selected_dir)
        }
      }
    })
    
    # Auto-populate from validation module if a validated dataset is available
    observe({
      if (!is.null(state$validated_dataset_dir) && 
          dir.exists(state$validated_dataset_dir) &&
          (is.null(input$dataset_dir) || input$dataset_dir == "")) {
        
        updateTextInput(session, "dataset_dir", value = state$validated_dataset_dir)
        
        showNotification("Using validated dataset from previous step", type = "message", duration = 3)
        
        # Auto-trigger validation check
        # Build the file tree and run validation
        tryCatch({
          file_tree <- build_file_tree(state$validated_dataset_dir, state$validated_dataset_dir)
          session$sendCustomMessage("run_validation", file_tree)
        }, error = function(e) {
          showNotification(paste("Error validating dataset:", e$message), type = "error")
        })
      }
    })
    
    # Test authentication - Use direct HTTP since token works
    observeEvent(input$test_auth, {
      req(input$osf_token)
      
      showNotification("Testing OSF connection...", id = "auth_test", duration = NULL)
      
      # Test token with direct HTTP call
      if (!requireNamespace("httr", quietly = TRUE)) {
        install.packages("httr")
      }
      
      library(httr)
      
      response <- tryCatch({
        GET(
          "https://api.osf.io/v2/users/me/",
          add_headers(Authorization = paste("Bearer", input$osf_token))
        )
      }, error = function(e) {
        NULL
      })
      
      if (!is.null(response) && status_code(response) == 200) {
        # Token is valid!
        # Set it for osfr to use
        Sys.setenv(OSF_PAT = input$osf_token)
        Sys.setenv(OSF_TOKEN = input$osf_token)  # Try both env vars
        
        # Parse user info - need to handle the JSON properly
        user_data <- tryCatch({
          content(response, "parsed", encoding = "UTF-8")
        }, error = function(e) {
          list(data = list(attributes = list(full_name = "OSF User")))
        })
        
        # Extract name safely
        user_name <- "OSF User"
        if (!is.null(user_data) && !is.null(user_data$data)) {
          if (!is.null(user_data$data$attributes)) {
            if (!is.null(user_data$data$attributes$full_name)) {
              user_name <- user_data$data$attributes$full_name
            }
          }
        }
        
        osf_state$authenticated <- TRUE
        osf_state$user_info <- list(
          authenticated = TRUE,
          name = user_name
        )
        
        removeNotification(id = "auth_test")
        showNotification(
          paste("Connected as", user_name), 
          type = "message", 
          duration = 5
        )
        
        updateSelectInput(
          session,
          "project_select", 
          choices = c("Enter project ID manually below" = "")
        )
        
      } else {
        removeNotification(id = "auth_test")
        
        status <- if (!is.null(response)) status_code(response) else "Network error"
        showNotification(
          paste("Authentication failed. Status:", status), 
          type = "error",
          duration = 10
        )
        
        osf_state$authenticated <- FALSE
      }
    })
    
    # Display auth status
    output$auth_status_display <- renderUI({
      if (osf_state$authenticated) {
        div(
          class = "alert",
          style = "background-color: #d4edda; border-color: #c3e6cb; color: #155724; padding: 10px; border-radius: 4px;",
          icon("check-circle", style = "margin-right: 8px;"),
          strong("Connected to OSF successfully")
        )
      } else if (!is.null(input$osf_token) && input$osf_token != "") {
        div(
          class = "alert",
          style = "background-color: #fff3cd; border-color: #ffeaa7; color: #856404; padding: 10px; border-radius: 4px;",
          "Click 'Test Connection' to authenticate"
        )
      } else {
        div(
          class = "alert", 
          style = "background-color: #d1ecf1; border-color: #bee5eb; color: #0c5460; padding: 10px; border-radius: 4px;",
          icon("info-circle", style = "margin-right: 8px;"),
          "Enter your OSF Personal Access Token to continue. ",
          tags$a(href = "https://osf.io/settings/tokens", target = "_blank", "Generate token here")
        )
      }
    })
    
    # Check dataset validity using the existing validator
    observeEvent(input$check_dataset, {
      req(input$dataset_dir)
      
      message("DEBUG: check_dataset triggered with dir: ", input$dataset_dir)
      
      dataset_path <- input$dataset_dir
      
      if (!dir.exists(dataset_path)) {
        showNotification("Directory does not exist", type = "error")
        osf_state$dataset_valid <- FALSE
        return()
      }
      
      showNotification("Building file tree for validation...", id = "validate_progress", duration = NULL)
      
      # Build the file tree just like the validation module does
      tryCatch({
        # Build file tree for the validator
        file_tree <- build_file_tree(dataset_path, dataset_path)
        message("DEBUG: File tree built successfully")
        
        removeNotification(id = "validate_progress")
        showNotification("Running validation...", id = "validate_progress2", duration = NULL)
        
        # Send to JavaScript validator
        message("DEBUG: Sending file tree to JavaScript validator")
        session$sendCustomMessage("run_validation", file_tree)
        message("DEBUG: Custom message sent to JS")
        
      }, error = function(e) {
        message("DEBUG: Error building file tree: ", e$message)
        removeNotification(id = "validate_progress")
        removeNotification(id = "validate_progress2")
        showNotification(paste("Error building file tree:", e$message), type = "error")
        osf_state$dataset_valid <- FALSE
      })
    })
    
    # The validation module uses a global input, not namespaced
    # Create a reactive that polls for validation results from parent session
    validation_check <- reactiveTimer(500)  # Check every 500ms
    
    observe({
      validation_check()  # Trigger on timer
      
      # Try to access parent session's validation results
      parent_session <- session$rootScope()
      
      if (!is.null(parent_session) && !is.null(parent_session$input$validation_results)) {
        validation_result <- isolate(parent_session$input$validation_results)
        
        # Create a unique identifier for this validation result to avoid re-processing
        validation_id <- paste(validation_result$valid, 
                              validation_result$summary$totalFiles, 
                              collapse = "-")
        
        # Only process if we have a dataset directory set and haven't processed this result yet
        if (!is.null(validation_result) && !is.null(input$dataset_dir) && input$dataset_dir != "" && 
            !isTRUE(osf_state$last_processed_validation_id == validation_id)) {
          
          # Mark this validation as processed to avoid re-processing
          osf_state$last_processed_validation_id <- validation_id
          
          message("DEBUG: validation_results found in parent session, valid = ", validation_result$valid)
          
          removeNotification(id = "validate_progress")
          removeNotification(id = "validate_progress2")
          
          if (validation_result$valid == TRUE) {
            message("DEBUG: Dataset is valid, proceeding...")
            
            # Dataset is valid - extract info from the validation results
            tryCatch({
              # Read the dataset description to get name and description
              desc_path <- file.path(input$dataset_dir, "dataset_description.json")
              desc_json <- jsonlite::fromJSON(desc_path)
              
              osf_state$dataset_info <- list(
                name = desc_json$name,
                description = desc_json$description,
                path = input$dataset_dir,
                file_count = validation_result$summary$totalFiles
              )
              
              osf_state$dataset_valid <- TRUE
              message("DEBUG: osf_state$dataset_valid set to TRUE")
              
              showNotification(
                paste("Dataset is valid! Found", validation_result$summary$totalFiles, "files."), 
                type = "message",
                duration = 5
              )
              
              # Debug: Check what elements exist in the DOM
              shinyjs::runjs(paste0(
                "console.log('Looking for element: ", session$ns("osf_project_section"), "');",
                "console.log('Element found: ', $('#", session$ns("osf_project_section"), "').length);",
                "console.log('Element visible: ', $('#", session$ns("osf_project_section"), "').is(':visible'));"
              ))
              
              # Force show the element multiple ways
              section_id <- session$ns("osf_project_section")
              message("DEBUG: Attempting to show section with ID: ", section_id)
              
              # Method 1: shinyjs show
              shinyjs::show(section_id)
              
              # Method 2: Direct jQuery
              shinyjs::runjs(paste0("$('#", section_id, "').show();"))
              
              # Method 3: Remove display:none style
              shinyjs::runjs(paste0("$('#", section_id, "').css('display', 'block');"))
              
              # Method 4: Remove the style attribute entirely
              shinyjs::runjs(paste0("$('#", section_id, "').removeAttr('style');"))
              
              message("DEBUG: Tried multiple show methods")
              
            }, error = function(e) {
              message("DEBUG: Error reading dataset description: ", e$message)
              
              # Validation passed but couldn't read details - still allow upload
              osf_state$dataset_info <- list(
                name = "Validated Dataset",
                description = "Psych-DS compliant dataset",
                path = input$dataset_dir,
                file_count = validation_result$summary$totalFiles
              )
              osf_state$dataset_valid <- TRUE
              showNotification("Dataset is valid!", type = "message", duration = 5)
              
              # Use multiple approaches to show the section
              section_id <- "osf_project_section"
              shinyjs::show(section_id)
              shinyjs::show(session$ns(section_id))
              shinyjs::runjs(paste0("$('#", session$ns(section_id), "').show();"))
            })
            
          } else {
            message("DEBUG: Dataset is invalid")
            
            # Dataset is invalid
            osf_state$dataset_valid <- FALSE
            
            # Use shinyjs to hide the next section with namespaced ID
            shinyjs::hide(session$ns("osf_project_section"))
            
            # Extract error information
            error_count <- 0
            if (!is.null(validation_result$issues$errors)) {
              error_count <- length(validation_result$issues$errors)
            }
            
            showModal(modalDialog(
              title = "Dataset Validation Failed",
              size = "m",
              div(
                p("This dataset does not pass Psych-DS validation."),
                p("Please use the ", strong("Validate Dataset"), " module to see detailed errors and fix them."),
                if (error_count > 0) {
                  div(
                    hr(),
                    p(strong(paste("Found", error_count, "validation error(s):")))
                  )
                }
              ),
              footer = tagList(
                modalButton("OK"),
                actionButton(session$ns("go_to_validate"), "Go to Validation Module", class = "btn-primary")
              ),
              easyClose = TRUE
            ))
          }
        }
      }
    })
    
    # Handle validation errors
    observeEvent(input$validation_error, {
      removeNotification(id = "validate_progress")
      removeNotification(id = "validate_progress2")
      
      showNotification(
        paste("Validation error:", input$validation_error), 
        type = "error",
        duration = 10
      )
      
      osf_state$dataset_valid <- FALSE
      shinyjs::hide(session$ns("osf_project_section"))
    })
    
    # Navigate to validation module if requested
    observeEvent(input$go_to_validate, {
      removeModal()
      # Pass the directory to the validation module via state
      state$pending_validation_dir <- input$dataset_dir
      # Switch to validate tab - use parent session for navigation
      updateTabItems(session = session$parent, inputId = "sidebar", selected = "validate")
    })
    
    # Helper function to build file tree (same as used in validation module)
    build_file_tree <- function(path, root_path, level = 0) {
      if (level > 10) {  # Prevent infinite recursion
        return(list())
      }
      
      items <- list.files(path, all.files = FALSE, full.names = FALSE)
      tree <- list()
      
      for (item in items) {
        # Skip hidden files and common non-data directories
        if (substr(item, 1, 1) == ".") next
        if (item %in% c("node_modules", "__pycache__", ".git")) next
        
        item_path <- file.path(path, item)
        
        if (dir.exists(item_path)) {
          # Directory
          tree[[item]] <- list(
            type = "directory",
            contents = build_file_tree(item_path, root_path, level + 1)
          )
        } else {
          # File
          rel_path <- gsub(paste0("^", normalizePath(root_path), .Platform$file.sep), "", 
                          normalizePath(item_path))
          rel_path <- gsub("\\\\", "/", rel_path)  # Normalize path separators
          
          # Read file content for text files
          file_text <- NULL
          if (grepl("\\.(json|csv|tsv|txt|md)$", item, ignore.case = TRUE)) {
            tryCatch({
              file_text <- readLines(item_path, warn = FALSE, encoding = "UTF-8")
              file_text <- paste(file_text, collapse = "\n")
            }, error = function(e) {
              file_text <- NULL
            })
          }
          
          tree[[item]] <- list(
            type = "file",
            file = list(
              name = item,
              path = rel_path,
              text = file_text
            )
          )
        }
      }
      
      return(tree)
    }
    
    # Display dataset status
    output$dataset_status_display <- renderUI({
      if (osf_state$dataset_valid && !is.null(osf_state$dataset_info)) {
        div(
          class = "alert",
          style = "background-color: #d4edda; border-color: #c3e6cb; color: #155724; padding: 10px; border-radius: 4px;",
          icon("check-circle", style = "margin-right: 8px;"),
          strong("Dataset: "), osf_state$dataset_info$name,
          tags$br(),
          tags$small(osf_state$dataset_info$description),
          tags$br(),
          tags$small(
            style = "color: #28a745; margin-top: 5px;",
            icon("check"), " Psych-DS validation passed"
          )
        )
      }
    })
    
    # Load user's OSF projects
    # Simplified loadProjects that doesn't rely on osf_ls_nodes
    loadProjects <- function() {
      # Since osf_ls_nodes is problematic, we'll just allow manual entry
      updateSelectInput(
        session,
        "project_select", 
        choices = c("Enter project ID manually below" = "")
      )
      
      showNotification("Please enter your OSF project ID manually or create a new project", 
                      type = "message", duration = 5)
    }
    
    # Control upload summary visibility based on readiness
    observe({
      message("DEBUG: Upload summary observer triggered")
      message("  authenticated: ", osf_state$authenticated)
      message("  dataset_valid: ", osf_state$dataset_valid)
      message("  project_select: ", input$project_select)
      message("  project_id_manual: ", input$project_id_manual)
      message("  project_option: ", input$project_option)
      message("  new_project_title: ", input$new_project_title)
      
      # Check each condition separately for debugging
      cond1 <- osf_state$authenticated
      cond2 <- osf_state$dataset_valid
      cond3a <- (!is.null(input$project_select) && input$project_select != "")
      cond3b <- (!is.null(input$project_id_manual) && input$project_id_manual != "")
      cond3c <- (input$project_option == "new" && !is.null(input$new_project_title) && input$new_project_title != "")
      cond3 <- (cond3a || cond3b || cond3c)
      
      message("  Condition breakdown:")
      message("    authenticated: ", cond1)
      message("    dataset_valid: ", cond2)
      message("    project selected: ", cond3a)
      message("    manual ID entered: ", cond3b)
      message("    new project with title: ", cond3c)
      message("    any project condition: ", cond3)
      message("  SHOULD SHOW: ", cond1 && cond2 && cond3)
      
      if (cond1 && cond2 && cond3) {
        summary_id <- session$ns("upload_summary_section")
        message("DEBUG: Showing upload summary section with ID: ", summary_id)
        
        # Try multiple methods to show it
        shinyjs::show(summary_id)
        shinyjs::runjs(paste0("$('#", summary_id, "').show();"))
        shinyjs::runjs(paste0("$('#", summary_id, "').css('display', 'block');"))
        shinyjs::runjs(paste0(
          "console.log('Trying to show upload summary');",
          "console.log('Element exists: ', $('#", summary_id, "').length);",
          "console.log('Element visible after show: ', $('#", summary_id, "').is(':visible'));"
        ))
      } else {
        summary_id <- session$ns("upload_summary_section")
        message("DEBUG: Hiding upload summary section with ID: ", summary_id)
        shinyjs::hide(summary_id)
      }
    })
    
    # Upload summary
    output$upload_summary_display <- renderUI({
      summary_items <- list()
      
      # Dataset info
      if (!is.null(osf_state$dataset_info)) {
        summary_items <- append(summary_items, list(
          p(strong("Dataset: "), osf_state$dataset_info$name)
        ))
      }
      
      # Project info
      if (input$project_option == "new") {
        summary_items <- append(summary_items, list(
          p(strong("New project: "), input$new_project_title),
          p(strong("Public: "), ifelse(input$make_public, "Yes", "No"))
        ))
      } else {
        project_id <- if (input$project_id_manual != "") {
          input$project_id_manual
        } else {
          input$project_select
        }
        
        if (!is.null(project_id) && project_id != "") {
          project_name <- names(osf_state$projects)[osf_state$projects == project_id]
          summary_items <- append(summary_items, list(
            p(strong("Project: "), ifelse(length(project_name) > 0, project_name, project_id))
          ))
        }
      }
      
      # Upload path
      if (!is.null(input$upload_path) && input$upload_path != "") {
        summary_items <- append(summary_items, list(
          p(strong("Upload to: "), "/", input$upload_path)
        ))
      }
      
      # File count
      data_files <- list.files(
        file.path(osf_state$dataset_info$path, "data"),
        recursive = TRUE,
        full.names = FALSE
      )
      
      summary_items <- append(summary_items, list(
        p(strong("Files to upload: "), length(data_files) + 1, " (including dataset_description.json)")
      ))
      
      div(
        class = "well",
        summary_items
      )
    })
    
    # Main upload function - using direct API calls instead of osfr
    observeEvent(input$start_upload, {
      showNotification("Starting upload...", id = "upload_start", duration = NULL)
      
      # Show progress div
      shinyjs::show("upload_progress")
      
      library(httr)
      library(jsonlite)
      
      tryCatch({
        token <- input$osf_token
        
        # Get or create project using direct API
        if (input$project_option == "new") {
          # Create new project via API
          body_json <- jsonlite::toJSON(list(
            data = list(
              type = "nodes",
              attributes = list(
                title = input$new_project_title,
                description = if(is.null(input$new_project_description) || input$new_project_description == "") "" else input$new_project_description,
                category = "project",
                public = input$make_public
              )
            )
          ), auto_unbox = TRUE)
          
          response <- POST(
            "https://api.osf.io/v2/nodes/",
            add_headers(
              Authorization = paste("Bearer", token),
              `Content-Type` = "application/vnd.api+json"
            ),
            body = body_json,
            encode = "raw"
          )
          
          if (status_code(response) != 201) {
            error_msg <- tryCatch({
              err_content <- content(response, "text", encoding = "UTF-8")
              err_content
            }, error = function(e) {
              paste("Status code:", status_code(response))
            })
            stop(paste("Failed to create project.", error_msg))
          }
          
          project_data <- content(response, "text", encoding = "UTF-8")
          project_json <- fromJSON(project_data)
          project_id <- project_json$data$id
          
          showNotification("Created new OSF project", type = "message")
          
        } else {
          # Use existing project
          project_id <- if (input$project_id_manual != "") {
            input$project_id_manual  
          } else {
            input$project_select
          }
        }
        
        # For file upload, we need to use OSF's waterbutler API
        # First, get the upload URL for the project
        storage_response <- GET(
          paste0("https://api.osf.io/v2/nodes/", project_id, "/files/"),
          add_headers(Authorization = paste("Bearer", token))
        )
        
        if (status_code(storage_response) != 200) {
          stop(paste("Failed to get storage info. Status:", status_code(storage_response)))
        }
        
        storage_text <- content(storage_response, "text", encoding = "UTF-8")
        storage_data <- fromJSON(storage_text)
        
        # Find the osfstorage provider
        osfstorage <- NULL
        for (i in seq_len(nrow(storage_data$data))) {
          if (storage_data$data$attributes$name[i] == "osfstorage") {
            osfstorage <- storage_data$data[i,]
            break
          }
        }
        
        if (is.null(osfstorage)) {
          stop("Could not find OSF storage for this project")
        }
        
        # Get the upload URL - this is typically the waterbutler URL
        # The URL format should be like: https://files.osf.io/v1/resources/PROJECT_ID/providers/osfstorage/
        upload_base_url <- paste0("https://files.osf.io/v1/resources/", project_id, "/providers/osfstorage/")
        
        # Get all files and folders in the dataset
        dataset_path <- osf_state$dataset_info$path
        all_files <- list.files(dataset_path, recursive = TRUE, full.names = TRUE, all.files = FALSE)
        
        # Filter out hidden files and common non-data directories
        all_files <- all_files[!grepl("^\\.|/\\.", all_files)]  # No hidden files
        all_files <- all_files[!grepl("(__pycache__|node_modules|\\.git)", all_files)]  # No cache/git
        
        showNotification(paste("Uploading", length(all_files), "files..."), id = "upload_files", duration = NULL)
        
        # First, create all necessary folders
        all_dirs <- unique(dirname(gsub(paste0("^", normalizePath(dataset_path), "/"), "", 
                                        sapply(all_files, normalizePath))))
        all_dirs <- all_dirs[all_dirs != "."]
        all_dirs <- sort(all_dirs)  # Sort to ensure parent dirs are created first
        
        created_folders <- list()
        
        for (dir_path in all_dirs) {
          if (dir_path != "" && dir_path != ".") {
            # Split path and create folders hierarchically
            path_parts <- strsplit(gsub("\\\\", "/", dir_path), "/")[[1]]
            current_path <- ""
            parent_folder_id <- NULL
            
            for (folder in path_parts) {
              full_path <- if (current_path == "") folder else paste0(current_path, "/", folder)
              
              # Only create if we haven't already
              if (!(full_path %in% names(created_folders))) {
                # Build folder URL using parent folder ID if available
                if (is.null(parent_folder_id)) {
                  # Creating at root
                  folder_url <- paste0(upload_base_url, "?kind=folder&name=", 
                                     URLencode(folder, reserved = TRUE))
                } else {
                  # Creating inside parent folder - clean the parent ID
                  clean_parent_id <- gsub("^osfstorage/", "", parent_folder_id)
                  clean_parent_id <- gsub("/$", "", clean_parent_id)
                  folder_url <- paste0("https://files.osf.io/v1/resources/", project_id,
                                     "/providers/osfstorage/", clean_parent_id, "/",
                                     "?kind=folder&name=", 
                                     URLencode(folder, reserved = TRUE))
                }
                
                message("DEBUG: Creating folder: ", full_path)
                if (!is.null(parent_folder_id)) {
                  message("DEBUG: Parent folder ID: ", parent_folder_id)
                }
                
                folder_response <- PUT(
                  folder_url,
                  add_headers(Authorization = paste("Bearer", token))
                )
                
                status <- status_code(folder_response)
                if (status %in% c(200, 201)) {
                  # Parse response to get folder ID
                  folder_content <- content(folder_response)
                  if (!is.null(folder_content$data$id)) {
                    folder_id <- folder_content$data$id
                    created_folders[[full_path]] <- folder_id
                    parent_folder_id <- folder_id  # This becomes parent for next level
                    message("DEBUG: Folder created: ", full_path, " with ID: ", folder_id)
                  } else {
                    created_folders[[full_path]] <- TRUE
                    message("DEBUG: Folder created but no ID found: ", full_path)
                  }
                } else if (status == 409) {
                  # Folder exists - need to get its ID
                  message("DEBUG: Folder already exists: ", full_path)
                  
                  # List the parent directory to find this folder
                  list_url <- if (is.null(parent_folder_id)) {
                    upload_base_url
                  } else {
                    # Use the parent folder ID we're tracking
                    clean_parent_id <- gsub("^osfstorage/", "", parent_folder_id)
                    clean_parent_id <- gsub("/$", "", clean_parent_id)
                    paste0("https://files.osf.io/v1/resources/", project_id, 
                           "/providers/osfstorage/", clean_parent_id, "/")
                  }
                  
                  list_response <- GET(
                    list_url,
                    add_headers(Authorization = paste("Bearer", token))
                  )
                  
                  if (status_code(list_response) == 200) {
                    list_content <- content(list_response)
                    # Find our folder in the listing
                    if (!is.null(list_content$data)) {
                      for (item in list_content$data) {
                        if (!is.null(item$attributes$name) && 
                            item$attributes$name == folder && 
                            item$attributes$kind == "folder") {
                          folder_id <- item$id
                          created_folders[[full_path]] <- folder_id
                          parent_folder_id <- folder_id  # Set as parent for next level
                          message("DEBUG: Found existing folder: ", full_path, " with ID: ", folder_id)
                          break
                        }
                      }
                    }
                  }
                  
                  if (!(full_path %in% names(created_folders)) || is.null(created_folders[[full_path]])) {
                    created_folders[[full_path]] <- TRUE
                    message("DEBUG: Folder exists but couldn't get ID: ", full_path)
                  }
                } else {
                  warning("Failed to create folder: ", full_path, " (status: ", status, ")")
                  
                  # Log error response for debugging
                  error_content <- tryCatch({
                    content(folder_response, "text", encoding = "UTF-8")
                  }, error = function(e) "")
                  if (nchar(error_content) > 0) {
                    message("DEBUG: Folder creation error: ", error_content)
                  }
                }
              } else {
                # Folder already created, use its ID as parent for next level
                parent_folder_id <- created_folders[[full_path]]
                message("DEBUG: Using existing folder: ", full_path, " with ID: ", parent_folder_id)
              }
              
              current_path <- full_path
            }
          }
        }
        
        # Now upload files
        withProgress(message = "Uploading dataset...", value = 0, {
          success_count <- 0
          failed_files <- character()
          
          for (i in seq_along(all_files)) {
            file_path <- all_files[i]
            
            # Get relative path from dataset root
            rel_path <- gsub(paste0("^", normalizePath(dataset_path), "/"), "", normalizePath(file_path))
            rel_path <- gsub("\\\\", "/", rel_path)  # Windows path fix
            
            file_name <- basename(file_path)
            file_dir <- dirname(rel_path)
            
            tryCatch({
              # Read file content
              file_content <- readBin(file_path, "raw", file.info(file_path)$size)
              
              # Build the upload URL - put file in its directory
              if (file_dir != "." && file_dir != "") {
                # Check if we have a folder ID for this directory
                folder_id <- if (file_dir %in% names(created_folders)) {
                  created_folders[[file_dir]]
                } else {
                  NULL
                }
                
                if (!is.null(folder_id) && folder_id != TRUE) {
                  # Upload to the specific folder using its ID
                  # Clean the folder ID - remove "osfstorage/" prefix if present
                  clean_folder_id <- gsub("^osfstorage/", "", folder_id)
                  clean_folder_id <- gsub("/$", "", clean_folder_id)  # Remove trailing slash
                  
                  file_upload_url <- paste0("https://files.osf.io/v1/resources/", project_id, 
                                          "/providers/osfstorage/", clean_folder_id, "/",
                                          "?kind=file&name=", 
                                          URLencode(file_name, reserved = TRUE))
                  message("DEBUG: Uploading to folder ID ", clean_folder_id, ": ", rel_path)
                } else {
                  # Fallback: try uploading with path in name (though this may fail)
                  message("DEBUG: Warning - no folder ID found for ", file_dir, ", trying path-based upload")
                  full_file_path <- paste0(file_dir, "/", file_name)
                  file_upload_url <- paste0(upload_base_url, 
                                          "?kind=file&name=", 
                                          URLencode(full_file_path, reserved = TRUE))
                }
              } else {
                # File is in root
                file_upload_url <- paste0(upload_base_url, 
                                        "?kind=file&name=", 
                                        URLencode(file_name, reserved = TRUE))
              }
              
              message("DEBUG: Uploading ", rel_path, " to ", file_upload_url)
              
              # Try to upload file
              file_response <- PUT(
                file_upload_url,
                add_headers(Authorization = paste("Bearer", token)),
                body = file_content,
                encode = "raw"
              )
              
              status <- status_code(file_response)
              message("DEBUG: Upload response status for ", rel_path, ": ", status)
              
              if (status %in% c(200, 201)) {
                success_count <- success_count + 1
                message("DEBUG: Successfully uploaded ", rel_path)
              } else if (status == 409) {
                # File exists - extract the upload URL from the error response
                message("DEBUG: File exists, attempting to update: ", rel_path)
                
                error_content <- content(file_response)
                
                # Extract the upload URL from the error response
                update_url <- NULL
                if (!is.null(error_content$data$links$upload)) {
                  update_url <- error_content$data$links$upload
                  message("DEBUG: Found update URL: ", update_url)
                }
                
                if (!is.null(update_url)) {
                  # Use the file's specific upload endpoint to update it
                  update_response <- PUT(
                    update_url,
                    add_headers(Authorization = paste("Bearer", token)),
                    body = file_content,
                    encode = "raw"
                  )
                  
                  update_status <- status_code(update_response)
                  message("DEBUG: Update response status for ", rel_path, ": ", update_status)
                  
                  if (update_status %in% c(200, 201)) {
                    success_count <- success_count + 1
                    message("DEBUG: Successfully updated ", rel_path)
                  } else {
                    failed_files <- c(failed_files, rel_path)
                    warning("Failed to update ", rel_path, ", status: ", update_status)
                    
                    # Get error details
                    update_error <- tryCatch({
                      content(update_response, "text", encoding = "UTF-8")
                    }, error = function(e) "")
                    message("DEBUG: Update error: ", update_error)
                  }
                } else {
                  # Couldn't find update URL in error response
                  failed_files <- c(failed_files, rel_path)
                  warning("File exists but no update URL found for ", rel_path)
                }
              } else {
                failed_files <- c(failed_files, rel_path)
                warning("Failed to upload ", rel_path, ", status: ", status)
                
                # Try to get error message
                error_content <- tryCatch({
                  content(file_response, "text", encoding = "UTF-8")
                }, error = function(e) "")
                
                if (nchar(error_content) > 0) {
                  message("DEBUG: Error response: ", substr(error_content, 1, 500))
                }
              }
              
            }, error = function(e) {
              failed_files <- c(failed_files, rel_path)
              warning("Exception uploading ", rel_path, ": ", e$message)
            })
            
            setProgress(i / length(all_files), 
                       detail = paste(success_count, "of", i, "files uploaded"))
          }
          
          # Generate and upload README if requested
          if (input$create_readme) {
            message("DEBUG: Checking for README generation")
            
            # First check if README already exists in the dataset
            existing_readme <- FALSE
            readme_files <- c("README.md", "readme.md", "README.MD", "Readme.md", "README.txt", "readme.txt")
            
            for (readme_name in readme_files) {
              if (file.exists(file.path(dataset_path, readme_name))) {
                existing_readme <- TRUE
                message("DEBUG: Found existing README: ", readme_name)
                break
              }
            }
            
            if (!existing_readme) {
              # Generate README content
              readme_content <- generateReadme(osf_state$dataset_info)
              
              # Upload README.md to OSF
              readme_upload_url <- paste0(upload_base_url, 
                                         "?kind=file&name=README.md")
              
              message("DEBUG: Uploading generated README.md")
              
              readme_response <- PUT(
                readme_upload_url,
                add_headers(Authorization = paste("Bearer", token)),
                body = charToRaw(readme_content),
                encode = "raw"
              )
              
              readme_status <- status_code(readme_response)
              
              if (readme_status %in% c(200, 201)) {
                showNotification("README.md generated and uploaded successfully", type = "message")
                message("DEBUG: README uploaded successfully")
              } else if (readme_status == 409) {
                # README exists in OSF, try to update it
                message("DEBUG: README exists in OSF, attempting update")
                
                error_content <- content(readme_response)
                update_url <- NULL
                if (!is.null(error_content$data$links$upload)) {
                  update_url <- error_content$data$links$upload
                }
                
                if (!is.null(update_url)) {
                  update_response <- PUT(
                    update_url,
                    add_headers(Authorization = paste("Bearer", token)),
                    body = charToRaw(readme_content),
                    encode = "raw"
                  )
                  
                  if (status_code(update_response) %in% c(200, 201)) {
                    showNotification("README.md updated successfully", type = "message")
                    message("DEBUG: README updated successfully")
                  } else {
                    showNotification("Could not update README.md", type = "warning")
                    message("DEBUG: Failed to update README, status: ", status_code(update_response))
                  }
                }
              } else {
                showNotification("Failed to upload README.md", type = "warning")
                message("DEBUG: Failed to upload README, status: ", readme_status)
              }
            } else {
              showNotification("Existing README found - preserving original", type = "message")
              message("DEBUG: Skipping README generation - existing README found")
            }
          }
          
          removeNotification(id = "upload_files")
          
          # Report results
          if (success_count == length(all_files)) {
            showNotification(paste("Successfully uploaded all", success_count, "files!"), type = "message")
          } else if (success_count > 0) {
            showNotification(paste("Uploaded", success_count, "of", length(all_files), "files.",
                                 length(failed_files), "failed."), type = "warning")
            if (length(failed_files) > 0 && length(failed_files) <= 5) {
              showNotification(paste("Failed files:", paste(failed_files, collapse = ", ")), 
                             type = "warning", duration = 10)
            }
          } else {
            showNotification("Upload failed - no files were uploaded", type = "error")
          }
        })
        
        removeNotification(id = "upload_start")
        
        # Success message with link
        project_url <- paste0("https://osf.io/", project_id)
        
        # Store the URL in a reactive value so we can access it from button click
        osf_state$last_project_url <- project_url
        
        showModal(modalDialog(
          title = "Upload Successful!",
          size = "m",
          div(
            icon("check-circle", style = "font-size: 24px; margin-right: 10px; color: #4caf50;"),
            h4("Your dataset has been uploaded to OSF"),
            hr(),
            p("Project URL:"),
            div(
              style = "background-color: #f6f8fa; padding: 10px; border-radius: 4px; margin: 10px 0;",
              code(project_url)
            ),
            div(
              style = "margin-top: 10px;",
              actionButton(
                session$ns("open_osf_link"),
                "Open in Browser",
                icon = icon("external-link-alt"),
                class = "btn-primary"
              ),
              tags$button(
                class = "btn btn-default",
                style = "margin-left: 10px;",
                onclick = paste0("navigator.clipboard.writeText('", project_url, "'); $(this).text('Copied!'); setTimeout(() => $(this).text('Copy Link'), 2000);"),
                "Copy Link"
              )
            ),
            if (input$project_option == "new" && !input$make_public) {
              p(
                style = "color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 4px; margin-top: 10px;",
                icon("info-circle"),
                "Note: Your project is currently private. You can make it public from the OSF website."
              )
            }
          ),
          footer = modalButton("Close"),
          easyClose = TRUE
        ))
        
        # Hide progress
        shinyjs::hide("upload_progress")
        
      }, error = function(e) {
        removeNotification(id = "upload_start")
        showNotification(paste("Upload failed:", e$message), type = "error")
        shinyjs::hide("upload_progress")
      })
    })
    
    # Helper function to generate README
    generateReadme <- function(dataset_info) {
      # Count data files
      data_path <- file.path(dataset_info$path, "data")
      data_files <- list()
      if (dir.exists(data_path)) {
        data_files <- list.files(data_path, recursive = TRUE, pattern = "\\.(csv|tsv|txt|json)$", ignore.case = TRUE)
      }
      
      # Get subdirectories in data folder
      data_dirs <- list.dirs(data_path, recursive = TRUE, full.names = FALSE)
      data_dirs <- data_dirs[data_dirs != ""]
      
      readme <- paste(
        "# ", dataset_info$name, "\n\n",
        dataset_info$description, "\n\n",
        "## Dataset Structure\n\n",
        "This is a Psych-DS compliant dataset containing:\n",
        "- `dataset_description.json`: Metadata about the dataset\n",
        "- `data/`: Directory containing ", length(data_files), " data file(s)\n",
        if (length(data_dirs) > 0) paste0("  - Subdirectories: ", paste(data_dirs, collapse = ", "), "\n") else "",
        "\n",
        "## Files\n\n",
        if (length(data_files) > 0) {
          paste("### Data Files\n",
                paste("- `", data_files, "`", sep = "", collapse = "\n"),
                "\n\n", sep = "")
        } else "",
        "## Psych-DS Standard\n\n",
        "This dataset follows the Psych-DS (Psychological Dataset) standard for organizing behavioral datasets.\n",
        "Learn more at: https://psych-ds.github.io/\n\n",
        "## Citation\n\n",
        "Please cite this dataset as specified in the dataset_description.json file.\n\n",
        "---\n",
        "*Generated on ", format(Sys.Date(), "%B %d, %Y"), " using the Psych-DS Uploader*",
        sep = ""
      )
      
      return(readme)
    }
    
    # Handle opening the OSF link in browser
    observeEvent(input$open_osf_link, {
      if (!is.null(osf_state$last_project_url)) {
        utils::browseURL(osf_state$last_project_url)
      }
    })
    
    # Progress display
    output$upload_progress_display <- renderUI({
      div(
        class = "alert",
        style = "background-color: #d1ecf1; border-color: #bee5eb; color: #0c5460; padding: 10px; border-radius: 4px;",
        icon("spinner", class = "fa-spin", style = "margin-right: 8px;"),
        "Upload in progress. Please do not close this window..."
      )
    })
  })
}
