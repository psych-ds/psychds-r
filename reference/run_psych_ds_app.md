# Run the Psych-DS Shiny Application

Launches the interactive Shiny application for creating, editing, and
validating Psych-DS compliant datasets.

## Usage

``` r
run_psych_ds_app(
  launch.browser = getOption("shiny.launch.browser", interactive()),
  port = getOption("shiny.port"),
  host = getOption("shiny.host", "127.0.0.1"),
  startup_mode = getOption("psychds.startup_mode", "recommended"),
  force_browser = getOption("psychds.force_browser", FALSE),
  ...
)
```

## Arguments

- launch.browser:

  Logical or function. Should the app be opened in a browser? Defaults
  to `TRUE` in interactive sessions.

- port:

  Integer. Port number for the application. If `NULL`, a random
  available port is used.

- host:

  Character. Host IP address. Defaults to `"127.0.0.1"` (localhost).

- startup_mode:

  Character. One of `"strict"`, `"recommended"`, or `"minimal"`.
  Controls dependency checking strictness:

  - `"strict"`: Requires all packages at minimum versions

  - `"recommended"`: Warns about outdated packages but continues

  - `"minimal"`: Only checks for critical dependencies

- force_browser:

  Logical. Force opening in external browser. Recommended for RStudio
  users experiencing viewer issues.

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

This function does not return a value. It launches the Shiny application
and blocks until the app is closed.

## Details

The application provides a guided interface for:

- Selecting project directories and data files

- Adding dataset metadata

- Standardizing filenames with Psych-DS keywords

- Generating data dictionaries

- Validating datasets against the Psych-DS specification

- Uploading to the Open Science Framework (OSF)

## See also

[`check_psychds_deps()`](https://psych-ds.github.io/psychds-r/reference/check_psychds_deps.md)
for verifying dependencies before running.

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard launch
run_psych_ds_app()

# Force external browser (if RStudio viewer has issues)
run_psych_ds_app(force_browser = TRUE)

# Minimal checking for faster startup
run_psych_ds_app(startup_mode = "minimal")

# Specific port
run_psych_ds_app(port = 3838)
} # }
```
