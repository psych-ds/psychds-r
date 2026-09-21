# Initialize Shiny App with Robust Package Management

Main entry point that ensures all dependencies are met

## Usage

``` r
run_app_safe(app_dir = system.file("shiny", package = "psychds"), ...)
```

## Arguments

- app_dir:

  Directory containing the Shiny app

- ...:

  Additional arguments passed to shiny::runApp
