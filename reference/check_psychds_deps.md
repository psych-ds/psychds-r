# Check psychds Dependencies

Verifies that all required and optional package dependencies are
installed and meet minimum version requirements.

## Usage

``` r
check_psychds_deps(
  install_missing = interactive(),
  detailed = FALSE,
  check_pdf = TRUE
)
```

## Arguments

- install_missing:

  Logical. If `TRUE` (and session is interactive), prompts the user to
  install any missing packages. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html).

- detailed:

  Logical. If `TRUE`, displays additional information about the R
  environment. Default is `FALSE`.

- check_pdf:

  Logical. Ignored. Retained for backwards compatibility. Default is
  `TRUE`.

## Value

Invisibly returns a list with `missing_required` and
`missing_recommended` character vectors.

## Details

This function checks for:

- Required packages: shiny, shinydashboard, shinyjs, shinyFiles, DT,
  jsonlite

- Optional packages: httr, rmarkdown, knitr

- Minimum version requirements for each package

If dependencies are missing, the function provides installation
instructions.

## Examples

``` r
# Quick check
check_psychds_deps()
#> Checking psychds dependencies...
#> ✓ shiny (1.13.0)
#> ✓ shinydashboard (0.7.3)
#> ✓ shinyjs (2.1.1)
#> ✓ shinyFiles (0.9.3)
#> ✓ DT (0.34.0)
#> ✓ jsonlite (2.0.0)
#> ✗ jsonvalidate - MISSING (required)
#> ✗ dplyr - MISSING (required)
#> ✗ tidyr - MISSING (required)
#> ✗ readr - MISSING (required)
#> 
#> Recommended packages:
#> ✓ httr (1.4.8)
#> ✓ rmarkdown (2.31)
#> ✓ knitr (1.51)
#> 
#> ⚠ Missing required packages: jsonvalidate, dplyr, tidyr, readr
#> 
#> To install missing packages, run:
#> install.packages(c("jsonvalidate", "dplyr", "tidyr", "readr"))

# Detailed check with environment info
check_psychds_deps(detailed = TRUE)
#> Checking psychds dependencies...
#> ✓ shiny (1.13.0)
#> ✓ shinydashboard (0.7.3)
#> ✓ shinyjs (2.1.1)
#> ✓ shinyFiles (0.9.3)
#> ✓ DT (0.34.0)
#> ✓ jsonlite (2.0.0)
#> ✗ jsonvalidate - MISSING (required)
#> ✗ dplyr - MISSING (required)
#> ✗ tidyr - MISSING (required)
#> ✗ readr - MISSING (required)
#> 
#> Recommended packages:
#> ✓ httr (1.4.8)
#> ✓ rmarkdown (2.31)
#> ✓ knitr (1.51)
#> 
#> Environment:
#>   R version: 4.6.0
#>   Platform: unix
#> 
#> ⚠ Missing required packages: jsonvalidate, dplyr, tidyr, readr
#> 
#> To install missing packages, run:
#> install.packages(c("jsonvalidate", "dplyr", "tidyr", "readr"))
```
