# Check psychds Dependencies

Verifies that all required and optional package dependencies are
installed and meet minimum version requirements.

## Usage

``` r
check_psychds_deps(install_missing = interactive())
```

## Arguments

- detailed:

  Logical. If `TRUE`, displays detailed information about installed
  package versions, R environment, and PDF generation capabilities.
  Default is `FALSE`.

- check_pdf:

  Logical. If `TRUE` (and `detailed = TRUE`), also checks PDF generation
  capabilities. Default is `TRUE`.

## Value

Invisibly returns `TRUE` if all required dependencies are satisfied,
`FALSE` otherwise.

## Details

This function checks for:

- Required packages: shiny, shinydashboard, shinyjs, shinyFiles, DT,
  jsonlite

- Optional packages: sortable, zip, pointblank, osfr

- Minimum version requirements for each package

- PDF generation capabilities (rmarkdown, TinyTeX, pagedown)

If dependencies are missing, the function provides installation
instructions.

## See also

[`setup_pdf_generation()`](https://psych-ds.github.io/psychds-r/reference/setup_pdf_generation.md)
for setting up PDF capabilities.

## Examples

``` r
# Quick check
check_psychds_deps()
#> Checking psychds dependencies...
#> ✓ shiny (1.12.1)
#> ✓ shinydashboard (0.7.3)
#> ✓ shinyjs (2.1.1)
#> ✓ shinyFiles (0.9.3)
#> ✓ DT (0.34.0)
#> ✓ jsonlite (2.0.0)
#> ✓ jsonvalidate (1.5.0)
#> ✓ dplyr (1.1.4)
#> ✓ tidyr (1.3.2)
#> ✓ readr (2.1.6)
#> 
#> Recommended packages:
#> ✓ httr (1.4.7)
#> ✓ rmarkdown (2.30)
#> ✓ knitr (1.51)
#> 
#> PDF Generation:
#> ○ No PDF generation available
#>   Run setup_pdf_generation() to enable
#> 
#> ✓ All required dependencies are installed!

# Detailed check with all version info
check_psychds_deps(detailed = TRUE)
#> Error in check_psychds_deps(detailed = TRUE): unused argument (detailed = TRUE)
```
