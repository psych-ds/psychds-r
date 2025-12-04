# Setup PDF Generation Capabilities

Interactive function to help users set up PDF generation

Interactive setup wizard for enabling PDF generation in psychds. Guides
users through installing TinyTeX or pagedown.

## Usage

``` r
setup_pdf_generation(method = "auto")

setup_pdf_generation(method = "auto")
```

## Arguments

- force:

  Logical. If `TRUE`, runs setup even if PDF generation is already
  available. Default is `FALSE`.

## Value

Invisibly returns `TRUE` if PDF generation is successfully configured,
`FALSE` otherwise.

## Details

PDF generation requires either:

- TinyTeX (recommended): A lightweight LaTeX distribution

- pagedown: An alternative that uses Chrome/Chromium

If neither is available, data dictionaries will be generated as HTML
files which can be printed to PDF from any web browser.

## See also

[`check_psychds_deps()`](https://psych-ds.github.io/psychds-r/reference/check_psychds_deps.md)
for checking current capabilities.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run interactive setup
setup_pdf_generation()

# Force re-setup even if already configured
setup_pdf_generation(force = TRUE)
} # }
```
