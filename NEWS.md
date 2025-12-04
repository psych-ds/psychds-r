# psychds (development version)

# psychds 0.1.0

## New Features

* Initial CRAN release

### Shiny Application
* Three-step wizard for creating Psych-DS compliant datasets
* Interactive file browser for selecting data files
* Automatic detection of CSV column structures
* Keyword-based filename standardization with batch operations
* Auto-naming from constant columns in data files

### Data Dictionary
* Professional HTML data dictionary generator with modern styling
* Support for variable descriptions, types, units, and constraints
* Categorical value documentation with labels and descriptions
* Summary statistics display
* Global missing value code definitions
* Print-to-PDF support via browser

### Validation
* Dataset structure validation against Psych-DS specification
* JSON schema validation for `dataset_description.json`
* Detailed error messages and fix suggestions
* Step-by-step validation progress display

### OSF Integration
* Authentication with OSF Personal Access Token
* Upload to existing projects or create new ones
* Folder structure preservation
* Automatic README generation

### Utilities
* `run_psych_ds_app()` - Launch the Shiny application
* `check_psychds_deps()` - Verify package dependencies
* `generate_html_dictionary()` - Programmatic dictionary generation

## Documentation

* Comprehensive pkgdown website
* Getting Started vignette
* Shiny App Guide vignette
* Data Dictionaries vignette
* Full function reference

## Known Issues

* PDF generation requires external LaTeX installation or browser print-to-PDF
* Large datasets (>100MB) may experience slower upload times to OSF and slower validation

---

# psychds 0.0.0.9000

* Initial development version
* Core Shiny application structure
* Basic validation functionality
