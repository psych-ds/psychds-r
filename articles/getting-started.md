# Getting Started with psychds

## Introduction

The **psychds** package helps researchers organize their psychological
and behavioral research data according to the [Psych-DS
standard](https://psych-ds.github.io/). This vignette will walk you
through:

1.  Installing the package
2.  Checking dependencies
3.  Launching the Shiny app
4.  Understanding the Psych-DS structure

## Installation

### From CRAN

``` r

install.packages("psychds")
```

### Development Version

``` r

# install.packages("remotes")
remotes::install_github("psych-ds/psychds-r", dependencies = TRUE)
```

## Checking Your Environment

Before using psychds, verify that all dependencies are properly
installed:

``` r

library(psychds)

# Basic check
check_psychds_deps()

# Detailed check with version information
check_psychds_deps(detailed = TRUE)
```

The detailed check will show:

- Installed package versions
- R environment information

## Launching the Application

The primary interface for psychds is an interactive Shiny application:

``` r

run_psych_ds_app()
```

This opens a browser window with the dataset builder wizard.

### Launch Options

``` r

# Force external browser (useful if RStudio viewer has issues)
run_psych_ds_app(force_browser = TRUE)

# Use a specific port
run_psych_ds_app(port = 3838)

# Minimal dependency checking for faster startup
run_psych_ds_app(startup_mode = "minimal")
```

## Validating from the Console

Existing datasets can be validated without launching the app:

``` r

validate_dataset("path/to/my-dataset")

# JSON output for programmatic use
validate_dataset("path/to/my-dataset", json = TRUE)
```

This runs the same official ‘Psych-DS’ validator used by the app, via a
bundled script. It requires ‘Node.js’ (\>= 18) to be installed and on
your PATH; everything else, including the validation schemas, ships with
the package, and no network access is needed.

## Understanding Psych-DS Structure

A Psych-DS compliant dataset has the following structure:

    my_dataset/
    ├── dataset_description.json    # Required: Dataset metadata
    ├── README.md                   # Recommended: Human-readable description
    ├── data/                       # Required: Contains data files
    │   ├── study-exp1_data.csv
    │   └── study-exp1_data.json    # Sidecar with variable metadata
    ├── analysis/                   # Optional
    ├── materials/                  # Optional
    └── results/                    # Optional

### Key Components

#### dataset_description.json

This JSON file contains metadata about your dataset using Schema.org
vocabulary:

``` json
{
  "@context": "https://schema.org/",
  "@type": "Dataset",
  "name": "Visual Attention Experiment 2024",
  "description": "A study examining visual attention in adults",
  "author": [
    {
      "@type": "Person",
      "givenName": "Jane",
      "familyName": "Smith"
    }
  ],
  "variableMeasured": [
    {
      "@type": "PropertyValue",
      "name": "participant_id",
      "description": "Unique participant identifier"
    }
  ]
}
```

#### File Naming Conventions

Psych-DS uses keyword-based filenames to describe data files:

| Keyword | Description              | Example           |
|---------|--------------------------|-------------------|
| study   | Study or experiment name | `study-attention` |
| session | Session number           | `session-1`       |
| subject | Participant identifier   | `subject-001`     |
| task    | Task name                | `task-stroop`     |
| run     | Run number               | `run-01`          |

Example: `study-attention_session-1_task-stroop_data.csv`

## Next Steps

- Read the [Shiny App
  Guide](https://psych-ds.github.io/psychds-r/articles/shiny-app-guide.md)
  for detailed usage instructions
- Learn about [Creating Data
  Dictionaries](https://psych-ds.github.io/psychds-r/articles/data-dictionaries.md)
- See the [Psych-DS documentation](https://psych-ds.github.io/) for the
  full specification

## Getting Help

If you encounter issues:

1.  Search [existing
    issues](https://github.com/psych-ds/psychds-r/issues)
2.  Open a [new issue](https://github.com/psych-ds/psychds-r/issues/new)
    with a reproducible example
