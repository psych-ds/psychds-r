# Check and Load Package Dependencies

This function provides robust package management following CRAN
policies. It checks versions, handles conflicts, and provides clear user
feedback.

## Usage

``` r
check_dependencies(
  min_versions = NULL,
  recommended_versions = NULL,
  startup_mode = getOption("psychds.startup_mode", "recommended")
)
```

## Arguments

- min_versions:

  Named list of minimum required package versions

- recommended_versions:

  Named list of recommended package versions

- startup_mode:

  Character: "strict", "recommended", or "minimal"

## Value

Logical indicating success
