# Validate a Psych-DS Dataset

Validates a dataset directory against the full 'Psych-DS' specification
using the same validator used by the 'psychds-validator' command-line
tool.

## Usage

``` r
validate_dataset(dir_path, json = FALSE, verbose = FALSE, use_events = FALSE)
```

## Arguments

- dir_path:

  Path to the 'Psych-DS' dataset directory to validate.

- json:

  Logical. If `TRUE`, output results as JSON instead of formatted text.
  Default is `FALSE`.

- verbose:

  Logical. If `TRUE`, show verbose output including warnings. Default is
  `FALSE`.

- use_events:

  Logical. If `TRUE`, show live checklist progress output. Default is
  `FALSE`.

## Value

Invisibly returns the exit status. Called for its side effect of
printing validation results to the console.

## Details

Requires 'Node.js' to be installed and available on the system PATH.
Install 'Node.js' from <https://nodejs.org>.

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard text output
validate_dataset("path/to/my-study")

# JSON output for programmatic use
validate_dataset("path/to/my-study", json = TRUE)

# Verbose with warnings
validate_dataset("path/to/my-study", verbose = TRUE)

# Live checklist
validate_dataset("path/to/my-study", use_events = TRUE)
} # }
```
