# cran-comments.md

## Test environments

* local: macOS 26.6.2 (Apple Silicon), R 4.4.2
* win-builder: R-devel (2026-09-20 r90574 ucrt), x86_64-w64-mingw32

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE

  New submission.

  Possibly misspelled words in DESCRIPTION: validator -- 'validator' is
  spelled correctly; it refers to the bundled 'Psych-DS' validation tool.

## Comments

* psychds provides an interactive 'Shiny' application and programmatic tools
  for creating and validating datasets that follow the Psych-DS standard
  (<https://psych-ds.github.io/>).
* The package bundles pre-built 'JavaScript' validators (inst/node and
  inst/shiny/www/js) so that dataset validation runs entirely locally, with
  no network access. Sources and licenses for the bundled libraries are
  documented in inst/COPYRIGHTS, referenced from the DESCRIPTION Copyright
  field. On some platforms this produces an installed-size NOTE (5.7 Mb in
  local checks).
* Examples for run_psych_ds_app() and run_app_safe() are wrapped in
  \dontrun{} because they launch a blocking 'Shiny' application; examples
  for validate_dataset() are wrapped in \dontrun{} because the function
  requires an external 'Node.js' installation (declared in
  SystemRequirements). Examples for check_psychds_deps() run
  unconditionally.
* No examples, tests, or vignettes write to the user's home filesystem or
  access the network; the application writes only to user-chosen locations
  at runtime.
