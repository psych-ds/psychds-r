# Installation

## Quick Install (Most Users)
```r
# Install from GitHub with all dependencies
remotes::install_github("psych-ds/psychds-r", dependencies = TRUE)

# Load and run
library(psychds)
run_psych_ds_app()
```

## Full Setup (Recommended)
```r
# 1. Install the package
remotes::install_github("psych-ds/psychds-r", dependencies = TRUE)

# 2. Load the package
library(psychds)

# 3. Check dependencies
check_psychds_deps()

# 4. Set up PDF generation (optional)
setup_pdf_generation()

# 5. Run the app
run_psych_ds_app()
```

## Troubleshooting

If the app doesn't display properly in RStudio:
```r
run_psych_ds_app(force_browser = TRUE)
```