# psychds <a href="https://psych-ds.github.io/psychds-r/"><img src="man/figures/logo.png" align="right" height="139" alt="psychds website" /></a>
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/psychds)](https://CRAN.R-project.org/package=psychds)
[![R-CMD-check](https://github.com/psych-ds/psychds-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/psych-ds/psychds-r/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview
**psychds** provides tools for creating, editing, and validating psychological research datasets following the [Psych-DS standard](https://psych-ds.github.io/). The package includes:

- An interactive **Shiny application** for building Psych-DS compliant datasets from existing directories
- Functions for **validating** existing datasets against the standard
- Tools for generating **data dictionaries** with professional formatting
- **OSF integration** for publishing validated datasets

## Installation

### From CRAN (not currently available)

```r
install.packages("psychds")
```

### Development version from GitHub

```r
# install.packages("remotes")
remotes::install_github("psych-ds/psychds-r", dependencies = TRUE)
```

## Quick Start

### Launch the Shiny Application

The easiest way to use psychds is through the interactive Shiny app:

```r
library(psychds)
run_psych_ds_app()
```

This opens a guided interface for:

1. Selecting your project directory and data files
2. Adding dataset metadata (name, description, authors)
3. Standardizing filenames with Psych-DS keywords
4. Generating data dictionaries
5. Validating your dataset
6. Exploring your dataset
7. Optionally uploading to OSF

### Check Dependencies

Before running the app, verify that all required packages are installed:

```r
check_psychds_deps(detailed = TRUE)
```

## Features

### 📁 Dataset Creation
Build Psych-DS compliant datasets with a step-by-step wizard:

- Select and organize data files
- Auto-detect CSV column structures
- Apply standardized naming conventions
- Generate `dataset_description.json` metadata

### 📋 Data Dictionaries
Create professional, human-readable data dictionaries:

- Document all variables with descriptions, types, and constraints
- Include categorical value definitions
- Generate professional readable dictionaries in HTML/PDF

### ✅ Validation
Validate datasets against the Psych-DS specification:

- Check required files and structure
- Validate JSON metadata against schemas
- Verify data file integrity
- Get detailed error messages and suggestions

### ☁️ OSF Integration
Upload validated datasets directly to the Open Science Framework:

- Authenticate with personal access token
- Create new projects or add to existing ones
- Preserve folder structure
- Auto-generate README files

## The Psych-DS Standard

Psych-DS (Psychological Dataset Standard) provides a specification for organizing behavioral and psychological research data. Key components include:

- **`dataset_description.json`**: Metadata about the dataset using Schema.org vocabulary
- **`data/`**: Directory containing data files with standardized names
- **Keyword-based filenames**: e.g., `study-attention_session-1_data.csv`
- **Data dictionaries**: Documentation of all variables

Learn more at [psych-ds.github.io](https://psych-ds.github.io/)

## Requirements

- R >= 4.0.0
- Required packages: shiny, shinydashboard, shinyjs, shinyFiles, DT, jsonlite
- Optional: sortable, zip, pointblank (for full functionality)

## Getting Help

- 📖 [Package documentation](https://psych-ds.github.io/psychds-r/)
- 🐛 [Report bugs](https://github.com/psych-ds/psychds-r/issues)
- 💬 [Discussions](https://github.com/psych-ds/psychds-r/discussions)
- 📧 [Contact the Psych-DS team](mailto:bleonard@mit.edu)

## Contributing

Contributions are welcome! Please read our [Contributing Guide](CONTRIBUTING.md) for details on:

- Reporting bugs
- Suggesting features
- Submitting pull requests

## Citation

If you use psychds in your research, please cite:

```bibtex
@software{psychds,
  title = {psychds: Tools for Creating and Validating Psych-DS Datasets},
  author = {{Psych-DS Development Team}},
  year = {2024},
  url = {https://github.com/psych-ds/psychds-r}
}
```

## License

MIT © Psych-DS Development Team

## Related Projects

- [Psych-DS Docs](https://psych-ds.github.io/)
- [Psych-DS Validator (JavaScript)](https://github.com/psych-ds/psychds-validator)
- [Psych-DS Schema](https://github.com/psych-ds/psych-ds)
