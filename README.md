# psychds: Create, Validate, and Explore Psych-DS Datasets

## Overview

`psychds` is an R package that provides a Shiny application and toolkit for creating, improving, and validating psychological datasets following the [Psych-DS standard](https://psych-ds.github.io/). This package aims to make it easier for researchers to adopt the Psych-DS standard by providing a user-friendly interface for dataset creation and validation.

## Installation

You can install the development version of psychds from GitHub:

```r
# install.packages("devtools")
devtools::install_github("psych-ds/psychds-r", dependencies = TRUE, force = TRUE)
```

## Usage

To launch the Shiny application:

```r
library(psychds)
run_psych_ds_app()
```

## Features

The `psychds` package provides the following features:

- **Create Datasets**: Convert existing data to the Psych-DS format with a step-by-step interface. Includes options for developing comprehensive data dictionaries using the "variableMeasured" field of the metadata.
- **Validate Datasets**: Check if datasets comply with the Psych-DS standard
- **Update Data Dictionary**: Create and edit data dictionaries for your datasets
- **Dataset Explorer**: Explore the structure and content of Psych-DS datasets
- **Upload to OSF**: Upload Psych-DS datasets to the Open Science Framework (OSF)

## Psych-DS Standard

The Psych-DS standard is a data organization standard for psychological datasets. It defines a consistent structure for organizing research data, making it easier to share, understand, and reuse psychological datasets. For more information about the Psych-DS standard, visit the [Psych-DS website](https://psych-ds.github.io/).

## Contributing

Contributions to the `psychds` package are welcome! Please feel free to submit issues and pull requests.

## License

This project is licensed under the MIT License - see the LICENSE file for details.
