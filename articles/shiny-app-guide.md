# Using the Shiny Application

## Overview

The psychds Shiny application provides a guided, step-by-step interface
for creating Psych-DS compliant datasets. This guide walks through each
section of the application.

## Launching the App

``` r
library(psychds)
run_psych_ds_app()
```

## Application Sections

The app has four main sections accessible from the sidebar:

1.  **Create Dataset** - Build a new Psych-DS dataset
2.  **Update Data Dictionary** - Edit variable definitions
3.  **Validate Dataset** - Check compliance with the standard
4.  **Upload to OSF** - Publish to Open Science Framework

------------------------------------------------------------------------

## Create Dataset

The dataset creation wizard has three steps:

### Step 1: Project Directory and Data Files

In this step, you:

1.  **Select a project directory** - This is the folder containing your
    research materials. We will be building a new dataset using the
    files you select from this directory.

2.  **Choose data files** - Browse and select CSV files to include.
    Click on files/directories to select/deselect them.

3.  **Configure optional directories** - Choose additional folders to
    create:

    - `analysis/` - Analysis scripts and pipelines
    - `materials/` - Study materials (stimuli, protocols, flyers)
    - `results/` - Output figures and tables
    - `products/` - Papers, presentations
    - `documentation/` - Additional documentation

**Your original files are safe!** The app only reads from your existing
files to create standardized copies. Your original data is never
modified.

### Step 2: Dataset Metadata

Enter information about your dataset:

**Required fields:**

- **Name** - A descriptive name for your dataset
- **Description** - Brief description of what the data contains

**Author information:**

- Add authors with their names and optional ORCID iDs
- Click “Add New Author” to add multiple authors

**Detected Variables:**

The app automatically detects column names from your CSV files.
Variables with the same name across files are assumed to have identical
definitions.

### Step 3: Standardize Filenames

Rename your data files using Psych-DS naming conventions:

1.  **Select files** - Check boxes to select multiple files for batch
    operations

2.  **Configure keywords** - For each file, set values for standard
    keywords:

    - study, session, subject, task, etc.

3.  **Auto-Name from Data** - Automatically fill keyword values from
    constant columns in your data (for instance, if each file contains a
    column like “subject_id” with the same value in every row, you can
    use this column to name the file)

4.  **Preview** - See the new filename as you configure keywords

**Example transformation:**

    Original: experiment_data_2024.csv
    New:      study-attention_session-1_data.csv

### Saving Your Dataset

After Step 3, you’ll be prompted to:

1.  Choose a name for your dataset folder
2.  Select where to save it
3.  Review the file structure preview
4.  Click “Create Dataset” to generate the Psych-DS compliant dataset

------------------------------------------------------------------------

## Update Data Dictionary

The data dictionary editor allows you to document all variables in your
dataset.

### Loading a Dataset

1.  Click “Load Dataset”
2.  Navigate to your Psych-DS dataset folder
3.  The app will detect all CSV files and extract variable names

### Editing Variables

Select a variable from the list to edit its properties:

**Basic Properties:**

- **Description** - What this variable represents
- **Data Type** - string, integer, number, boolean, date, etc.
- **Unit** - Measurement unit (if applicable)

**Constraints:**

- **Required** - Whether the variable must have a value
- **Unique** - Whether all values must be unique
- **Min/Max** - Valid range for numeric variables
- **Pattern** - Regex pattern for string validation

**Categorical Values:**

For variables with a fixed set of allowed values:

1.  Click “Add Value”
2.  Enter the value, label, and description
3.  Repeat for all categories

Example for a “condition” variable: \| Value \| Label \| Description \|
\|——-\|——-\|————-\| \| control \| Control Group \| No intervention \| \|
treatment \| Treatment Group \| Received intervention \|

### Global Missing Value Codes

Define codes that represent missing values across all variables:

- Common codes: `NA`, `N/A`, `-999`, `.`, `null`
- Add custom codes specific to your data

### Generating a Data Dictionary

Click “Generate Human-Readable Dictionary” to create a formatted HTML
document:

1.  Choose whether to include missing value codes
2.  Click “Generate Dictionary”

The HTML file is saved to your dataset folder and can be: - Opened in
any web browser - Printed to PDF using Ctrl+P / Cmd+P - Shared with
collaborators

------------------------------------------------------------------------

## Validate Dataset

Check your dataset against the Psych-DS specification:

### Running Validation

1.  Select your dataset directory
2.  Click “Validate”
3.  Review the results

### Validation Checks

The validator checks:

- ✅ Required files present (`dataset_description.json`)
- ✅ Valid JSON syntax and structure
- ✅ Required metadata fields populated
- ✅ Data files in correct location
- ✅ Filename conventions followed
- ✅ Schema.org vocabulary used correctly

### Understanding Results

Results are shown as a checklist with:

- **Pass** (green) - Check passed
- **Fail** (red) - Issue found that must be fixed

Click on any item to see details and suggestions for fixing issues.

------------------------------------------------------------------------

## Upload to OSF

Publish your validated dataset to the Open Science Framework:

### Authentication

1.  Go to [osf.io/settings/tokens](https://osf.io/settings/tokens)
2.  Generate a Personal Access Token with appropriate permissions
3.  Paste the token in the app
4.  Click “Test Connection” to verify

### Selecting Your Dataset

1.  Browse to your validated Psych-DS dataset
2.  Click “Verify Dataset” to confirm it’s ready for upload

### Upload Options

**Destination:** - Create a new OSF project - Add to an existing project

**Configuration:** - Preserve folder structure (recommended) -
Auto-generate README file - Set folder path on OSF

### Uploading

1.  Review the upload summary
2.  Click “Start Upload”
3.  Wait for completion
4.  Get a link to your OSF project

------------------------------------------------------------------------

## Troubleshooting

### App won’t start

``` r
# Check dependencies
check_psychds_deps(detailed = TRUE)

# Try forcing external browser
run_psych_ds_app(force_browser = TRUE)
```

### Validation errors

- Read the error message carefully
- Check that `dataset_description.json` exists and is valid JSON
- Ensure the `data/` folder exists with at least one data file

For more help, [open an
issue](https://github.com/psych-ds/psychds-r/issues).
