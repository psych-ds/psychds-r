# Creating Data Dictionaries

## What is a Data Dictionary?

A data dictionary is a document that describes all variables in your
dataset. It serves as essential documentation for:

- **Yourself** - Remember what variables mean months later
- **Collaborators** - Help others understand your data
- **Reproducibility** - Enable others to reuse your data correctly
- **Compliance** - Meet requirements for data sharing mandates

## Using the Shiny App

The easiest way to create a data dictionary is through the psychds Shiny
app:

``` r
library(psychds)
run_psych_ds_app()
```

Navigate to **“Update Data Dictionary”** in the sidebar.

### Step-by-Step Process

1.  **Load your dataset** - Select a Psych-DS dataset folder
2.  **Edit each variable** - Add descriptions, types, and constraints
3.  **Define categorical values** - Document allowed values and their
    meanings
4.  **Set missing value codes** - Define how missing data is coded
5.  **Generate the dictionary** - Save into metadata or export as HTML

See the [Shiny App
Guide](https://psych-ds.github.io/psychds-r/articles/shiny-app-guide.md)
for detailed instructions.

## Output Formats

### HTML (Default)

The HTML output features:

- Professional, modern styling
- Table of contents with navigation
- Print-optimized CSS

To save as PDF, open the HTML file in a browser and press `Ctrl+P` (or
`Cmd+P` on Mac).

## Best Practices

### Writing Good Descriptions

❌ **Bad:** “age” ✅ **Good:** “Participant age in years at the time of
initial assessment”

❌ **Bad:** “rt” ✅ **Good:** “Mean response time in milliseconds across
all correct trials in the Stroop task”

### Choosing Data Types

| Use        | When                                 |
|------------|--------------------------------------|
| `string`   | Text, IDs, categorical values        |
| `integer`  | Whole numbers (counts, ages)         |
| `number`   | Decimals (measurements, proportions) |
| `boolean`  | True/false values                    |
| `date`     | Dates (ISO 8601 format)              |
| `datetime` | Timestamps                           |

### Documenting Categorical Variables

Always define what each category means, even if it seems obvious:

| Value | Label             | Description                                  |
|-------|-------------------|----------------------------------------------|
| `1`   | Strongly Disagree | Likert rating indicating strong disagreement |
| `2`   | Disagree          | Likert rating indicating disagreement        |
| `3`   | Neutral           | Likert rating indicating neutral response    |
| `4`   | Agree             | Likert rating indicating strong agreement    |
| `5`   | Strongly Agree    | Likert rating indicating strong agreement    |

### Missing Values

Document all codes that represent missing data:

| Code   | Common Meaning                 |
|--------|--------------------------------|
| `NA`   | Not Available (R default)      |
| `N/A`  | Not Applicable                 |
| `-999` | Common numeric missing code    |
| `.`    | SAS/Stata missing              |
| `""`   | Empty string                   |
| `-1`   | Sometimes used for “not asked” |

## Integration with Psych-DS

The data dictionary complements the `dataset_description.json` metadata.
While `dataset_description.json` contains machine-readable metadata, the
data dictionary provides human-readable documentation.

Both should be included in your Psych-DS dataset:

    my_dataset/
    ├── dataset_description.json    # Machine-readable
    ├── data_dictionary.html        # Human-readable
    ├── README.md
    └── data/
        └── study-exp1_data.csv
