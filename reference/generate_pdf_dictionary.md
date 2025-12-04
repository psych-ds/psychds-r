# Generate Human-Readable PDF Data Dictionary

Creates a professional PDF document from the data dictionary metadata
using either rmarkdown (preferred) or a simpler HTML-to-PDF approach

## Usage

``` r
generate_pdf_dictionary(
  dictionary_data,
  dataset_info = NULL,
  output_file = NULL,
  include_stats = TRUE
)
```

## Arguments

- dictionary_data:

  List containing the data dictionary information

- dataset_info:

  List containing dataset metadata (name, description, etc.)

- output_file:

  Character path for the output PDF file

- include_stats:

  Logical whether to include variable statistics if available

## Value

Path to the generated PDF file
