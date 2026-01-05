# Luminex Preprocess

This repository contains an R function to process Luminex Excel files (e.g. from ProcartaPlex kit) into a tidy long-format dataset suitable for downstream analysis.

The function extracts both replicate and replicate-average measurements, optionally trims analyte names using a regex, and combines data from multiple files and sheets.

------------------------------------------------------------------------

## Requirements

-   R \>= 4.0
-   R packages:
    -   `readxl`
    -   `dplyr`
    -   `tidyr`
    -   `purrr`
    -   `stringr`

You can install missing packages with:

``` r
install.packages(c("readxl", "dplyr", "tidyr", "purrr", "stringr"))
```

------------------------------------------------------------------------

## Installation

Clone this repo:

``` bash
git clone git@github.com:DGreenwd/luminex_preprocess.git
```

Source the main function in R:

``` r
source("luminex_excel_processor/bin/read_luminex_excel.R")
```

------------------------------------------------------------------------

## Usage Example

``` r
# List all Excel files
files <- list.files("path/to/excel/files", full.names = TRUE)

# Optional regex to trim analyte names
trim_regex <- "\\s\\([0-9]+\\)"

# Read and process the Excel files
df <- read_luminex_excel(
  path_to_files = files,
  trim_regex = trim_regex,
  exclude_sheets = c("Standard Curve", "QC Summary")
)

# Preview the output
head(df)
```

The resulting df will have the following columns:

-   Type -- sample or standard type
-   Well -- well identifier
-   Analyte -- analyte name (trimmed if trim_regex is provided)
-   Value -- measurement value
-   Set -- "average" or "replicate"
-   Measure -- original worksheet name
