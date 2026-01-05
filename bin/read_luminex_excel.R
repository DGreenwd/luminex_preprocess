read_luminex_excel <- function(
    path_to_files, ## Charachter vector of strings with length N files 
    trim_regex = NULL,   ## Regex of strings to remove from analyte names  # .e.g all analyte names may have suffix " (##)" 
    exclude_sheets = "Standard Curve" ## Charachter vector with sheets to be excluded
) {
  
  ##############################
  # Argument validation
  ##############################
  
  if (!is.character(path_to_files) || length(path_to_files) == 0) {
    stop("`path_to_files` must be a non-empty character vector of file paths.")
  }
  
  if (!all(file.exists(path_to_files))) {
    stop(
      "The following files do not exist:\n",
      paste(path_to_files[!file.exists(path_to_files)], collapse = "\n")
    )
  }
  
  ## Regex of strings to remove from analyte names 
  # .e.g all analyte names may have suffix " (##)" 
  if (!is.null(trim_regex) &&
      (!is.character(trim_regex) || length(trim_regex) != 1)) {
    stop("`trim_regex` must be NULL or a single character string.")
  }
  
  if (!is.character(exclude_sheets)) {
    stop("`exclude_sheets` must be a character vector.")
  }
  
  # Dependencies
  require(readxl)
  require(dplyr)
  require(tidyr)
  require(purrr)
  require(stringr)
  
  ##############################
  # Check sheet name consistency
  ##############################
  
  sheets <- readxl::excel_sheets(path_to_files[1])
  
  if (!all(sapply(path_to_files[-1], function(x) {
    all(readxl::excel_sheets(x) %in% sheets)
  }))) {
    stop(
      "Inconsistent worksheet structure across input Excel files. ",
      "All files are expected to contain the same sheet names as the first file."
    )
  }
  
  # Exclude sheets
  sheets <- setdiff(sheets, exclude_sheets)
  
  if (length(sheets) == 0) {
    stop(
      "After excluding sheets (",
      paste(exclude_sheets, collapse = ", "),
      "), no worksheets remain to process."
    )
  }
  
  ##############################
  # Identify header rows and data tail rows
  ##############################
  
  x <- readxl::read_xlsx(
    path_to_files[1],
    sheet = sheets[1],
    col_names = FALSE,
    cell_cols(c("A", "B"))
  )
  

  ## Identify row containing the header rows and tail rows
  ## There are two header rows, one for the replicate-averaged value and one for the replicate values.
  ## Each has values "Type" and "Well" in col 1 and 2 respectively 
  
  header_rows <- which(x[, 1] == "Type" & x[, 2] == "Well")
  
  if (length(header_rows) != 2) {
    stop(
      "Expected exactly two header rows identified by 'Type' and 'Well'. ",
      "Found ", length(header_rows), "."
    )
  }
  
  ##Tail rows (last rows to retain):
  ## for the replicate average values, the tail row corresponds to the third NA value in column 1, minus 1 row 
  ## for the replicate values, the tail row corresponds to the sixth NA value in column 1, minus 1 row 
  tail_rows <- c(
    x %>%
      pull(1) %>%
      is.na() %>%
      which() %>%
      nth(3) - 1,
    x %>%
      pull(1) %>%
      is.na() %>%
      which() %>%
      nth(6) - 1
  )
  
  if (any(is.na(tail_rows))) {
    stop(
      "Failed to determine tail rows for data blocks. ",
      "Input worksheet structure may be malformed."
    )
  }
  
  ##############################
  # Extract data across files and sheets
  ##############################
  
  ### Apply the extraction to the cartesian product of files and sheet names (all sheets for all files)
  
  output <- tidyr::expand_grid(file = path_to_files, sheet = sheets) %>%
    mutate(data = map2(file, sheet, function(file, sheet) {
      
      y <- readxl::read_xlsx(
        path = file,
        sheet = sheet,
        col_names = FALSE
      )
      
      analyte_names <- unlist(y[header_rows[1] - 1, ])
      
      y_average <- y[header_rows[1]:tail_rows[1], ]
      y_reps    <- y[header_rows[2]:tail_rows[2], ]
      
      y_average[1, which(!is.na(analyte_names))] <-
        as.list(analyte_names[!is.na(analyte_names)])
      
      y_reps[1, which(!is.na(analyte_names))] <-
        as.list(analyte_names[!is.na(analyte_names)])
      
      y_average <- y_average %>%
        rename_with(~ as.character(y_average[1, ])) %>%
        slice(-1)
      
      y_reps <- y_reps %>%
        rename_with(~ as.character(y_reps[1, ])) %>%
        slice(-1)
      
      y_average <- y_average %>%
        pivot_longer(
          !c("Type", "Well"),
          names_to = "Analyte",
          values_to = "Value"
        ) %>%
        mutate(Set = "average", Measure = sheet)
      
      y_reps <- y_reps %>%
        pivot_longer(
          !c("Type", "Well"),
          names_to = "Analyte",
          values_to = "Value"
        ) %>%
        mutate(Set = "replicate", Measure = sheet)
      
      y_out <- bind_rows(y_average, y_reps)
      
      if (!is.null(trim_regex)) {
        y_out <- y_out %>%
          mutate(Analyte = stringr::str_remove_all(Analyte, trim_regex))
      }
      
      y_out <- y_out %>% 
        mutate(Filename = file)
      
      y_out
    })) %>%
    select(data) %>%
    unnest(data)
  
  output
}

