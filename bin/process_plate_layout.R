process_plate_layout <- function(path,
                                 valid_rows = LETTERS[1:8],
                                 valid_columns = 1:12,
                                 expected_wells = 96) {
  
  require(readxl)
  require(dplyr)
  require(readr)
  require(tidyr)
  require(stringr)

  raw <- read_xlsx(path,
                   .name_repair = "unique_quiet",
                   col_names = FALSE)
  
  colnames(raw) = paste0("X",seq(from=1,to = ncol(raw)))
  
  #browser()
  
  long <- raw %>%
    # Get plate number as new column and fill down  
    mutate(
      plate = if_else(
        str_detect(X1, "^Plate"),
        X1,
        NA_character_
      )
    ) %>%
    fill(plate,.direction = "down") %>%
    
    # Now remove plate header row
    filter(
      !str_detect(X1, "^Plate"),
      !is.na(X1),
      X1 != "Position"
    ) %>%
    
    # X1 is the row
    rename(row = X1) %>%
    
    # Pivot longer so that the 
    pivot_longer(
      cols = -c(plate, row),
      names_to = "column",
      values_to = "sample_id"
    ) %>%
    # Count unique elements in "column" column 
    group_by(row,plate) %>% 
    # First extract number from column (X2...X12), and order 
    mutate(
      column = parse_number(column),
    ) %>% 
    arrange(column) %>% 
    # Replace with column count
    mutate(column = row_number()) %>% 
    ungroup() %>% 
    # Add well number 
    mutate(
      well   = paste0(row, column)
    )
  
  # Check plate layout 
  if(
    validate_plate_layout(
    long,
    valid_rows      = valid_rows,
    valid_columns   = valid_columns,
    expected_wells  = expected_wells
    ) == T){
      long %>%
    filter(!is.na(sample_id)) %>%
    select(
      plate_id = plate,
      row,
      column,
      well,
      sample_id
    ) %>%
    as_tibble()
  }


}


validate_plate_layout <- function(df,
                                  valid_rows = LETTERS[1:8],
                                  valid_columns = 1:12,
                                  expected_wells = 96) {
  
  
  require(glue)
  require(tidyr)
  require(dplyr)
  
  ##############################
  # Validate row identifiers
  ##############################

  ## Rows should have fixed-position labels (A–H for 96-well plates)
  bad_rows <- df %>%
    distinct(row) %>%
    filter(!row %in% valid_rows)

  if (nrow(bad_rows) > 0) {
    abort(glue(
      "Invalid row identifiers detected: {paste(bad_rows$row, collapse = ', ')}"
    ))
  }

  ##############################
  # Validate column identifiers
  # Columns should be integers (1–12 for 96-well plates)
  ##############################
  
  bad_columns <- df %>%
    distinct(column) %>%
    filter(!column %in% valid_columns)

  if (nrow(bad_columns) > 0) {
    abort(glue(
      "Invalid column identifiers detected: {paste(bad_columns$column, collapse = ', ')}"
    ))
  }

  ##############################
  # Validate populated well count per plate
  # We validate *after* pivoting so sparsity or duplication is detected
  ##############################

  bad_plates <- df %>%
    filter(!is.na(sample_id)) %>%
    count(plate, name = "n_wells") %>%
    filter(n_wells != expected_wells)

  if (nrow(bad_plates) > 0) {
    abort(glue(
      "Plates with incorrect well counts:\n{paste(bad_plates$plate, bad_plates$n_wells, sep = ': ', collapse = '\n')}"
    ))
  }

  # return TRUE so this can be used programmatically if desired
  invisible(TRUE)
}
