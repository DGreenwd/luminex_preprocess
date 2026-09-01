read_luminex_excel <- function(
    path_to_files, ## Character vector of strings with length N files 
    trim_regex = NULL,   ## Regex of strings to remove from analyte names  # .e.g all analyte names may have suffix " (##)" 
    exclude_sheets = "Standard Curve", ## Character vector with sheets to be excluded
    include_filename = T, # Logical, whether the file name from path_to_files is appended as a column 
    plate_metadata = c("Plate ID", "Acquisition Date"), ## Character vector with information in header about plate to be included as additional fields
    verbose = T,
    technical_replicates = T
) {
  
  ##############################
  # Argument validation
  ##############################
  
  if (!is.character(path_to_files) || length(path_to_files) == 0) {
    stop("`path_to_files` must be a non-empty character vector of file paths.")
  }
  
  if (any(grepl("^https",path_to_files))){
    cat("File path(s) appear to be URLs.\n")
    url_fls = T
  } else {
    url_fls = F
    if (!all(file.exists(path_to_files))) {
      stop(
        "The following files do not exist:\n",
        paste(path_to_files[!file.exists(path_to_files)], collapse = "\n")
      )
    }
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
  
  # URL temporary files 
  if(url_fls){
    
    
    # Generate one tempfile per URL
    temp_files <- sapply(path_to_files, function(x) tempfile(fileext = ".xlsx"))
  
    # Download each file
    for (i in seq_along(path_to_files)) {
      download.file(path_to_files[i], destfile = temp_files[i], mode = "wb",quiet = T)
    }
    
    # Store original filenames and temp file names 
    path_df = data.frame(filename = temp_files,
                         original_name = path_to_files)
    
    # For consistent functionality, overwrite path_to_files with temp_file paths 
    path_to_files = temp_files 
    
  }
  
  ##############################
  # Check sheet name consistency
  ##############################
  
  sheets <- readxl::excel_sheets(path_to_files[1])
  
  # Exclude sheets
  sheets <- setdiff(sheets, exclude_sheets)
  
  if (!all(sapply(path_to_files[-1], function(x) {
    all(setdiff(readxl::excel_sheets(x),exclude_sheets) %in% sheets)
  }))) {
    
    bad_sheets <- lapply(path_to_files[-1], function(f) { setdiff(readxl::excel_sheets(f), sheets) }) 
    names(bad_sheets) = path_to_files[-1]
    bad_sheets <- Filter(length, bad_sheets) # drop empty entries
    
    
    # stop(
    #   "Inconsistent worksheet structure across input Excel files. ",
    #   "All files are expected to contain the same sheet names as the first file.\n",
    #   "Issues:",
    #   str(bad_sheets)
    # )
    stop(
      paste0(
        "Inconsistent worksheet structure across input Excel files.\n",
        "All files are expected to contain the same sheet names as the first file.\n",
        "Consider adding these sheets to argument `exclude_sheets`:\n",
        paste(sprintf(
          "%s: %s",
          names(bad_sheets),
          vapply(
            bad_sheets,
            paste,
            collapse = ", ",
            FUN.VALUE = character(1)
          )
        ), collapse = "\n"),
        sep = ""
      )
    )
    
  }
  
  if (length(sheets) == 0) {
    stop(
      "After excluding sheets (",
      paste(exclude_sheets, collapse = ", "),
      "), no worksheets remain to process."
    )
  }

  
  ##############################
  # Extract data across files and sheets
  ##############################
  
  ### Apply the extraction to the cartesian product of files and sheet names (all sheets for all files)
  
  output <- 
    tidyr::expand_grid(file = path_to_files, sheet = sheets) %>%
    mutate(data = map2(file, sheet, function(file, sheet) {
      
      if(verbose){
         print(paste0("File: ",file," ",
                   "Sheet: ", sheet))
      }
      
      if(technical_replicates){
        head_tail <-
          identify_header_and_tail(path = file,
                                   sheet = sheet,
                                   technical_replicates = technical_replicates)
        header_rows <- head_tail$header_rows
        tail_rows <- head_tail$tail_rows
        
        y <- readxl::read_xlsx(
          .name_repair = "unique_quiet",
          path = file,
          sheet = sheet,
          col_names = FALSE
        )
        
        analyte_names <- unlist(y[header_rows[1] - 1,])
        
        y_average <- y[header_rows[1]:tail_rows[1],]
        y_reps    <- y[header_rows[2]:tail_rows[2],]
        
        y_average[1, which(!is.na(analyte_names))] <-
          as.list(analyte_names[!is.na(analyte_names)])
        
        y_reps[1, which(!is.na(analyte_names))] <-
          as.list(analyte_names[!is.na(analyte_names)])
        
        y_average <- y_average %>%
          rename_with( ~ as.character(y_average[1,])) %>%
          slice(-1)
        
        y_reps <- y_reps %>%
          rename_with( ~ as.character(y_reps[1,])) %>%
          slice(-1)
        
        y_average <- y_average %>%
          pivot_longer(!c("Type", "Well"),
                       names_to = "Analyte",
                       values_to = "Value") %>%
          mutate(Set = "average", Measure = sheet)
        
        y_reps <- y_reps %>%
          pivot_longer(!c("Type", "Well"),
                       names_to = "Analyte",
                       values_to = "Value") %>%
          mutate(Set = "replicate", Measure = sheet)
        
        y_out <- bind_rows(y_average, y_reps)
        

      }
      
      if(technical_replicates == F){
        head_tail <-
          identify_header_and_tail(path = file,
                                   sheet = sheet,
                                   technical_replicates = technical_replicates)
        header_rows <- head_tail$header_rows
        tail_rows <- head_tail$tail_rows
        
        y <- readxl::read_xlsx(
          .name_repair = "unique_quiet",
          path = file,
          sheet = sheet,
          col_names = FALSE
        )
        
        analyte_names <- unlist(y[header_rows[1] - 1,])
        
        y_average <- y[header_rows[1]:tail_rows[1],]
        
        y_average[1, which(!is.na(analyte_names))] <-
          as.list(analyte_names[!is.na(analyte_names)])
        
        y_average <- y_average %>%
          rename_with( ~ as.character(y_average[1,])) %>%
          slice(-1)
        
        y_average <- y_average %>%
          pivot_longer(!c("Type", "Well"),
                       names_to = "Analyte",
                       values_to = "Value") %>%
          mutate(Set = "average", Measure = sheet)
        
        y_out <- y_average
        
        
      }
      

      if (!is.null(trim_regex)) {
        y_out <- y_out %>%
          mutate(Analyte = stringr::str_remove_all(Analyte, trim_regex))
      }
      
      if(include_filename){
        y_out <- y_out %>% 
          mutate(Filename = file)
      }
    
      
      if (!is.null(plate_metadata)) {
        
        # Extract non-NA  before the first header row
        plate_data_lines <- y[2:(header_rows[1] - 1), 1] %>%
          unlist() %>%
          discard(is.na)
        
        # Split into key/value pairs
        kv <- str_split_fixed(plate_data_lines, ":", n = 2)
        
        # Trim values and create named vector
        plate_data <- str_trim(kv[, 2]) %>% set_names(kv[, 1])

        if (length(intersect(plate_metadata,names(plate_data))) == 0) {
          stop(
            "Plate metadata (",
            paste(setdiff(plate_metadata,names(plate_data)), collapse = ", "),
            ") not identified in header."
          )
        } else {
          y_out <- y_out %>% bind_cols(as.list(plate_data[plate_metadata]))
        }

      }
      
      
      y_out
    })) %>%
    select(data) %>%
    unnest(data)
  
  # Lower case columns
  output <- output %>% rename_with(~ gsub("\\s+", "_", tolower(.x)))
  
  # If files are accessed from url
  if(url_fls){
    # unlink temporary files created
    unlink(temp_files)
    
    output <- output %>% 
      left_join(.,path_df,by="filename") %>% 
      mutate(filename=original_name) %>% 
      select(-original_name)
  }
  
  ##############################
  # Data checks 
  ##############################
  
  # Check for failure to parse plate_id
  tmp <- output %>%
    group_by(filename) %>%
    filter(any(is.na(plate_id) | plate_id == "")) %>%
    select(filename, plate_id) %>%
    distinct()
  
  if (nrow(tmp) > 0) {
    warning(
      paste0(
        "Invalid plate_id produced:\n",
        paste(
          paste0(
            "  ", tmp$filename,
            " -> '", ifelse(is.na(tmp$plate_id), "NA", tmp$plate_id), "'"
          ),
          collapse = "\n"
        )
      )
    )
  }
  
  output
}

