identify_header_and_tail <- function(path,
                                     sheet,
                                     technical_replicates = T,
                                     .name_repair = "unique_quiet",
                                     col_names = FALSE,
                                     cell_cols = c("A", "B")) {
  # Read only the first two columns (A and B by default)
  x <- readxl::read_xlsx(
    path = path,
    sheet = sheet,
    .name_repair = .name_repair,
    col_names = col_names,
    cell_cols(cell_cols)
  )
  
  # Identify header rows:rows where col1 == "Type" and col2 == "Well"
  header_rows <- which(x[[1]] == "Type" & x[[2]] == "Well")
  
  if (length(header_rows) != 2 & technical_replicates) {
    stop(
      paste0("Expected exactly two header rows identified by 'Type' and 'Well'. ",
             "Found ",
             length(header_rows),
             "."
      ))
  }
  
  
  # Identify NA indices in the first column and compute tail rows
  na_idx <- which(is.na(x[[1]]))
  
  tail_rows <- c(if (length(na_idx) >= 3)
    na_idx[3] - 1
    else
      NA_integer_,
    if (length(na_idx) >= 6)
      na_idx[6] - 1
    else
      NA_integer_)
  
  if (any(is.na(tail_rows)) & technical_replicates) {
    stop(
      "Failed to determine tail rows for data blocks. ",
      "Input worksheet structure may be malformed."
    )
  }
  
  return(list(header_rows = header_rows, tail_rows = tail_rows))
}
