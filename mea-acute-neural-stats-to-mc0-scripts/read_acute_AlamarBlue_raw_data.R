read_acute_AlamarBlue_raw_data <- function(filei) {
  
  dat <- suppressMessages(readxl::read_xls(filei, col_names = FALSE, 
                                           col_types = 'text')) # suppress message of renaming columns
  # alternatively, could force to read from range = 'A12:M20',
  # but this is less flexible for variable formatting
  setDT(dat)
  setnames(dat, old = names(dat), new = c('row_char',1:(ncol(dat)-1)))
  
  # Subset to data rows starting at first occurrence of the letter "A" in first column
  # through next 7 rows
  row_index_of_letter_A <- which(dat$row_char %in% 'A')[1]
  dat <- dat[(row_index_of_letter_A) : (row_index_of_letter_A + 7)]
  
  # Check that all rows and columns are present
  
  # check all and only columns 1-12 present
  if(length(setdiff(1:12, names(dat))) != 0) {
    cat(basename(filei),' failed: ')
    cat('columns 1-12 not present\n')
    return(data.table())
  }
  if(ncol(dat) > 13) {
    cat(basename(filei),' failed: ')
    cat('extra columns are present\n')
    return(data.table())
  }
  
  # check rows A-H present
  if(length(setdiff(LETTERS[1:8], dat$row_char)) != 0) {
    cat(basename(filei),' failed: ')
    cat('rows A-H not present\n')
    return(data.table())
  }
  
  # Melt the data
  dat.long <- melt(dat, 
                   id.vars = 'row_char',
                   value.name = 'rval_not_blank_corrected',
                   variable.name = 'coli')
  
  # Check no values are NA or missing
  dat.long[, rval_not_blank_corrected := as.numeric(rval_not_blank_corrected)]
  if(nrow(dat.long[is.na(rval_not_blank_corrected)]) != 0) {
    cat(basename(filei),' failed: ')
    cat('Some raw values are NA or not numeric\n')
    return(data.table())
  }
  
  # Check no rvals <= 12 (would indicate that the column # may have been read in as a data value,
  # due to unexpected data alignment in sheet, because usual values are > 300)
  if(nrow(dat.long[rval_not_blank_corrected <= 12]) != 0) {
    cat(basename(filei),' failed: ')
    cat('Some raw values are likely column #s, indicating unexpected data alignment\n')
    return(data.table())
  }
  
  # Add desired columns
  dat.long[, filename := basename(filei)]
  dat.long[, acsn := 'AB']
  dat.long[, rowi := match(row_char, LETTERS)]
  
  return(dat.long)
}