read_acute_LDH_raw_data <- function(filei) {
  
  # Read in data
  dat <- suppressMessages(readxl::read_xls(filei, col_names = FALSE, 
                                        col_types = 'text')) # suppress message of renaming columns
  setDT(dat)
  setnames(dat, old = names(dat), new = paste0('column',1:ncol(dat)))
  
  # Assumptions:
  # * All plates are 96-well (8x12)
  # * For every plate, column 1, there is a non-NA entry that contains the plate info (e.g., "20201104_20201117_MW71-7104_TSCA Acute DR_Plate 1")
  # * For every plate, in column 2, there is a cell that contains the word "Temperature"
  # * Raw data values for each plate start 1 row below every occurrence of the word "Temperature" and continue for the next 7 rows
  # * Raw data values for all plates are in columns 3 - 14
  
  # Cycle through every plate, as indicated by non-NA entries in column1
  dat[, data_row_index := .I]
  plate.info.rows <- dat[!is.na(column1), c(data_row_index)]
  temperature.rows <- dat[grepl('Temperature',column2), c(data_row_index)]
  if(length(plate.info.rows) != length(temperature.rows)) {
    cat(basename(filei),' failed: ')
    cat('There is not 1 occurrence of the tag phrase "Temperature" for every apparent plate\n')
    return(data.table())
  }
  
  dat.all.plates <- data.table()
  
  for (i in 1:length(plate.info.rows)) {
    
    plate.info.rowi <- plate.info.rows[i]
    temperature.rowi <- temperature.rows[i]
    
    # Get subset of data corresponding to current plate
    dat.platei <- dat[c((temperature.rowi + 1) : (temperature.rowi + 1 + 7)), # rows
                      .SD, .SDcols = c(3:14)] # columns
    
    setnames(dat.platei, old = names(dat.platei), new = as.character(1:ncol(dat.platei)))
    dat.platei[, rowi := 1:8]
    dat.platei[, plate.info := dat[plate.info.rowi, c(column1)]]
    
    dat.all.plates <- rbind(dat.all.plates, dat.platei)
    
  }
  
  # Melt it
  dat.long <- melt(dat.all.plates, 
                   id.vars= c('plate.info','rowi'),
                   value.name = 'rval_not_blank_corrected',
                   variable.name = 'coli')
  
  # Check no values are NA or missing
  dat.long[, rval_not_blank_corrected := as.numeric(rval_not_blank_corrected)]
  if(nrow(dat.long[is.na(rval_not_blank_corrected)]) != 0) {
    cat(basename(filei),' failed: ')
    cat('Some raw values are NA or not numeric\n')
    return(data.table())
  }
  
  # Check no raw values > 10 (This would indicate that the column # may have been read in as a data value,
  # due to unexpected data alignment in sheet. Usual raw values are < 3)
  if(nrow(dat.long[rval_not_blank_corrected > 10]) != 0) {
    cat(basename(filei),' failed: ')
    cat('Some raw values are likely column #s, indicating unexpected data alignment\n')
    return(data.table())
  }
  
  # Add desired columns
  dat.long[, filename := basename(filei)]
  dat.long[, acsn := 'LDH']
  
  return(dat.long)
}
