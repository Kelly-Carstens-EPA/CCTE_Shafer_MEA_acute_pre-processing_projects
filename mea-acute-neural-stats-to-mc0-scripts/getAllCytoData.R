# new cytotox data collection scripts
# this is better because it is more robust to slight changes in the placement of the data in the sheet
# which is particularly important now that I need down to row H for LDH data

getAllCytoData <- function(dataset_title) {
  
  cat("\n\nLoad Cytotoxicity Data:\n")
  
  # only need to specifiy files_log if you want to use a specific files_log
  # instead of just the most rect calculations files log
  calc_files <- read_files(dataset_title = dataset_title, files_type = "calculations")
  
  cat("\nReading data from files...\n")
  
  cytodat <- list()
  for (i in 1:length(calc_files)) {
    cat("\n",basename(calc_files[i]),"\n",sep="")
    add.dat <- getFileCytoData(calc_files[i])
    cytodat <- rbind(cytodat, add.dat)
    rm(add.dat)
  }
  
  # set rvals to numeric
  cytodat[, rval := as.numeric(rval)]
  
  # Set any negative values to 0
  negative_rvals <- cytodat[rval < 0, c(rval)]
  if(length(negative_rvals) > 0) {
    cat(paste0("some values are negative (",signif(min(negative_rvals),5)," - ",signif(max(negative_rvals),5),"):\n"))
    print(cytodat[, .N, by = .(acnm, rval_is_neg = rval < 0)])
    cat(paste0("These will be set to 0\n"))
    cytodat[rval < 0, rval := 0.0]
  }
  
  # check for NA values, in any field
  res <- unlist(cytodat[, lapply(.SD, function(coli) sum(is.na(coli))), .SDcols = names(cytodat)])
  col.sums.with.nas <- which(res != 0)
  if (length(col.sums.with.nas) > 0) {
    cat('The following columns in cytodat have NAs:',paste0(names(col.sums.with.nas), collapse = ", "),
        '\nConfirm data values in sheet and that values were read in correctly.')
  }

  cat("\ncytodat is ready\n")
  return(cytodat)
}