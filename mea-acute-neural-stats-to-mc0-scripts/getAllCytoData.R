# new cytotox data collection scripts
# this is better because it is more robust to slight changes in the placement of the data in the sheet
# which is particularly important now that I need down to row H for LDH data

getAllCytoData <- function(main.output.dir, dataset_title, files_log = "") {
  
  cat("\n\nLoad Cytotoxicity Data:\n")
  
  # only need to specifiy files_log if you want to use a specific files_log
  # instead of just the most rect calculations files log
  calc_files <- read_files(check.dir = main.output.dir, files_log = files_log, files_type = "calculations")
  
  cat("\nReading data from files...\n")
  
  cytodat <- list()
  for (i in 1:length(calc_files)) {
    cat("\n",basename(calc_files[i]),"\n",sep="")
    add.dat <- getFileCytoData(calc_files[i])
    cytodat <- rbind(cytodat, add.dat)
    rm(add.dat)
  }
  
  cytodat[, "files_log" := basename(files_log)]
  
  # # save the data as .RData
  # filename <- file.path(main.output.dir, paste0("output/",dataset_title,"_cytodat_",as.character.Date(Sys.Date()),".RData"))
  # save(cytodat, file = filename)
  # rm(cytodat)
  # cat("\n\nRData is saved:",filename)
  
  # check for/summarize NA values
  na_indicies <- which(is.na(cytodat), arr.ind = TRUE)
  if (length(na_indicies) == 0) {
    cat("There are no NA values in cytodat.\n")
  }
  else {
    cat("There are some NA values in cytodat:\n")
    print(cytodat[na_indicies[, "row"], .SD, .SDcols = setdiff(names(cytodat),"files_log")])
  }
  
  cat("\ncytodat is ready\n")
  return(cytodat)
}