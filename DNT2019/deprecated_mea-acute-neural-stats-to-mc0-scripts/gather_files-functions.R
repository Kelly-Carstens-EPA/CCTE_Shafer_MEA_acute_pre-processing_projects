# script to gather the wanted mea acute files

selectInputFiles <- function(start.dir, output.dir, dataset_title = ""){
  
  # get starting folder, to initialize starting screen
  culture.dirs <- list.dirs(path = start.dir, recursive = F)
  
  file_names <- c()
  previousfolder <- culture.dirs[1]
  
  repeat {
    
    add.files <- choose.files(default = previousfolder)
    
    # loop breaks when user hits cancel
    if (length(add.files) == 0) {
      break
    }
    
    file_names <- c(file_names, add.files)
    previousfolder <- dirname(tail(add.files,n=1))
  }
  # just in case any files were selected twice
  file_names <- unique(file_names)
  
  writeLogFile(file_names, output.dir, dataset_title)
}

writeLogFile <- function(file_names, output.dir, dataset_title) {
  
  # create log file name
  log_file <- file.path(output.dir, paste0(dataset_title, "_files_log_",as.character.Date(Sys.Date()),".txt"))
  
  # create the log file
  sink(file = log_file, append = F)
  cat(paste0(dataset_title," files used for MEA acute pre-processing for TCPL\n"))
  cat("Created with the script gather_files-functions.R\n")
  cat("Date ran: ")
  cat(as.character.Date(Sys.time()))
  cat("\nEvery line ending in '.csv' or '.xlsx' or '.xls' will be read as an input file")
  
  all_dirs <- unique(dirname(file_names))
  
  cat(paste0("\n\nCollected ",length(file_names)," files from ",length(all_dirs)," directories."))
  
  for (diri in all_dirs) {
    cat("\n\n",diri,"\n",sep = "")
    diri_files <- file_names[dirname(file_names) == diri]
    cat(diri_files, sep = "\n")
  }
  
  closeAllConnections()
  
  print(paste0(log_file," is ready."))
  
}


read_files <- function(check.dir = "", log_file = "") {
  
  if (log_file == "") {
    # read the data from the most recent log_file in check.dir
    log_files <- list.files(path = path.expand(check.dir), pattern = "_files_log_", recursive = F, full.names = T)
    log_file <- log_files[order(basename(log_files), decreasing = T)[1]] # get the most recent log file
  }

  cat("\nReading from ",basename(log_file),"...",sep="")
  
  # function to read the files from the log file
  files_table <- read.table(log_file, sep = "\n", stringsAsFactors = F, col.names = c("col1"), blank.lines.skip = T)
  setDT(files_table)
  allfiles <- files_table[grepl("(.csv$)|(.xlsx$)|(.xls$)",col1),c(col1)]
  allfiles <- unique(allfiles) # in case any overmatching in grepl statement in writeLogFile resulted in duplicated entries
  cat("\nDone.")
  return(allfiles)
}
