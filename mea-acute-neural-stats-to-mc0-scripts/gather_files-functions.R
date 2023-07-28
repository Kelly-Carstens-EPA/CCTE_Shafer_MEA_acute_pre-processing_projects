# script to gather the wanted mea acute files

selectInputFiles <- function(start.dir, project_name, files_type = "neural_stats", append = FALSE){
  
  # get starting folder, to initialize starting screen
  culture.dirs <- list.dirs(path = start.dir, recursive = F)
  
  if (append) {
    file_names <- read_files(project_name, files_type = files_type)
  } else {
    file_names <- c()
  }
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
  
  writeLogFile(file_names, project_name, files_type)
}

writeLogFile <- function(file_names, project_name, files_type) {
  
  # create log file name
  log_file <- file.path(project_name, paste0(project_name, "_",files_type,"_files_log.txt"))
  
  # create the log file
  sink(file = log_file, append = F)
  cat(paste0(project_name," files used for MEA acute pre-processing for TCPL\n"))
  cat("File types: ",files_type,"\n",sep="")
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


read_files <- function(project_name, files_type = "neural_stats") {
  
  files_log <- list.files(project_name, pattern = paste0(files_type,"_files_log"), recursive = F, full.names = T)
  # (to accommodate older formats when I wasn't relying on version control)
  if (length(files_log) > 1) {
      files_log <- files_log[order(basename(files_log), decreasing = T)[1]] # get the most recent log file  stopifnot(length(files_log) == 1)  
  }
  
  if (length(files_log) == 0) {
    cat('\nNo files_log present')
    return(c())
  }
  cat("\nReading from ",basename(files_log),"...",sep="")
  
  # function to read the files from the log file
  files_table <- read.table(files_log, sep = "\n", stringsAsFactors = F, col.names = c("col1"), blank.lines.skip = T)
  setDT(files_table)
  allfiles <- files_table[grepl("(\\.csv$)|(\\.xlsx$)|(\\.xls$)",col1),c(col1)]
  allfiles <- unique(allfiles) # in case any overmatching in grepl statement in writeLogFile resulted in duplicated entries
  cat("\nGot ",length(allfiles)," files.",sep="")
  return(allfiles)
}
