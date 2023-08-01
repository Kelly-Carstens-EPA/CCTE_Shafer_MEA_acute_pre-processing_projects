writeFilesLog <- function(file_names, project_name, files_type) {
  
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