readFilesLog <- function(project_name, files_type = "neural_stats") {
  
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
  allfiles <- unique(allfiles) # in case any overmatching in grepl statement in writeFilesLog resulted in duplicated entries
  cat("\nGot ",length(allfiles)," files.",sep="")
  return(allfiles)
}
