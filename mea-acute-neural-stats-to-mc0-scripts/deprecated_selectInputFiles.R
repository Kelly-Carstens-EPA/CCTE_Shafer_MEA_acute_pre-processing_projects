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
  
  writeFilesLog(file_names, project_name, files_type)
}