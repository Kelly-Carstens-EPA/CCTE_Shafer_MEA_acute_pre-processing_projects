# function to run all files through fileToLongDat and print summary of wllq changes
extractAllData <- function(project_name, 
                           acsn_map,
                           append = F, 
                           plate.id.tag.location = NULL,
                           noisy_functions) {
  
  cat("\n\nLevel 1 - Extract All Data:\n")
  
  # load the existing data, if any
  if (append) {
    # read the data from the most recent dat.neural.stats RData file
    data_files <- list.files(path = file.path(project_name,'output'), 
                             pattern = paste0(project_name,"_dat.neural.stats"), recursive = F, full.names = T)
    data_file <- data_files[order(basename(data_files), decreasing = T)[1]] # get the most recent data file
    load(data_file)
    completed_files <- dat.neural.stats[, unique(srcf)]
  } else {
    dat.neural.stats <- data.table()
    completed_files <- c()
  }
  
  # get the files from the files_log that are not already in dat.neural.stats
  all_files <- readFilesLog(project_name, files_type = "neural_stats")
  new_files_basenames <- sort(setdiff(basename(all_files), completed_files))
  new_files <- all_files[basename(all_files) %in% new_files_basenames]
  
  cat("\nReading data from files...\n")
  newdat <- list()
  for (i in 1:length(new_files)) {
    add.dat <- fileToLongdat(filei = new_files[i], 
                             acsn_map = acsn_map,
                             num_wells_per_plate = 48, 
                             plate.id.tag.location = plate.id.tag.location,
                             noisy_functions = noisy_functions)
    newdat <- rbind(newdat, add.dat)
    rm(add.dat)
    if (i %% 50 == 0)
      cat('...',i,'complete\n')
  }
  
  dat.neural.stats <- rbind(dat.neural.stats, newdat)
  
  cat('Done!\n')
  
  return(dat.neural.stats)

}
