# function to run all files through fileToLongDat and print summary of wllq changes
extractAllData <- function(output.dir, dataset_title, run.type.tag.location, append = F, files_log = "",
                           plate.id.tag.location = numeric(0)) {
  
  # load the exisitng data, if any
  if (append) {
    # read the data from the most recent dat RData file
    data_files <- list.files(path = path.expand(paste0(output.dir,"/output")), pattern = paste0(dataset_title,"_dat1_"), recursive = F, full.names = T)
    data_file <- data_files[order(basename(data_files), decreasing = T)[1]] # get the most recent data file
    load(data_file)
    completed_files <- dat1[, unique(srcf)]
  } else {
    dat1 <- list()
    completed_files <- c()
  }
  
  # get the files from the files_log that are not already in dat1
  if (files_log == "") {
    files_logs <- list.files(path = output.dir, pattern = paste0("neural_stats_files_log_"), recursive = F, full.names = T)
    files_log <- files_logs[order(basename(files_logs), decreasing = T)[1]] # get the most recent files log
  }
  all_files <- read_files(output.dir, files_log)
  new_files_basenames <- sort(setdiff(basename(all_files), completed_files))
  new_files <- all_files[basename(all_files) %in% new_files_basenames]
  
  cat("\nReading data from files...")
  newdat <- list()
  for (filei in new_files) {
    add.dat <- tryCatch(fileToLongdat(filei, run.type.tag.location, plate.id.tag.location = plate.id.tag.location),
                        error = function(e) {
                          warning(paste0(e))
                          return(data.table())
                        })
    newdat <- rbind(newdat, add.dat)
    rm(add.dat)
  }
  newdat[, "files_log" := basename(files_log)]
  
  # save the updated data
  dat1 <- rbind(dat1, newdat)
  outfile <- paste0(output.dir, "/output/",dataset_title,"_dat1_",as.character.Date(Sys.Date()),".RData")
  save(dat1, file = outfile)
  cat("\n",outfile, " is ready.\n",sep="")
  
  # print summary of wllq changes
  print(dat1[wllq == 0, .(wllq_set_to_zero = paste0(sort(unique(well)),collapse=",")), by = c("experiment.date","plate.id")][order(experiment.date, plate.id)])
  
}