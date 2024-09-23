# function to calculate the percent change values
# result is dat2
collapsePlateData <- function(main.output.dir, dataset_title, main.dir) {
  
  # read the data from the most recent dat1 RData file
  cat("\n\nLevel 2 - Collapse Data by Plate ID:\n")
  cat("\nLoading...\n")
  dat1 <- get_latest_dat(lvl = "dat1",dataset_title, main.dir = main.dir)
  
  # data_files <- list.files(path = path.expand(paste0(main.output.dir,"/output")), pattern = paste0(dataset_title,"_dat1_"), recursive = F, full.names = T)
  # data_file <- data_files[order(basename(data_files), decreasing = T)[1]] # get the most recent data file
  # cat("\nLoading",data_file,"...")
  # load(data_file)
  
  cat("\nCollapsing treated and baseline data...")
  dat1[, date_plate := paste(experiment.date, plate.id ,sep = "_")] # don't want to just use plate.id, since it could have been re-used
  plates <- unique(dat1$date_plate)
  
  dat2 <- list()
  for (plate in plates) {
    
    cat("\n",plate, sep = "")
    
    bdat <- dat1[date_plate == plate & run_type == "baseline"]
    tdat <- dat1[date_plate == plate & run_type == "treated"]
    
    if (length(setdiff(c("baseline","treated"),dat1[date_plate == plate, unique(run_type)])) != 0 ) {
      stop(paste0("\nBaseline and/or treated files not found for ",plate))
    }
    
    if (noisy_functions) {
      cat("\n\tBaseline stats file name: ",unique(basename(bdat$srcf)),sep="")
      cat("\n\tTreated stats file name: ",unique(basename(tdat$srcf)),"\n",sep="")
    }
    
    # check same number of data rows
    if (nrow(bdat) != nrow(tdat)) stop("\nUnequal number of rows for baseline and treated data.")
    # check if the recording lengths are greater than 30% diff
    # unless either file was completely exlcuded bc recording too long or short
    if(1 %in% unique(bdat$wllq) && 1 %in% unique(tdat$wllq)) {
      percent_diff_recording_length <- (unique(tdat$analysis_duration) - unique(bdat$analysis_duration)) / unique(bdat$analysis_duration)
      if (abs(percent_diff_recording_length) > 0.3) {
        stop(paste0("\nRecording lengths differ significantly.\nBaseline: ", unique(bdat$analysis_duration), " s",
                    "\nTreated: ",unique(tdat$analysis_duration)," s"))
      }
    }
    
    # get the  columns we need (drop the settings data, as well as run_type)
    usecols <- c("acnm","apid","experiment.date","plate.id","well","coli","rowi", "activity_value", "wllq","srcf","wllq_notes","files_log")
    
    platedat <- merge(bdat[,..usecols], tdat[, ..usecols], by = c("acnm","apid","experiment.date","plate.id","well","coli","rowi","files_log"), 
                      suffixes = c(".b",".t"))
    
    # calculate the percent change in activity
    platedat[, rval := ( (activity_value.t - activity_value.b) / activity_value.b ) * 100]
    
    # define the plate wllq 
    # wllq.t is NA, unless the recording time was too long or short, then wllq.t is 0
    # platedat[, wllq := min(wllq.b, wllq.t, na.rm = T)] # this took the min of the entire vector instead of row by row
    platedat[, wllq := ifelse(!is.na(wllq.t), wllq.t, wllq.b)]
    platedat[, wllq_notes := paste0(wllq_notes.t, wllq_notes.b)]
    
    # create the source file name
    platedat[, srcf := paste(srcf.b, srcf.t, sep = ";")]
    
    # just get the columns we need
    add.dat <- platedat[, .(acnm, apid, experiment.date, plate.id, coli, rowi, wllq, wllq_notes, rval, srcf, files_log)]
    dat2 <- rbind(dat2, add.dat)
    rm(add.dat)
  }
  
  # document which dat1 file is being used (loaded from get_latest_dat)
  dat2[, "dat1" := basename(RData_files_used)]

  # save the data as .RData
  filename <- file.path(main.output.dir, "output",paste0(dataset_title,"_dat2_",as.character.Date(Sys.Date()),".RData"))
  save(dat2, file = filename)

  cat("\n",basename(filename), " is ready.\n",sep="")
}