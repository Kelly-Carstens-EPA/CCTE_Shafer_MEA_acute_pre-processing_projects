getFileCytoData <- function(sourcefile) {
  
  # get the experiment date from Plate 1
  # plate1_dat <- as.data.frame(read_excel(sourcefile, sheet = "Plate 1", range = "A1:J10", col_names = paste0("col",1:10)))
  plate1_dat <- as.data.table(read.xlsx(sourcefile, sheet = "Plate 1", rows = 1:10, cols = 1:10, colNames = FALSE))
  colnames(plate1_dat) <- paste0("col",1:ncol(plate1_dat))
  exp_date_index <- which(plate1_dat == "Experiment ID", arr.ind = T) # returns a 2-element vector of the row and col index
  experiment_date <- plate1_dat[exp_date_index[1], unlist(.SD), .SDcols = exp_date_index[2] + 1]
  if (!('experiment_date' %in% ls())) {
    cat(paste0("Can't find experiment date in ",sourcefile,".\nSetting experiment_date to NA"))
    experiment_date <- NA_character_
  }
  
  AB_dat <- getAssayData("AB",sourcefile)
  LDH_dat <- getAssayData("LDH",sourcefile)
  
  allfiledat <- rbind(AB_dat, LDH_dat)
  
  # clean up
  # wherever treatment is NA, use what is listed under conc (e.g for Lysis wells)
  allfiledat[is.na(treatment), treatment := conc]
  
  # Exclude the blank wells, where wells outside of the main plate and have NA rval
  allfiledat <- allfiledat[!(!(Row %in% c("A","B","C","D","E","F") & coli %in% 1:8) & is.na(rval))]
  
  # finalize the columns
  allfiledat[, experiment.date := experiment_date]
  allfiledat[, apid := experiment_date]
  allfiledat[, srcf := basename(sourcefile)]
  allfiledat[, rowi := sapply(Row, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
  allfiledat[, coli := as.numeric(coli)]
  
  allfiledat <- allfiledat[,c("treatment","apid","experiment.date","plate.id","rowi","coli","conc","rval","srcf","acnm")]
  
  return(allfiledat)
}