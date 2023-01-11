collapsePlateData <- function(files, log_file, output.dir = "", plate.id.location = NULL, div.location = 4) {
  
  files <- files[order(basename(files))]
  alldat <- list()
  
  cat("\nReading data from files...")
  
  # sink(file = log_file)
  for (filei in files) {
    # cat("\n",basename(filei),sep="")
    add.dat <- fileToLongdat(filei, plate.id.location = plate.id.location, div.location = div.location)
    alldat <- rbind(alldat, add.dat)
    rm(add.dat)
  }
  # closeAllConnections()
  
  # calculate the percent change in values
  cat("\n\n\nCollapsing treated and baseline data...")
  apids <- unique(alldat$apid)
  
  alldat2 <- list()
  for (apidi in apids) {
    
    cat("\n\n",apidi, sep = "")
    
    if (length(setdiff(c("baseline","treated"),alldat[apid == apidi, unique(run_type)])) != 0 ) {
      stop(paste0("Baseline and/or treated files not found for ",apidi))
    }
    
    bdat <- alldat[apid == apidi & run_type == "baseline"]
    tdat <- alldat[apid == apidi & run_type == "treated"]
    
    cat("\n\tBaseline stats file name: ",unique(basename(bdat$stats_compiler_filename)),
        ", raw file: ",unique(basename(bdat$raw_filename)),sep="")
    cat("\n\tTreated stats file name: ",unique(basename(tdat$stats_compiler_filename)),
        ", raw file: ",unique(basename(tdat$raw_filename)),sep="")
    
    platedat <- merge(bdat, tdat, by = c("tcpl_acsn","file_acsn","plate.id","date","apid","well","coli","rowi","DIV"), 
                      suffixes = c(".b",".t"))
    
    # calculate the percent change in activity
    platedat[, rval := ( (activity_value.t - activity_value.b) / activity_value.b ) * 100]
    
    # take the wllq from baseline recording as the well quality, and set NA rval wllq to 0
    platedat[, wllq := wllq.b]
    platedat[is.na(rval), wllq := 0]
    
    # create the source file name
    platedat[, srcf := paste(stats_compiler_filename.b, stats_compiler_filename.t, sep = ";")]
    
    # just get the columns we need
    add.dat <- platedat[, .(tcpl_acsn, plate.id, apid, coli, rowi, wllq, rval, srcf)]
    alldat2 <- rbind(alldat2, add.dat)
    
  }
  rm(list = c("alldat"))
  
  # save the data as .RData
  filename <- file.path(output.dir, paste0(dataset_title,"_alldat2_",as.character.Date(Sys.Date()),".RData"))
  save(alldat2, file = filename)
  rm(alldat2)
  
  cat("\n\nRData is saved:",filename)
}