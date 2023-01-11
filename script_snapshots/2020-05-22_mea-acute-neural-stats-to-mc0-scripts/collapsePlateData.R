# function to run all files through fileToLongDat and print summary of wllq changes
extractAllData <- function(output.dir, dataset_title, run.type.tag.location, append, files_log = "",
                           check.settings = T, check.parameters = T, check.timing = T, threshold = 30.0, plate.id.tag.location = numeric(0)) {
  
  # load the exisitng data, if any
  if (append) {
    # read the data from the most recent alldat RData file
    data_files <- list.files(path = path.expand(paste0(output.dir,"/output")), pattern = paste0(dataset_title,"_alldat1_"), recursive = F, full.names = T)
    data_file <- data_files[order(basename(data_files), decreasing = T)[1]] # get the most recent data file
    load(data_file)
    completed_files <- alldat1[, unique(srcf)]
  } else {
    alldat1 <- list()
    completed_files <- c()
  }
  
  # get the files from the files_log that are not already in alldat1
  all_files <- read_files(output.dir, files_log)
  new_files_basenames <- sort(setdiff(basename(all_files), completed_files))
  new_files <- all_files[basename(all_files) %in% new_files_basenames]

  cat("\nReading data from files...")
  newdat <- list()
  for (filei in new_files) {
    add.dat <- tryCatch(fileToLongdat(filei, run.type.tag.location,
                             check.settings, check.parameters, check.timing, 
                             threshold = 30.0, plate.id.tag.location = plate.id.tag.location),
                        error = function(e) {
                          warning(paste0(e))
                          return(list())
                        })
    newdat <- rbind(newdat, add.dat)
    rm(add.dat)
  }
  
  # save the updated data
  alldat1 <- rbind(alldat1, newdat)
  outfile <- paste0(output.dir, "/output/",dataset_title,"_alldat1_",as.character.Date(Sys.Date()),".RData")
  save(alldat1, file = outfile)
  cat("\n",outfile, " is ready.\n",sep="")
  
  # print summary of wllq changes
  print(alldat1[wllq == 0, .(wllq_set_to_zero = paste0(sort(unique(well)),collapse=",")), by = "apid"][order(apid)])

}


collapsePlateData <- function(main.output.dir) {
  
  # read the data from the most recent alldat1 RData file
  data_files <- list.files(path = path.expand(paste0(main.output.dir,"/output")), pattern = paste0(dataset_title,"_alldat1_"), recursive = F, full.names = T)
  data_file <- data_files[order(basename(data_files), decreasing = T)[1]] # get the most recent data file
  cat("\nLoading",data_file,"...")
  load(data_file)
  
  # calculate the percent change in values
  cat("\nCollapsing treated and baseline data...")
  apids <- unique(alldat1$apid)
  
  alldat2 <- list()
  for (apidi in apids) {
    
    cat("\n\n",apidi, sep = "")
    
    bdat <- alldat1[apid == apidi & run_type == "baseline"]
    tdat <- alldat1[apid == apidi & run_type == "treated"]
    
    if (length(setdiff(c("baseline","treated"),alldat1[apid == apidi, unique(run_type)])) != 0 ) {
      stop(paste0("\nBaseline and/or treated files not found for ",apidi))
    }
    if (nrow(bdat) != nrow(tdat)) stop("\nUnequal number of rows for baseline and treated data.")
    
    cat("\n\tBaseline stats file name: ",unique(basename(bdat$srcf)),sep="")
    cat("\n\tTreated stats file name: ",unique(basename(tdat$srcf)),sep="")
    
    # get the  columns we need (drop the settings data, as well as run_type)
    usecols <- c("tcpl_acsn","apid","well","coli","rowi", "activity_value", "wllq","srcf")
    
    platedat <- merge(bdat[,..usecols], tdat[, ..usecols], by = c("tcpl_acsn","apid","well","coli","rowi"), 
                      suffixes = c(".b",".t"))
    
    # calculate the percent change in activity
    platedat[, rval := ( (activity_value.t - activity_value.b) / activity_value.b ) * 100]
    
    # take the wllq from baseline recording as the well quality. Then set wllq to 0 for any rows with NA rval
    platedat[, wllq := wllq.b]
    
    # create the source file name
    platedat[, srcf := paste(srcf.b, srcf.t, sep = ";")]
    
    # just get the columns we need
    add.dat <- platedat[, .(acsn = tcpl_acsn, apid, coli, rowi, wllq, rval, srcf)]
    alldat2 <- rbind(alldat2, add.dat)
    rm(add.dat)
  }

  # save the data as .RData
  filename <- file.path(main.output.dir, "output",paste0(dataset_title,"_alldat2_",as.character.Date(Sys.Date()),".RData"))
  save(alldat2, file = filename)

  cat("\n",filename, " is ready.\n",sep="")
}