#' Title
#'
#' @param filei neural statistics compiler file to be read
#' @param acsn_map map of assay component names as they appear in neural statistics compiler to the acnm's registered in TCPL
#' @param num_wells_per_plate number of wells per plate. Used to restrict the number of data rows read 
#' @param plate.id.tag.location  # (for backwards compatibility) define the underscore-separated component of the file name that contains the plate ID 
#' (e.g., If file name is "20201118_MW78-009_000.csv, then plate.id.tag.location = 2). Preferred setting is to leave this as NULL and to get plate ID from the file body, 
#' which is probably less prone to typo's)
#' @param noisy_functions whether to display output to document progress
#'
#' @return
#' @export
#'
#' @examples
fileToLongdat <- function(filei, 
                          acsn_map,
                          num_wells_per_plate = 48, 
                          plate.id.tag.location = NULL,
                          noisy_functions = FALSE) {
  
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  
  # get the index of the tag phrase 'Well Averages'
  well.averages.rowi <- grep("[Ww]ell [Aa]verages", file_col1)
  
  # find the next blank line after well averages index
  next.blank.row.dist <- which(is.na(file_col1[well.averages.rowi:length(file_col1)]) | file_col1[well.averages.rowi:length(file_col1)] == "")[1]
  
  # COLLECT THE DATA --------------------------------------------------------
  
  # read in the data from well.averages.rowi index
  dat <- as.data.table(read.table(filei, sep = ",", header = T, skip = (well.averages.rowi - 1), nrows = (next.blank.row.dist-2),
                                  stringsAsFactors = F))
  dat <- dat[, 1:(num_wells_per_plate+1)] # in case the file was saved differently and it collected all 769 columns
  setnames(dat, old = grep("[Ww]ell.[Aa]verages", names(dat),value=T), new = "acsn")
  dat <- dat[acsn != "Treatment/ID"] # remove Treatment/ID row (usually is blank)
  dat[, c(setdiff(names(dat),'acsn')) := lapply(.SD, as.numeric), .SDcols = setdiff(names(dat),'acsn')] # make all well columns double, not integer
  
  # melt the data in long file format
  longdat <- melt(dat, id.vars = "acsn", variable.name = "well", value.name = "activity_value", variable.factor = F)
  
  # Check if any acsn have all NA activity values (would indicate something wrong in file)
  stopifnot(nrow(longdat[, .(sum(!is.na(activity_value))), by = .(acsn)][V1 == 0]) == 0)
  rm(dat)
  
  
  # COLLECT ID DATA ---------------------------------------------------------
  
  # get relevant data from file header
  headdat <- data.table(file_col1, file_col2)
  
  # get the plate ID, experiment date, and analysis duration
  # error note: if there are multiple of any value, an error will be thrown when try to add to longdat
  if (is.null(plate.id.tag.location)) {
    plate.id <- headdat[grepl("[Pp]late [Ss]erial [Nn]umber",file_col1), paste0("MW",file_col2)]
  } else {
    # for backwards compatibility, when Plate Serial number not listed in file body
    plate.id <- strsplit(basename(filei), split = "_")[[1]][plate.id.tag.location]
    plate.id <- sub(" ","",plate.id)
    if(!grepl('^MW',plate.id)) plate.id <- paste0('MW',plate.id)
  }
  if (nchar(plate.id) < 3) {
    plate.id <- NA_character_
    warning(paste0("\nplate.id not found for ",basename(filei)))
  }
  
  date <- headdat[grepl("[Ee]xperiment [Ss]tart [Tt]ime",file_col1), format(as.Date(file_col2, format = "%m/%d/%Y"), "%Y%m%d")]
  if (length(date) == 0 || is.na(date)) stop(paste0("\ndate not found."))
  
  analysis_start <- headdat[grepl("Analysis Start",file_col1), as.numeric(file_col2)]
  analysis_duration <- headdat[grepl("Analysis Duration",file_col1), as.numeric(file_col2)]
  setting_min.num.spks.network.burst <- headdat[grepl("Minimum Number of Spikes \\(network bursts\\)",file_col1), as.numeric(file_col2)]
  setting_axis.version <- headdat[grepl("AxIS Version",file_col1), paste0(unique(file_col2),collapse=",")]
  original_file_time <- headdat[grepl("Original File Time",file_col1), as.character(file_col2)]
  exp_start_time <- headdat[grepl("Experiment Start Time",file_col1), as.character(file_col2)]
  
  
  # ADD ID DATA TO LONGDAT --------------------------------------------------
  
  # add date, apid, rowi, and coli
  longdat[, plate.id := plate.id]
  longdat[, experiment.date := date]
  longdat[, apid := date]
  longdat[, coli := as.integer(stri_extract(well, regex = '[0-9]+'))]
  longdat[, rowi := match(stri_extract(well, regex = '[A-Z]+'), LETTERS)]
  
  # add file time info & other settings
  longdat[, `:=`(experiment_start_time = exp_start_time,
                 original_file_time = original_file_time,
                 analysis_start = analysis_start, 
                 analysis_duration = analysis_duration)]
  longdat[, `:=`("setting_min.num.spks.network.burst" = setting_min.num.spks.network.burst, 
                 "setting_axis.version" = setting_axis.version)]
  
  # add neural_stats_file
  longdat[, neural_stats_file := basename(filei)]

  # map to the acnm's. Throw an error if any acsn's aren't in the acsn map table
  longdat <- merge(longdat, acsn_map, by = c("acsn"), all.x = TRUE)
  if (any(is.na(unique(longdat$acnm)))) {
    stop(paste0("The following assay components where not found in acsn_to_acnm_map.csv:\n",
                paste0(longdat[is.na(acnm),sort(unique(acsn))],collapse=" ,")))
  }
  
  if (noisy_functions) {
    cat("Processed",basename(filei), "\n")
  }
  
  return(longdat)
}
