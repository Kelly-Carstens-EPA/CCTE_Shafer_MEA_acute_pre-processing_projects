getTagPhraseIndex <- function(char_vector, grep_tagPhrases, errorifmissing = T, checkMultiple = T) {
  
  # note that we cycle through the grep_tagPhrases sequentially
  # we do not check for all possible matches
  for (tagPhrase in grep_tagPhrases) {
    phrase.index <- grep(tagPhrase, char_vector)
    if (length(phrase.index) > 0) {
      if (length(phrase.index) == 1 || !checkMultiple) {
        return(phrase.index)
      } else {
        stop(paste0("there are multiple occurrences of ",tagPhrase," in char_vector"))
      }
    }
  }
  
  if (errorifmissing) {
    stop(paste0("No match found for ", grep_tagPhrases, " in char_vector",collpase=","))
  } else {
    return(integer(0))
  }
  
}
# # confirmation:
# cvector <- c("hi","testing","Yes","oh yah","hi-yah!")
# getTagPhraseIndex(cvector, "am I here?", errorifmissing = T)
# # returns error
# getTagPhraseIndex(cvector, "am I here?", errorifmissing = F)
# # integer(0)
# getTagPhraseIndex(cvector, "hi", errorifmissing = T, checkMultiple = F)
# # [1] 1 5
# getTagPhraseIndex(cvector, "hi", errorifmissing = T)
# # throws an error, since checkMultiple default is T

get_acsn_map <- function() {
  # could make this vary depending on what input file type?
  # preferably, we might load these in tcpl as acsn's in the future
  file_acsn_using = c(
    'Number of Spikes',
    'Mean Firing Rate (Hz)',
    'Number of Bursts',
    'Burst Duration - Avg (s)',
    'Number of Spikes per Burst - Avg',
    'Mean ISI within Burst - Avg',
    'Burst Percentage - Avg',
    'Burst Percentage - Std',
    'Number of Spikes per Network Burst - Avg',
    'Number of Spikes per Network Burst - Std',
    'Number of Elecs Participating in Burst - Avg',
    'Network Burst Percentage',
    'Area Under Cross-Correlation',
    'Width at Half Height of Cross-Correlation',
    'Synchrony Index'
  )
  
  tcpl_acsn <- c(
    'NHEERL_MEA_acute_spike_number',
    'NHEERL_MEA_acute_firing_rate_mean',
    'NHEERL_MEA_acute_burst_number',
    'NHEERL_MEA_acute_burst_duration_mean',
    'NHEERL_MEA_acute_per_burst_spike_number_mean',
    'NHEERL_MEA_acute_interburst_interval_mean',
    'NHEERL_MEA_acute_burst_percentage_mean',
    'NHEERL_MEA_acute_burst_percentage_std',
    'NHEERL_MEA_acute_per_network_burst_spike_number_mean',
    'NHEERL_MEA_acute_per_network_burst_spike_number_std',
    'NHEERL_MEA_acute_bursting_electrodes_number_mean',
    'NHEERL_MEA_acute_network_burst_percentage',
    'NHEERL_MEA_acute_cross_correlation_area',
    'NHEERL_MEA_acute_cross_correlation_HWHM',
    'NHEERL_MEA_acute_synchrony_index'
  )
  
  acsn_map <- data.table(file_acsn_using, tcpl_acsn)
  return(acsn_map)
}
# acsn_map <- get_acsn_map()
# write.csv(acsn_map, file = "~/mea_acute/test_output/acsn_map1.csv")

checkForAllParameters <- function(files, skip_param) {
  files_with_missing_parameters <- 0
  acsn_map <- get_acsn_map()
  
  check_acsns <- setdiff(acsn_map$file_acsn, skip_param)
  
  for (filei in files) {
    file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
    file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
    file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
    
    # get the index of the tag phrase 'Well Averages'
    well.averages.rowi <- grep("[Ww]ell [Aa]verages", file_col1)
    
    # find the next blank line after well averages index
    next.blank.row.dist <- which(is.na(file_col1[well.averages.rowi:length(file_col1)]))[1]
    
    # check if the data includes all of the wanted acitivity measure
    missing_paramaters <- setdiff(check_acsns, file_col1[(well.averages.rowi + 2):(well.averages.rowi + next.blank.row.dist - 2)])
    if (length(missing_paramaters) > 0) {
      cat(paste0("\n",basename(filei)," is missing ",missing_paramaters))
      files_with_missing_parameters <- files_with_missing_parameters + 1
    }
  }
  cat("\nThere are ",files_with_missing_parameters," that are missing some parameter data.",sep = "")
}

checkForFlags <- function(filei, threshold) {
  
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  
  analysis.start.rowi <- getTagPhraseIndex(file_col1, "Analysis Start")
  analysis.start <- file_col2[analysis.start.rowi]
  if (as.numeric(analysis.start) == 0.0) {
    return("zero.start")
  }
  else if (as.numeric(analysis.start) < threshold) {
    return("low.start")
  } 
  else {
    return("high.start")
  }
}

writeFlagSummary <- function(files, log_file, threshold = 10) {
  
  low.start.files <- c()
  high.start.files <- c()
  
  for (filei in files) {
    val <- checkForFlags(filei, threshold)
    if (val == "low.start" ) {
      low.start.files <- c(low.start.files, filei)
    }
    else if (val == "high.start") {
      high.start.files <- c(high.start.files, filei)
    }
  }
  
  
  sink(log_file)
  
  cat("Created with the script data_prep-functions.R, writeFlagSummary() on",as.character.Date(Sys.Date()),sep = " ")
  cat("\nThere are ",(length(files) - (length(low.start.files) + length(high.start.files)))," files that start at 0 seconds.",sep="")
  cat("\nThere are ",length(low.start.files)," files that start after 0 but before ",threshold," seconds.",sep="")
  cat("\nThere are ",length(high.start.files)," files that start after ",threshold," seconds.",sep="")
  
  cat("\n\nFiles that start after 0 but before ",threshold," seconds.",sep="")
  for (i in 1:length(low.start.files)) {
    cat("\n",low.start.files[i], sep = "")
  }
  
  cat("\n\nFiles that start after ",threshold," seconds.",sep="")
  for (i in 1:length(high.start.files)) {
    cat("\n",high.start.files[i], sep = "")
  }
  
  closeAllConnections()
  
  cat("\n",log_file," is ready.",sep="")
}


fileToLongdat <- function(filei, plate.id.location = NULL, div.location = 4) {
  
  # possible limitations:
  # - if tag phrase is different from Well.Averages
  
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  
  # get the index of the tag phrase 'Well Averages'
  well.averages.rowi <- grep("[Ww]ell [Aa]verages", file_col1)
  
  # find the next blank line after well averages index
  next.blank.row.dist <- which(is.na(file_col1[well.averages.rowi:length(file_col1)]))[1]
  
  # read in the data from well.averages.rowi index
  dat <- as.data.table(read.table(filei, sep = ",", header = T, skip = (well.averages.rowi - 1), nrows = (next.blank.row.dist-2),
                                  stringsAsFactors = F))
  setnames(dat, old = grep("[Ww]ell.[Aa]verages", names(dat),value=T), new = "file_acsn")
  dat <- dat[file_acsn != "Treatment/ID"] # remove Treatment/ID row
  dat[, names(dat)[-1] := lapply(.SD, as.numeric), .SDcols = names(dat)[-1]] # make all well columns double, not int
  
  # check if any entire rows are NA
  for (i in 1:nrow(dat)) {
    all_row_NA <- all(is.na(dat[i, c(which(names(dat)!="file_acsn")),with=F]))
    if(all_row_NA) {
      assign(x = "dat", value = dat, envir = .GlobalEnv) # pass dat to global env to can check
      stop("NA row in Well Averages data. Data table 'dat' passed to Global Env for inspection")
    }
  }
  
  # melt the data in long file format
  longdat <- melt(dat, id.vars = "file_acsn", variable.name = "well", value.name = "activity_value", variable.factor = F)
  
  
  # THINGS TO FLAG
  
  # might be able to remove this later, if enough confirmations that all okay
  analysis.start.rowi <- getTagPhraseIndex(file_col1, "Analysis Start")
  analysis.start <- file_col2[analysis.start.rowi]
  if (as.numeric(analysis.start) != 0.0) {
    if (as.numeric(analysis.start) < 10.0) {
      warning(paste0("\nAnalysis start is greater than zero (",analysis.start, "s) for ",basename(filei)))
    } else {
      cat(paste0("\nAnalysis start is greater than zero (",analysis.start, "s) for ",basename(filei)))
      include.file.char <- readline(prompt = "\nInclude file anyways? (y/n): ")
      if (!(include.file.char %in% c("Y","y"))) {
        return(NULL)
      }
    }
  }
  
  
  # COLLECT ID DATA
  
  # get the plate ID
  # if file contains "plate serial number", get data from there
  # otherwise, get the plate.id from the provided tag number in file name
  plate.rowi <- getTagPhraseIndex(file_col1, "[Pp]late [Ss]erial [Nn]umber", errorifmissing = F, checkMultiple = T)
  if (length(plate.rowi) > 0) {
    plate.id <- paste0("MW",file_col2[plate.rowi])
  } else {
    if (!is.null(plate.id.location)) {
      plate.id <- strsplit(basename(filei), split = "_")[[1]][plate.id.location]
      plate.id <- sub(" ","",plate.id)
    }
    else {
      stop(paste0("\nCan't find plate id for ",basename(filei)))
    }
  }
  
  # get the culture date
  date.rowi <- getTagPhraseIndex(file_col1, "[Ee]xperiment [Ss]tart [Tt]ime", errorifmissing = T, checkMultiple = T)
  date <- format(as.Date(file_col2[date.rowi], format = "%m/%d/%Y"), "%Y%m%d")
  # date <- strsplit(basename(filei), split = "_")[[1]][2]
  
  # determine if file is baseline or treated recording
  # option 1: check the input file name, for extra 1 or 0 - but, this can be inconsistent, depending on if it was a re-run
  # option 2: determine the run from the .raw file name (but not all might have that...)
  tagPhrases <- c("File Name", "AxIS Spike File Name")
  source_filename.rowi <- getTagPhraseIndex(file_col1, tagPhrases)
  
  # get the end of the raw file name. This is usually 00(000) for baseline,
  # and 00(001) for treated. But sometimes there are more 0's
  raw.file <- file_col2[source_filename.rowi]
  raw.end <- tail(strsplit(raw.file, split = "_")[[1]],n=1)
  if(grepl("1",raw.end)) {
    run_type = "treated"
  } else {
    run_type = "baseline"
  }
  
  
  # ADD ID DATA TO LONGDAT
  
  # plate, date, apid
  longdat[, `:=`(plate.id = plate.id, date = date)]
  longdat[, apid := paste(date,plate.id,sep="_")]
  
  # rowi and coli
  longdat[, coli := sub(pattern = "[[:alpha::]]*","",well)] # remove the letters from well to get coli
  longdat[, rowc := sub(pattern = "[[:digit::]]*","",well)] # remove the digits from well to get rowc
  longdat[, rowi := sapply(rowc, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
  longdat[, rowc := NULL]
  longdat[, stats_compiler_filename := filei]
  
  # source files, run_type
  longdat[, raw_filename := raw.file]
  longdat[, run_type := run_type]
  # setnames(longdat, old = "activity_value", new = paste0("activity_value.",run_type))
  
  # add the DIV, just for record-keeping
  longdat[, DIV := strsplit(basename(filei), split="_")[[1]][div.location]]
  
  
  # SET THE WELL QUALITY
  
  # for baseline recordings, do 2 checks for wllq
  if (run_type == "baseline") {
    
    longdat[, wllq := 1]
    
    # if nAE is less than 10, set wllq=0
    low_ae_wells <- longdat[file_acsn == "Number of Active Electrodes" & activity_value < 10, well]
    
    # if Mfr is less than 50 spikes per minute
    mfr_threshold <- 50/60
    low_mfr_wells <- longdat[file_acsn == "Mean Firing Rate (Hz)" & activity_value < mfr_threshold, well]
    
    weak_wells <- c(low_ae_wells, low_mfr_wells)
    longdat[well %in% weak_wells, wllq := 0]
  }
  else {
    # don't assign wllq for treated runs at this stage
    longdat[, wllq := NA]
  }
  
  
  # add in the tcpl_acsn. Unwanted parameters will be dropped
  acsn_map <- get_acsn_map()
  longdat <- merge(longdat, acsn_map, by.x = c("file_acsn"), by.y = c("file_acsn_using"))
  
  cat(paste0("\n\nProcessed ",basename(filei)))
  cat(paste0("\nRun type: ",run_type))
  if (run_type == "baseline") {
    cat("\nWells with wllq set to 0:",weak_wells)
  }
  return(longdat)
}
