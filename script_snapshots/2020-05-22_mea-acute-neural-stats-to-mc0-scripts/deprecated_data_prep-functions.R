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

fileToLongdat <- function(filei, run.type.tag.location, plate.id.location = NULL, 
                          check.settings = T, check.parameters = T, check.timing = T, threshold = 30.0) {
  
  # possible limitations:
  # - if tag phrase is different from Well.Averages
  
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  
  # get the index of the tag phrase 'Well Averages'
  well.averages.rowi <- grep("[Ww]ell [Aa]verages", file_col1)
  
  # find the next blank line after well averages index
  next.blank.row.dist <- which(is.na(file_col1[well.averages.rowi:length(file_col1)]) | file_col1[well.averages.rowi:length(file_col1)] == "")[1]
  
  
  # CHECK FOR THINGS TO FLAG
  
  filei.settings <- if(check.settings) checkAxisSettings(filei, file_col1, file_col2) else TRUE
  filei.parameters <- if(check.parameters) checkForAllParameters(filei, file_col1, filei_col2) else TRUE
  filei.timing <- if(check.timing) checkAnalysisTiming(filei, file_col1, file_col2, threshold) else TRUE
  flags.list <- c(filei.settings, filei.parameters, filei.timing)
  if(any(flags.list != TRUE)) {
    cat("\n")
    stop(paste(flags.list[which(flags.list != TRUE)], collapse = "\n"))
  }
  
  # COLLECT THE DATA
  
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
  rm(dat)
  
  # COLLECT ID DATA
  
  # get the plate ID
  # if file contains "plate serial number", get data from there
  # otherwise, get the plate.id from the provided tag number in file name
  plate.rowi <- tryCatch(getTagPhraseIndex(file_col1, "[Pp]late [Ss]erial [Nn]umber", errorifmissing = T, checkMultiple = T),
                         error = function(e) {
                           print(paste0("Error in ",filei))
                           print(paste(e))
                           return("")
                         })
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
  
  # get the experiment date
  date.rowi <- getTagPhraseIndex(file_col1, "[Ee]xperiment [Ss]tart [Tt]ime", errorifmissing = T, checkMultiple = T)
  date <- format(as.Date(file_col2[date.rowi], format = "%m/%d/%Y"), "%Y%m%d")
  
  # determine run type from filei
  # _00 is for baseline. _01 is for treated
  # user enters run.type.tag.location in input file names (usually 5)
  # e.g. TC_20190508_MW68-0808_13_00(000).csv is baseline, and TC_20190508_MW68-0808_13_01(000).csv is treated.
  # ignore the 0's and 1's that come after the first 2 digits in that tag
  run.type.tag <- strsplit(basename(filei), split = "_")[[1]][run.type.tag.location]
  run_type <- switch(substring(run.type.tag,1,2), 
                     "00" = "baseline",
                     "01" = "treated",
                     stop(paste0("\nrun type cannot be determined for ",basename(filei))))
  
  
  # ADD ID DATA TO LONGDAT
  
  # plate, date, apid
  longdat[, `:=`(plate.id = plate.id, date = date)]
  longdat[, apid := paste(date,plate.id,sep="_")]
  
  # rowi and coli
  longdat[, coli := sub(pattern = "[[:alpha::]]*","",well)] # remove the letters from well to get coli
  longdat[, rowc := sub(pattern = "[[:digit::]]*","",well)] # remove the digits from well to get rowc
  longdat[, rowi := sapply(rowc, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
  longdat[, rowc := NULL]
  
  # add srcf, run_type
  longdat[, srcf := basename(filei)]
  longdat[, run_type := run_type]
  
  
  # SET THE WELL QUALITY
  
  # for baseline recordings, do 2 checks for wllq
  if (run_type == "baseline") {
    
    longdat[, wllq := 1]
    
    # if nAE is less than 10, set wllq=0
    low_ae_wells <- longdat[file_acsn == "Number of Active Electrodes" & activity_value < 10, well]
    
    # # MFR - may add in check here later
    # mfr_threshold <- 50/60
    # low_mfr_wells <- longdat[file_acsn == "Mean Firing Rate (Hz)" & activity_value < mfr_threshold, well]
    
    # weak_wells <- c(low_ae_wells, low_mfr_wells)
    weak_wells <- low_ae_wells
    longdat[well %in% weak_wells, wllq := 0]
  }
  else {
    # don't assign wllq for treated wells yet
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
