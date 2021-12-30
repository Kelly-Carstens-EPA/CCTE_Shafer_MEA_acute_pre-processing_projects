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


fileToLongdat <- function(filei, run.type.tag.location,
                          plate.id.tag.location = numeric(0), include.all.settings = F, guess_run_type_later = F,
                          standard_analysis_duration_requirement = get('standard_analysis_duration_requirement', envir = .GlobalEnv)) {
  
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  
  # get the index of the tag phrase 'Well Averages'
  well.averages.rowi <- grep("[Ww]ell [Aa]verages", file_col1)
  
  # find the next blank line after well averages index
  next.blank.row.dist <- which(is.na(file_col1[well.averages.rowi:length(file_col1)]) | file_col1[well.averages.rowi:length(file_col1)] == "")[1]
  
  # COLLECT THE DATA
  
  # read in the data from well.averages.rowi index
  dat <- as.data.table(read.table(filei, sep = ",", header = T, skip = (well.averages.rowi - 1), nrows = (next.blank.row.dist-2),
                                  stringsAsFactors = F))
  dat <- dat[, 1:49] # in case the file was saved differently and it collected all 769 columns
  setnames(dat, old = grep("[Ww]ell.[Aa]verages", names(dat),value=T), new = "acsn")
  dat <- dat[acsn != "Treatment/ID"] # remove Treatment/ID row
  dat[, names(dat)[-1] := lapply(.SD, as.numeric), .SDcols = names(dat)[-1]] # make all well columns double, not int
  
  # check if any entire rows are NA
  for (i in 1:nrow(dat)) {
    all_row_NA <- all(is.na(dat[i, c(which(names(dat)!="acsn")),with=F]))
    if(all_row_NA) {
      assign(x = "dat", value = dat, envir = .GlobalEnv) # pass dat to global env to can check
      stop("NA row in Well Averages data. Data table 'dat' passed to Global Env for inspection")
    }
  }
  
  # melt the data in long file format
  longdat <- melt(dat, id.vars = "acsn", variable.name = "well", value.name = "activity_value", variable.factor = F)
  rm(dat)
  
  # COLLECT ID DATA

  # get relevant data from file header
  headdat <- data.table(file_col1, file_col2)
  
  # get the plate ID, experiment date, and analysis duration
  # error note: if there are multipe of any value, an error will be thrown when try to add to longdat
  if (length(plate.id.tag.location) == 0) {
    plate.id <- headdat[grepl("[Pp]late [Ss]erial [Nn]umber",file_col1), paste0("MW",file_col2)]
  }
  else {
    # for backwards compatibility, when Plate Serial number not listed in file body
    plate.id <- strsplit(basename(filei), split = "_")[[1]][plate.id.tag.location]
    plate.id <- sub(" ","",plate.id)
  }
  if (nchar(plate.id) < 3) stop(paste0("\nplate.id not found."))
  
  date <- headdat[grepl("[Ee]xperiment [Ss]tart [Tt]ime",file_col1), format(as.Date(file_col2, format = "%m/%d/%Y"), "%Y%m%d")]
  if (length(date) == 0 || is.na(date)) stop(paste0("\ndate not found."))
  
  analysis.start <- headdat[grepl("Analysis Start",file_col1), as.numeric(file_col2)]
  analysis.duration <- headdat[grepl("Analysis Duration",file_col1), as.numeric(file_col2)]
  setting_min.num.spks.network.burst <- headdat[grepl("Minimum Number of Spikes \\(network bursts\\)",file_col1), as.numeric(file_col2)]
  setting_axis.version <- headdat[grepl("AxIS Version",file_col1), paste0(unique(file_col2),collapse=",")]
  original_file_time <- headdat[grepl("Original File Time",file_col1), as.character(file_col2)]
  exp_start_time <- headdat[grepl("Experiment Start Time",file_col1), as.character(file_col2)]
  
  if (include.all.settings) {
    
    headdat[is.na(file_col1), file_col1 := ""] # just to standardize. I'm not sure how to control if empty comes up as NA or ""
    headdat[is.na(file_col2), file_col2 := ""]
    
    # determine which rows define a "setting"
    headdat[, settings_header := grepl("Settings",file_col1)]
    headdat[, settings_info := FALSE]
    under_settings <- FALSE
    for (i in 1:nrow(headdat)) {
      if (headdat[i,settings_header]) {
        under_settings <- TRUE
      }
      else if (headdat[i, file_col1] == "") {
        under_settings <- FALSE
      }
      if (under_settings) {
        headdat[i, settings_info := TRUE]
      }
    }
    
    # not sure why FALSE reads as 'false' sometimes...
    headdat[grepl("[Ff]alse",file_col2), file_col2 := "FALSE"]
    headdat[grepl("[Tt]rue",file_col2), file_col2 := "TRUE"]
    
    # remove measurements that vary by plate
    variable_values <- c("   Original File Time","   Experiment Start Time","   Plate Serial Number")
    settings.dat <- headdat[settings_info == T & settings_header == F & !(file_col1 %in% variable_values)]
    settings.dat[, file_col1 := sub(" *","",file_col1)] # get rid of extra spaces
    
    # add the settings data to longdat
    longdat[, c(settings.dat$file_col1) := lapply(settings.dat$file_col2,function(x) rep(x, nrow(longdat)))]
  }

  
  # ADD ID DATA TO LONGDAT
  
  # set apid as the experiment date
  longdat[, plate.id := plate.id]
  longdat[, experiment.date := date]
  longdat[, apid := date]
  
  # rowi and coli
  longdat[, coli := sub(pattern = "[[:alpha::]]*","",well)] # remove the letters from well to get coli
  longdat[, coli := as.integer(coli)] # make this column an integer 
  longdat[, rowc := sub(pattern = "[[:digit::]]*","",well)] # remove the digits from well to get rowc
  longdat[, rowi := sapply(rowc, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
  longdat[, rowc := NULL]
  
  # add srcf, run_type
  longdat[, srcf := basename(filei)]
  longdat[, run_type := run_type]
  
  # add the analysis timing data
  longdat[, `:=`(analysis_start = analysis.start, analysis_duration = analysis.duration)]
  
  # add other settings data
  longdat[, `:=`("setting_min.num.spks.network.burst" = setting_min.num.spks.network.burst, "setting_axis.version" = setting_axis.version)]
  
  
  # SET THE WELL QUALITY
  
  # for baseline recordings, do 2 checks for wllq
  if (run_type == "baseline") {
    
    longdat[, wllq := 1] # set the default wllq
    longdat[, wllq_notes := ""]
    
    # if nAE is less than 10 or is NA, set wllq=0
    low_ae_wells <- longdat[acsn == "Number of Active Electrodes" & (activity_value < 10 | is.na(activity_value)), well]
    longdat[well %in% low_ae_wells, `:=` (wllq = 0, wllq_notes = "Baseline # of AE < 10; ")]
    
    # if the MFR is very low or near the theoretical upper limit, remove that well
    # see the script mfr_baseline_cutoff_investigation.R 
    # or the notbeook 'MEA Acute Pre-Process for TCPL', Tab "Development", Page "Mean Firing Rate Baseline Cutoff"
    # for more details
    mfr_upper_threshold <- 3.4036511 # this is the 95th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
    mfr_lower_threshold <- 0.6377603 # this is the 5th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
    high_mfr_wells <- longdat[acsn == "Mean Firing Rate (Hz)" & activity_value > mfr_upper_threshold, well]
    longdat[well %in% high_mfr_wells, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR > ",mfr_upper_threshold," Hz; "))]
    low_mfr_wells <- longdat[acsn == "Mean Firing Rate (Hz)" & (activity_value < mfr_lower_threshold | is.na(activity_value)), well]
    longdat[well %in% low_mfr_wells, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR < ",mfr_lower_threshold," Hz; "))]
  }
  else {
    # don't assign wllq for treated wells (yet)
    longdat[, `:=` (wllq = NA_integer_, wllq_notes = "")]
  }
  # for baseline or treated, if recording length is very short or very  long, remove it
  if (standard_analysis_duration_requirement) {
    if (abs(analysis.duration - 2400) > 1400) cat(basename(filei),"will be removed. Recording length is",analysis.duration,"\n")
    longdat[analysis.duration < 1000, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length < 1000 s; "))]
    longdat[analysis.duration > 3800, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length > 3800 s; "))]    
  }
  
  # map to the acnm's. Throw an error if any acsn's aren't in the acsn map table
  longdat <- merge(longdat, acsn_map, by = c("acsn"), all.x = TRUE)
  if (any(is.na(unique(longdat$acnm)))) {
    stop(paste0("The following assay components where not found in acsn_to_acnm_map.csv:\n",
                paste0(longdat[is.na(acnm),sort(unique(acsn))],collapse=" ,")))
  }
  
  if (noisy_functions) {
    cat("Processed",basename(filei), "\n")
  }
  else {
    cat(".")
  }

  return(longdat)
}
