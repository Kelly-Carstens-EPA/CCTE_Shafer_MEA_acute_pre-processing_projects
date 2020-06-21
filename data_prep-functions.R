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
  # conversation with Tim 06/16/2020 - 
  # Half Width at Half Height of Cross-Correlation and 
  # Width at Half Height of Cross-Correlation are the same thing. 
  # Axion just misnamed it at first time.
  # Half Width at Half Height of Cross-Correlation is the correct name

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
    'Synchrony Index',
    'Half Width at Half Height of Cross-Correlation',
    'Width at Half Height of Cross-Correlation'
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
    'NHEERL_MEA_acute_synchrony_index',
    'NHEERL_MEA_acute_cross_correlation_HWHM',
    'NHEERL_MEA_acute_cross_correlation_HWHM'
  )
  
  acsn_map <- data.table(file_acsn_using, tcpl_acsn)
  assign("acsn_map",acsn_map,envir = .GlobalEnv)
}
# acsn_map <- get_acsn_map()
# write.csv(acsn_map, file = "neural_stats_endpoint_to_tcpl_acsn_map.csv", row.names = T)


fileToLongdat <- function(filei, run.type.tag.location,
                          plate.id.tag.location = numeric(0), include.all.settings = F) {
  
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
    low_ae_wells <- longdat[file_acsn == "Number of Active Electrodes" & (activity_value < 10 | is.na(activity_value)), well]
    longdat[well %in% low_ae_wells, `:=` (wllq = 0, wllq_notes = "Baseline # of AE < 10; ")]
    
    # if the MFR is very low or near the theoretical upper limit, remove that well
    # see the script mfr_baseline_cutoff_investigation.R 
    # or the notbeook 'MEA Acute Pre-Process for TCPL', Tab "Development", Page "Mean Firing Rate Baseline Cutoff"
    # for more details
    mfr_upper_threshold <- 3.4036511 # this is the 95th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
    mfr_lower_threshold <- 0.6377603 # this is the 5th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
    high_mfr_wells <- longdat[file_acsn == "Mean Firing Rate (Hz)" & activity_value > mfr_upper_threshold, well]
    longdat[well %in% high_mfr_wells, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR > ",mfr_upper_threshold," Hz; "))]
    low_mfr_wells <- longdat[file_acsn == "Mean Firing Rate (Hz)" & (activity_value < mfr_lower_threshold | is.na(activity_value)), well]
    longdat[well %in% low_mfr_wells, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR < ",mfr_lower_threshold," Hz; "))]
  }
  else {
    # don't assign wllq for treated wells (yet)
    longdat[, `:=` (wllq = NA, wllq_notes = "")]
  }
  # for baseline or treated, if recording length is very short or very  long, remove it
  if (abs(analysis.duration - 2400) > 1400) cat("\n",basename(filei),"will be removed. Recording length is",analysis.duration)
  longdat[analysis.duration < 1000, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length < 1000 s; "))]
  longdat[analysis.duration > 3800, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length > 3800 s; "))]
  
  
  # add in the tcpl_acsn. Unwanted parameters will be dropped
  longdat <- merge(longdat, acsn_map, by.x = c("file_acsn"), by.y = c("file_acsn_using"))
  setnames(longdat, old = "tcpl_acsn", new = "acsn")
  longdat[, file_acsn := NULL] # remove unneeded column. Can always add back with get_acsn_map
  
  cat(paste0("\nProcessed ",basename(filei)))
  # cat(paste0("\nRun type: ",run_type))
  # if (run_type == "baseline") {
  #   cat("\nWells with wllq set to 0:",weak_wells)
  # }
  return(longdat)
}
