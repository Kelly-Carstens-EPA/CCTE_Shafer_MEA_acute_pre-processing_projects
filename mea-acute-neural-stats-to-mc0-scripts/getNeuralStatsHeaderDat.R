getNeuralStatsHeaderDat <- function(filei) {
  
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  headdat <- data.table(file_col1, file_col2)
  
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
  # longdat[, c(settings.dat$file_col1) := lapply(settings.dat$file_col2,function(x) rep(x, nrow(longdat)))]
  
  settings.dat
  
}