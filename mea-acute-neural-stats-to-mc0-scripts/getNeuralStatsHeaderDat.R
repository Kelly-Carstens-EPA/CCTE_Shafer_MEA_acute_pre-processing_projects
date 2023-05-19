getNeuralStatsHeaderDat <- function(filei) {
  
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  headdat <- data.table(file_col1, file_col2)
  
  headdat[is.na(file_col1), file_col1 := ""] # just to standardize. I'm not sure how to control if empty comes up as NA or ""
  headdat[is.na(file_col2), file_col2 := ""]
  
  # not sure why FALSE reads as 'false' sometimes...
  headdat[grepl("[Ff]alse",file_col2), file_col2 := "FALSE"]
  headdat[grepl("[Tt]rue",file_col2), file_col2 := "TRUE"]
  
  # Identify the setting:
  
  # Example setting chunk:
  # foo bar Settings type
  #  setting 1
  #  setting 2
  #  ..
  #  setting last
  # 
  # Next section
  #  ...
  
  # So I want to repeat the setting type in a new column until a blank row is encountered
  headdat[, setting_header := grepl("Settings",file_col1)]
  setting_type_col <- c()
  setting_type <- ''
  for (i in 1:nrow(headdat)) {
    
    if (headdat[i,setting_header]) {
      setting_type <- headdat[i, file_col1]
    }
    else if (headdat[i, file_col1] == "") {
      setting_type <- ''
    }
    setting_type_col <- c(setting_type_col, setting_type)
    
  }
  headdat[, setting_type := setting_type_col]

  # Extract setting data, clean, and return  
  settings.dat <- headdat[setting_type != ''] # remove rows that don't define a setting
  settings.dat <- settings.dat[setting_header == FALSE] # can remove setting header now that it's saved under setting_type
  settings.dat[, file_col1 := sub('^[ ]+','',file_col1)] # remove leading space
  settings.dat <- settings.dat[, .(setting_type, setting = file_col1, setting_val = file_col2)]
  settings.dat[, neural_stats_file := basename(filei)]
  
  settings.dat
  
}