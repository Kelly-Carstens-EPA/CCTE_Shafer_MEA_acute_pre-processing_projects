# deprecated functions, that I might use again

# previous flags in filetoalongData
# check if the data includes all of the wanted acitivity measure
acsn_map <- get_acsn_map()
missing_paramaters <- setdiff(acsn_map$file_acsn, file_col1[(well.averages.rowi + 2):(well.averages.rowi + next.blank.row.dist - 2)])
if (length(missing_paramaters) > 0) warning(paste0("\n",basename(filei)," is missing ",missing_paramaters))

# might be able to remove this later, if enough confirmations that all okay
analysis.start.rowi <- getTagPhraseIndex(file_col1, "Analysis Start")
analysis.start <- file_col2[analysis.start.rowi]
if (as.numeric(analysis.start) != 0.0) {
  if (as.numeric(analysis.start) < 30.0) {
    warning(paste0("\nAnalysis start is greater than zero (",analysis.start, "s) for ",basename(filei)))
  } else {
    warning(paste0("\nAnalysis start is greater than zero (",analysis.start, "s) for ",basename(filei)))
  }
}

# check analysis duration
analysis.durn.rowi <- getTagPhraseIndex(file_col1, "Analysis Duration")
analysis.durn <- file_col2[analysis.durn.rowi]
if (abs(2400 - as.numeric(analysis.durn)) > 30.0) {
  warning(paste0("\nAnalysis duration is (",analysis.durn, "s) for ",basename(filei)))
}

if (flags_only) return(0)



checkNamesOld <- function() {
checknames <- unlist(strsplit(basename(files), split = "\\.csv"))

# remove these 2 recordings - DIVs named differently
checknames <- checknames[!(checknames %in% c("TC_20190612_MW69-0114_15_00(000)(001)","TC_20190612_MW69-0114_14_01(000)(001)"))]

name_prefix <- sub("_[01]{2}(\\([01]{3}\\)){,3}","",checknames)
# a few exceptions
name_prefix <- sub("\\(002\\)","",name_prefix)

name.ending <- c()
for (i in 1:length(checknames)) {
  add.ending <- sub(name_prefix[i], "", checknames[i])
  name.ending <- c(name.ending, add.ending)
}
name_table <- data.table(full.name = checknames, name.prefix = name_prefix, name.ending = name.ending)
name_table[, c("file1","file2","dummy_file3"):=list(sort(unique(name.ending))[1], sort(unique(name.ending))[2], sort(unique(name.ending))[3]), by = "name.prefix"]
unique(name_table[, .(file1, file2, dummy_file3)])
summary.table <- name_table[, .(num_plates = .N), by = c("file1","file2")]
summary.table[order(-num_plates)]
# file1              file2 num_plates
# 1:      _00(000)(000)      _00(001)(000)         22
# 2:      _00(000)(001)      _00(001)(001)         14
# 3:      _00(000)(001)      _01(000)(001)          8
# 4:      _00(000)(000)      _01(001)(000)          6
# 5:      _00(000)(000)      _01(000)(000)          6
# 6: _00(000)(000)(000) _01(000)(000)(000)          4
# 7: _00(000)(001)(000)      _00(001)(000)          2
# 8: _00(000)(000)(000) _01(001)(000)(000)          2
# 9:      _00(000)(002)      _01(000)(002)          2
# 10:      _00(000)(001)      _00(001)(000)          2
# 11:      _00(000)(000)      _00(001)(001)          2
# 12:      _01(000)(001)               <NA>          1
cat(summary.table[order(-num_plates)])
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

checkForAllParameters <- function(files) {
  files_with_missing_parameters <- 0
  acsn_map <- get_acsn_map()
  
  for (filei in files) {
    file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
    file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
    file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
    
    # get the index of the tag phrase 'Well Averages'
    well.averages.rowi <- grep("[Ww]ell [Aa]verages", file_col1)
    
    # find the next blank line after well averages index
    next.blank.row.dist <- which(is.na(file_col1[well.averages.rowi:length(file_col1)]))[1]
    
    # check if the data includes all of the wanted acitivity measure
    missing_paramaters <- setdiff(acsn_map$file_acsn, file_col1[(well.averages.rowi + 2):(well.averages.rowi + next.blank.row.dist - 2)])
    # if (length(missing_paramaters) > 0) cat(paste0("\n",basename(filei)," is missing ",missing_paramaters))
    # else cat("\n",paste0(basename(filei)," has all parameters."),sep = "")
    if (length(missing_paramaters) > 0) {
      cat(paste0("\n",basename(filei)," is missing ",missing_paramaters))
      files_with_missing_parameters <- files_with_missing_parameters + 1
    }
  }
  cat("\nThere are ",files_with_missing_parameters," that are missing some parameter data.",sep = "")
}

writeFlagSummary <- function(output.dir, dataset_title, log_file = "", threshold = 10) {
  
  # leave log_file == "", and read_files will read from the most recent files_log in output.dir
  # if you want a specific file_log, you can specify with log_file
  if (log_file == "") {
    # read the data from the most recent log_file in check.dir
    log_files <- list.files(path = path.expand(output.dir), pattern = "_files_log_", recursive = F, full.names = T)
    log_file <- log_files[order(basename(log_files), decreasing = T)[1]] # get the most recent log file
  }
  
  files <- read_files(output.dir, log_file)
  
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
  
  durations_summary_file <- file.path(output.dir, paste0(dataset_title, "_analysis_start_summary_",as.character.Date(Sys.Date()),".txt"))
  
  sink(durations_summary_file)
  
  cat("Created with the script data_prep-functions.R, writeFlagSummary() on",as.character.Date(Sys.Date()),sep = " ")
  cat("\nReading files from",log_file)
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

