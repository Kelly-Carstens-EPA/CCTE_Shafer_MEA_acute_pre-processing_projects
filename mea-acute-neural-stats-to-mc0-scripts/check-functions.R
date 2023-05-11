# Functions to flag common potential issues with the input data

# check the file names, make sure that they follow the naming convention of _00 for baseline and _01 for treated
checkFileNames <- function(run.type.tag.location, check.dir, dataset_title, files_log = "", guess = FALSE) {
  
  files <- read_files(check.dir, files_log)
  checknames <- basename(files)
  run.type.tags <- c()
  for (filei in files) {
    run.type.tag <- strsplit(basename(filei), split = "_")[[1]][run.type.tag.location]
    run.type.tags <- c(run.type.tags, substring(run.type.tag,1,2))
  }
  name.prefixes <- c()
  for (filei in files) {
    name.prefix <- paste0(strsplit(basename(filei), split = "_")[[1]][1:(run.type.tag.location-1)],collapse="_")
    name.prefixes <- c(name.prefixes, name.prefix)
  }
  compare.names <- data.table("filename" = checknames, "run.type.tag" = run.type.tags, "name.prefix" = name.prefixes)
  names.summary <- compare.names[, .(run.type.tags = paste0(sort(unique(run.type.tag)),collapse=","),
                                     filenames = paste0(sort(unique(filename)),collapse=",")), by = "name.prefix"]
  misnamed.files.table <- names.summary[run.type.tags != "00,01", .(filenames, run.type.tags)]
  
  if (nrow(misnamed.files.table) == 0){
    cat("\nAll files are named correctly.\n")
  }
  else if (!guess) {
    cat("\nThe following files appear to be named incorrectly:\n")
    print(misnamed.files.table)
  }
  else if (guess) {
    # sort the files, then extract the first tag for each pair of files that is different
    sort.files <- sort(checknames)
    run.type.tag.location.vector <- c()
    for (i in seq(from=1, to = length(sort.files)-1, by = 2)) {
      file1 <- strsplit(sort.files[i], split = '_')[[1]]
      file2 <- strsplit(sort.files[i+1], split = '_')[[1]]
      run.type.tag.location <- rep(which(file1 != file2)[1], 2)
      run.type.tag.location.vector <- c(run.type.tag.location.vector, run.type.tag.location)      
    }
    # output vector in same order as read from files.log
    names(run.type.tag.location.vector) <- sort.files
    run.type.tag.location.vector <- run.type.tag.location.vector[match(checknames, names(run.type.tag.location.vector))]
    cat('\nStore the run.type.tag.location.vector\n')
    return(run.type.tag.location.vector)
  }
  
  # if (nrow(misnamed.files.table) > 0 & guess) {
  #   # get the full string at run.type.tag.location, not just the first 2 digits
  #   run.type.tags <- c()
  #   for (filei in files) {
  #     run.type.tag <- strsplit(basename(filei), split = "_")[[1]][run.type.tag.location]
  #     run.type.tags <- c(run.type.tags, sub("\\.csv","",run.type.tag))
  #   }
  #   compare.names <- data.table("filename" = checknames, "run.type.tag" = run.type.tags, "name.prefix" = name.prefixes)
  #   names.summary <- compare.names[, .(run.type.tags = paste0(sort(unique(run.type.tag)),collapse=","),
  #                                      filenames = paste0(sort(unique(filename)),collapse=",")), by = "name.prefix"]
  #   
  # }
  
}


# confirm that AxIS settings are all standard. Might need to change if usign a different version
checkAxisSettings <- function(filei, file_col1, file_col2) {
  
  # If not loaded, load the "standard settings"
  if (!exists("standard_settings")) {
    load('../mea-acute-neural-stats-to-mc0-scripts/standard_settings.RData')
  }
  
  # extract the settings data
  headdat <- data.table(file_col1, file_col2)
  headdat[is.na(file_col1), file_col1 := ""] # just to standardize. I'm not sure how to control if empty comes up as NA or ""
  headdat[is.na(file_col2), file_col2 := ""]
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
  
  # remove measurements that vary by plate
  variable_values <- c("   Original File Time","   Experiment Start Time","   Plate Serial Number")
  headdat <- headdat[!(file_col1 %in% variable_values)]
  
  # a few common un-important differences
  headdat[grepl("[Ff]alse",file_col2), file_col2 := "FALSE"]
  headdat[grepl("[Tt]rue",file_col2), file_col2 := "TRUE"]
  
  filei_settings <- headdat[settings_info==T]

  # for backwards compatibility, only look at the settings in template that are also in filei
  filei_settings_entries <- unique(filei_settings$file_col1)
  test.equality <- all.equal(standard_settings[file_col1 %in% filei_settings_entries], filei_settings)
  
  if (test.equality != TRUE) {
    message <- paste0(basename(filei)," has some different settings '",test.equality,"'")
    if(grepl("file_col2",test.equality)) {
      indicies <- which(filei_settings$file_col2 != standard_settings[file_col1 %in% filei_settings_entries, file_col2])
      differences <- merge(filei_settings[indicies, .(file_col1, "file_settings" = file_col2)], 
            standard_settings[indicies, .(file_col1, "standard_settings" = file_col2)], 
            by = c("file_col1"))
      message <- paste0(message, "\n", paste0(names(differences),collapse="\t\t\t\t"))
      for (i in 1:nrow(differences)) {
        message <- paste0(message, "\n", paste0(differences[i,],collapse = "\t\t\t\t"))
      }
    }
    
    return(message)
  } 
  else {
    return(TRUE)
  }
}

checkForAllParameters <- function(filei, file_col1, file_col2) {

  # acsn_map <- get_acsn_map()
  
  # get the index of the tag phrase 'Well Averages'
  well.averages.rowi <- grep("[Ww]ell [Aa]verages", file_col1)
  
  # find the next blank line after well averages index
  next.blank.row.dist <- which(is.na(file_col1[well.averages.rowi:length(file_col1)]) | file_col1[well.averages.rowi:length(file_col1)] == "")[1]
  
  # check if the data includes all of the wanted activity measures
  missing_paramaters <- setdiff(acsn_map$file_acsn, file_col1[(well.averages.rowi + 2):(well.averages.rowi + next.blank.row.dist - 2)])
  
  # if only missing one or the other of 'Half Width at Half Height of Cross-Correlation' or 'Width at Half Height of Cross-Correlation', that is okay
  if (length(missing_paramaters) == 1 && missing_paramaters %in% c("Half Width at Half Height of Cross-Correlation","Width at Half Height of Cross-Correlation")) {
    missing_paramaters <- c()
  }
  
  if(length(missing_paramaters) > 0) {
    paste_parameters <- paste0(missing_paramaters, collapse = ", ")
    message <- paste0(basename(filei), " is missing ",paste_parameters)
    return(message)
  } 
  else {
    return(TRUE)
  }
}

checkAnalysisTiming <- function(filei, file_col1, file_col2, threshold = 500) {
  
  message <- c()
  
  # check analysis duration
  analysis.duration.rowi <- getTagPhraseIndex(file_col1, "Analysis Duration")
  analysis.duration <- file_col2[analysis.duration.rowi]
  if (abs(as.numeric(analysis.duration) - 2400) > threshold) {
    message <- paste0(basename(filei), " analysis duration is ",analysis.duration,"s")
    return(message)
  } 
  else {
    return(TRUE)
  }
  
  # checking analysis start for how long after 0
  # (not checking anymore)
  # analysis.start.rowi <- getTagPhraseIndex(file_col1, "Analysis Start")
  # analysis.start <- file_col2[analysis.start.rowi]
  # if ((as.numeric(analysis.start) - 0.0) > threshold) {
  #   message1 <- paste0(" analysis starts at ",analysis.start, "s")
  # }
}


# function to run the checks for multiple files 
runChecks <- function(files, check.settings = F, check.parameters = T, check.timing = T) {
  
  settings_summary <- c()
  parameters_summary <- c()
  timing_summary <- c()
  
  for (filei in files) {
    file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
    file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be "" or NA
    file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
    
    if (check.settings) {
      filei.settings <- checkAxisSettings(filei, file_col1, file_col2)
      settings_summary <- c(settings_summary, filei.settings)
    }
    
    if (check.parameters) {
      filei.parameters <- checkForAllParameters(filei, file_col1, filei_col2)
      parameters_summary <- c(parameters_summary, filei.parameters)
    }
    
    if (check.timing) {
      filei.timing <- checkAnalysisTiming(filei, file_col1, file_col2)
      timing_summary <- c(timing_summary, filei.timing)
    }
  }
  
  return(list("settings_summary" = settings_summary, 
              "parameters_summary" = parameters_summary, 
              "timing_summary" = timing_summary))
}

writeCheckSummary <- function(dataset_title, check.settings = F, check.parameters = T, check.timing = T,
                              run_without_sink = FALSE) {
  
  check_summary_file <- paste0(dataset_title,"_check_summary.txt")
  if(!run_without_sink) sink(check_summary_file)
  cat("Created with the script check-functions.R, writeCheckSummary() on",as.character.Date(Sys.Date()),sep = " ")
  
  # will read from the files_log in  as well as create the check_summary file in output.dir
  files <- read_files(dataset_title)
  
  # do the checks
  check.results <- runChecks(files, check.settings, check.parameters, check.timing)
  
  # wherever each check is not TRUE, summarize the results
  if (check.settings){
    settings_summary <- check.results[["settings_summary"]]
    cat("\n\nNeural Statistics Compiler Settings Check:")
    cat("\nThere are",length(which(settings_summary != TRUE)),"files with non-standard settings:\n")
    cat(settings_summary[settings_summary != TRUE], sep = "\n")
  }
  if (check.parameters){
    parameters_summary <- check.results[["parameters_summary"]]
    cat("\n\nNeural Statistics Compiler Parameters Summary:")
    cat("\nThere are",length(which(parameters_summary != TRUE)),"files with parameter data missing:\n")
    cat(parameters_summary[parameters_summary != TRUE], sep = "\n")
  }
  if (check.timing){
    timing_summary <- check.results[["timing_summary"]]
    cat("\n\nNeural Statistics Compiler Timing Summary:")
    cat("\nThere are",length(which(timing_summary != TRUE)),"files with analysis duration above or below the threshold:\n")
    cat(timing_summary[timing_summary != TRUE], sep = "\n")
  }
    
  closeAllConnections()
  cat("\n",basename(check_summary_file)," is ready.",sep="")
}


# creating the file to compare the standard settings
# this file will likely already exist, so you don't need to re-run it
createStandardSettings <- function(filei = "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA/20190529 Culture DNT G3 G4/Group 3/Neural Statistics Compiler/TC_20190529_MW69-0102_13_00(000)(000).csv") {
  
  # extract the data
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be "" or NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  
  # build the standard from file
  headdat <- data.table(file_col1, file_col2)
  headdat[is.na(file_col1), file_col1 := ""] # just to standardize. I'm not sure how to control if empty comes up as NA or ""
  headdat[is.na(file_col2), file_col2 := ""]
  headdat[, settings_header := grepl("Settings",file_col1)]
  headdat[, settings_info := FALSE]
  under_settings <- FALSE
  for (i in 1:nrow(headdat)) {
    if (headdat[i,settings_header]) {
      under_settings <- TRUE
    }
    else if (is.na(headdat[i, file_col1]) | headdat[i, file_col1] == "") {
      under_settings <- FALSE
    }
    
    if (under_settings) {
      headdat[i, settings_info := TRUE]
    }
  }
  
  # remove measurements that vary by plate
  variable_values <- c("   Original File Time","   Experiment Start Time","   Plate Serial Number")
  headdat <- headdat[!(file_col1 %in% variable_values)]
  
  # a few common un-important differences
  headdat[grepl("[Ff]alse",file_col2), file_col2 := "FALSE"]
  headdat[grepl("[Tt]rue",file_col2), file_col2 := "TRUE"]
  
  standard_settings <- headdat[settings_info==T]
  save(standard_settings, file = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts/standard_settings.RData")
}
# createStandardSettings()
