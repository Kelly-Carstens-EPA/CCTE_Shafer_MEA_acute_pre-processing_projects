rm(list = ls())
###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/TSCA2019/Acute TSCA Conc Response"
dataset_title <- "TSCA2019" # e.g. "name2020"
select.neural.stats.files <- F # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- ""
use_sheet <- "" # sheet name in spidmap_file
# optional adjutsment; usually can use defaults:
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl" # where the dataset_title folder will be created
override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
noisy_functions <- TRUE
standard_analysis_duration_requirement <- TRUE # default should be true
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(openxlsx)
library(stringi)

# set up folders and working directory
if (!dir.exists(file.path(root_output_dir,dataset_title))) dir.create(file.path(root_output_dir,dataset_title))
# setwd(file.path(root_output_dir,dataset_title))
main.output.dir <- getwd()
if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# source all functions in folder 'mea-acute-neural-stats-to-mc0-scripts'
scripts <- list.files(path = "mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T, recursive = F)
sapply(scripts, source)

# loading acsn_acnm map
<<<<<<< HEAD
acsn_map <- as.data.table(read.csv(file.path(root_output_dir,"neural_stats_acsn_to_tcpl_acnm_map.csv"), stringsAsFactors = F))
=======
acsn_map <- as.data.table(read.csv(file.path(root_output_dir,"neural_stats_acsn_to_tcpl_acnm_map.csv")))
>>>>>>> master
acsn_map <- acsn_map[, .(acsn, acnm)]

# Just want to get a list of all cultures in the main folder
main.folders <- list.files(path = 'L:/Lab/NHEERL_MEA/Project TSCA 2019/Acute TSCA Conc Response', pattern = '[0-9]{8}', include.dirs = T)
cat(main.folders, sep ='\n')
wb <- createWorkbook()
addWorksheet(wb, 'TSCA cultures')
writeData(wb, 1, data.table('culture_folders' = main.folders))
saveWorkbook(wb, file = 'TSCA2019/tables/TSCA2019_culture_folders_to_review.xlsx')

# How differentiate between treated and controls with this naming convention?


cat(paste0(dataset_title, " MEA Acute TCPL Level 0 Data Prep Running Log\nDate: ",as.character.Date(Sys.Date()),"\n"))
cat("\nLevel 0 - Gather and Check Files:\n")

# Scan for readme's that might affect dosing, wllq
txt.files <- list.files(path = start.dir, pattern = '\\.txt', recursive = T, full.names = T)
readmes <- txt.files[grepl('read( )*me',tolower(txt.files))]
for (readme in readmes) {
  cat(dirname(readme),'\n')
  cat(scan(readme, what = character(), sep = '\n', quiet = T), sep = '\n')
  cat('\n')
}
# empty - no readme's to check

# select input files to use, store files in .txt file
if (select.neural.stats.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
  
  # Using a new method to select files instead
  # for each culture, get all files under "Neural Statistic Compiler" folder
  # if there are not exactly 6 files per folder -> flag (including if there are just 0)
  neural.stats.files <- c()
  for (folderi in main.folders) {
    add.files <- list.files(path = file.path('L:/Lab/NHEERL_MEA/Project TSCA 2019/Acute TSCA Conc Response',folderi,'Neural Statistic Compiler'), pattern = '\\.csv', full.names = T,
                            recursive = T)
    neural.stats.files <- c(neural.stats.files, add.files)
  }
  length(neural.stats.files) # 255
  
  # any cultures not match any files?
  neural.stats.files.folders <- basename(dirname(dirname(neural.stats.files)))
  setdiff(main.folders, neural.stats.files.folders)
  # empty -> good
  
  # Any cultures not match exactly 6 files?
  counts <- table(neural.stats.files.folders)
  counts[counts != 6]
  # neural.stats.files.folders
  # 20201125 Culture G2 20210512 Culture G24 (200H02 may need repeat) 
  #                   8                                             7
  
  # 20201125 - this is because of the issue with the break in recording
  # I'm guessing that the file labelled "treated" is the attempt to calculate the averaged values
  # However, the "treated" file is slighlty problematic for 2 reasons:
  # - it doesn't contain any parameter data in the header
  # - I don't understand how the values were calculated from the other 2 files 
  #   (doesn't quite line up with sums, averages or time-weighted averages as I would guess)
  # So I have created a version of a consensus file that I will use instead
  neural.stats.files <- neural.stats.files[!basename(neural.stats.files) %in% c('AC_20201125_MW71-7113_13_00(001)(000).csv','AC_20201125_MW71-7113_13_00(002)(001).csv')]
  
  # 20210512 - there are 3 files for 75-8213
  # Looking at AC_20210512_MW75-8213_13_35(001)(000).csv, it only contains a few rows of data, with none of the usual headers
  # Is not usable in current state, will assume that the 002 version if what we want
  neural.stats.files <- neural.stats.files[!basename(neural.stats.files) %in% c('AC_20210512_MW75-8213_13_35(001)(000).csv')]
  
  # REcheck
  neural.stats.files.folders <- basename(dirname(dirname(neural.stats.files)))
  counts <- table(neural.stats.files.folders)
  counts[counts != 6] #empty, good
  
  # Write files to log
  current.neural.files <- read_files(main.output.dir, files_type = 'neural_stats')
  setdiff(basename(current.neural.files), basename(neural.stats.files))
  # empty -> so there is nothign that I selected previously that I have not included
  writeLogFile(neural.stats.files, main.output.dir, dataset_title, files_type = 'neural_stats')
  
}


if (select.calculations.files) {
  # selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
  
  # Using new method:
  
  # Get cyto files (need plate maps)
  calc.files <- c()
  for (folderi in main.folders) {
    add.files <- list.files(path = file.path('L:/Lab/NHEERL_MEA/Project TSCA 2019/Acute TSCA Conc Response',folderi), pattern = '\\.xlsx', full.names = T,
                            recursive = F)
    calc.files <- c(calc.files, add.files)
  }
  # Remove the dummy "ghost" files
  calc.files <- Filter(f = function(x) !grepl('\\~\\$',basename(x)), calc.files)
  length(calc.files) # 37
  
  # any cultures not match any files?
  calc.files.folders <- basename(dirname(calc.files))
  setdiff(main.folders, calc.files.folders)
  # empty -> good
  
  # Any cultures not more than 1 file?
  counts <- table(calc.files.folders)
  counts[counts != 1] # empty!
  
  current.calc.files <- read_files(main.output.dir, files_type = 'calculations')
  # error -> there is no calc file currently
  # setdiff(basename(current.calc.files), basename(calc.files))
  writeLogFile(calc.files, main.output.dir, dataset_title, files_type = 'calculations')
  
}


# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
run.type.tag.location <- checkFileNames(run.type.tag.location, main.output.dir, dataset_title, guess = T)
# OUTPUT --------------------------------------------------------- 
# Reading from TSCA2019_neural_stats_files_log_2021-10-25.txt...
# Got 220 files.
# Store the run.type.tag.location.vector
# ---------------------------------------------------------------- 
unique(run.type.tag.location) # 5 6 4


# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT --------------------------------------------------------- 
# TSCA2019_check_summary_2021-12-16.txt is ready.
# ---------------------------------------------------------------- 
# all parameters present in every file
# 1 file seems to have an analysis duration that is far too short:
# AC_20210512_MW75-8213_13_35(002)(000).csv analysis duration is 420.75s
# only about 7 min
# I will ask Kathleen if this is the only version of the 

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location, append = T)
<<<<<<< HEAD

# Troubleshooting an error: 
# get the files from the files_log that are not already in dat1
files_logs <- list.files(path = main.output.dir, pattern = paste0("neural_stats_files_log_"), recursive = F, full.names = T)
files_log <- files_logs[order(basename(files_logs), decreasing = T)[1]] # get the most recent files log
all_files <- read_files(output.dir, files_log)
i <- 1
test1 <- fileToLongdat(all_files[i], run.type.tag.location[i], plate.id.tag.location = numeric(0), guess_run_type_later = F)
str(test1)
ncol(test1) # 19
# these updates might have fixed it

extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location, append = T)

# I think there might be something wrong with file 6
i <- 6
test6 <- fileToLongdat(all_files[i], run.type.tag.location[i], plate.id.tag.location = numeric(0), guess_run_type_later = F)
str(test6)
# hmm, looks fine to me
ncol(test6) # 19
setdiff(names(test1), names(test6)) # empty

extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location, append = T)

# maybe actually file #43?
i <- 43
test43 <- fileToLongdat(all_files[i], run.type.tag.location[i], plate.id.tag.location = numeric(0), guess_run_type_later = F)
str(test43)
# nope, totall fine

# Why am I getting this error???!

# oh wait, I had set append = TRUE :(((((((((
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location, append = F)
# success!
# With warnings that 6 plates don't have a plate id
# (I realized this is because they don't have a plate id in the file header,
# bc ran with different version of software)


# RESUME HERE --------------------------------
# Motivation: run type tags in these files are quite inconsistent
# Would be so much easier to just use the file times as reported by axis
# dat1 from 12/30/2021 was made with updated level 1 functions
# contains exp start time, original file time,
# but no wllq or run type

# Any values NA?
dat1 <- get_latest_dat('dat1', dataset_title)
dat1[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = names(dat1)]
# So the activity_value is the only column that is NA sometimes -> didn't actually have to use fill = T

# Resolving warnings of missing plate.id's
dat1[plate.id == 'MW', .N, by = .(experiment.date, srcf)]
#    experiment.date                                                              srcf    N
# 1:        20210824 AC_20210811_MW75-9119_13(000)_Neural Statistics Compiler(000).csv 2064
# 2:        20210824 AC_20210811_MW75-9119_13(001)_Neural Statistics Compiler(000).csv 2064
# 3:        20210824 AC_20210811_MW75-9120_13(000)_Neural Statistics Compiler(000).csv 2064
# 4:        20210824 AC_20210811_MW75-9120_13(001)_Neural Statistics Compiler(000).csv 2064
# 5:        20210824 AC_20210811_MW75-9201_13(000)_Neural Statistics Compiler(000).csv 2064
# 6:        20210824 AC_20210811_MW75-9201_13(001)_Neural Statistics Compiler(000).csv 2064
dat1[plate.id == 'MW', plate.id := stri_extract_first(srcf, regex = 'MW[0-9\\-]{7}')]
dat1[experiment.date == '20210824', .N, by = .(srcf, plate.id)]
#                                                                 srcf  plate.id    N
# 1: AC_20210811_MW75-9119_13(000)_Neural Statistics Compiler(000).csv MW75-9119 2064
# 2: AC_20210811_MW75-9119_13(001)_Neural Statistics Compiler(000).csv MW75-9119 2064
# 3: AC_20210811_MW75-9120_13(000)_Neural Statistics Compiler(000).csv MW75-9120 2064
# 4: AC_20210811_MW75-9120_13(001)_Neural Statistics Compiler(000).csv MW75-9120 2064
# 5: AC_20210811_MW75-9201_13(000)_Neural Statistics Compiler(000).csv MW75-9201 2064
# 6: AC_20210811_MW75-9201_13(001)_Neural Statistics Compiler(000).csv MW75-9201 2064
# looks good!!

# Next step is to assign the run_type
# (see determine_run_type -> next need to address some formatting in times, then how to check all 5 run type assignment methods),
# followed by assigning the wllq
# Then make this flow into existing level 2

# Determine the run type based on ordering of the run_type tag
# (which was determined by the first tag in the file names that 
# is unique for each pair of consecutive files when sort by file name) 
file.names.split <- stri_split(str = names(run.type.tag.location), fixed = '_')
run.type.tags <- unlist(lapply(1:length(run.type.tag.location), function(i) file.names.split[i][[1]][run.type.tag.location[i]]))
run.type.tag.location.tb <- data.table('srcf' = names(run.type.tag.location), 
                                       'run.type.tag.location' = run.type.tag.location,
                                       'run.type.tag' = run.type.tags)
dat1 <- merge(dat1, run.type.tag.location.tb, by = 'srcf', all.x = T)
dat1[, file_run_type_tag_rank := frank(run.type.tag, ties.method = 'dense'), by = .(experiment.date, plate.id)]

# Convert file times from character to a comparable numeric value, e.g. POSIX?
# e.g. as.POSIXct(..., format = ...)
dat1[, experiment_start_time_posix := as.POSIXlt(experiment_start_time, format = c('%M/%d/%Y %H:%M:%OS')), by = .(srcf)]
dat1[, original_file_time_posix := as.POSIXlt(original_file_time, format = c('%M/%d/%Y %H:%M:%OS')), by = .(srcf)]

# Check for and fix any NAs in POSIX file time (time may be in incorrect format, or AM/PM did not copy from csv)
dat1[is.na(experiment_start_time_posix) | is.na(original_file_time_posix), .N, by = .(srcf, original_file_time, experiment_start_time)]
#                                         srcf original_file_time experiment_start_time    N
# 1: AC_20210428_MW75-8205_15_00(000)(000).csv    5/13/2021 12:56       5/13/2021 12:33 2112
# How does the treated file look?
dat1[grepl('AC_20210428_MW75-8205_15',srcf), .N, by = .(srcf, original_file_time)]
#                                         srcf  original_file_time    N
# 1: AC_20210428_MW75-8205_15_00(000)(000).csv     5/13/2021 12:56 2112
# 2: AC_20210428_MW75-8205_15_00(001)(000).csv 05/13/2021 14:03:00 2112
# huh, so the dates in the treated file are formatted normally
# I confirmed in the csv file that 5/13/2021 12:56 is "PM" ;)
dat1[srcf == 'AC_20210428_MW75-8205_15_00(000)(000).csv', 
     original_file_time_posix := as.POSIXlt('05/13/2021 12:56:00', format = c('%M/%d/%Y %H:%M:%OS'))]
dat1[srcf == 'AC_20210428_MW75-8205_15_00(000)(000).csv', 
     experiment_start_time_posix := as.POSIXlt('05/13/2021 12:33:00', format = c('%M/%d/%Y %H:%M:%OS'))]

# code snip for Kelly
dat1[srcf == 'AC_20210428_MW75-8205_15_00(000)(000).csv', .N] # 2112
as.POSIXlt('05/13/2021 12:56:00', format = c('%M/%d/%Y %H:%M:%OS'))
dat1[srcf == 'AC_20210428_MW75-8205_15_00(000)(000).csv', original_file_time_posix := as.POSIXlt('05/13/2021 12:56:00', format = c('%M/%d/%Y %H:%M:%OS'))]
dat1[srcf == 'AC_20210428_MW75-8205_15_00(000)(000).csv', .N, by = .(original_file_time_posix)]
class(as.POSIXlt('05/13/2021 12:56:00', format = c('%M/%d/%Y %H:%M:%OS')))
class(dat1$original_file_time_posix)

dat1[srcf == 'AC_20210428_MW75-8205_15_00(000)(000).csv', 
     original_file_time_posix := as.Date(as.POSIXlt('05/13/2021 12:56:00', format = c('%M/%d/%Y %H:%M:%OS')))]

dat1[, original_file_time_posix := as.Date(as.POSIXlt('05/13/2021 12:56:00', format = c('%M/%d/%Y %H:%M:%OS')))]
dat1[, original_file_time_posix2 := as.POSIXlt('05/13/2021 12:56:00', format = c('%M/%d/%Y %H:%M:%OS'))]


# Determine the run type based on 3 other methods -> these methods are likely more reliable, but will compare them all
dat1[, file_exp_start_time_rank := frank(experiment_start_time_posix, ties.method = 'dense'), by = .(experiment.date, plate.id)]
dat1[, file_original_file_time_rank := frank(original_file_time_posix, ties.method = 'dense'), by = .(experiment.date, plate.id)]
dat1[, file_name_rank := frank(srcf, ties.method = 'dense'), by = .(experiment.date, plate.id)]

# How to compare all columns?
# one idea...
dat1[, multiple_unique_ranks := 
       pmax(file_run_type_tag_rank, file_exp_start_time_rank, file_original_file_time_rank, file_name_rank)
     - pmin(file_run_type_tag_rank, file_exp_start_time_rank, file_original_file_time_rank, file_name_rank)]




=======
>>>>>>> master
# OUTPUT --------------------------------------------------------- 
# Level 1 - Extract All Data:
#   
#   Reading from TSCA2019_neural_stats_files_log_2021-12-16.txt...
# Got 220 files.
# Reading data from files...
# Processed AC_20201125_MW71-7111_13_00(001)(000).csv 
# Processed AC_20201125_MW71-7111_13_00(002)(000).csv 
# Processed AC_20201125_MW71-7112_13_00(000)(000).csv 
# Processed AC_20201125_MW71-7112_13_00(001)(000).csv 
# Processed AC_20201125_MW71-7115_15_00(000)(000).csv 
# Processed AC_20201125_MW71-7115_15_00(001)(000).csv 
# ...
# TSCA2019_dat1_2021-12-16.RData is ready.
# Summary of dates/plates with wllq=0 at Level 1:
# (73 plates afffected)
# over 50 instances of this warning:
# Warning messages:
# 1: In fileToLongdat(new_files[i], run.type.tag.location[i],  ... : 
#                       run type cannot be determined for AC_20210505_MW75-8207_13_35(000)(000).csv.
#                     No wllq checks will be done for this recording.
# ---------------------------------------------------------------- 

# Options:
# - try to determine how the function works to determine the run type, edi tit so that all my file names will be interpretted correctly
# - just assign the run_type here, and re-do the wllq assignments here

# Let's just review how the function works,
# then see how bad this situation is (can I write a simple rule, or much easier to just fix on a case by case basis here?)

# How filetoLongdat guesses the run type:
run_type <- switch(substring(run.type.tag,1,2), 
                   "00" = "baseline",
                   "01" = "treated",
                   sub("\\.csv","",run.type.tag))
# okay, so this is pretty rigid

# view dat1
dat1 <- get_latest_dat(lvl = "dat1", dataset_title)

dat1[, .N, by = .(run_type)]
#        run_type      N
# 1:     baseline 301344
# 2: 35(000)(000)  40128
# 3: 35(001)(000)  38016
# 4: 35(002)(000)  10560
# 5: 35(003)(000)   8448
# 6: 35(004)(000)   8448
# 7: 35(005)(000)   8448
# 8: 35(006)(000)   4224
# 9: 35(007)(000)   4224
# 10: 35(008)(000)   4224
# 11: 35(009)(000)   4224
# 12: 35(010)(000)   4224
# 13: 35(011)(000)   4224
# 14: 15(000)(000)   4224
# 15: 15(001)(000)   4224

# Hmm... I'm curious if all/most of those not labelled baseline are just treated,
# perhaps not much to sort through?
dat1[, .N, by = .(run_type == 'baseline' | run_type == '35(000)(000)')]
# run_type      N
# 1:     TRUE 341472
# 2:    FALSE 107712
# Nope, there are far more that are currently labelled "baseline",
# So I'm guessing that several that are labelled baseline
# are not actually baseline

dat1[run_type == 'baseline', .N, by = .(srcf)]
#                                         srcf    N
# 1: AC_20201104_MW71-7104_13_00(000)(000).csv 2064
# 2: AC_20201104_MW71-7104_13_00(001)(000).csv 2064
# 3: AC_20201104_MW71-7105_13_00(001)(000).csv 2064
# 4: AC_20201104_MW71-7105_13_00(002)(000).csv 2064
# Oh yeah, this is definitely not right


# RESUME HERE -------------------------------------------------------------

# Try to figure out how to assign the run type,
# first just in this code, then see if you can translate a rule for most cases to fileToLongdat()
# If you need to ask Kathleen or ask her to rename in some cases, that's valid too
# But also thinking about the future... we need something that's goign to be dummy-proof
# (either a hard and fast naming rule, or )


# other things could check (from running this a logn time ago:) ------------
print(dat1[, .N/length(unique(dat1$acnm)), by = "wllq_notes"])
# view all experiment.date's and plate.id's. Are there any NA/missing labels?

# making sure the baseline/treatment labelling looks correct
dat1[, .N, by = .(apid, srcf, run_type)] # oh dear, all are labelled baseline rn!

# add teh run type tag location to each srcf
run.type.tag.tb <- data.table(srcf = names(run.type.tag.location), run.type.tag.location = run.type.tag.location)
dat1 <- merge(dat1, run.type.tag.tb, by = 'srcf')
dat1[run.type.tag.location == 5, run.type.tag := stri_replace_all_regex(srcf, pattern = paste0(c(rep('[^_]*_',times=4)),collapse=''), replacement = '')]
dat1[run.type.tag.location == 6, run.type.tag := stri_replace_all_regex(srcf, pattern = paste0(c(rep('[^_]*_',times=5)),collapse=''), replacement = '')]
dat1[, run.type.tag := stri_replace_all_regex(run.type.tag, pattern = '\\.csv', '')]
dat1[, baseline.run.type.tag := sort(unique(run.type.tag))[1], by = .(apid, plate.id, experiment.date)]
dat1[, run_type := ifelse(run.type.tag == baseline.run.type.tag, 'baseline', 'treated')]
dat1[, .N, by = .(apid, srcf, run_type, run.type.tag)] # looks good!

# At Kathleen's request, getting the recording name for each file
extract_recording_name <-  function(filei) {
  
  file_scan <- scan(file = filei, what = character(), sep = "\n", blank.lines.skip = F, quiet=T) # empty lines will be just ""
  file_col1 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][1], USE.NAMES = F) # empty lines will be NA
  file_col2 <- sapply(file_scan, function(x) strsplit(x, split = ",")[[1]][2], USE.NAMES = F) # if nothing in second col, will be NA
  
  # get relevant data from file header
  headdat <- data.table(file_col1, file_col2)
  
  rec.name <- headdat[grepl('Recording Name',file_col1), file_col1]
  return(data.table(recording_name = rec.name, srcf = basename(filei)))
  
}

files <- read_files(main.output.dir)
# Reading from TSCA2019_neural_stats_files_log_2021-05-10.txt...
# Got 84 files.
setdiff(basename(files), unique(dat1$srcf)) # empty!
add.dat <- data.table()
for (filei in files) {
  add.dat <- rbind(add.dat, extract_recording_name(filei))
}
str(add.dat)
dat1 <- merge(dat1, add.dat, by = 'srcf', all = T)
dat1[is.na(recording_name)] # empty
dat1[is.na(run_type)] # empty
rm(add.dat)

# export the requested data to excel
# I'm going to try to note the group, from the srcf name
filename.tb <- data.table(srcf = basename(files), fullname = files)
dat1 <- merge(dat1, filename.tb, by = 'srcf', all = T)
dat1[, .N, by = .(fullname)]
dat1[, culture_folder := basename(dirname(dirname(fullname)))]
dat1[, .N, by = .(culture_folder)]

# I'm pretty sure they won't want the txt prefix in the recording name
dat1[, recording_name := sub('Recording Name: ','',recording_name)]
dat1[, culture_date := sub(' .*$','',culture_folder)]
dat1[culture_date != recording_name, .N, by = .(culture_date, recording_name)]
# not all equa
dat1[experiment.date != recording_name, .N, by = .(culture_date, recording_name)] # many cases
# I guess I'll include all 3 for now
dat1[, group := sub('^[^G]* ','',culture_folder)]
dat1[, .N, by = .(group)]

prep.dat <- dat1[run_type == 'baseline' & grepl('^2021',experiment.date) & acsn %in% c('Weighted Mean Firing Rate (Hz)','Number of Active Electrodes'), 
                 .(culture_date, group, experiment.date, recording_name, plate.id, well, acsn, activity_value, wllq, wllq_notes)]
setnames(prep.dat, old = c('experiment.date','plate.id'), new = c('experiment_date','plate'))
prep.dat2 <- dcast(prep.dat, culture_date + group + experiment_date + recording_name + plate + well + wllq + wllq_notes ~ acsn, value.var = 'activity_value')
prep.dat2

prep.dat2[, .N, by = .(culture_date, group, experiment_date)]

wb <- createWorkbook()
openxlsx::addWorksheet(wb, sheetName = 'Sheet1')
writeData(wb, sheet = 1, x = prep.dat2)
saveWorkbook(wb, file = 'TSCA2019_MEA_Acute_baseline_recordings_from_2021.xlsx', overwrite = T)
rm(dat1)



# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir, dataset_title, main.dir = root_output_dir)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# look at data so far
dat2 <- get_latest_dat(lvl = "dat2", dataset_title)
dat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
# 
#
# ---------------------------------------------------------------- 
rm(dat2)

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 
rm(cytodat)

# load dat3 and finalize it
cat("\n\nLevel 4 - Finalize well ID information:\n")
dat4 <- get_latest_dat(lvl = "dat3", dataset_title)
dat4[, dat2 := NULL]
dat4[, dat3 := basename(RData_files_used)]


# FINALIZE WLLQ
cat("\nFinalize Wllq:")
# set wllq to zero where rval is NA
cat("\nNA rval's:",dat4[wllq==1 & is.na(rval),.N])
#
cat("\nInf rval's (baseline==0):",dat4[wllq==1 & is.infinite(rval),.N])
# 
dat4[is.na(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is NA; "))]
dat4[is.infinite(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is Inf; "))]
cat("\nWell quality set to 0 for these rval's.\n")

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.
# for example, updateWllq(dat4, date = "20190530", plate = "MW68-0807", well = "C6", wllq_note = "Contamination", override_check = override_wllq_checks)

# start a pdf to save the summary graphs
graphics.off()
pdf(file = file.path(main.output.dir, paste0(dataset_title, "_summary_figures_report_",as.character.Date(Sys.Date()),".pdf")), width = 10, height = 8)

# VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS

cat("\nVerifying control compound labels:\n")
# view and standardize treatment names, so can compare all relevant values below
dat4[, .N, by = "treatment"]
dat4[grepl("DMSO",treatment), treatment := "DMSO"]

# visually confirm if the PICRO, TTX, LYSIS were added before the second recording for MEA endpoints
# varies across experiments, sometimes across days
# if not, the PICRO, TTX, LYSIS wells only contained media for the MEA endpoints
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","? Lysis","1:250 LDH","1:2500 LDH") & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean"]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# RESPONSE:
# yes/no, it appears that the PICRO, TTX, LYSIS were added before the second treatment
# rename the treatment in the wells as needed

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Re-label the treatments to refect this

# for Cell Titer Blue assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","? Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")
# make updates if needed
# dat4[, AB.trt.finalized := FALSE] # set this to TRUE for individual plates as you update as needed
# 
# # for every other culture, the "Media" well in F1 contains Lysis at the time of the AB reading (or could change by well F1 vs by the name "Media"...)
# dat4[AB.trt.finalized == FALSE & grepl("AB",acnm) & treatment == "Media", .(plate.id, experiment.date, rowi, coli, wllq, rval, wllq_notes)] # all are in row 6, col 1
# dat4[AB.trt.finalized == FALSE & grepl("AB",acnm) & treatment == "Media", `:=`(treatment = "Lysis",conc = 10, AB.trt.finalized = TRUE)]

# # view updated stripchart
# plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","? Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acnm)]
# view_activity_stripchart(plotdat, title_additions = "Media renamed to Lysis")

# for LDH assay:
plotdat <- dat4[treatment %in% c("DMSO","PICRO","TTX","BIC","Media","Lysis","? Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acnm)]
view_activity_stripchart(plotdat, title_additions = "No Changes to Treatment Labels")

# looks like media wells really do just contain Media
# actually makes some sense based on the assay - 
# first, 50uL of culture Media from each well in MEA plate is transfered to LDH plate (so F1 just contains Media in LDH plate)
# then 20uL of Media is added to F1 in the MEA plate. Then (all?) of the contents are removed and mixed
# a Media/Alamar Blue mixture is added to eadch well of MEA plate
# then culture Media/Blue mixutre is taken from MEA plates to CTB plates
# all this to say, I understand now why the 
# The only time when well F1 would have Lysis in the LDH assay is the same time when Lysis is added before the second recording,
# which was already clearly marked in the Group 1 file. 
# So LDH well treatments should usually = neural stats well treatments, if Lysis, PICRO, etc. were added before the second recording

# final treatment updates:
cat("Confirm that the rest of these treatments look normal (nothing NA, 0, etc):\n")
cat(dat4[, unique(treatment)], sep = ", ")
cat("\n")


# ASSIGN SPIDS
cat("\nAssign spid's:\n")
cat("Using spidmap file:",spidmap_file,"\n")
spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
names(spidmap)
setnames(spidmap, old = "NCCT ID", new = "spid")
setnames(spidmap, old = "Chemical ID", new = "treatment")
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1]   
dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] 
dat4[grepl("DMSO",treatment), spid := "DMSO"]
dat4[treatment == "Media", spid := "Media"]
dat4[treatment == "PICRO", spid := "Picrotoxin"]
dat4[treatment == "TTX", spid := "Tetrodotoxin"]
dat4[grepl("Lysis",treatment), spid := "Tritonx100"]
dat4[grepl("Lysis",treatment), unique(conc), by = "treatment"]
unique(dat4$spid) # confirm no NA spids
if(any(is.na(unique(dat4$spid)))) {
  stop(paste0("The following treatments don't have a corresponding spid:", dat4[is.na(spid), unique(treatment)]))
} else {
  cat("No spids are NA.\n")
}
cat("Number of unique spids:",dat4[,length(unique(spid))],"\n")


# PREPARE LDH P WELLS (must verify wllq, treatments first)
dat4 <- prepare_LDH_p_wells(dat4)


# ASSIGN WLLT
dat4 <- assign_wllt(dat4)


# CHECK CONC'S
cat("\nFinalize Concentrations:\n")
dat4[, conc_original := conc]
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done?

# update conc for DMSO, PICRO, TTX, BIC, and full Lysis wells
# dmso
dat4[treatment == "DMSO",unique(conc)]
# [1] "Control"
# Use the percent DMSO by volume?
# dat4[treatment == "DMSO", conc := "0.001"]

# picro
dat4[treatment == "PICRO", .N, by = "conc"]
# 
# based on lab notebook, this is usually 25
# dat4[treatment == "PICRO", conc := "25"]

# ttx
dat4[treatment == "TTX", .N, by = "conc"]
# 
# based on lab notebook, this is usually 1
# dat4[treatment == "TTX", conc := "1"]

cat("\nConcentration Corrections:\n")
# any other compounds to update??
# need to do concentration correction??
cat("CHANGES MADE/rationale")
# cat("The following treatment have char conc. Will be set to NA:\n")
# print(suppressWarnings(dat4[is.na(as.numeric(conc)), .N, by = c("spid","treatment","conc")]))
# dat4[, conc := suppressWarnings(as.numeric(conc))]

# final updates, view conc's, make table of control conc's
dat4 <- assign_common_conc(dat4)


# ASSIGN ACID
cat("\nAssign ACId:\n")
cat("(not doing this for now, since new acnm's need to be registered)\n")
# dat4 <- add_acid(dat4) # holding off, need to register new acid's


# check that all data is there, nothing is missing, view plots
data_checks(dat4)

# closing graphics after last plots
graphics.off()

# create a nice summary of wllq assignments for each well
createWllqSummary(dat4, dataset_title)
cat("(note that the wllq is not quite final -\nwllq will be updated for outlier DMSO wells will before creating lvl 0 snapshot)\n")

# save dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acnm, wllt, wllq, wllq_notes, rval, srcf, dat3)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))
cat("\ndat4 saved on:",as.character.Date(Sys.Date()), "\n")

# you're done!
