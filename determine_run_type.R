# Determine run type for dat1

# How I determined the run_type in fileToLongdat:
# determine run type from filei
# _00 is for baseline. _01 is for treated
# user enters run.type.tag.location in input file names (usually 5)
# e.g. TC_20190508_MW68-0808_13_00(000).csv is baseline, and TC_20190508_MW68-0808_13_01(000).csv is treated.
# ignore the 0's and 1's that come after the first 2 digits in that tag
# run.type.tag <- strsplit(basename(filei), split = "_")[[1]][run.type.tag.location]
# if (guess_run_type_later) {
#   run_type <- sub("\\.csv","",run.type.tag)
# } else{
#   run_type <- switch(substring(run.type.tag,1,2), 
#                      "00" = "baseline",
#                      "01" = "treated",
#                      sub("\\.csv","",run.type.tag))
# }
# if(!run_type %in% c('baseline','treated')) warning(paste0("\nrun type cannot be determined for ",basename(filei),'.\nNo wllq checks will be done for this recording.'))
# 

# (might transform this to apply to individual objects, then do datat table at diff level?)
determine_run_type <- function(dat1) {
  
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
       experiment_start_time_posix := as.POSIXlt('05/13/2021 12:33:00', format = c('%M/%d/%Y %H:%M:%OS'))]]

  # What would be the alternative here?
# Could I have something of a switch/multiple tries?

# Or i coudl add on seconds if they are missing?
# OR, I coudl just ignore the seconds, since that doesn't really matter?

as.POSIXlt('11/17/2020 11:37:54', format = c('%m/%d/%Y %H:%M'))
# yeah, it just adds 00 for the seconds!
as.POSIXlt('5/13/2021 12:33', format = c('%m/%d/%Y %H:%M'))


as.POSIXlt('05', format = c('%m'))
as.POSIXlt('5', format = c('%m'))


# OR, is the AM/PM a critical piece of lost information?

# how about it AM/PM is included in the text, then I will use that for determining the time
# else I will assume 24 hour time
# BUT, it seems like teh AM/PM may have been scrubbed?
dat1[grepl('AC_20210428_MW75-8205_15',srcf), .N, by = .(srcf, original_file_time, analysis_start)]
  
# So at what step woudl the AM/PM have gotten scrubbed?
# Is it possibel that R did some automatic conversion to a 24-hour time?
# What I do, def need to make this not prone to change if R ever changes its underlying methods!!
  
  # Determine the run type based on 3 other methods -> these methods are likely more reliable, but will compare them all
  dat1[, file_exp_start_time_rank := frank(experiment_start_time_posix, ties.method = 'dense'), by = .(experiment.date, plate.id)]
  dat1[, file_original_file_time_rank := frank(original_file_time_posix, ties.method = 'dense'), by = .(experiment.date, plate.id)]
  dat1[, file_name_rank := frank(srcf, ties.method = 'dense'), by = .(experiment.date, plate.id)]
  
  
  # Confirm nothing is NA
  dat1[, lapply(.SD,function(coli)sum(is.na(coli))), .SDcols = grep('file_',names(dat1),val=T)]
  # original_file_time file_exp_start_time_rank file_original_file_time_rank original_file_time_posix file_name_rank
  # 1:                  0                        0                            0                     2112              0
  # file_run_type_tag_rank
  # 1:                      0
  dat1[is.na(experiment_start_time_posix), .N] # 2112 as well
  dat1[is.na(original_file_time_posix), .N, by = .(original_file_time, srcf)]
  # original_file_time                                      srcf    N
  # 1:    5/13/2021 12:56 AC_20210428_MW75-8205_15_00(000)(000).csv 2112
  # okay, so format is different, I have updated that in tryFormats
  # BUT, in excel, it is clear that the time includes am/pm
  # but that somehow didn't get included in experiment_start_time
  # so that could lead to issues if e.g. baseline is at 11:30am and treated at 12:30 pm
  # So how to get am/pm and include in tryFormats?
  # Or just don't accommodate this format, make yourself change it in the file?
  
  test.time1 <- '5/13/2021 11:33' # intention: AM
  test.time2 <- '5/13/2021 12:45' # intention: PM
  test.time3 <- '5/13/2021 1:45' # intention: PM
  x1 <- as.POSIXlt(test.time1, tryFormats = c('%M/%d/%Y %H:%M:%OS', '%m/%d/%Y %H:%M'))
  x2 <- as.POSIXlt(test.time2, tryFormats = c('%M/%d/%Y %H:%M:%OS', '%m/%d/%Y %H:%M'))
  x3 <- as.POSIXlt(test.time3, tryFormats = c('%M/%d/%Y %H:%M:%OS', '%m/%d/%Y %H:%M'))
  sort(c(x1, x2, x3))
  # [1] "2021-05-13 01:45:00 EDT" "2021-05-13 11:33:00 EDT" "2021-05-13 12:45:00 EDT"
  # okay, so this is where we could run into issues
  # I think the program is defaulting to AM
  
  # I don't think I can programmatically handle this, I think I should through an error if it occurs
  # Perhaps add an option to manually enter the time in the proper format?
  
  # Wherever the time is NA, prompt to enter new time
  
  
  # How to compare all columns?
  # one idea...
  dat1[, multiple_unique_ranks := 
         pmax(file_run_type_tag_rank, file_exp_start_time_rank, file_original_file_time_rank, file_name_rank)
       - pmin(file_run_type_tag_rank, file_exp_start_time_rank, file_original_file_time_rank, file_name_rank)]
  
}
