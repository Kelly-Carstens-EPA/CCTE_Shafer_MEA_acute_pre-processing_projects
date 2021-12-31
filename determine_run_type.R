# Determine run type for dat1

# How I determined the run_type in fileToLongdat:
# determine run type from filei
# _00 is for baseline. _01 is for treated
# user enters run.type.tag.location in input file names (usually 5)
# e.g. TC_20190508_MW68-0808_13_00(000).csv is baseline, and TC_20190508_MW68-0808_13_01(000).csv is treated.
# ignore the 0's and 1's that come after the first 2 digits in that tag
run.type.tag <- strsplit(basename(filei), split = "_")[[1]][run.type.tag.location]
if (guess_run_type_later) {
  run_type <- sub("\\.csv","",run.type.tag)
} else{
  run_type <- switch(substring(run.type.tag,1,2), 
                     "00" = "baseline",
                     "01" = "treated",
                     sub("\\.csv","",run.type.tag))
}
if(!run_type %in% c('baseline','treated')) warning(paste0("\nrun type cannot be determined for ",basename(filei),'.\nNo wllq checks will be done for this recording.'))


# (might transform this to apply to individual objects, then do datat table at diff level?)
determine_run_type <- function(dat1) {
  
  dat1[, date_plate := paste(experiment.date, plate.id ,sep = "_")] # don't want to just use plate.id, since it could have been re-used
  
  # Convert file times from character to a comparable numeric value, e.g. POSIX?
  # e.g. as.POSIXct(..., format = ...)
  dat1[, file_exp_start_time_rank := frank(experiment.start.time, ties.method = 'dense'), by = .(date_plate)]
  dat1[, file_original_file_time_rank := frank(original.file.time, ties.method = 'dense'), by = .(date_plate)]
  dat1[, file_ending_text_rank := frank(run_type, ties.method = 'dense'), by = .(date_plate)]
  
}