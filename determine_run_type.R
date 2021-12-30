# Determine run type for dat1

# (might transform this to apply to individual objects, then do datat table at diff level?)
determine_run_type <- function(dat1) {
  
  dat1[, date_plate := paste(experiment.date, plate.id ,sep = "_")] # don't want to just use plate.id, since it could have been re-used
  dat1[, file_exp_start_time_rank := frank(experiment.start.time, ties.method = 'dense'), by = .(date_plate)]
  dat1[, file_original_file_time_rank := frank(original.file.time, ties.method = 'dense'), by = .(date_plate)]
  dat1[, file_ending_text_rank := frank(run_type, ties.method = 'dense'), by = .(date_plate)]
  
}