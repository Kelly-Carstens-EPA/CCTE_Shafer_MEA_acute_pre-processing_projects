# function to calculate the percent change values
# result is dat.percent.change
calcActivityPercentChange <- function(dat.neural.stats) {
  
  cat("\nCollapsing treated and baseline data...")
  
  # Separate the baseline and treated data into separate tables
  bdat <- dat.neural.stats[run_type == "baseline"]
  tdat <- dat.neural.stats[run_type == "treated"]
  
  # get the  columns we need for dat.percent.change
  idcols <- intersect(c("acnm","acsn","apid","experiment.date","plate.id","well","coli","rowi",
                        "culture_folder","group_char","group_int","culture.date"),
                      names(dat.neural.stats))
  # idcols must include at least:
  # acnm or acsn
  # well or rowi + coli
  # experiment.date + plate.id
  
  # Merge baseline and treated
  bdat[, in_bdat := 1]
  tdat[, in_tdat := 1]
  dat.percent.change <- merge(bdat, 
                              tdat, 
                              by = idcols, 
                              suffixes = c(".b",".t"))
  
  # check same number of data rows in baseline and treated
  if (nrow(dat.percent.change[is.na(in_bdat) | is.na(in_tdat)]) != 0) 
    stop("Some data rows in baseline and treated do not align.\nSee dat.percent.change[is.na(in_bdat) | is.na(in_tdat)]")
  
  # check if the recording lengths are greater than 30% diff where wllq_by_recording == 1
  dat.percent.change[, analysis_duration_per_diff := (analysis_duration.t - analysis_duration.b)/((analysis_duration.t + analysis_duration.b)*0.5)*100]
  if(nrow(dat.percent.change[abs(analysis_duration_per_diff) > 30 & !(wllq_by_recording.b %in% 0 | wllq_by_recording.t %in% 0)]) != 0)
    stop("Some analysis durations for baseline and treated recordings differ by > 30% (which is an arbitrary threshold of notable difference).\nSee dat.percent.change[abs(analysis_duration_per_diff) > 30 & !(wllq_by_recording.b %in% 0 | wllq_by_recording.t %in% 0)]")
  
  # calculate the percent change in activity
  dat.percent.change[, rval := ( (activity_value.t - activity_value.b) / activity_value.b ) * 100]
  
  # Merge the wllq_by_recording across baseline and treated
  dat.percent.change[, wllq_by_recording := pmin(wllq_by_recording.b, wllq_by_recording.t, na.rm = T)]
  dat.percent.change[is.na(wllq_by_recording_notes.b), wllq_by_recording_notes.b := '']
  dat.percent.change[is.na(wllq_by_recording_notes.t), wllq_by_recording_notes.t := '']
  dat.percent.change[, wllq_notes_by_recording := paste0(wllq_by_recording_notes.t, wllq_by_recording_notes.b)]
  dat.percent.change[!is.na(wllq_notes_by_recording), wllq_ref_by_recording := 'activity level cutoffs']
  
  # Subset to desired columns
  dat.percent.change <- dat.percent.change[, .(culture.date, culture_folder, group_char,group_int, experiment.date, apid, plate.id, rowi, coli, 
                                               acnm, acsn, wllq_by_recording, wllq_notes_by_recording, wllq_ref_by_recording, rval,
                                               activity_value.t, activity_value.b,
                                               neural_stats_file.b, neural_stats_file.t, 
                                               low_ae_flag.b, high_mfr_flag.b, low_mfr_flag.b)]
  
  cat('\ndone!')
  
  return(dat.percent.change)
}
