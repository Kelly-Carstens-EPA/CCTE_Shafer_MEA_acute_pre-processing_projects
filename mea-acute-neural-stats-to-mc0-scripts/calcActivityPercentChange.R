# function to calculate the percent change values
# result is dat2
calcActivityPercentChange <- function(project_name) {
  
  # read the data from the most recent dat1 RData file
  cat("\n\nLevel 2 - Collapse Data by Plate ID:\n")
  cat("\nLoading...\n")
  dat1 <- get_latest_dat(lvl = "dat1",project_name)
  
  cat("\nCollapsing treated and baseline data...")

  # Separate the baseline and treated data into separate tables
  bdat <- dat1[run_type == "baseline"]
  tdat <- dat1[run_type == "treated"]
  
  # get the  columns we need for dat2
  usecols <- c("acnm","apid","experiment.date","plate.id","well","coli","rowi",
               "culture_folder","group_char","group_int","culture.date",
               # columns that differ for baseline vs treated
               "activity_value", "wllq_lvl1","wllq_lvl1_notes","neural_stats_file","analysis_start","analysis_duration")
  
  # Merge baseline and treated
  bdat[, in_bdat := 1]
  tdat[, in_tdat := 1]
  dat2 <- merge(bdat[, .SD, .SDcols = c(usecols,'in_bdat')], 
                tdat[, .SD, .SDcols = c(usecols,'in_tdat')], 
                by = c("acnm","apid","experiment.date","plate.id","well","coli","rowi",
                       "culture_folder","group_char","group_int","culture.date"), 
                suffixes = c(".b",".t"))
  
  # check same number of data rows in baseline and treated
  if (nrow(dat2[is.na(in_bdat) | is.na(in_tdat)]) != 0) 
    stop("Some data rows in baseline and treated do not align.\nSee dat2[is.na(in_bdat) | is.na(in_tdat)]")
  
  # check if the recording lengths are greater than 30% diff where wllq_lvl1 == 1
  dat2[, analysis_duration_per_diff := (analysis_duration.t - analysis_duration.b)/((analysis_duration.t + analysis_duration.b)*0.5)*100]
  if(nrow(dat2[abs(analysis_duration_per_diff) > 30 & !(wllq_lvl1.b %in% 0 | wllq_lvl1.t %in% 0)]) != 0)
    stop("Some analysis durations for baseline and treated recordings differ by > 30% (which is an arbitrary threshold of notable difference).\nSee dat2[abs(analysis_duration_per_diff) > 30 & !(wllq_lvl1.b %in% 0 | wllq_lvl1.t %in% 0)]")
  
  # calculate the percent change in activity
  dat2[, rval := ( (activity_value.t - activity_value.b) / activity_value.b ) * 100]
  
  # define the plate wllq_lvl2
  dat2[, wllq_lvl2 := pmin(wllq_lvl1.b, wllq_lvl1.t, na.rm = T)]
  dat2[is.na(wllq_lvl1_notes.b), wllq_lvl1_notes.b := '']
  dat2[is.na(wllq_lvl1_notes.t), wllq_lvl1_notes.t := '']
  dat2[, wllq_lvl2_notes := paste0(wllq_lvl1_notes.t, wllq_lvl1_notes.b)]
  
  # Subset to desired columns
  dat2 <- dat2[, .(culture.date, culture_folder, group_char,group_int, experiment.date, apid, plate.id, rowi, coli, 
                   acnm, wllq_lvl2, wllq_lvl2_notes, rval,
                   neural_stats_file.b, neural_stats_file.t)]
  
  cat('\ndone!')
  
  return(dat2)
}
