level1_set_wllq <- function(dat1, standard_analysis_duration_requirement = T) {
  
  # initialize default
  dat1[run_type == 'baseline', `:=`(wllq_lvl1 = 1, wllq_lvl1_notes = '')]
  
  # If nAE is less than 10 or is NA, set wllq_lvl1=0
  dat1[run_type == 'baseline', 
       low_ae_flag := as.numeric(activity_value[acsn == "Number of Active Electrodes"] < 10 
                                 | is.na(activity_value[acsn == "Number of Active Electrodes"])),
       by = .(experiment.date, plate.id, well)]
  dat1[low_ae_flag == 1, `:=` (wllq_lvl1 = 0, wllq_lvl1_notes = "Baseline # of AE < 10; ")]
  
  # If the MFR is very low or near the theoretical upper limit, remove that well
  # see the script mfr_baseline_cutoff_investigation.R 
  # or the notbeook 'MEA Acute Pre-Process for TCPL', Tab "Development", Page "Mean Firing Rate Baseline Cutoff"
  # for more details
  
  # High MFR flag
  mfr_upper_threshold <- 3.4036511 # this is the 95th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq_lvl1==1 and nAE>10
  dat1[run_type == 'baseline', 
       high_mfr_flag := as.numeric(activity_value[acsn == "Mean Firing Rate (Hz)"] > mfr_upper_threshold),
       by = .(experiment.date, plate.id, well)]
  dat1[high_mfr_flag == 1, `:=`(wllq_lvl1 = 0, 
                                wllq_lvl1_notes = paste0(wllq_lvl1_notes, "Baseline MFR > ",mfr_upper_threshold," Hz; "))]
  
  # Low MFR flag
  mfr_lower_threshold <- 0.6377603 # this is the 5th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq_lvl1==1 and nAE>10
  dat1[run_type == 'baseline', 
       low_mfr_flag := as.numeric(activity_value[acsn == "Mean Firing Rate (Hz)"] < mfr_lower_threshold 
                                  | is.na(activity_value[acsn == "Mean Firing Rate (Hz)"])),
       by = .(experiment.date, plate.id, well)]
  dat1[low_mfr_flag == 1, `:=`(wllq_lvl1 = 0,
                               wllq_lvl1_notes = paste0(wllq_lvl1_notes, "Baseline MFR < ",mfr_lower_threshold," Hz; "))]
  
  
  # For baseline or treated, if recording length is very short or very  long, remove it
  if (standard_analysis_duration_requirement) {
    stopifnot(nrow(dat1[is.na(analysis_duration)]) == 0) # check that the analysis duration is defined for all cases
    if (nrow(dat1[abs(analysis_duration - 2400) > 1400]) > 0){
      cat('The following files have recording length below 1000s or above 3800s. wllq_lvl1 will be set to 0:\n')
      print(dat1[abs(analysis_duration - 2400) > 1400, .N, by = .(neural_stats_file, analysis_duration)])
    }
    dat1[analysis_duration < 1000, `:=` (wllq_lvl1 = 0, wllq_lvl1_notes = paste0(wllq_lvl1_notes,"Recording length < 1000 s; "))]
    dat1[analysis_duration > 3800, `:=` (wllq_lvl1 = 0, wllq_lvl1_notes = paste0(wllq_lvl1_notes,"Recording length > 3800 s; "))]    
  }
  
  return(dat1)
}