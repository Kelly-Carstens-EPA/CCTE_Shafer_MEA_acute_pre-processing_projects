# Function to set the well quality
# Just pulled out from fileToLongdat
# Not sure if I want to run this on a file by file basis, or for the entire dataset

assign_wllq <- function(longdat, run_type) {
  
  # for baseline or treated, if recording length is very short or very  long, remove it
  if (standard_analysis_duration_requirement) {
    if (abs(analysis.duration - 2400) > 1400) cat(basename(filei),"will be removed. Recording length is",analysis.duration,"\n")
    longdat[analysis.duration < 1000, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length < 1000 s; "))]
    longdat[analysis.duration > 3800, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes,"Recording length > 3800 s; "))]    
  }
  
  # for baseline recordings, do 2 checks for wllq
  if (run_type == "baseline") {
    
    longdat[, wllq := 1] # set the default wllq
    longdat[, wllq_notes := ""]
    
    # if nAE is less than 10 or is NA, set wllq=0
    low_ae_wells <- longdat[acsn == "Number of Active Electrodes" & (activity_value < 10 | is.na(activity_value)), well]
    longdat[well %in% low_ae_wells, `:=` (wllq = 0, wllq_notes = "Baseline # of AE < 10; ")]
    
    # if the MFR is very low or near the theoretical upper limit, remove that well
    # see the script mfr_baseline_cutoff_investigation.R 
    # or the notbeook 'MEA Acute Pre-Process for TCPL', Tab "Development", Page "Mean Firing Rate Baseline Cutoff"
    # for more details
    mfr_upper_threshold <- 3.4036511 # this is the 95th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
    mfr_lower_threshold <- 0.6377603 # this is the 5th percentile of the DNT2019, ToxCast2016, APCRA2019 data where wllq==1 and nAE>10
    high_mfr_wells <- longdat[acsn == "Mean Firing Rate (Hz)" & activity_value > mfr_upper_threshold, well]
    longdat[well %in% high_mfr_wells, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR > ",mfr_upper_threshold," Hz; "))]
    low_mfr_wells <- longdat[acsn == "Mean Firing Rate (Hz)" & (activity_value < mfr_lower_threshold | is.na(activity_value)), well]
    longdat[well %in% low_mfr_wells, `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "Baseline MFR < ",mfr_lower_threshold," Hz; "))]
  }
  else {
    # don't assign wllq for treated wells (yet)
    longdat[, `:=` (wllq = NA_integer_, wllq_notes = "")]
  }

}