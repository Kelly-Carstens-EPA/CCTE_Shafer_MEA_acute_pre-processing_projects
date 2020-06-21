# functions for processing dat4 to dat4

updateWllq <- function(dat4, date, plate, well, wllq_note, acsns = unique(dat4$acsn), override_check = F) {
  well_row <- switch(substring(well, 1,1),"A"=1,"B"=2,"C"=3,"D"=4,"E"=5,"F"=6,"G"=7,"H"=8)
  well_col <- as.numeric(substring(well, 2,2))
  # see if all rvals are NAs already
  cat("\n")
  print(dat4[acsn %in% acsns & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col,
             .(.N, assays = paste0(unique(acsn)[1:3],collapse = ",")), by = "rval"])
  nrows <- dat4[acsn %in% acsns & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col, .N]
  if (!override_check) check <- readline(prompt = paste0("Well quality will be set to 0 for ",nrows," data rows. Proceed? (y/n): "))
  if (override_check || check %in% c("Y","y","Yes","yes")) {
    dat4[acsn %in% acsns & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col, 
         `:=`(wllq=0, wllq_notes = paste0(wllq_notes, wllq_note,"; "))]
    cat("Well quality set to zero for",nrows,"rows.\n")
  }
}

assign_wllt <- function(dat4) {
  
  # dmso, media
  dat4[spid == "DMSO", wllt := "n"]
  dat4[spid == "Media", wllt := "b"]
  
  # Lysis wells
  dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & spid == "Tritonx100", wllt := "x"]
  dat4[acsn == "NHEERL_MEA_acute_AB" & spid == "Tritonx100", wllt := "p"]
  # wllt should have already been defined for the different lysis wells for LDH acsn
  if (any(is.na(dat4[spid == "Tritonx100" & acsn == "NHEERL_MEA_acute_LDH", unique(wllt)]))) {
    cat("Some Lysis wells do not have wllt assigned.")
  }
  
  # PICRO/BIC - increase firing rate, do not affect CTB/LDH
  dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & spid %in% c("Picrotoxin","Bicuculline"), wllt := "p"] # gain of signal positive control
  dat4[acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB") & spid %in% c("Picrotoxin","Bicuculline"), wllt := "z"] # filler
  
  # TTX - stops all electrical activity, does not affect CTB/LDH
  dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & spid == "Tetrodotoxin", wllt := "v"] # viability control
  dat4[acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB") & spid == "Tetrodotoxin", wllt := "x"] # filler
  
  # treated compounds
  cat("\nwllt will be set to 't' for the MEA endpoints for the following treatments:\n")
  print(dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), unique(treatment)])
  dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), wllt := "t"]
  
  cat("\nwllt will be set to 't' for the cytotoxicity endpoints for the following treatments:\n")
  print(dat4[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), unique(treatment)])
  dat4[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), wllt := "t"]
  
  # summary info
  dat4[wllt != "t" & grepl("LDH",acsn), .(LDH_wllt = paste0(unique(wllt),collapse=",")), by = c("treatment","spid")]
  wllt_summary <- dat4[wllt != "t" & grepl("(LDH)|(AB)|(firing)",acsn), .(wllt = paste0(unique(wllt),collapse=",")), by = c("treatment","spid","acsn")]
  wllt_summary <- dcast(wllt_summary, treatment + spid ~ acsn, value.var = "wllt")[order(spid)]
  setnames(wllt_summary, old = c("NHEERL_MEA_acute_AB", "NHEERL_MEA_acute_LDH", "NHEERL_MEA_acute_firing_rate_mean"), 
           new = c("CellTiter Blue","LDH","MEA endpoints"))
  print(wllt_summary)
  
  return(dat4)
}

add_acid <- function(dat4, dbname = "invitrodb") {
  if (dbname == "invitrodb") {
    acid_acsn_map <- fread("../acid_acsn_map_invitrodb_2020-06-26.csv")
  }
  else {
    stop("don't have acid map prepared for that dbname yet")
  }
  dat4 <- merge(dat4, acid_acsn_map[, .(acsn,acid)], by = "acsn")
  return(dat4)
}

createWllqSummary <- function(dat4, dataset_title) {
  # eventually make this an xlsx, with addl page for numerical summaries by acsn, plate, date, etc. Including where wllq=0 bc rval is NA
  # maybe also by trt too, so that user can see if somethign needs to be repeat? eh, no big
  dat4[, endpoint_type := ifelse(grepl("(LDH)|(AB)",acsn), "cytotox","mea")]
  wllq_summary <- dat4[wllq == 0 & !wllq_notes == "rval is NA; ", .(treatment = paste0(unique(treatment),collapse=","), spid = paste0(unique(spid),collapse=","), conc = paste0(unique(conc),collapse=","), wllq_notes = paste0(unique(wllq_notes), collapse = ""), endpoints = paste0(unique(endpoint_type),collapse=","), srcf = paste0(unique(srcf),collapse=",")), 
                        by = c("experiment.date","plate.id","rowi","coli")][order(experiment.date,plate.id,rowi,coli)]
  fwrite(wllq_summary, file = paste0(dataset_title,"_summary_of_wells_where_wllq=0.csv"), sep = ",")
  dat4[, endpoint_type := NULL]
  return(paste0(paste0(dataset_title,"_summary_of_wells_where_wllq=0.csv")," is ready."))
}
