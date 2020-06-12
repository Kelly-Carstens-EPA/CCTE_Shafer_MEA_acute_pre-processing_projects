# functions for processing dat4 to dat4

updateWllq <- function(dat4, date, plate, well, wllq_note, acsns = unique(dat4$acsn), override_check = F) {
  well_row <- switch(substring(well, 1,1),"A"=1,"B"=2,"C"=3,"D"=4,"E"=5,"F"=6,"G"=7,"H"=8)
  well_col <- as.numeric(substring(well, 2,2))
  # see if all rvals are NAs already
  print(dat4[acsn %in% acsns & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col,
             .(.N, assays = paste0(unique(acsn)[1:3],collapse = ",")), by = "rval"])
  nrows <- dat4[acsn %in% acsns & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col, .N]
  if (!override_check) check <- readline(prompt = paste0("Well quality will be set to 0 for ",nrows," data rows. Proceed? (y/n): "))
  if (override_check || check %in% c("Y","y","Yes","yes")) {
    dat4[acsn %in% acsns & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col, 
         `:=`(wllq=0, wllq_notes = paste0(wllq_notes, wllq_note,"; "))]
    cat("Well quality set to zero for",nrows,"rows.")
  }
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