# fix typos in trt_conc_dat and cytodat (taken from flat file and Conc_Resp_Log_JS, respectively)

fix_typos_toxcast2016 <- function(cytodat, trt_conc_dat) {
  
  # let's make coli not a factor
  cytodat[, .N, by = "coli"] # all 2520, for 1-8
  cytodat[, coli := as.numeric(as.character(coli))]
  
  cytodat[, unique(Row), by = "rowi"] # looks good
  cytodat[, Row := NULL]
  cytodat[, srcf := "Conc_Response_Log_JS.xlsx"]
  
  # fix some plate.id assignmnets
  unique(cytodat$plate.id)
  cytodat[plate.id == "RawAbsorbance", .N, by = c("experiment.date","acsn")]
  # experiment.date                 acsn  N
  # 1:        20160623 NHEERL_MEA_acute_LDH 48
  # 2:        20160628 NHEERL_MEA_acute_LDH 48
  # 3:        20160630 NHEERL_MEA_acute_LDH 48
  # 4:        20160712 NHEERL_MEA_acute_LDH 48
  # 5:        20160714 NHEERL_MEA_acute_LDH 48
  
  cytodat[plate.id == "RawAbsorbance", unique(experiment.date)]
  cytodat[plate.id == "RawData"] # one plate of AB data
  
  # fixing plate.id assignments, where the plate.id value is shifted up one spot in Conc_Response_Log_JS.xlsx
  cytodat[experiment.date %in% c("20160623","20160628", "20160630", "20160712", "20160714") & grepl("LDH",acsn), unique(plate.id), by = "experiment.date"]
  cytodat[experiment.date == "20160623" & acsn == "NHEERL_MEA_acute_LDH" & plate.id == "RawAbsorbance", plate.id := "MW1073-14"]
  cytodat[experiment.date == "20160628" & acsn == "NHEERL_MEA_acute_LDH" & plate.id == "RawAbsorbance", plate.id := "MW1130-17"]
  cytodat[experiment.date == "20160630" & acsn == "NHEERL_MEA_acute_LDH" & plate.id == "RawAbsorbance", plate.id := "MW1139-02"]
  cytodat[experiment.date == "20160712" & acsn == "NHEERL_MEA_acute_LDH" & plate.id == "RawAbsorbance", plate.id := "MW1139-05"]
  cytodat[experiment.date == "20160714" & acsn == "NHEERL_MEA_acute_LDH" & plate.id == "RawAbsorbance", plate.id := "MW1139-08"]
  cytodat[experiment.date == "20160621" & acsn == "NHEERL_MEA_acute_AB" & plate.id == "RawData", plate.id := "MW1072-9"]
  
  unique(cytodat$experiment.date) # all good

  
  # checking for inconsistencies in trt_conc_dat
  unique(trt_conc_dat$plate.id)
  unique(trt_conc_dat$experiment.date)
  
  
  # check for differences between the 2 files
  nrow(trt_conc_dat)
  # [1] 9936
  nrow(cytodat)
  # 20160 -> right, cytodat will have more than twice as many rows, because I am including more cultures
  setdiff(cytodat$apid, trt_conc_dat$apid) # "20160607" - I added this exp date, so I expected this
  (diffplates <- unique(setdiff(cytodat$plate.id, trt_conc_dat$plate.id), setdiff(trt_conc_dat$plate.id, cytodat$plate.id)))
  cytodat[plate.id %in% diffplates, unique(experiment.date)] # "20151201" "20151215" "20160607". Okay, so so plates are diffierent not just because of the exp date
  
  # plate differences in "20151201":
  cytodat[experiment.date == "20151201", unique(plate.id)] # "MW1086-26" "MW1086-34" "MW1086-35"
  trt_conc_dat[experiment.date == "20151201", unique(plate.id)] # "MW1086-25" "MW1086-34" "MW1086-35"
  # in conc resp log, MW1086-26 is in 1201, MW1085-25 is in 1203
  trt_conc_dat[plate.id == "MW1086-25"]
  # in flat file (where trt_conc_dat taken from), MW1086-25 is in both 1201 and 1203. There is no MW1086-26 in flat file. I think that was just a typo
  trt_conc_dat[experiment.date == "20151201" & plate.id == "MW1086-25", plate.id := "MW1086-26"]
  
  # plate differences in "20151215":
  cytodat[experiment.date == "20151215", unique(plate.id)] # "MW1090-7" "MW1089-8"  "MW1090-8"
  trt_conc_dat[experiment.date == "20151215", unique(plate.id)] # "MW1090-7" "MW1089-90" "MW1090-6"  
  
  # summary:
  # "MW1090-7" "MW1089-90" "MW1090-6" 
  # plates listed in flat file
  # neural stats plates
  # plates listed at top of conc resp log file
  
  # "MW1090-7" "MW1089-8"  "MW1090-8"
  # cyto dat in conc resp log file
  # plate labels in MEA Cytotox folder, for LDH and AB ...\MEA Cytotox\20151202 Culture
  
  # I can ask: Look at the LDH and AB data corresponding to "MW1089-90". Does that match the AB/LDH data for "MW1089-8" or "MW1090-8"?
  # (loaded the flat file data)
  # dat[MEA_PLATE_ID_RUN1 == "MW 1089-90" | MEA_PLATE_ID_RUN2 == "MW 1089-90" | MEA_PLATE_ID_RUN3 == "MW 1089-90", 
    # .(MEA_PLATE_ID_RUN1, MEA_PLATE_ID_RUN2, MEA_PLATE_ID_RUN3, AB_WELL_ID, AB_.DEAD_RUN1, AB_.DEAD_RUN2, AB_.DEAD_RUN3)]
  
  # % viability values from MW 1089-8 in the conc resp log definitely match the AB_.DEAD_Run1 values corresponding to MW 1089-90 in the flatfile
  # % viability values from MW 1090-8 in the conc resp log definitely match the AB_.DEAD_Run3 values corresponding to MW 1090-7 in the flatfile
  # oof... now I'm not sure that I can trust the one plate that does match... (MW1090-7)
  # Maybe this doesn't mater... it would be nice to have some confidence that I know which data corresponds to which plate. But I don't think I can know for sure here
  # and it is not a make-or-break issue if the exact well here does not correspond to the exact well there.
  
  # I could look at the activity in each well...
  # I am just going to guess
  # 1089-8 for cytodat will correspond to 1089-90 in neural stats data (and in flat file, so can merge trts - but it's all the same for same exp date)
  # "MW1090-7" will be referenced the same
  # MW1090-8 for the cytodat will correspond to 1090-6 in the neural stats data
  # previous strategy: relabel cytotox plate to match neural plates from same culture date
  # cytodat[plate.id == "MW1089-90"] # this plate has not been referenced anywhere else
  # cytodat[experiment.date == "20151215" & plate.id == "MW1089-8", plate.id := "MW1089-90"]
  # cytodat[plate.id == "MW1090-6"] # this plate has not been referenced anywhere else
  # cytodat[experiment.date == "20151215" & plate.id == "MW1090-8", plate.id := "MW1090-6"]
  
  # new idea: just add the trt_conc data for these 2 plates to the table, but keep the plate.id's for cyto and neural endpoints separate
  trt_conc_dat[experiment.date == "20151215", unique(treatment), by = c("rowi","coli")] # 48 rows - this was consistent for every plate
  (exp_date_iddat <- trt_conc_dat[experiment.date == "20151215", .(treatment = unique(treatment), conc = unique(conc)), by = c("rowi","coli","apid","experiment.date")])
  plate1 <- exp_date_iddat
  plate1[, plate.id := "MW1089-8"]
  trt_conc_dat <- rbind(trt_conc_dat,plate1, use.names = T)
  plate2 <- exp_date_iddat
  plate2[, plate.id := "MW1090-8"]
  trt_conc_dat <- rbind(trt_conc_dat,plate2, use.names = T)
  
  # confirming all good now:
  (diffplates <- unique(setdiff(cytodat$plate.id, trt_conc_dat$plate.id), setdiff(trt_conc_dat$plate.id, cytodat$plate.id)))
  cytodat[plate.id %in% diffplates, unique(experiment.date)] # "20160607"
  # huh, why only 2 plates?
  cytodat[experiment.date == "20160607", unique(plate.id)] # okay, there are 3 plates here. probs just got re-used somewhere
  
  # let's if all of the date_plates are the same
  cytodat[, date_plate := paste(experiment.date,plate.id,sep = "_")]
  trt_conc_dat[, date_plate := paste(experiment.date,plate.id,sep = "_")]
  (diffplates <- unique(setdiff(cytodat$date_plate, trt_conc_dat$date_plate), setdiff(trt_conc_dat$date_plate, cytodat$date_plate)))
  # [1] "20150804_MW1041-17" "20150929_MW1047-34" "20151203_MW1048-11" "20151203_MW1048-14" "20151203_MW1048-15" "20151208_MW1086-36" "20151208_MW1086-37"
  # [8] "20151208_MW1086-38" "20160218_MW1086-38" "20160607_MW1062-28" "20160607_MW1063-1"  "20160607_MW1063-3"  "20160614_MW1072-8"
  
  # "20150804_MW1041-17"
  cytodat[date_plate == "20150804_MW1041-17"] # present here
  trt_conc_dat[experiment.date == "20150804", unique(plate.id)] # "MW1073-13" "MW1073-14" "MW1073-15"
  cytodat[experiment.date == "20150804", unique(plate.id)] # "MW1073-13" "MW1073-14" "MW1073-15" "MW1041-17"
  # looking at conc resp log, this looks like just a typo for the AB data...
  cytodat[date_plate == "20150804_MW1041-17", plate.id := "MW1073-14"]
  # i'm going to hold off on this. Maybe I should just keep the plate names, and just merge by culture?
  
  # "20150929_MW1047-34"
  cytodat[date_plate == "20150929_MW1047-34"]
  trt_conc_dat[experiment.date == "20150929", unique(plate.id)] # "MW1077-15" "MW1072-6"  "MW1072-4" 
  cytodat[experiment.date == "20150929", unique(plate.id)] # "MW1077-15" "MW1072-6"  "MW1072-4"  "MW1047-34"
  cytodat[date_plate == "20150929_MW1047-34", plate.id := "MW1072-4"]
  
  # "20151203"
  trt_conc_dat[experiment.date == "20151203", unique(plate.id)] # "MW1086-19" "MW1086-24" "MW1086-25"
  cytodat[experiment.date == "20151203", unique(plate.id)] # "MW1086-19" "MW1086-24" "MW1086-25" "MW1048-11" "MW1048-14" "MW1048-15"
  # the AB data is missing for this culture, regardless if you look for "MW1048-11" "MW1048-14" "MW1048-15" or "MW1086-19" "MW1086-24" "MW1086-25"
  # I am going to rename these to "MW1086-19" "MW1086-24" "MW1086-25", just for consistency
  cytodat[date_plate == "20151203_MW1048-11", plate.id := "MW1086-19"]
  cytodat[date_plate == "20151203_MW1048-14", plate.id := "MW1086-24"]
  cytodat[date_plate == "20151203_MW1048-15", plate.id := "MW1086-25"]
  
  # "20151208"
  trt_conc_dat[experiment.date == "20151208", unique(plate.id)] # "MW1068-36" "MW1068-37" "MW1068-38"
  cytodat[experiment.date == "20151208", unique(plate.id)] # "MW1086-36" "MW1086-37" "MW1086-38"
  # now this just looks like some flip-flop of the values. It hardly matters what the 'real' plate.id was, just that it is consistent
  trt_conc_dat[date_plate == "20151208_MW1068-36", plate.id := "MW1086-36"]
  trt_conc_dat[date_plate == "20151208_MW1068-37", plate.id := "MW1086-37"]
  trt_conc_dat[date_plate == "20151208_MW1068-38", plate.id := "MW1086-38"]
  
  # "20160218_MW1086-38"
  cytodat[date_plate == "20160218_MW1086-38"]
  trt_conc_dat[experiment.date == "20160218", unique(plate.id)] # "MW1086-35" "MW1086-36" "MW1086-37"
  cytodat[experiment.date == "20160218", unique(plate.id)] # "MW1086-36" "MW1086-37" "MW1086-38" "MW1086-35"
  # The plate.id's in the conc resp log for LDH are "MW1086-36" "MW1086-37" "MW1086-38"
  # I compared the values with the LDH values in L:\Lab\NHEERL_MEA\MAESTRO SYSTEM\ToxCast Compounds\Phase I and II Con Response\MEA Cytotox\20160203 Culture\TC_20160203_20160218_LDH.xls
  # The plate.id's in the conc resp log appear to have just been shifted by 1 digit
  cytodat[date_plate == "20160218_MW1086-36" & grepl("LDH",acsn), plate.id := "MW1086-35"]
  cytodat[date_plate == "20160218_MW1086-37" & grepl("LDH",acsn), plate.id := "MW1086-36"]
  cytodat[date_plate == "20160218_MW1086-38" & grepl("LDH",acsn), plate.id := "MW1086-37"]
  # cytodat[date_plate == "20150929_MW1047-34", plate.id := "MW1072-4"]
  
  # 20160607 is the added culture, this is okay
  
  # "20160614_MW1072-8" 
  trt_conc_dat[experiment.date == "20160614", unique(plate.id)] # "MW1072-08" "MW1072-42" "MW1072-43"
  cytodat[experiment.date == "20160614", unique(plate.id)] # "MW1072-8"  "MW1072-42" "MW1072-43"
  # I added the 0 because it is present in the neural stats data. Will do the same for cytodat
  cytodat[date_plate == "20160614_MW1072-8", plate.id := "MW1072-08"]

  # confirm all date_plate's match now
  cytodat[, date_plate := paste(experiment.date,plate.id,sep = "_")]
  trt_conc_dat[, date_plate := paste(experiment.date,plate.id,sep = "_")]
  (diffplates <- unique(setdiff(cytodat$date_plate, trt_conc_dat$date_plate), setdiff(trt_conc_dat$date_plate, cytodat$date_plate)))
  # "20160607_MW1062-28" "20160607_MW1063-1"  "20160607_MW1063-3" 
  
  # confirming that each date has exaclty 3 plates
  cytodat[, .(ifelse(length(unique(plate.id)) == 3, TRUE, FALSE)), by = "experiment.date"]
  # TRUE for all 70 experiment dates
  
  # merge the data together
  cytodat <- merge(trt_conc_dat, cytodat, by = c("apid","coli","rowi","experiment.date","plate.id","date_plate"), all = T)
  # there wont't be any data for 1089-90 or 1090-6 in cytodat, and that is okay
  return(cytodat)
}