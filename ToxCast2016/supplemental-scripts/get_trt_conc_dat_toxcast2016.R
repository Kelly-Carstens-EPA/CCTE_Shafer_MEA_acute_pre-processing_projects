# script to get the cytotoxicity data for the ToxCast data set

get_trt_conc_dat_toxcast2016 <- function() {
  
  # my summary of what is needed, adapated from source_to_lvl0_nheerl_mea_acute.R:
  flatfile <- "L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv"
  dat <- read.csv(flatfile, stringsAsFactors = F)
  
  # change plateID 'MW 1042-50' to 'MW 1044-1' for run 3 (said corrected in raw file, but still here)
  # (taken from source_to_lvl0_nheerl_mea_aucte.R)
  dat$MEA_PLATE_ID_RUN3[dat$MEA_PLATE_ID_RUN3=='MW 1042-50'] <- 'MW 1044-1'
  
  setDT(dat)
  id.cols <- c( "ALIQUOT_PLATE_BARCODE", "ALIQUOT_WELL_ID",       "ALIQUOT_CONC",          "ALIQUOT_CONC_UNIT",     "EPA_SAMPLE_ID",         "CONCENTRATION",        
                "ALIQUOT_VOLUME",        "ALIQUOT_VOLUME_UNIT",   "ALIQUOT_SOLVENT",       "EXPERIMENT_DATE", "MEA_WELL_ID._ALL_RUNS", "MEA_PLATE_ID_RUN1",
                "MEA_PLATE_ID_RUN2", "MEA_PLATE_ID_RUN3")
  usedat <- dat[, ..id.cols]
  
  # make sure things are standardized, so that I can elminate these columns
  unique(usedat$ALIQUOT_CONC_UNIT) # NA      "mM"    "mg/ml"
  usedat[usedat$ALIQUOT_CONC_UNIT == "mg/ml"] # TX001586. That just seems really weird. will look into this in the other data set
  
  setnames(x = usedat, old = c("EPA_SAMPLE_ID","CONCENTRATION","EXPERIMENT_DATE","MEA_WELL_ID._ALL_RUNS"), new = c("treatment","conc","experiment.date","well"))
  
  unique(usedat$conc) # looks like concentration-correction has been done
  usedat[is.na(conc), .N, by = "treatment"]
  usedat[treatment == "BC", treatment := "BIC"] # just to standardize
  
  # melt plate.id values
  usedat <- melt(usedat, measure.vars = c("MEA_PLATE_ID_RUN1","MEA_PLATE_ID_RUN2", "MEA_PLATE_ID_RUN3"), value.name = "plate.id", variable.factor = F)
  usedat[, variable := NULL]
  usedat[, plate.id := sub(" ","",plate.id)]

  # assign coli and rowi
  usedat[, coli := as.character(as.numeric(sub("[[:alpha:]]*","",well)))]
  usedat[, rowc := sub("[[:digit:]]*$","",well)]
  usedat[, rowi := sapply(rowc, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
  usedat[, rowc := NULL]
  # usedat[, unique(well), by = c("rowi","coli")] # verification
  usedat[, `:=`(rowi = as.numeric(rowi), coli = as.numeric(coli))]
  
  # after running combineNeuralAndCyto the first time, I see that there were some typo's in the plate ID. Fixing here now
  # (it only matters for combining with the neural stats data. Other than that, all that matters is for apid to be distinct)
  usedat[experiment.date == "20150804" & plate.id == "MW1079-13", plate.id := "MW1073-13"]
  usedat[experiment.date == "20151201" & plate.id == "MW186-25", plate.id := "MW1086-25"]
  usedat[experiment.date == "20160614" & plate.id == "MW1072-8", plate.id := "MW1072-08"]
  
  # get apid
  usedat[, experiment.date := as.character(experiment.date)]
  usedat[, apid := experiment.date]
  
  return(usedat[, .(treatment, conc, apid, experiment.date, plate.id, coli, rowi)])
}
