# script to get the cytotoxicity data for the ToxCast data set

get_cyto_dat_toxcast2016 <- function() {
  
  # tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
  
  # my summary of what is needed:
  flatfile <- "L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv"
  dat <- read.csv(flatfile, stringsAsFactors = F)
  
  # change plateID 'MW 1042-50' to 'MW 1044-1' for run 3 (said corrected in raw file, but still here)
  # (taken from source_to_lvl0_nheerl_mea_aucte.R)
  dat$MEA_PLATE_ID_RUN3[dat$MEA_PLATE_ID_RUN3=='MW 1042-50'] <- 'MW 1044-1'
  
  setDT(dat)
  dat.sm <- dat[, c(1:14, 273:280)]
  dat.sm[, all.equal(MEA_WELL_ID._ALL_RUNS, LDH_WELL__ID, AB_WELL_ID)] # TRUE, all well id col's are the same
  
  id.cols <- c( "ALIQUOT_PLATE_BARCODE", "ALIQUOT_WELL_ID",       "ALIQUOT_CONC",          "ALIQUOT_CONC_UNIT",     "EPA_SAMPLE_ID",         "CONCENTRATION",        
                "ALIQUOT_VOLUME",        "ALIQUOT_VOLUME_UNIT",   "ALIQUOT_SOLVENT",       "EXPERIMENT_DATE", "MEA_WELL_ID._ALL_RUNS")
  
  # get the desired data from each run
  usecols <- c(id.cols, "MEA_PLATE_ID_RUN1","LDH_.DEAD_RUN1", "AB_.DEAD_RUN1")
  run1 <- dat.sm[, ..usecols]
  updatecols <- grep("_RUN1",names(run1),value=T)
  setnames(run1, old = updatecols, new = sub("_RUN1", "", updatecols))
  
  usecols <- c(id.cols, "MEA_PLATE_ID_RUN2","LDH_.DEAD_RUN2", "AB_.DEAD_RUN2")
  run2 <- dat.sm[, ..usecols]
  updatecols <- grep("_RUN2",names(run2),value=T)
  setnames(run2, old = updatecols, new = sub("_RUN2", "", updatecols))
  
  usecols <- c(id.cols, "MEA_PLATE_ID_RUN3","LDH_.DEAD_RUN3", "AB_.DEAD_RUN3")
  run3 <- dat.sm[, ..usecols]
  updatecols <- grep("_RUN3",names(run3),value=T)
  setnames(run3, old = updatecols, new = sub("_RUN3", "", updatecols))
  
  flatdat <- rbind(run1, run2, run3)
  flatdat[, MEA_PLATE_ID := sub(" ","",MEA_PLATE_ID)]
  flatdat[, apid := paste0(EXPERIMENT_DATE,"_",MEA_PLATE_ID)]
  
  # target cols: platedat[, .(tcpl_acsn, plate.id, apid, coli, rowi, wllq, rval, srcf)]
  
  # assign acsn
  flatdat.long <- melt(flatdat, measure.vars = c("LDH_.DEAD","AB_.DEAD"), variable.factor = F, value.name = "rval", variable.name = "file_acsn")
  tcplLoadAcid(fld = "asid", val = 20) # confirming these are the correct acnms for AB and LDH
  flatdat.long[file_acsn == "LDH_.DEAD", tcpl_acsn := "NHEERL_MEA_acute_LDH"]
  flatdat.long[file_acsn == "AB_.DEAD", tcpl_acsn := "NHEERL_MEA_acute_AB"]
  
  # Any negative values?
  flatdat.long[rval < 0, .N, by = "tcpl_acsn"]
  # tcpl_acsn    N
  # 1: NHEERL_MEA_acute_LDH    8
  # 2:  NHEERL_MEA_acute_AB 4147
  flatdat.long[rval < 0, summary(rval)]
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  # -91.50  -14.50   -8.00  -10.85   -3.70   -0.1
  # most are quite small. 
  # I think it would be safe to set negative AB values to zero, as I am doing for the other data sets
  flatdat.long[tcpl_acsn == "NHEERL_MEA_acute_AB" & rval < 0, rval := 0.0]
  
  # assign coli and rowi
  flatdat.long[, coli := as.character(as.numeric(sub("[[:alpha:]]*","",MEA_WELL_ID._ALL_RUNS)))]
  flatdat.long[, rowc := sub("[[:digit:]]*$","",MEA_WELL_ID._ALL_RUNS)]
  flatdat.long[, rowi := sapply(rowc, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
  flatdat.long[, rowc := NULL]
  
  # add srcf
  flatdat.long$srcf <- basename(flatfile)
  
  setnames(flatdat.long, old = c("tcpl_acsn","CONCENTRATION","EPA_SAMPLE_ID"),new = c("acsn","conc","spid"))
  
  # from combineNeuralAndCyto, I see that there were some typo's in the plate ID. Fixing here now
  # (it only matters for combining with the neural stats data. Other than that, all that matters is for apid to be distinct)
  flatdat.long[apid == "20150804_MW1079-13", apid := "20150804_MW1073-13"]
  flatdat.long[apid == "20151201_MW186-25", apid := "20151201_MW1086-25"]
  flatdat.long[apid == "20160614_MW1072-8", apid := "20160614_MW1072-08"]
  
  return(flatdat.long[, .(spid, conc, apid, rval, acsn, coli, rowi, srcf)])
}
