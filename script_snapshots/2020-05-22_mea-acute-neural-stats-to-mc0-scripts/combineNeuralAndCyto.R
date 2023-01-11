# combine the cyto and neural stats data

combineNeuralAndCyto <- function(cytodat, main.output.dir, dataset_title) {

  # read the data from the most recent file in main.ouput.dir
  alldat2_files <- list.files(path = paste0(main.output.dir,"/output"), pattern = paste0("_alldat2_"), recursive = F, full.names = T)
  alldat2_file <- alldat2_files[order(basename(alldat2_files), decreasing = T)[1]] # get the most recent file
  load(alldat2_file)
  
  # throw warning if not all same apid's
  cyto_only_apid <- setdiff(unique(cytodat$apid), unique(alldat2$apid))
  if (length(cyto_only_apid)) warning(paste0("The following apid's are only found in cytodat (and not in alldat2): ",paste0(cyto_only_apid,"\nWllq will be set to 1 for these plates.",collapse=", ")))
  alldat2_only_apid <- setdiff(unique(alldat2$apid),unique(cytodat$apid))
  if (length(alldat2_only_apid)) warning(paste0("The following apid's are only found in alldat2 (and not in cytodat): ",paste0(alldat2_only_apid,collapse=", ")))
  
  # get trt (or spid), conc values for alldat2 from cytodat
  alldat3 <- list()
  endpoints <- unique(alldat2$acsn)
  add_cols <- setdiff(names(cytodat), names(alldat2))
  use_cols <- c(add_cols, "apid","rowi","coli")
  for (endpoint in endpoints) {
    add.dat <- merge(alldat2[acsn == endpoint], cytodat[acsn == unique(acsn)[1], ..use_cols], by = c("apid","rowi","coli"))
    alldat3 <- rbind(alldat3, add.dat)
    rm(add.dat)
  }
  
  # get the wllq for cytodat - just use mean firing rate, since rval cannot be NA there to get the table
  wllq_summary <- alldat2[acsn == "NHEERL_MEA_acute_firing_rate_mean", .(wllq), by = c("apid","rowi","coli")]
  cytodat <- merge(wllq_summary, cytodat, all.y = T, by = c("apid","rowi","coli"))
  # if there were any plates that are only in cytodat and not in neural stats, assign wllq=1 here
  cytodat[is.na(wllq), wllq:=1]
  
  usecols <- intersect(names(alldat3), names(cytodat))
  alldat3 <- rbind(alldat3[,..usecols], cytodat[,..usecols])
  
  file_name <- paste0(main.output.dir,"/output/",dataset_title,"_alldat3_",as.character.Date(Sys.Date()),".RData")
  save(alldat3, file = file_name)
  cat(file_name, " is ready.\n",sep="")
}
