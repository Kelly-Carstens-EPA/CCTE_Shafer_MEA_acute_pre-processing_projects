# combine the cyto and neural stats data

combineNeuralAndCyto <- function(cytodat, main.output.dir, dataset_title) {

  # read the data from the most recent file in main.ouput.dir
  dat2_files <- list.files(path = paste0(main.output.dir,"/output"), pattern = paste0("_dat2_"), recursive = F, full.names = T)
  dat2_file <- dat2_files[order(basename(dat2_files), decreasing = T)[1]] # get the most recent file
  load(dat2_file)
  
  # create a date_plate column to keep track of unique plates
  dat2[, date_plate := paste(experiment.date,plate.id,sep = "_")]
  cytodat[, date_plate := paste(experiment.date,plate.id,sep = "_")]
  
  # throw warning if cytodat and dat2 have some unshared plates
  cyto_only_date_plate <- setdiff(unique(cytodat$date_plate), unique(dat2$date_plate))
  if (length(cyto_only_date_plate)) warning(paste0("The following date_plate's are only found in cytodat (and not in dat2): ",paste0(cyto_only_date_plate,"\nWllq will be set to 1 for these plates.",collapse=", ")))
  dat2_only_date_plate <- setdiff(unique(dat2$date_plate),unique(cytodat$date_plate))
  if (length(dat2_only_date_plate)) stop(paste0("The following date_plate's are only found in dat2 (and not in cytodat): ",paste0(dat2_only_date_plate,collapse=", ")))
  
  # get treatment and conc values for dat2 from cytodat
  dat3 <- list()
  endpoints <- unique(dat2$acsn)
  # sometimes the conc collected for the LDH and CTB data has diff number of sig figs, so I can't just use unique(conc)
  # instead, I check that the 2 conc values are the same to 5 decimal places, then use the average of the 2 conc values
  collapseConc <- function(x) {
    vals <- unique(x)
    if (length(vals)==1) return(vals)
    else if (length(vals)==2) {
      if(all(!is.na(suppressWarnings(as.numeric(x))))) {
        vals <- as.numeric(vals)
        if (abs(vals[1] - vals[2]) < 0.00001) {
          vals <- mean(vals, na.rm = T)
          return(vals)
        } 
        else stop(paste0("Conc values are not the same for this well: ",paste0(vals,collapse=",")))
      }
      else
        stop(paste0("have 2 different names for conc in this well: ",paste0(vals,collapse=",")))
    }
    else
      stop(paste0("There are more than 2 different concs for this well:",paste0(vals,collapse=",")))
  }
  well_id_dat <- cytodat[rowi %in% c(1:6) & coli %in% c(1:8), .(treatment = unique(treatment), conc = as.character(collapseConc(conc))), by = c("plate.id","experiment.date","rowi","coli")]
  
  # in case there was more than 1 unique treatment, or other issues
  if (nrow(well_id_dat) != (length(unique(dat2$date_plate))+length(cyto_only_date_plate))*48) {
    assign("well_id_dat",well_id_dat, envir = .GlobalEnv)
    stop("There are differences in CTB and LDH well id data; not sure how to merge with dat2. See well_id_dat")
  }
  for (endpoint in endpoints) {
    add.dat <- merge(dat2[acsn == endpoint], well_id_dat, by = c("plate.id","experiment.date","rowi","coli"))
    dat3 <- rbind(dat3, add.dat)
    rm(add.dat)
  }
  
  dat3[, dat2 := basename(dat2_file)]
  
  # get the wllq for cytodat, using mean firing rate acsn data rows
  wllq_summary <- dat2[, .(wllq = ifelse(unique(wllq) == 0, 0, 1), wllq_notes = paste0(unique(wllq_notes), collapse="")), by = c("plate.id","experiment.date","rowi","coli")]
  # if wllq set to 0 only because the recording was too long or too short, let wllq be 1 for cytodat
  wllq_summary[grepl("Recording length",wllq_notes) & lengths(regmatches(wllq_notes, gregexpr(";",wllq_notes))) == 1, `:=`(wllq = 1, wllq_notes = "")]
  # if wllq set to 0 only because the mfr is above the upper threshold, let wllq be 1 for cytodat
  # (we remove these wells because an excessively high mfr value will affect the percent change values, not because somethign is likely wrong with the well, I believe)
  wllq_summary[grepl("Baseline MFR > ",wllq_notes) & lengths(regmatches(wllq_notes, gregexpr(";",wllq_notes))) == 1, `:=`(wllq = 1, wllq_notes = "")]
  cytodat <- merge(wllq_summary, cytodat, all.y = T, by = c("plate.id","experiment.date","rowi","coli"))
  # if there were any plates that are only in cytodat and not in neural stats, or LDH/CTB wells not included in MEA plate, assign wllq=1 here
  cytodat[is.na(wllq), `:=`(wllq=1, wllq_notes = "")]
  cytodat[, "dat2" := NA]
  
  usecols <- intersect(names(dat3), names(cytodat))
  dat3 <- rbind(dat3[,..usecols], cytodat[,..usecols])
  
  dat3[, rval := as.numeric(rval)]
  
  file_name <- paste0(main.output.dir,"/output/",dataset_title,"_dat3_",as.character.Date(Sys.Date()),".RData")
  save(dat3, file = file_name)
  cat(file_name, " is ready.\n",sep="")
}
