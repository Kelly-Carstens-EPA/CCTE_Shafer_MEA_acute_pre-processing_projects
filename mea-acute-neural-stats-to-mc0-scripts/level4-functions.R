# functions for processing dat4 to dat4

updateWllq <- function(dat4, date, plate, well, wllq_note, acnms = unique(dat4$acnm), override_check = F) {
  cat("\n", paste("Experiment date:",date, plate, well, wllq_note, "Summary:\n", sep = " "), sep = "")
  if (length(setdiff(acnms, unique(dat4$acnm)))>0) {
    stop(paste0("The following acnm's are not in dat4: ",paste0(setdiff(acnms, unique(dat4$acnm)),collapse=",")))
  }
  well_row <- switch(substring(well, 1,1),"A"=1,"B"=2,"C"=3,"D"=4,"E"=5,"F"=6,"G"=7,"H"=8)
  well_col <- as.numeric(substring(well, 2,2))
  # see if all rvals are NAs already
  view_acnms <- paste0("CCTE_Shafer_MEA_acute_",c("LDH","AB","firing_rate_mean","active_electrodes_number","burst_number","network_burst_number"))
  rval_summary <- dat4[acnm %in% intersect(acnms, view_acnms) & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col,
                       .(rval, acnm)]
  rval_summary[, acnm := sub("CCTE_Shafer_MEA_acute_","",acnm)]
  print(rval_summary)
  nrows <- dat4[acnm %in% acnms & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col, .N]
  if (!override_check) check <- readline(prompt = paste0("Well quality will be set to 0 for ",nrows," data rows. Proceed? (y/n): "))
  if (override_check || check %in% c("Y","y","Yes","yes")) {
    dat4[acnm %in% acnms & plate.id == plate & experiment.date == date & rowi == well_row & coli == well_col, 
         `:=`(wllq=0, wllq_notes = paste0(wllq_notes, wllq_note,"; "))]
    cat("Well quality set to zero for",nrows,"rows.\n")
  }
}


view_activity_stripchart <- function(plotdat, title_additions = "") {
  
  if (grepl("LDH",unique(plotdat$acnm))) {
    title <- paste0("LDH Blank-Corrected Optical Density Values for Control Compounds\n",title_additions)
  }
  else if (grepl("AB",unique(plotdat$acnm))) {
    title <- paste0("CellTiter Blue Blank-Corrected Fluorescence Values for Control Compounds\n",title_additions)
  }
  else {
    title <- paste0("Percent Change in ",sub("CCTE_Shafer_MEA_acute_","",unique(plotdat$acnm))," for Control Compounds\n",title_additions)
  }
  plotdat$treatment <- factor(x = plotdat$treatment, levels = sort(unique(plotdat$treatment)), ordered = T)
  stripchart(rval ~ treatment, plotdat[wllq == 1], vertical = T, pch = 1, method = "jitter", main = title, cex.axis = 0.75)
  # if there is any data with wllq===0, plot it in a different color
  if(plotdat[, any(wllq == 0)]) {
    stripchart(rval ~ treatment, plotdat[wllq == 0],
               vertical = T, pch = 1, method = "jitter", add=T, col = "red")
    legend(x = "topright", legend = c("wllq=0"), col = "red", pch = 1, bg = "transparent", cex = 0.75)
  }
}


assign_wllt <- function(dat4) {
  
  cat("\nAssign Wllt:\n")
  
  # guessing if there are any new control compounds outside of usual
  treated_spid_prefixes <- unique(dat4[grepl("[0-9]",spid), unique(substring(spid,1,2))])
  new_non_treated_compounds <- setdiff(dat4[!grepl(paste0("(",treated_spid_prefixes,")",collapse="|"),spid), unique(spid)], 
                                       c("DMSO","Water","Media","Picrotoxin","Tetrodotoxin", "Tritonx100", "Bicuculline"))
  if (length(new_non_treated_compounds) > 0) {
    warning(paste0("Wllt is not defined for the following treatment with sample IDs:\n",paste0(new_non_treated_compounds,collapse=", "),
                "\nWill set wllt:='t' for these compounds."))
  }
  if (dat4[, any(is.na(unique(spid)))]) {
    stop(paste0("Spid is NA for the following treatments:\n",paste0(dat4[is.na(spid), unique(treatment)],collapse=", ")))
  }
  
  # dmso, water, media
  dat4[spid %in% c("DMSO","Water"), wllt := "n"]
  dat4[spid == "Media", wllt := "b"]
  
  # Lysis wells
  dat4[!(acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB")) & spid == "Tritonx100", wllt := "v"]
  dat4[acnm == "CCTE_Shafer_MEA_acute_AB" & spid == "Tritonx100", wllt := "p"]
  # wllt should have already been defined for the different lysis wells for LDH acnm
  if (any(is.na(dat4[spid == "Tritonx100" & acnm == "CCTE_Shafer_MEA_acute_LDH", unique(wllt)]))) {
    cat("Some Lysis wells do not have wllt assigned.")
  }
  
  # PICRO/BIC - increase firing rate, do not affect CTB/LDH
  dat4[!(acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB")) & spid %in% c("Picrotoxin","Bicuculline"), wllt := "p"] # gain of signal positive control
  dat4[acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB") & spid %in% c("Picrotoxin","Bicuculline"), wllt := "z"] # filler
  
  # TTX - stops all electrical activity, does not affect CTB/LDH
  dat4[!(acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB")) & spid == "Tetrodotoxin", wllt := "p"]
  dat4[acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB") & spid == "Tetrodotoxin", wllt := "x"] # filler
  
  # treated compounds
  cat("wllt will be set to 't' for the MEA components for the following spid's:\n")
  cat(dat4[!(acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB")) & is.na(wllt), unique(spid)],sep=", ")
  dat4[!(acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB")) & is.na(wllt), wllt := "t"]
  
  cat("\nwllt will be set to 't' for the cytotoxicity components for the following spid's:\n")
  cat(dat4[(acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB")) & is.na(wllt), unique(spid)], sep = ", ")
  cat("\n")
  dat4[(acnm %in% c("CCTE_Shafer_MEA_acute_LDH","CCTE_Shafer_MEA_acute_AB")) & is.na(wllt), wllt := "t"]
  
  # summary info
  use_acnms <- c("CCTE_Shafer_MEA_acute_AB", "CCTE_Shafer_MEA_acute_LDH", "CCTE_Shafer_MEA_acute_firing_rate_mean")
  wllt_summary <- dat4[wllt != "t" & acnm %in% use_acnms, .(wllt = paste0(unique(wllt),collapse=",")), by = c("spid","treatment","acnm")]
  wllt_summary <- dcast(wllt_summary, treatment + spid ~ acnm, value.var = "wllt", fill = "-")[order(spid)]
  setnames(wllt_summary, old = use_acnms, new = c("CellTiter Blue","LDH","MEA components"))
  cat("\nWell Type Assignments for Control Compounds by assay component:\n")
  print(wllt_summary)
  
  cat("\nUnique of wllt:\n")
  print(dat4[,unique(wllt)])
  
  return(dat4)
}


assign_common_conc <- function(dat4, check_conc_correction = TRUE, expected_concs = c(0.03,0.1,0.3,1,3,10,30)) {
  
  # assign conc to NA for these treatments:
  dat4[spid == "Media", conc := NA_real_]
  dat4[spid == "Tritonx100", conc := NA_real_]
  
  # make conc's numeric
  if(is.character(dat4$conc)) {
    cat("All conc's as char:\n")
    cat(dat4[, sort(unique(conc), na.last = FALSE)], sep = ", ")
  }
  cat("\nAll conc's as numeric:\n")
  dat4[, conc := as.numeric(conc)]
  cat(dat4[, sort(unique(conc), na.last = FALSE)], sep = ", ")
  
  # check wllt = "t" wells
  if (dat4[wllt == "t", any(is.na(conc))]) {
    stop(paste("\nThe following treatments have conc NA:",paste0(dat4[wllt == "t" & is.na(conc), unique(treatment)],collapse=",")))
  }
  
  # check if concentration corrections have been done
  if (check_conc_correction) {
    # guess the column in the spid map containing the aliquot concentration
    conc_col <- grep("([Cc]onc)|(CONC)",names(spidmap), val = T)
    if (length(conc_col) != 1) {
      cat("\nEnter the name of the Aliquot Concentration Column in the spidmap.\n")
      cat("Head of spidmap:\n")
      print(head(spidmap))
      conc_col <- readline(prompt = ": ")
    }
    setnames(spidmap, old = conc_col, new = "stock_conc")
    
    # compare the concentrations
    cat("\nAll compounds are assumed to have conc's",expected_concs,"\n(You can change this by setting the 'expected_concs' argument of the fun assign_common_conc()).\n")
    compare_concs <- merge(dat4[wllt == 't', .(source_concs = paste0(sort(unique(signif(conc,3))),collapse=",")), by = c("spid","treatment")], 
                           spidmap[, .(stock_conc, spidmap_guess_concs = paste0(signif(stock_conc/20*expected_concs,3),collapse=",")), by = "spid"],
                           by = "spid", all.x = TRUE)
    if(nrow(compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs)]) > 0) {
      cat("The following concentrations for the following compounds might need to be corrected:\n")
      print(compare_concs[source_concs != spidmap_guess_concs | is.na(spidmap_guess_concs)])
      response <- readline(prompt = "Continue anyways? (y/n): ")
      if (!(response %in% c("y","Y","yes","Yes"))) {
        stop("Update conc's, then re-run\n")
      }
    }
    else {
      cat("All compounds have the expected concetration-corrected values")
    }
  }
  
  # control conc summary:
  cat("\n\nFinal Control Compound Conc Assignments by assay component:\n")
  use_acnms <- c("CCTE_Shafer_MEA_acute_AB", "CCTE_Shafer_MEA_acute_LDH", "CCTE_Shafer_MEA_acute_firing_rate_mean")
  conc_summary <- dat4[wllt != "t" & acnm %in% use_acnms, .(conc = paste0(unique(conc),collapse=",")), by = c("spid","treatment","acnm")]
  conc_originals <- dat4[wllt != "t" & acnm %in% use_acnms, .(conc = paste0(unique(conc_original),collapse=","), acnm = "conc_assigned_in_source_data"), by = c("spid","treatment")]
  conc_summary <- rbind(conc_summary, conc_originals)
  conc_summary <- dcast(conc_summary, treatment + spid ~ acnm, value.var = "conc", fill = "-")[order(spid)]
  setnames(conc_summary, old = c("conc_assigned_in_source_data",use_acnms), new = c("Conc Label in Source File","CellTiter Blue","LDH","MEA components"))
  print(conc_summary[, c("treatment","spid","Conc Label in Source File","CellTiter Blue","LDH","MEA components"), with = FALSE])
  
  dat4[, conc_original := NULL]
  dat4
}


add_acid <- function(dat4, dbname = "invitrodb") {
  if (dbname == "invitrodb") {
    acid_acsn_map <- fread("../acid_acsn_map_invitrodb_2020-06-26.csv")
  }
  else {
    stop("don't have acid map prepared for that dbname yet")
  }
  dat4 <- merge(dat4, acid_acsn_map[, .(acnm,acid)], by = "acnm")
  return(dat4)
}


data_checks <- function(dat4) {
  
  # check that all data is there, nothing is missing
  cat("\nFinal Checks:\n")
  cat("Number of unique acnm's present:",length(unique(dat4$acnm)),"\n")
  cat("Wllq breakdown:\n")
  print(dat4[, .N, by = "wllq"]) # note if wllq is NA anywhere
  dat4[, date_plate := paste(experiment.date, plate.id, sep = "_")]
  cat("Number of plates tested:", length(unique(dat4$date_plate)),"\n")
  cat("Number of experiment dates:", length(unique(dat4$experiment.date)), "\n")
  check.points <- dcast(dat4[, .N, by = c("acnm","date_plate")], date_plate ~ acnm, value.var = "N", fill = 0)
  setnames(check.points, old = names(check.points), new = sub("CCTE_Shafer_MEA_acute_","",names(check.points)))
  
  cat("LDH plates are expected to have ")
  getMode <- function(x) {
    vals <- unique(x)
    num_instances <- sapply(vals, function(val) sum(x == val))
    vals[which(num_instances == max(num_instances))]
  }
  LDH_pts <- dat4[grepl("LDH",acnm), .N, by = c("experiment.date","plate.id")][, getMode(N)]
  cat(LDH_pts, "points.\n")
  
  cat(paste0("\nThe following plates don't have the expected number of points (48 for MEA & AB ",LDH_pts," for LDH):\n"))
  standard_cols <- setdiff(names(check.points), c("date_plate","LDH"))
  pts_flag <- FALSE
  for (date_plate in unique(check.points$date_plate)) {
    
    if (check.points[date_plate, any(.SD != 48), .SDcols = c(standard_cols)]) {
      pts_flag <- TRUE
      MEA_pts <- check.points[date_plate, .(sort(unique(.SD))), .SDcols = setdiff(standard_cols, "AB")]
      print(check.points[date_plate, .(date_plate, AB, LDH, MEA_pts = paste0(sort(unique(unlist(MEA_pts))),collapse=","))])
    }
    else if (check.points[date_plate, c(LDH)] != LDH_pts) {
      pts_flag <- TRUE
      MEA_pts <- check.points[date_plate, .(sort(unique(.SD))), .SDcols = setdiff(standard_cols, "AB")]
      print(check.points[date_plate, .(date_plate, AB, LDH, MEA_pts = paste0(sort(unique(unlist(MEA_pts))),collapse=","))])
    }
  }
  if(!pts_flag) {
    cat("(all plates have the expected number of points for each assay component)\n")
  }
  
  # PLOTS to visually confirm results
 
  # MEA points from -100 to 300
  # by acnm
  default_oma <- par("oma")
  par(oma = c(default_oma[1]+5, default_oma[2:4]))
  boxplot(rval ~ sub("CCTE_Shafer_MEA_acute_","",acnm), dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm)], main = paste0("All MEA components for ",dataset_title,"\nwhere wllq=1 (rval's above 300 not shown)"), 
          ylim = c(-100, 300), las = 2, cex.axis = 0.6, xlab = "")
  par(oma = default_oma)
  
  # by wllt/conc
  dat4[, wllt_conc := ifelse(wllt == "t", paste0(signif(conc,digits=1)), wllt)]
  dat4$wllt_conc <- factor(dat4$wllt_conc, 
                           levels = c(dat4[wllt!="t",sort(unique(wllt))], paste0(dat4[wllt=="t",sort(unique(signif(conc,digits=1)))])), ordered = T)
  stripchart(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval < 300], 
             vertical = T, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", ylab = "rval (percent change in activity)", col = "lightblue", 
             main = paste0("All MEA Components by conc for ",dataset_title,"\nwhere wllq=1 and rval < 300"))
  boxplot(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval < 300], outline = F, col = "transparent", boxwex = 0.5, add = T)
  abline(h = 0, lty = "dashed")
  
  # View the extent of extreme outliers (usually due to very small baseline value)
  stripchart(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval >= 300], 
             vertical = T, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", col = "blue", 
             main = paste0("Outlier MEA Points in ",dataset_title,"\nwhere wllq=1 and rval >= 300"))
  cat("\nSummary of MEA rval's above 300% change by acnm (for wllt 't' or 'n'):\n")
  print(dat4[wllt %in% c("t","n") & wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval >= 300, .(wllts = paste0(sort(unique(wllt)),collapse=","), .N), by = c("acnm")][order(-N)])
  
  # View Cytotox components
  stripchart(rval ~ wllt_conc, dat4[wllq == 1 & grepl("AB",acnm)], 
             vertical = TRUE, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", main = paste0("AB Blank-Corrected Values for ",dataset_title,"\nwhere wllq == 1"))
  stripchart(rval ~ wllt_conc, dat4[wllq == 1 & grepl("LDH",acnm)], 
             vertical = TRUE, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", main = paste0("LDH Blank-Corrected Values for ",dataset_title,"\nwhere wllq == 1"))
  dat4[, wllt_conc := NULL]
}


createWllqSummary <- function(dat4, dataset_title) {
  # eventually make this an xlsx, with addl page for numerical summaries by acnm, plate, date, etc. Including where wllq=0 bc rval is NA
  # maybe also by trt too, so that user can see if somethign needs to be repeat? eh, no big
  dat4[, endpoint_type := ifelse(grepl("(LDH)|(AB)",acnm), "cytotox","mea")]
  wllq_summary <- dat4[wllq == 0 & !wllq_notes == "rval is NA; ", .(treatment = paste0(unique(treatment),collapse=","), spid = paste0(unique(spid),collapse=","), conc = paste0(unique(conc),collapse=","), wllq_notes = paste0(unique(wllq_notes), collapse = ""), endpoints = paste0(unique(endpoint_type),collapse=","), srcf = paste0(unique(srcf),collapse=",")), 
                        by = c("experiment.date","plate.id","rowi","coli")][order(experiment.date,plate.id,rowi,coli)]
  fwrite(wllq_summary, file = paste0(dataset_title,"_summary_of_wells_where_wllq=0.csv"), sep = ",")
  dat4[, endpoint_type := NULL]
  return(paste0(paste0(dataset_title,"_summary_of_wells_where_wllq=0.csv")," is ready."))
}
