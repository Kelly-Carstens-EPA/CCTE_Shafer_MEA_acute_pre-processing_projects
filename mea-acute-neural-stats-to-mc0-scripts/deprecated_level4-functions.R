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
  boxplot(rval ~ sub("CCTE_Shafer_MEA_acute_","",acnm), dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm)], main = paste0("All MEA components for ",project_name,"\nwhere wllq=1 (rval's above 300 not shown)"), 
          ylim = c(-100, 300), las = 2, cex.axis = 0.6, xlab = "")
  par(oma = default_oma)
  
  # by wllt/conc
  dat4[, wllt_conc := ifelse(wllt == "t", paste0(signif(conc,digits=1)), wllt)]
  dat4$wllt_conc <- factor(dat4$wllt_conc, 
                           levels = c(dat4[wllt!="t",sort(unique(wllt))], paste0(dat4[wllt=="t",sort(unique(signif(conc,digits=1)))])), ordered = T)
  stripchart(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval < 300], 
             vertical = T, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", ylab = "rval (percent change in activity)", col = "lightblue", 
             main = paste0("All MEA Components by conc for ",project_name,"\nwhere wllq=1 and rval < 300"))
  boxplot(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval < 300], outline = F, col = "transparent", boxwex = 0.5, add = T)
  abline(h = 0, lty = "dashed")
  
  # View the extent of extreme outliers (usually due to very small baseline value)
  stripchart(rval ~ wllt_conc, dat4[wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval >= 300], 
             vertical = T, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", col = "blue", 
             main = paste0("Outlier MEA Points in ",project_name,"\nwhere wllq=1 and rval >= 300"))
  cat("\nSummary of MEA rval's above 300% change by acnm (for wllt 't' or 'n'):\n")
  print(dat4[wllt %in% c("t","n") & wllq == 1 & !grepl("(AB)|(LDH)",acnm) & rval >= 300, .(wllts = paste0(sort(unique(wllt)),collapse=","), .N), by = c("acnm")][order(-N)])
  
  # View Cytotox components
  stripchart(rval ~ wllt_conc, dat4[wllq == 1 & grepl("AB",acnm)], 
             vertical = TRUE, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", main = paste0("AB Blank-Corrected Values for ",project_name,"\nwhere wllq == 1"))
  stripchart(rval ~ wllt_conc, dat4[wllq == 1 & grepl("LDH",acnm)], 
             vertical = TRUE, pch = 1, method = "jitter", xlab = "wllt or approx. conc for 't' wells", main = paste0("LDH Blank-Corrected Values for ",project_name,"\nwhere wllq == 1"))
  dat4[, wllt_conc := NULL]
}


createWllqSummary <- function(dat4, project_name) {
  # eventually make this an xlsx, with addl page for numerical summaries by acnm, plate, date, etc. Including where wllq=0 bc rval is NA
  # maybe also by trt too, so that user can see if somethign needs to be repeat? eh, no big
  dat4[, endpoint_type := ifelse(grepl("(LDH)|(AB)",acnm), "cytotox","mea")]
  wllq_summary <- dat4[wllq == 0 & !wllq_notes == "rval is NA; ", .(treatment = paste0(unique(treatment),collapse=","), spid = paste0(unique(spid),collapse=","), conc = paste0(unique(conc),collapse=","), wllq_notes = paste0(unique(wllq_notes), collapse = ""), endpoints = paste0(unique(endpoint_type),collapse=","), srcf = paste0(unique(srcf),collapse=",")), 
                        by = c("experiment.date","plate.id","rowi","coli")][order(experiment.date,plate.id,rowi,coli)]
  fwrite(wllq_summary, file = paste0(project_name,"_summary_of_wells_where_wllq=0.csv"), sep = ",")
  dat4[, endpoint_type := NULL]
  return(paste0(paste0(project_name,"_summary_of_wells_where_wllq=0.csv")," is ready."))
}
