# function to screen wells that are outside 2 SD from the mean for Mean Firing Rate rval
# Mean and SD based on the entire data set

remove_dmso_outliers <- function(dat4, view_acnm = "CCTE_Shafer_MEA_acute_firing_rate_mean") {
  
  require(data.table)

  # how many wells will be removed with 2*SD +/- mean?
  mean_dmso_mfr <- dat4[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & wllq == 1 & wllt == "n", mean(rval)]
  sd_dmso_mfr <- dat4[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & wllq == 1 & wllt == "n", sd(rval)]
  print("Number of wllt='n' wells with MFR rval outside of bounds:")
  print(dat4[acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & wllt == "n" & abs(rval - mean_dmso_mfr) > 2*sd_dmso_mfr, .N, by = c("origin","wllq")][order(-wllq)])
  
  # extract the wells we want to remove
  dmso_outside_wells <- dat4[wllt == "n" & acnm == "CCTE_Shafer_MEA_acute_firing_rate_mean" & abs(rval - mean_dmso_mfr) > 2*sd_dmso_mfr, .(spid, apid, experiment.date, plate.id, rowi, coli, wllt, remove_dmso_low_mfr_rval = TRUE)]
  dat4 <- merge(dat4, dmso_outside_wells, by = c("spid","apid","experiment.date","plate.id","rowi","coli","wllt"), all = T)
  # dat4[grepl("(AB)|(LDH)",acnm), remove_dmso_low_mfr_rval := FALSE] # don't remove cytotoxicity values for this reason - edit 7/16/20: Tim says we should remove all values for these wells
  
  # visualize the results
  dat4$apid <- factor(dat4$apid, levels = sort(unique(dat4$apid)), ordered = T)
  
  # plot parameters to set
  wllq0_col <- "palevioletred"
  boxplot_col <- "black"
  remove_col <- "red"
  
  graphics.off()
  pdf(file = paste0("lvl0_snapshots/figs/dmso_outlier_visualization_",view_acnm,"_",as.character.Date(Sys.Date()),".pdf"), height = 8, width = 12, pointsize = 10)
  y_label <- ifelse(grepl("(AB)|(LDH)",view_acnm),paste0("Blank-corrected Value for ",sub("CCTE_Shafer_MEA_acute_","",view_acnm)), 
                    paste0("Percent Change in ",sub("CCTE_Shafer_MEA_acute_","",view_acnm)))
  stripchart(rval ~ apid, dat4[acnm == view_acnm & wllt == "n" & wllq == 1], 
             pch = 1, vertical = T, method = "jitter",
             ylim = range(dat4[acnm == view_acnm & wllt == "n", rval], na.rm=T), cex.axis = 0.75, las = 2, ylab = y_label)
  stripchart(rval ~ apid, dat4[acnm == view_acnm & wllt == "n" & wllq == 0], 
             pch = 1, vertical = T, method = "jitter",
             add = T, col = wllq0_col)
  stripchart(med_rval ~ apid, dat4[acnm == view_acnm & wllt == "n" & wllq == 1, .(med_rval = median(rval)), by = "apid"], 
             pch = 19, vertical = T, method = "jitter",
             add = T)
  mean_acnm <- dat4[acnm == view_acnm & wllq == 1 & wllt == "n", mean(rval)]
  sd_acnm <- dat4[acnm == view_acnm & wllq == 1 & wllt == "n", sd(rval)]
  abline(h = 2*sd_acnm + mean_acnm, lty = "dashed", col = "blue")
  abline(h = -2*sd_acnm + mean_acnm, lty = "dashed", col = "blue")
  stripchart(rval ~ apid, dat4[acnm == view_acnm & wllt == "n" & wllq == 1 & remove_dmso_low_mfr_rval == TRUE], 
             pch = 19, vertical = T, method = "jitter",
             add = T, col = remove_col)
  abline(v = dat4[, as.numeric(max(apid))+0.5, by = "origin"]$V1, lty = "dashed")
  text(x = sort(dat4[, .(mean(c(as.numeric(max(apid)),as.numeric(min(apid))))), by = "origin"]$V1), y = 0.85*max(dat4[acnm==view_acnm & wllt == "n", rval],na.rm = T) +c(0,0,0,-10), labels = dat4[order(apid), unique(origin)], cex = 0.75, adj = 0.5)
  legend(x = "topleft", legend = c("DMSO wllq=1","Median DMSO","DMSO wllq=0","Mean DMSO +/-2SD","DMSO MFR outside mean +/-2SD"),
         col = c("black","black",wllq0_col,"blue",remove_col), pch = c(1,19,1,95,19),
         bg = "transparent", cex = 0.75)
  title(main = paste0("DMSO wells Removed Where MFR More than 2 SD from the Mean of DMSO wells\n",sub("CCTE_Shafer_MEA_acute_","",view_acnm)," ",min(dat4$apid)," - ",max(dat4$apid)))
  graphics.off()
  
  # set wllq to 0 for these wells
  print("Wells that will be set to wllq=0 for each acnm:")
  print(dat4[remove_dmso_low_mfr_rval == TRUE, .N, by = "acnm"]) # confirming this looks right
  dat4[remove_dmso_low_mfr_rval == TRUE, `:=`(wllq = 0, wllq_notes = paste0(wllq_notes, "DMSO % change MFR > 2*SD from the mean; "))]
  dat4[, remove_dmso_low_mfr_rval := NULL]
  
  print(paste0(paste0("lvl0_snapshots/figs/dmso_outlier_visualization_",view_acnm,"_",as.character.Date(Sys.Date()),".pdf"), " is ready."))
  dat4
}