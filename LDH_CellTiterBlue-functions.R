# processing LDH data to get percent of total LDH values

prepare_LDH_p_wells <- function(dat4) {
  
  cat("\nPrepare LDH 'p' wells (using Lysis or Half Lysis wells):\n")
  require(stringi)
  
  LDH_dat <- dat4[grepl("LDH",acnm)]
  
  # we are not goign to set wllq to 0 for lysis and 1/2 lysis triplicate wells for now,
  # because there is 1 apid where there would be no 1/2 lysis wells available, 
  # and these values still look very reasonable
  # LDH_dat <- update_wllq_lysis_triplicates(dat4, LDH_dat)
  
  # get a summary of the Lysis wells available for each apid
  apid_pwells <- LDH_dat[grepl("LDH",acnm) & grepl("(ysis)|(LYSIS)",treatment), 
                      .(pwells = paste0(sort(unique(treatment)),collapse=","), any_wllq1 = ifelse(1 %in% unique(wllq),1,0)), by = "apid"]
  
  # for apid that have at least one 1/2 Lysis well with wllq=1, multiply the 1/2 Lysis values by 2 and set wllt = "p"
  half.lysis.plates <- apid_pwells[stri_detect(pwells, fixed = '1/2') & any_wllq1 == 1, apid]
  LDH_dat[grepl("LDH",acnm) & apid %in% half.lysis.plates & stri_detect(treatment, fixed = '1/2'), 
       `:=`(treatment = paste0("2 * ",treatment), rval = 2*rval, wllt = "p")]
  
  # for these plates, set wllt for full lysis wells as "x". We don't want these to be part of the "p" wells on these plates
  LDH_dat[apid %in% half.lysis.plates & grepl("(ysis)|(LYSIS)",treatment) & !stri_detect(treatment, fixed = '1/2'), wllt := "x"]
  
  # for apid with no 1/2 Lysis well with wllq=1, label full Lysis wells as "p"
  LDH_dat[apid %in% apid_pwells[!stri_detect(pwells, fixed = '1/2') & any_wllq1 == 1, apid] & grepl("(ysis)|(LYSIS)",treatment), wllt := "p"]
  
  if (length(apid_pwells[!stri_detect(pwells, fixed = '1/2') & any_wllq1 == 1, apid]) > 1) {
    cat("The following apid's do not have any 1/2 Lysis wells with wllq=1. Full Lysis wells will be used instead\n")
    cat(sort(apid_pwells[!stri_detect(pwells, fixed = '1/2') & any_wllq1 == 1, apid]), sep = "\n")
  }
  
  cat("Treatments assigned to wllt 'p' for each apid:\n")
  pwell_summary <- LDH_dat[wllt == "p" & wllq == 1, .(LDH_trts_in_p_wells = paste0(sort(unique(treatment)),collapse=","), .N), by = "apid"]
  print(pwell_summary)
  
  # check if any apid does not have any lysis wells, or none with wllq = 1
  # or, if I grep'ed the wrong treatment name "ysis" or "LYSIS" above
  no_p_apid <- setdiff(unique(LDH_dat$apid), unique(pwell_summary$apid))
  if (length(no_p_apid) > 0) {
    cat("The following apid's do not have any Lysis wells with wllq=1.\n")
    cat(no_p_apid, sep = "\n")
  }
  
  # LDH positive control wells - remove these wells, because I'm not sure how to assign the conc, wllt (can't be 'p') or spid
  # might add back in the future
  LDH_dat <- LDH_dat[!(treatment %in% c("1:250 LDH","1:2500 LDH"))]
  
  # plot the results for visualization
  yrange <- LDH_dat[, range(rval, na.rm = T)]
  boxplot(rval ~ apid, LDH_dat[wllq == 1 & !(wllt %in% c("p","x"))], ylim = yrange, main = "LDH Blank-Corrected values by apid, where wllq=1", las = 2, cex.axis = 0.75)
  stripchart(rval ~ apid, LDH_dat[wllt == "p" & wllq == 1],
             vertical = T, add = T,col = "gray")
  stripchart(V1 ~ apid, LDH_dat[wllt == "p" & wllq == 1, median(rval), by = "apid"],
             vertical = T, add = T,col = "blue")
  legend(x = "topleft", legend = c("p wells","median p wells"), pch = c(0,0), col = c("gray","blue"), bg = "transparent")
  
  # summary of median 1/2 lysis wells by apid
  cat("\nSummary of median p wells by apid:\n")
  print(LDH_dat[wllt == "p" & wllq == 1, .(pval = median(rval)), by = "apid"])
  
  # replace previous LDH data with new LDH_dat
  dat4 <- dat4[!grepl("LDH",acnm)]
  dat4 <- rbind(dat4, LDH_dat, fill = T) # dat4 might not have wllt column yet
  return(dat4)

}

# deprecated/not using at this time
update_wllq_lysis_triplicates <- function(dat4, LDH_dat) {
  
  # need to verify this function before using
  # if we ever decide to remove the 1/2 lysis wells derived from Lysis well with wllq==0.
  
  # find the Lysis wells used
  # (Since the Lysis is sometimes added after teh LDH plate is created, but always before the CTB experiment, 
  # the AB data reliably tells me which well was Lysed on each MEA plate.id)
  lysis_wells <- dat4[grepl("AB",acnm) & spid == "Tritonx100", .(experiment.date, plate.id, rowi, coli, wllq, wllq_notes, spid)]
  
  # confirm there is exactly 1 Lysis well per plate
  if(nrow(lysis_wells[, .N, by = c("experiment.date","plate.id")][N != 1]) > 0 ) {
    stop(paste0("The following plates do not have exactly 1 Lysis well:\n",lysis_wells[, .N, by = c("experiment.date","plate.id")][N != 1]))
  }
  
  # if wllq==0 for an AB well only because the rval is 0, then set that wllq assignment to 1 for the LDH wells
  lysis_wells[wllq_notes == "rval is NA; ", `:=`(wllq = 1, wllq_notes = "")]
  
  # remove "rval is NA; " from the wllq notes string
  lysis_wells[, wllq_notes := sub("rval is NA; ","",wllq_notes)]
  
  # append the wllq info for the MEA plate Lysis wells, from which all other Lysis wells are the plate were derived.
  # e.g. the 3 Lysis and 3 1/2 Lysis wells in Row H of the LDH plate derived from the F1 Lysis well
  LDH_dat <- merge(LDH_dat, lysis_wells, by = c("experiment.date","plate.id","spid"), all.x = T, suffixes = c("",".mea_plate"))
  
  # update the wllq in the triplecate Lysis and 1/2 Lysis wells where needed
  if (LDH_dat[wllq == 1 & wllq.mea_plate == 0, any(rowi < 7)]) {
    stop("Unexpected rows matched Lysis wells update.")
  }
  LDH_dat[wllq == 1 & wllq.mea_plate == 0, `:=`(wllq = 0, wllq_notes = paste0("MEA plate Lysis well ",LETTERS[rowi.mea_plate], coli.mea_plate, " has wllq = 0: ",wllq_notes.mea_plate))]
  cat("wllq updated for the following LDH plate wells:\n")
  print(LDH_dat[grepl("MEA plate Lysis well",wllq_notes), .(experiment.date, rowi, coli, treatment, wllq, wllq_notes, rval)])
  LDH_dat[, grep(".mea_plate",names(LDH_dat), val = T) := NULL]
  
}


# (deprecated function, not using this)
calculate_per_total_LDH <- function(dat4, use_half_lysis = T) {
  
  # !note that I need the wllq assignments before I can do this, so get do it right after creating cytodat
  # Also good to verify treatments before doing this
  
  if (use_half_lysis) {
    # calculate the median DMSO well in each apid
    print(dat4[acnm == "NHEERL_MEA_acute_LDH" & grepl("? Lysis",treatment) & wllq == 1, median(rval), by = "apid"][order(apid)])
    print(dat4[acnm == "NHEERL_MEA_acute_LDH" & grepl("? Lysis",treatment) & wllq == 1, summary(rval)])
    # we have to use the median, because we are trying to avoid situation where 1 plate is just really bad
    # don't want the small values there to pull down the median
    # maybe in the future I will look into 
    
    yrange <- dat4[grepl("LDH",acnm) & !grepl("(LDH)",treatment) & wllq == 1, range(rval)]
    boxplot(rval ~ apid, dat4[acnm == "NHEERL_MEA_acute_LDH" & treatment %in% c("DMSO","PICRO","TTX") & wllq == 1], ylim = yrange,
            main = "LDH Blank-Corrected Values in DMSO, PICRO, and TTX wells by apid\n Squares are ? Lysis wells, Median ? Lysis wells are Blue")
    stripchart(rval ~ apid, dat4[acnm == "NHEERL_MEA_acute_LDH" & grepl("? Lysis",treatment) & wllq == 1],
               vertical = T, add = T, col = "gray")
    stripchart(V1 ~ apid, dat4[acnm == "NHEERL_MEA_acute_LDH" & grepl("? Lysis",treatment) & wllq == 1, median(rval), by = "apid"],
               vertical = T, add = T, col = "blue")
    
    # # check out by plate for the first culture, which looks a bit concerning
    # boxplot(rval ~ plate.id, dat4[apid == "20190528" & acnm == "NHEERL_MEA_acute_LDH" & treatment %in% c("DMSO","PICRO","TTX") & wllq == 1], ylim = c(-0.1, 1.7))
    # stripchart(rval ~ plate.id, dat4[apid == "20190528" & acnm == "NHEERL_MEA_acute_LDH" & grepl("? Lysis",treatment) & wllq == 1],
    #            vertical = T, add = T)
    # # no, plate68-0811 is not lower in DMSO wells than the other 2
    # 
    # # seems silly, but what if we use mean instead?
    # boxplot(rval ~ apid, dat4[acnm == "NHEERL_MEA_acute_LDH" & treatment %in% c("DMSO","PICRO","TTX") & wllq == 1], ylim = c(-0.1, 1.7))
    # stripchart(V1 ~ apid, dat4[acnm == "NHEERL_MEA_acute_LDH" & grepl("? Lysis",treatment) & wllq == 1, mean(rval), by = "apid"],
    #            vertical = T, add = T)
    # # that does look better for the first exp date... but is that enough to justify it?
    
    # extract the LDH data
    LDH_dat <- dat4[acnm == "NHEERL_MEA_acute_LDH"] # wllq should already be zero where rval is NA
    positive.controls <- LDH_dat[wllq == 1 & treatment == "? Lysis", .(double.half.lysis.median.apid = 2*median(rval)), by = "apid"]
    # make sure every apid has a positive control (possibly not if none wllq==1)
    if (nrow(positive.controls) != length(unique(dat4$apid))) stop(paste0(setdiff(unique(dat4$apid), positive.controls$apid)," does not have any 1/2 Lysis wells with wllq=1"))
    LDH_dat <- merge(LDH_dat, positive.controls, by = "apid")
    
    # divide the rval's by the median double half lysis rval's by apid
    LDH_dat[, rval := (rval/double.half.lysis.median.apid)*100]
    LDH_dat[, double.half.lysis.median.apid := NULL]
    
    # trt, conc, wllt labels for LDH wells - I'm not sure what to do with these, will just remove for now
    LDH_dat <- LDH_dat[!(treatment %in% c("1:250 LDH","1:2500 LDH"))]
    
    # # trt, conc, wllt for Lysis wells
    # LDH_dat[treatment == "Lysis", wllt := "p"] # includes well F1, if labelled Lysis vs Media, as well as 3 wells in row H
    # LDH_dat[treatment == "Lysis", conc := 10]
    # 
    # # trt, conc, wllt for 1/2 Lysis wells - note the rval's here have not been multiplied by 2
    # LDH_dat[treatment == "? Lysis", wllt := "p"]
    # LDH_dat[treatment == "Lysis", conc := 5]
  }
  # (for ToxCast, set the 1 lysis F1 well to "n". Don't need any extra rows)
  if (!use_half_lysis) {
    # just use Lysis wells to normalize
    LDH_dat <- dat4[acnm == "NHEERL_MEA_acute_LDH"] # wllq should already be zero where rval is NA
    positive.controls <- LDH_dat[wllq == 1 & treatment %in% c("Lysis","LYSIS"), .(lysis.median.apid = median(rval)), by = "apid"]
    if (nrow(positive.controls) != length(unique(dat4$apid))) stop(paste0(setdiff(unique(dat4$apid), positive.controls$apid)," does not have any Lysis wells with wllq=1"))
    LDH_dat <- merge(LDH_dat, positive.controls, by = "apid")
    LDH_dat[, rval := (rval / lysis.median.apid)*100]
    LDH_dat[, lysis.median.apid := NULL]
    
  }
  
  # replace previous LDH data with new LDH_dat
  dat4 <- dat4[acnm != "NHEERL_MEA_acute_LDH"]
  dat4 <- rbind(dat4, LDH_dat)
  return(dat4)
}