# processing LDH data to get percent of total LDH values

processLDH <- function(dat4, use_half_lysis = T) {
  
  # !note that I need the wllq assignments before I can do this, so get do it right after creating cytodat
  # Also good to verify treatments before doing this
  
  if (use_half_lysis) {
    # calculate the median DMSO well in each apid
    print(dat4[acsn == "NHEERL_MEA_acute_LDH" & grepl("½ Lysis",treatment) & wllq == 1, median(rval), by = "apid"][order(apid)])
    print(dat4[acsn == "NHEERL_MEA_acute_LDH" & grepl("½ Lysis",treatment) & wllq == 1, summary(rval)])
    # we have to use the median, because we are trying to avoid situation where 1 plate is just really bad
    # don't want the small values there to pull down the median
    # maybe in the future I will look into 
    
    yrange <- dat4[grepl("LDH",acsn) & !grepl("(LDH)",treatment) & wllq == 1, range(rval)]
    boxplot(rval ~ apid, dat4[acsn == "NHEERL_MEA_acute_LDH" & treatment %in% c("DMSO","PICRO","TTX") & wllq == 1], ylim = yrange,
            main = "LDH Blank-Corrected Values in DMSO, PICRO, and TTX wells by apid\n Squares are ½ Lysis wells, Median ½ Lysis wells are Blue")
    stripchart(rval ~ apid, dat4[acsn == "NHEERL_MEA_acute_LDH" & grepl("½ Lysis",treatment) & wllq == 1],
               vertical = T, add = T, col = "gray")
    stripchart(V1 ~ apid, dat4[acsn == "NHEERL_MEA_acute_LDH" & grepl("½ Lysis",treatment) & wllq == 1, median(rval), by = "apid"],
               vertical = T, add = T, col = "blue")
    
    # # check out by plate for the first culture, which looks a bit concerning
    # boxplot(rval ~ plate.id, dat4[apid == "20190528" & acsn == "NHEERL_MEA_acute_LDH" & treatment %in% c("DMSO","PICRO","TTX") & wllq == 1], ylim = c(-0.1, 1.7))
    # stripchart(rval ~ plate.id, dat4[apid == "20190528" & acsn == "NHEERL_MEA_acute_LDH" & grepl("½ Lysis",treatment) & wllq == 1],
    #            vertical = T, add = T)
    # # no, plate68-0811 is not lower in DMSO wells than the other 2
    # 
    # # seems silly, but what if we use mean instead?
    # boxplot(rval ~ apid, dat4[acsn == "NHEERL_MEA_acute_LDH" & treatment %in% c("DMSO","PICRO","TTX") & wllq == 1], ylim = c(-0.1, 1.7))
    # stripchart(V1 ~ apid, dat4[acsn == "NHEERL_MEA_acute_LDH" & grepl("½ Lysis",treatment) & wllq == 1, mean(rval), by = "apid"],
    #            vertical = T, add = T)
    # # that does look better for the first exp date... but is that enough to justify it?
    
    # extract the LDH data
    LDH_dat <- dat4[acsn == "NHEERL_MEA_acute_LDH"] # wllq should already be zero where rval is NA
    positive.controls <- LDH_dat[wllq == 1 & treatment == "½ Lysis", .(double.half.lysis.median.apid = 2*median(rval)), by = "apid"]
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
    # LDH_dat[treatment == "½ Lysis", wllt := "p"]
    # LDH_dat[treatment == "Lysis", conc := 5]
  }
  # (for ToxCast, set the 1 lysis F1 well to "n". Don't need any extra rows)
  if (!use_half_lysis) {
    # just use Lysis wells to normalize
    LDH_dat <- dat4[acsn == "NHEERL_MEA_acute_LDH"] # wllq should already be zero where rval is NA
    positive.controls <- LDH_dat[wllq == 1 & treatment %in% c("Lysis","LYSIS"), .(lysis.median.apid = median(rval)), by = "apid"]
    if (nrow(positive.controls) != length(unique(dat4$apid))) stop(paste0(setdiff(unique(dat4$apid), positive.controls$apid)," does not have any Lysis wells with wllq=1"))
    LDH_dat <- merge(LDH_dat, positive.controls, by = "apid")
    LDH_dat[, rval := (rval / lysis.median.apid)*100]
    LDH_dat[, lysis.median.apid := NULL]
    
  }
  
  # replace previous LDH data with new LDH_dat
  dat4 <- dat4[acsn != "NHEERL_MEA_acute_LDH"]
  dat4 <- rbind(dat4, LDH_dat)
  return(dat4)
}


# Alamar Blue
# want to normalize by whatever is a DMSO control.

# question: will the trt assignment of LDH and AB wells every change from what is in file?
# a - yes, at least for AB
# q - what are the options for normalization?

# for AB, just set DMSO to n, then bval.apid.nwlls.med, then resp as resp.fc = function(aeids) {
# 
# e1 <- bquote(dat[J(.(aeids)), resp := cval/bval])
# list(e1)
# 
# }

# For LDH, I could, set 1/2 Lysis wells to "n", then set bval to median of "n" wells, then resp = cval/bval
# Or, set 1/2 Lysis wells to "p", then set bval to 0 (not an option), then use resp = (cval - bval)/pval * 100