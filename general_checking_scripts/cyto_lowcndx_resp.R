# resp cndx 1 adn 2 for cytotoxicity endpoints
library(data.table)
library(tcpl)

setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
source("analysis-functions/get_latest_dat.R")

dat3 <- get_latest_dat(lvl = "dat3")
# [1] "Getting data from folders APCRA2019, DNT2019, ToxCast2016"
# [1] "APCRA2019_dat3_2020-06-15.RData"
# Loading objects:
#   dat3
# [1] "DNT2019_dat3_2020-06-10.RData"
# Loading objects:
#   dat3
# [1] "ToxCast2016_dat3_2020-06-10.RData"
# Loading objects:
#   dat3

dat4 <- get_latest_dat(lvl = "dat4")
# [1] "Getting data from folders APCRA2019, DNT2019, ToxCast2016"
# [1] "APCRA2019_dat4_2020-06-15.RData"
# Loading objects:
#   dat4
# [1] "DNT2019_dat4_2020-06-10.RData"
# Loading objects:
#   dat4
# [1] "ToxCast2016_dat4_2020-06-10.RData"
# Loading objects:
#   dat4

dat3_cyto <- dat3[grepl("(AB)|(LDH)",acsn)]
dat4_cyto <- dat4[grepl("(AB)|(LDH)",acsn)]

cyto <- merge(dat3_cyto, dat4_cyto[, .(rval, treatment, spid, conc, rowi, coli, plate.id, experiment.date, wllq, wllq_notes, wllt)], by = c("plate.id","experiment.date","rowi","coli"),
      suffixes = c(".dat3",".dat4"))
setnames(cyto, old = c("wllq.dat4","wllq_notes.dat4","treatment.dat4","conc.dat4"), new = c("wllq","wllq_notes","treatment","conc"))
cyto[, c("wllq.dat3","wllq_notes.dat3","treatment.dat3","conc.dat3") := list(NULL)]
cyto[is.na(origin), origin := "ToxCast2016"]

# ASSIGN cndx --------------------------------------------------------------------
# taken from mc1 tcpl. ACID changed to acsn
get_cndx <- function(dat) {
  ## Set conc to three significant figures
  dat[ , conc := signif(conc, 3)]
  
  ## Define replicate id
  # Order by the following columns
  setkeyv(dat, c('acsn', 'srcf', 'apid', 'coli', 'rowi', 'spid', 'conc')) 
  # Define rpid column for test compound wells
  nconc <- dat[wllt == "t" , 
               list(n = lu(conc)), 
               by = list(acsn, apid, spid)][ , list(nconc = min(n)), by = acsn]
  dat[wllt == "t" & acsn %in% nconc[nconc > 1, acsn],
      rpid := paste(acsn, spid, wllt, srcf, apid, "rep1", conc, sep = "_")]
  dat[wllt == "t" & acsn %in% nconc[nconc == 1, acsn],
      rpid := paste(acsn, spid, wllt, srcf, "rep1", conc, sep = "_")]
  # Define rpid column for non-test compound wells
  dat[wllt != "t", 
      rpid := paste(acsn, spid, wllt, srcf, apid, "rep1", conc, sep = "_")] 
  # Increment rpid 
  dat_rpid <- dat[ , rpid]
  j = 2L
  while (any(duplicated(dat_rpid))) {
    ind <- duplicated(dat_rpid)
    dat_rpid[ind] <- sub("_rep[0-9]+", paste0("_rep", j), dat_rpid[ind])
    j <- j + 1
  }
  dat[ , rpid := dat_rpid]
  rm(dat_rpid)
  # Remove conc values from rpid
  dat[ , rpid := sub("_([^_]+)$", "", rpid, useBytes = TRUE)]
  
  ## Define concentration index
  indexfunc <- function(x) as.integer(rank(unique(x))[match(x, unique(x))])
  dat[ , cndx := indexfunc(conc), by = list(rpid)]
  return(dat)
}
# --------------------------------------------------------------------------

cyto <- get_cndx(cyto)
cyto[wllt == "n", cndx := 0]
boxplot(rval.dat3 ~ cndx, cyto[grepl("AB",acsn) & wllt %in% c("n","t") & wllq == 1])
stripchart(rval.dat3 ~ cndx, cyto[grepl("AB",acsn) & wllt %in% c("n","t") & wllq == 1], pch = 1, method = "jitter", vertical = T)
dmso_med_table <- cyto[grepl("AB",acsn) & wllt %in% c("n") & wllq == 1, .(dmso_median = median(rval.dat3, na.rm = T)), by = "apid"]
cndx1_med_table <- cyto[grepl("AB",acsn) & wllt %in% c("t") & wllq == 1 & cndx == 1, .(cndx1_median = median(rval.dat3, na.rm = T)), by = "apid"]
cndx2_med_table <- cyto[grepl("AB",acsn) & wllt %in% c("t") & wllq == 1 & cndx == 2, .(cndx2_median = median(rval.dat3, na.rm = T)), by = "apid"]
# med_table <- merge(merge(cndx2_med_table, cndx1_med_table), dmso_med_table)
# med_table[, `:=`(dmso1_diff = abs(cndx1_median - dmso_median), dmso2_diff = abs(cndx2_median - dmso_median))]
# med_table[, max_diff := ifelse(dmso1_diff>dmso2_diff,dmso1_diff, dmso2_diff)]
# med_table <- med_table[order(-max_diff)]
cndx12_med_table <- cyto[grepl("LDH",acsn) & wllt %in% c("t") & wllq == 1 & cndx %in% c(1,2), .(cndx12_median = median(rval.dat3, na.rm = T)), by = "apid"]
med_table <- merge(merge(merge(cndx2_med_table, cndx1_med_table), cndx12_med_table), dmso_med_table)
med_table[, abs_diff := abs(cndx12_median - dmso_median)]
med_table <- med_table[order(-abs_diff)]
med_table[, rank := c(1:nrow(med_table))]

# let's see the top 16 crazy plates
par(mfrow = c(4,4), oma = c(0,0,3,0))
yrange <- cyto[grepl("AB",acsn), range(rval.dat3, na.rm = T)]
for (plate in med_table$apid[1:16]) {
  boxplot(rval.dat3 ~ cndx, cyto[apid == plate & grepl("AB",acsn) & wllt %in% c("n","t") & wllq == 1], ylim = yrange)
  twolow_med <- cyto[apid == plate & grepl("AB",acsn) & wllt %in% c("n","t") & wllq == 1 & cndx %in% c(0,1,2), median(rval.dat3, na.rm = T)]
  dmso_med <- cyto[apid == plate & grepl("AB",acsn) & wllt %in% c("n") & wllq == 1, median(rval.dat3, na.rm = T)]
  abline(h = twolow_med, col = "red")
  abline(h = dmso_med, col = "blue")
  title(main = paste0(cyto[apid == plate, unique(origin)], " ",plate))
}
mtext(text = paste0("Alamar Blue Blank-corrected Fluorescence by Cndx"), outer = T, cex = 1.5)
legend()
cyto[grepl("AB",acsn) & wllt %in% c("n","t"), .N, by = "cndx"]

cyto[grepl("AB",acsn) & apid == "20150730" & wllt == 'n', .(rowi, coli, treatment, conc, rval.dat3, wllq)]

# if bval is made lower by cndx 1 and 2 for AB,
# then values are divided by a smaller in magnitude value
# which means the the cval/bval values might be larger in magnitude
# which means that 1 - cval/bval would be smaller
# right, which coudl lead to removed hits. 

# for the 18 removed hits, was bval on the large-end?
# or did bval go from unusually high to more normal?
load("sbox_dat/sbox_dat_2020-06-11.RData")
mc3_mthds # yes, this run did use bval.apid.nwllslowconc.med for LDH and AB normalization
mc3[acid == 2440, summary(bval)]

cyto[grepl("AB",acsn) & spid == "TP0001411H06", unique(apid)] # "20150924"

# for several of these removed hits... even if they were divided by a slightly larger bval...
# most were still borderline hits, right? Let's verify that

mc5_org <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/output/01APR2020/mc5_mc6_nheerl_mea_acute.csv", stringsAsFactors = FALSE)
setDT(mc5_org)
setnames(mc5_mc6, old = c("flag_ids","flag_length"), new = c("mc6_flags","flag.length"))
usecols <- c("aenm","spid","dsstox_substance_id","casn","hitc","aeid","mc6_flags","flag.length")

# can only combine for compounds that were ran previously, the toxcast compounds
cmc5 <- merge(mc5_mc6[, ..usecols], mc5_org[,..usecols], by = setdiff(usecols,c("hitc","aeid","mc6_flags","flag.length")), suffixes = c(".new",".org"))

# see removed hits
cmc5[grepl("AB",aenm) & hitc.new == 0 & hitc.org == 1]
# flags: 11 = border.hit
cmc5[grepl("AB",aenm) & hitc.new == 0 & hitc.org == 1 & !grepl("11",mc6_flags.org)] # 8 of the 18 have this flag
# 6 = single.hit.high
cmc5[grepl("AB",aenm) & hitc.new == 0 & hitc.org == 1 & !grepl("11",mc6_flags.org) & !grepl("6",mc6_flags.org)] # 12 of the 18 have this flag
# only 4 in this category. all have flag 17, efficacy less than 50%. If this meaninful here though?
# curious if these are from the same plate...
see_spids <- cmc5[grepl("AB",aenm) & hitc.new == 0 & hitc.org == 1 & !grepl("11",mc6_flags.org) & !grepl("6",mc6_flags.org), c(spid)]
dat4[spid %in% see_spids, unique(apid), by = "spid"]
# spid       V1
# 1: TP0001413C01 20160517
# 2: TP0001414F01 20160419
# 3: TP0001414E12 20160419
# 4: TP0001414E11 20160419
med_table[apid == "20160419"]
# apid cndx2_median cndx1_median dmso_median dmso1_diff dmso2_diff max_diff
# 1: 20160419     33106.17     33027.33    32827.33        200   278.8333 278.8333
# literally the 3rd smallest difference from dmso median of all 99 plates. I don't think that is why these hits were removed
med_table[apid == "20160517"] # 77th out of 99 largest max_diff from dmso median. Again, I think we're okay here
# hits were probably removed because of changes in bmad/coff overall
# could even just be because we are pipelining with more compounds now.
# I feel okay with using cndx 1 and 2 for AB


# LDH ----------------------------------------------------------------------
dmso_med_table <- cyto[grepl("LDH",acsn) & wllt %in% c("n") & wllq == 1, .(dmso_median = median(rval.dat3, na.rm = T)), by = "apid"]
cndx1_med_table <- cyto[grepl("LDH",acsn) & wllt %in% c("t") & wllq == 1 & cndx == 1, .(cndx1_median = median(rval.dat3, na.rm = T)), by = "apid"]
cndx2_med_table <- cyto[grepl("LDH",acsn) & wllt %in% c("t") & wllq == 1 & cndx == 2, .(cndx2_median = median(rval.dat3, na.rm = T)), by = "apid"]
cndx12_med_table <- cyto[grepl("LDH",acsn) & wllt %in% c("t") & wllq == 1 & cndx %in% c(1,2), .(cndx12_median = median(rval.dat3, na.rm = T)), by = "apid"]
med_table <- merge(merge(merge(cndx2_med_table, cndx1_med_table), cndx12_med_table), dmso_med_table)
# med_table[, `:=`(dmso1_diff = abs(cndx1_median - dmso_median), dmso2_diff = abs(cndx2_median - dmso_median))]
# med_table[, max_diff := ifelse(dmso1_diff>dmso2_diff,dmso1_diff, dmso2_diff)]
med_table[, abs_diff := abs(cndx12_median - dmso_median)]
med_table <- med_table[order(-abs_diff)]
med_table[, rank := c(1:nrow(med_table))]

# let's see the top 16 crazy plates
par(mfrow = c(4,4), oma = c(0,0,3,0))
# yrange <- cyto[grepl("LDH",acsn) & wllt %in% c("n","t") & wllq == 1, range(rval.dat3, na.rm=T)]
for (plate in med_table$apid[1:16]) {
  boxplot(rval.dat3 ~ cndx, cyto[apid == plate & grepl("LDH",acsn) & wllt %in% c("n","t") & wllq == 1])
  twolow_med <- cyto[apid == plate & grepl("LDH",acsn) & wllt %in% c("n","t") & wllq == 1 & cndx %in% c(0,1,2), median(rval.dat3, na.rm = T)]
  dmso_med <- cyto[apid == plate & grepl("LDH",acsn) & wllt %in% c("n") & wllq == 1, median(rval.dat3, na.rm = T)]
  abline(h = twolow_med, col = "red")
  abline(h = dmso_med, col = "blue")
  title(main = paste0(cyto[apid == plate, unique(origin)], " ",plate))
}
mtext(text = paste0("LDH Blank-corrected Optical Density by Cndx"), outer = T, cex = 1.5)

med_table[dmso_median < cndx12_median] # only 17 of the 99 plates have cndx12med > dmso_median
# and these abs diff's aren't even that big (largest ranks 26th out of all abs diff's)

# lookign at boxplots, so many dmso wells are high?? Why all this "death" in dmso wells??
# if we do normalize/zero-center, I think using cndx 1 and 2 woudl be more than fine

# Question 2: should we zero-center at all?
cmc5[grepl("LDH",aenm) & hitc.new == 0 & hitc.org == 1] # 23 removed hits
cmc5[grepl("LDH",aenm) & hitc.new == 1 & hitc.org == 0] # 5 added hits

cmc5[grepl("LDH",aenm) & hitc.new == 0 & hitc.org == 1 & grepl("11",mc6_flags.org)] # 21 of the 23 were border hits
cmc5[grepl("LDH",aenm) & hitc.new == 0 & hitc.org == 1 & !grepl("11",mc6_flags.org)] # the other 2 have flags, 18,17 and 17,10

# added hits
cmc5[grepl("LDH",aenm) & hitc.new == 1 & hitc.org == 0]
# all have mc6_flags.new of 17 (efficacy less than 50), 16 (overfit.hit), 11 (border.hit). 4 have 7 (singlpt.hit.mid)

# I think it is okay to do the zero-centering.

