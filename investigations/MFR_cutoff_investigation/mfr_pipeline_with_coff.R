# repipeline the mfr mea acute test data with the coff applied
library(tcpl)
library(RMySQL)
library(data.table)

setwd(paste0("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/"))
load("MFR_cutoff_investigation/data/dat4_toxcast2016_dnt2019_aprca2019_2020-05-21.RData")
load("MFR_cutoff_investigation/data/dat1_toxcast2016_dnt2019_aprca2019_2020-05-21.RData")

# filter the data based on mean firing rate coff

# include where wllq==0 or no?
dat1[run_type == "baseline" & !is.na(activity_value), .N, by = "wllq"]
# first we'll do this with all wells, including where wllq == 0
lower_percentile_x <- quantile(dat1[run_type == "baseline" & !is.na(activity_value), c(activity_value)], c(.01))
plot(density(dat1[run_type == "baseline" & !is.na(activity_value), c(activity_value)]), type = "l")
abline(v = lower_percentile_x)
lower_percentile_x*60
# 1% 
# 10.75747 
lower_percentile_x2 <- quantile(dat1[run_type == "baseline" & !is.na(activity_value) & wllq == 1, c(activity_value)], c(.01))
lines(density(dat1[run_type == "baseline" & !is.na(activity_value) & wllq == 1, c(activity_value)]), col = "blue")
abline(v = lower_percentile_x2, col = "blue")
lower_percentile_x2*60
# 1% 
# 19.91997
lines(density(dat1[run_type == "baseline" & wllq == 0 & !is.na(activity_value), c(activity_value)]), col = "red")
# I am going to use the wllq==1 wells only at 1%

# apply the filter
lower_bound <- quantile(dat1[run_type == "baseline" & !is.na(activity_value) & wllq == 1, c(activity_value)], c(.01))
upper_bound <- quantile(dat1[run_type == "baseline" & !is.na(activity_value) & wllq == 1, c(activity_value)], c(.99))
low_baseline_wells <- dat1[run_type == "baseline" & !is.na(activity_value) & wllq == 1 & activity_value < lower_bound, paste(apid, coli, rowi,sep="_")]
high_baseline_Wells <- dat1[run_type == "baseline" & !is.na(activity_value) & wllq == 1 & activity_value > upper_bound, paste(apid, coli, rowi,sep="_")]
dat4[, well_id := paste(apid, rowi, coli, sep = "_")]
dat4[well_id %in% high_baseline_Wells, wllq := 0]
dat4[well_id %in% low_baseline_wells, wllq := 0]
dat4[, well_id := NULL]

# for compounds with unknoqn conc, labelled 0 from DNT, set conc to 1.0
dat4[conc == "0" & grepl("20190618",apid), unique(spid)]
# [1] "EPAPLT0169C05" "EPAPLT0167E10" "EPAPLT0167A02" "EPAPLT0167A08" "EPAPLT0167A04" "EPAPLT0167D09"
dat4[conc == "0" & grepl("20190618",apid), conc := "1.0"]

tcplConf(user = Sys.getenv('INVITRODB_USER_MY'), pass=Sys.getenv('INVITRODB_PASS_MY'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

# ----------------------- Assign acid's
# just doing mean firing rate, for now
mfr_acid <- tcplLoadAcid(fld = "acnm", val = "NHEERL_MEA_acute_firing_rate_mean")$acid
dat4[, acid := mfr_acid]
str(dat4)

# ----------------------- Write lvl 0 data!
# close any open connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())

mc0 <- dat4[, list(acid, spid, apid, rowi, coli, wllt, wllq, conc, rval, srcf)]

# got an error bc probably doesn't like the wllt of "media"
mc0[nchar(wllt)>1,unique(wllt)] # all media
mc0[wllt == "media", wllt:="a"]

# trying again:
tcplConfList() # I am on sbox
tcplWriteLvl0(mc0, type = "mc") # May 27 2020 11:27am
# The following test compounds did not map to the tcpl databases:
# [1] "EPAPLT0167A11" "EPAPLT0167D11" "EPAPLT0154A05" "EPAPLT0154C04" "EPAPLT0154F01"
# Error in tcplWriteLvl0(mc0, type = "mc") : 
#   Must correct the test compound mapping before loading the data.

# right, I removed these since I could not add them to the data base
mc0 <- mc0[!(spid %in% c("EPAPLT0167A11", "EPAPLT0167D11", "EPAPLT0154A05", "EPAPLT0154C04", "EPAPLT0154F01"))]

# trying again:
tcplConfList() # I am on sbox
tcplWriteLvl0(mc0, type = "mc") # May 27 2020 12:37am
# Completed delete cascade for 2 ids (54.25 secs)
# [1] TRUE

# ----------------------------------- tcpl run with hack function
# close any open connections
all_con <- dbListConnections(MySQL())
for (con in all_con) {
  dbDisconnect(con)
}
dbListConnections(MySQL())
rm(list = ls())

# assay.list <- tcplLoadAeid(fld = "asid", val=20, add.fld = "acid")
mfr_acid <- tcplLoadAcid(fld = "acnm", val = "NHEERL_MEA_acute_firing_rate_mean")

tcplLoadData_original <- tcpl::tcplLoadData

# source my hack function
source('C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents/mea_acute/new_acute_processing/tcplLoadData_hack.R', echo=FALSE)

# replace tcplLoadData in tcpl namespace with my function
assignInNamespace("tcplLoadData", tcplLoadData_hack, ns = "tcpl")

dbListConnections(MySQL())
tcplConfList() # sbox confirmed

# May 27, 2020 12:40pm 
tcplRun(slvl = 1L, elvl = 6L, id = mfr_acid$acid, type = "mc")

# Loaded L0 ACID2432 (13938 rows; 7.26 secs)
# Processed L1 ACID2432 (13938 rows; 2.11 secs)
# Writing level 1 data for 1 ids...
# Completed delete cascade for 2 ids (45.42 secs)
# Writing level 1 complete. (76.17 secs)
# Loaded L1 ACID2432 (13938 rows; 19.36 secs)
# Processed L2 ACID2432 (13262 rows; 0.07 secs)
# Writing level 2 data for 1 ids...
# Completed delete cascade for 2 ids (30.07 secs)
# Writing level 2 complete. (60.48 secs)
# Loaded L2 ACID2432 (13262 rows; 26.73 secs)
# Processed L3 ACID2432 (AEIDS: 2425, 2442; 26524 rows; 3.55 secs)
# Writing level 3 data for 1 ids...
# Completed delete cascade for 2 ids (20.12 secs)
# Writing level 3 complete. (90.29 secs)
# Loaded L3 AEID2425 (12401 rows; 34.82 secs)
# Processed L4 AEID2425 (12401 rows; 18.75 secs)
# Loaded L3 AEID2442 (12401 rows; 34.45 secs)
# Processed L4 AEID2442 (12401 rows; 16.51 secs)
# Writing level 4 data for 2 ids...
# Completed delete cascade for 2 ids (0.05 secs)
# Writing level 4 complete. (130.96 secs)
# Loaded L4 AEID2425 (515 rows; 0.06 secs)
# Processed L5 AEID2425 (515 rows; 0.31 secs)
# Loaded L4 AEID2442 (515 rows; 0.06 secs)
# Processed L5 AEID2442 (515 rows; 0.28 secs)
# Writing level 5 data for 2 ids...
# Completed delete cascade for 2 ids (0.03 secs)
# Writing level 5 complete. (4.14 secs)
# Warning in .local(conn, statement, ...) :
#   Unsigned INTEGER in col 4 imported as numeric
# Loaded L5 AEID2425 (515 rows; 0.35 secs)
# Processed L6 AEID2425 (515 rows; 2.27 secs)
# Warning in .local(conn, statement, ...) :
#   Unsigned INTEGER in col 4 imported as numeric
# Loaded L5 AEID2442 (515 rows; 0.29 secs)
# Processed L6 AEID2442 (515 rows; 2.44 secs)
# Writing level 6 data for 2 ids...
# Completed delete cascade for 2 ids (0.01 secs)
# Writing level 6 complete. (2.32 secs)
# 
# 
# Total processing time: 8.9 mins 
# 
# $l1
# ACID2432 
# TRUE 
# 
# $l1_failed
# character(0)
# 
# $l2
# ACID2432 
# TRUE 
# 
# $l2_failed
# character(0)
# 
# $l3
# ACID2432 
# TRUE 
# 
# $l3_failed
# character(0)
# 
# $l4
# AEID2425 AEID2442 
# TRUE     TRUE 
# 
# $l4_failed
# character(0)
# 
# $l5
# AEID2425 AEID2442 
# TRUE     TRUE 
# 
# $l5_failed
# character(0)
# 
# $l6
# AEID2425 AEID2442 
# TRUE     TRUE 
# 
# $l6_failed
# character(0)

# when I'm done, re-assign to original tcplLoadData function
assignInNamespace("tcplLoadData", tcplLoadData_original, ns = "tcpl")
