# notes on how wllt is currently assigned

# summary conclusions:
# non-spid wells allowed: "Tritonx100" "Bicuculline"  "DMSO" 
library(tcpl)

tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))
shafer.assays <- tcplLoadAcid(fld = "asid",val=20)
mea.acute <- shafer.assays[grepl("MEA_acute",acnm)]
mc0.from.db <- tcplPrepOtpt(tcplLoadData(lvl=0,type='mc',val=mea.acute$acid,fld='acid'))
unique(mc0.from.db$spid) # includes "Tritonx100" "Bicuculline"  "DMSO" 

unique(alldat4$spid) # includes "BIC" "BC"  "DMSO" "LYSIS"
alldat4[spid %in% c("BIC","BC"), spid := "Bicuculline"]
alldat4[spid %in% c("LYSIS"), spid := "Tritonx100"] # determined while first pipelining the acute data that LYSIS is tritonx100

# assign updated spids
no_spid_dat <- alldat4[spid %in% c("DMSO","Bicuculline","Tritonx100")] # set this aside
# source_to_lvl0 script source_to_lvl0_asid_20.R generates srcf, which is a reshaped, and cleaned form of the original source file
# First map the legacy sample IDs to the current IDs (TP), the xlsx was provided from Chemtrack since it was not mapped in invitrodb
colnames(alldat4)[colnames(alldat4) == 'spid'] <- 'spid_legacy'
alldat4$spid_legacy <- as.character(alldat4$spid_legacy)

library(xlsx)
TX_TP <- read.xlsx("L:/Lab/ToxCast_Data/toxcast_data/files/nheerl_mea_acute/source/unblinded_tx_to_tp_codes_NHEERL_SHAFER_ACUTE.xlsx",  1, stringsAsFactors = F)
colnames(TX_TP)[colnames(TX_TP) == "EPA_SAMPLE_ID"] <- "spid"
colnames(TX_TP)[colnames(TX_TP) == "BOTTLE_ID"] <- "spid_legacy"

alldat4 <- merge(x = alldat4, y = TX_TP[, c("spid", "spid_legacy")], all.x = TRUE, by = "spid_legacy")

alldat4 <- alldat4[!is.na(spid)]
alldat4[, spid_legacy := NULL]
alldat4 <- rbind(alldat4, no_spid_dat) # add back in dmso, BIC, and Tritonx100 data

setdiff(unique(alldat4$spid), unique(mc0.from.db$spid))
setdiff(unique(mc0.from.db$spid), unique(alldat4$spid)) # all matching, character(0)

# final cleaning up
nrow(alldat4[is.na(rval)])
# 9168 (was 6066 in prev script)
# now, it is [1] 11231
alldat4[wllq==1 & is.na(rval),.N] # 187

# set wllq to 0 where rval is Na... I think this is kinda goofy, but okay
alldat4[is.na(rval), wllq := 0]

# undertand wllt
mc0.from.db[, paste0(sort(unique(spid))[1:3]), by = "wllt"]
mc0.from.db[spid == "DMSO", unique(wllt)] # all n
mc0.from.db[spid == "Bicuculline", unique(wllt), by = "acnm"]
# acnm V1
# 1:                        NHEERL_MEA_acute_spike_number  p
# 2:                    NHEERL_MEA_acute_firing_rate_mean  p
# 3:                        NHEERL_MEA_acute_burst_number  p
# 4:                 NHEERL_MEA_acute_burst_duration_mean  p
# 5:         NHEERL_MEA_acute_per_burst_spike_number_mean  p
# 6:            NHEERL_MEA_acute_interburst_interval_mean  p
# 7:               NHEERL_MEA_acute_burst_percentage_mean  p
# 8:                NHEERL_MEA_acute_burst_percentage_std  p
# 9: NHEERL_MEA_acute_per_network_burst_spike_number_mean  p
# 10:  NHEERL_MEA_acute_per_network_burst_spike_number_std  p
# 11:     NHEERL_MEA_acute_bursting_electrodes_number_mean  p
# 12:            NHEERL_MEA_acute_network_burst_percentage  p
# 13:              NHEERL_MEA_acute_cross_correlation_area  p
# 14:              NHEERL_MEA_acute_cross_correlation_HWHM  p
# 15:                     NHEERL_MEA_acute_synchrony_index  p
# 16:                                 NHEERL_MEA_acute_LDH  z
# 17:                                  NHEERL_MEA_acute_AB  z
mc0.from.db[spid == "Tritonx100", unique(wllt), by = "acnm"]
# acnm V1
# 1:                        NHEERL_MEA_acute_spike_number  x
# 2:                    NHEERL_MEA_acute_firing_rate_mean  x
# 3:                        NHEERL_MEA_acute_burst_number  x
# 4:                 NHEERL_MEA_acute_burst_duration_mean  x
# 5:         NHEERL_MEA_acute_per_burst_spike_number_mean  x
# 6:            NHEERL_MEA_acute_interburst_interval_mean  x
# 7:               NHEERL_MEA_acute_burst_percentage_mean  x
# 8:                NHEERL_MEA_acute_burst_percentage_std  x
# 9: NHEERL_MEA_acute_per_network_burst_spike_number_mean  x
# 10:  NHEERL_MEA_acute_per_network_burst_spike_number_std  x
# 11:     NHEERL_MEA_acute_bursting_electrodes_number_mean  x
# 12:            NHEERL_MEA_acute_network_burst_percentage  x
# 13:              NHEERL_MEA_acute_cross_correlation_area  x
# 14:              NHEERL_MEA_acute_cross_correlation_HWHM  x
# 15:                     NHEERL_MEA_acute_synchrony_index  x
# 16:                                 NHEERL_MEA_acute_LDH  p
# 17:                                  NHEERL_MEA_acute_AB  p

# summary:
alldat4[, wllt := "t"]
alldat4[spid == "DMSO", wllt := "n"]
alldat4[spid == "Tritonx100" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "p"]
alldat4[spid == "Tritonx100" & !(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), wllt := "x"]
alldat4[spid == "Bicuculline" & !(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), wllt := "p"]
alldat4[spid == "Bicuculline" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "z"]

# check if any changes in conc (or wllt, wllq, etc)
alldat4$coli <- as.integer(alldat4$coli)
compare_stuff <- merge(alldat4, mc0.from.db, by = c("spid","apid","rowi","coli","srcf"), suffixes = c("",".idb"))
compare_stuff[, all.equal(wllq.idb, wllq)] # TRUE
compare_stuff[, all.equal(wllt.idb, wllt)] # TRUE - means that I implemented the assignments correctly above
