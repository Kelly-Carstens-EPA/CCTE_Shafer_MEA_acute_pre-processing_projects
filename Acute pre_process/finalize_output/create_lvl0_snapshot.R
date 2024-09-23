# script to save the latest dat4 from all folders, and save a corresponding mc0 file

library(data.table)
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
source("mea-acute-neural-stats-to-mc0-scripts/get_latest_dat.R")
source('mea-acute-neural-stats-to-mc0-scripts/remove_dmso_outliers.R')

dat4 <- get_latest_dat(lvl = "dat4")

# Make dataset wide adjustments to wllq
dat4 <- remove_dmso_outliers(dat4)

# Save the level 0 srcf and corresponding dat4 snaphshots

# all dat4
save(dat4, file = paste0("lvl0_snapshots/dat4_",as.character.Date(Sys.Date()),".RData"))

# mc0 dat
mc0_char <- paste0("mea_acute_lvl0")
assign(mc0_char, value = dat4[, .(spid, apid, rowi, coli, conc, acnm, wllt, wllq, srcf, rval)])
save(list = c(mc0_char), file = paste0("lvl0_snapshots/",mc0_char,"_",as.character.Date(Sys.Date()),".RData"))