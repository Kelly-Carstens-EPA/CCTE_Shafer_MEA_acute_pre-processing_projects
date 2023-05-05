# creating plots with latest sbox mfr graphs
# - with wllq set to 0 where mfr less than or greater than coff
# - only mfr has been pipelined
library(tcpl)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='sbox_invitrodb_v3_2', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

mfr_aeid <- tcplLoadAeid(fld = "acnm", val = "NHEERL_MEA_acute_firing_rate_mean")

setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/MFR_cutoff_investigation/figs")

for (i in mfr_aeid$aeid){
  tcplMakeAeidPlts(i,
                   lvl=6L, 
                   odir=getwd())
}
