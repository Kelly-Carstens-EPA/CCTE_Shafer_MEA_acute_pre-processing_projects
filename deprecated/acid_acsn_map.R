# 06/16/2020
# saving map to access when i don't have access to the database
library(data.table)
library(tcpl)
library(RMySQL)
tcplConf(user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), db='invitrodb', drvr='MySQL', host = Sys.getenv('INVITRODB_HOST_RO'))

shafer.assays <- tcplLoadAcid(fld = "asid", val = 20, add.fld = "acsn")
mea.acute <- shafer.assays[grepl("MEA_acute",acnm), .(acid, acnm, acsn)]

setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/")
fwrite(mea.acute, file = "acid_acsn_map_invitrodb_2020-06-26.csv", row.names = F)
