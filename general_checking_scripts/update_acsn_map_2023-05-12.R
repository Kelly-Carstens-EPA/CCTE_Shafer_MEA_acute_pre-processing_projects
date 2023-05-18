# ------------------------------------------------------------------------ #
# Updating the acsn_map
# May 12 2023
# ------------------------------------------------------------------------ #

library(tcpl)
library(data.table)
tcplConf('MySQL',
         Sys.getenv('INVITRODB_USER_RO'),
         Sys.getenv('INVITRODB_PASS_RO'),
         Sys.getenv('INVITRODB_HOST_RO'),
         'invitrodb')
acid.tb <- tcplLoadAcid(fld ='asid', val = 20, add.fld = 'anm')
acid.tb <- acid.tb[anm == 'CCTE_Shafer_MEA_acute']

# Read in existing map
acsn_map <- as.data.table(read.csv("neural_stats_acsn_to_tcpl_acnm_map.csv"))

# All endpoints currently registered are in acsn_map?
setdiff(acid.tb$acnm, acsn_map$acnm)
# "CCTE_Shafer_MEA_MFR" "CCTE_Shafer_MEA_LDH" "CCTE_Shafer_MEA_AB" 
# just the sc endpoints are excluded - this is fine for now

# All endpoints in acsn_map registered in invitrodb?
setdiff(acsn_map$acnm, acid.tb$acnm)
# "CCTE_Shafer_MEA_acute_per_network_burst_interspike_interval_cv"
# this is a weird endpoint that isn't always present in the Neural Statistics Compiler files
# I think we can ignore it

# Update the "status" column
acsn_map[, Status_2023.05.12 := ifelse(acnm %in% acid.tb$acnm, 'registered in invitrodb','not registered in invitrodb')]
acsn_map <- acsn_map[, .(acsn, acnm, Status_2020.07.17, Status_2023.05.12, Category, Description..adapted.from.AxISUserGuide.1.5..a.slightly.older.version.)]

# save updated table
write.csv(acsn_map, file = 'neural_stats_acsn_to_tcpl_acnm_map.csv', row.names = F)
