# ------------------------------------------------------------------------ #
# Updating the acsn_map
# To note which endpoints are export ready 1
# ------------------------------------------------------------------------ #

library(tcpl)
library(data.table)
tcplConf('MySQL',
         Sys.getenv('INVITRODB_USER_RO'),
         Sys.getenv('INVITRODB_PASS_RO'),
         Sys.getenv('INVITRODB_HOST'),
         'invitrodb')
acid.tb <- tcplLoadAcid(fld ='asid', val = 20, add.fld = c('anm'))
acid.tb <- acid.tb[anm == 'CCTE_Shafer_MEA_acute']

# Read in existing map
acsn_map <- as.data.table(read.xlsx("neural_stats_acsn_to_tcpl_acnm_map.xlsx"))

# All endpoints currently registered are in acsn_map?
setdiff(acid.tb$acnm, acsn_map$acnm)
# "CCTE_Shafer_MEA_MFR" "CCTE_Shafer_MEA_LDH" "CCTE_Shafer_MEA_AB" 
# just the sc endpoints are excluded - this is fine for now

# All endpoints in acsn_map registered in invitrodb?
setdiff(acsn_map$acnm, acid.tb$acnm)
# [1] "CCTE_Shafer_MEA_acute_per_network_burst_interspike_interval_cv"
# [2] "CCTE_Shafer_MEA_acute_resistance_avg"                          
# [3] "CCTE_Shafer_MEA_acute_resistance_std"                          
# [4] "CCTE_Shafer_MEA_acute_electrodes_covered_number"               
# [5] "CCTE_Shafer_MEA_acute_resistance_mean_weighted
# IDK about the first one
# other are impedance specific


# Update the "status" column based on export ready status in v3_5
tcplConf('MySQL',
         Sys.getenv('INVITRODB_USER_RO'),
         Sys.getenv('INVITRODB_PASS_RO'),
         Sys.getenv('INVITRODB_HOST'),
         'prod_internal_invitrodb_v3_5')
acid.tb <- tcplLoadAcid(fld ='asid', val = 20, add.fld = c('anm'))
acid.tb <- acid.tb[anm == 'CCTE_Shafer_MEA_acute']


acsn_map[, Status_invitroDBv3.5 := ifelse(acnm %in% acid.tb$acnm, 'export_ready=1','export_ready=0 or not registered')]
acsn_map[, .N, by = .(Status_invitroDBv3.5)]
# Status_invitroDBv3.5  N
# 1:                   export_ready=1 18
# 2: export_ready=0 or not registered 35
# hmm, I though tjust 15 + 2 cyto?
acsn_map[Status_invitroDBv3.5 == 'export_ready=1', .(acsn, acnm)]
# ah, that's right, 2 acsn's map to the same acnm

# save updated table
wb <- createWorkbook()
addWorksheet(wb, 'acsn_map')
writeData(wb, 1, acsn_map)
saveWorkbook(wb, file = 'neural_stats_acsn_to_tcpl_acnm_map.xlsx', overwrite = T)
