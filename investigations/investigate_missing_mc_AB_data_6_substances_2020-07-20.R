# Checking out why we don't have any AB data for 6 substances
# July 20, 2022

library(data.table)

# Load current mc0
load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/lvl0_snapshots/dat4_2020-07-29.RData')

# These 6 spid don't have AB hit calls in level 5:
check.spid <- c('TP0001412F03' ,'TP0001412F02' ,'TP0001412F01' ,'TP0001412E12' ,'TP0001412E11' ,'TP0001412F04')

dat4[spid %in% check.spid & grepl('AB',acnm), .N, by = .(spid, acnm, wllq, wllq_notes, experiment.date, plate.id, origin)]
# spid                     acnm wllq                                                       wllq_notes experiment.date  plate.id      origin N
# 1: TP0001412E11 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-19 ToxCast2016 7
# 2: TP0001412E11 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-24 ToxCast2016 7
# 3: TP0001412E11 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-25 ToxCast2016 7
# 4: TP0001412E12 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-19 ToxCast2016 7
# 5: TP0001412E12 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-24 ToxCast2016 7
# 6: TP0001412E12 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-25 ToxCast2016 7
# 7: TP0001412F01 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-19 ToxCast2016 7
# 8: TP0001412F01 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-24 ToxCast2016 6
# 9: TP0001412F01 CCTE_Shafer_MEA_acute_AB    0 Baseline # of AE < 10; Baseline MFR < 0.6377603 Hz; rval is NA;         20151203 MW1086-24 ToxCast2016 1
# 10: TP0001412F01 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-25 ToxCast2016 7
# 11: TP0001412F02 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-19 ToxCast2016 7
# 12: TP0001412F02 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-24 ToxCast2016 7
# 13: TP0001412F02 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-25 ToxCast2016 7
# 14: TP0001412F03 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-19 ToxCast2016 7
# 15: TP0001412F03 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-24 ToxCast2016 7
# 16: TP0001412F03 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-25 ToxCast2016 7
# 17: TP0001412F04 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-19 ToxCast2016 7
# 18: TP0001412F04 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-24 ToxCast2016 7
# 19: TP0001412F04 CCTE_Shafer_MEA_acute_AB    0                                                     rval is NA;         20151203 MW1086-25 ToxCast2016 7
# so the rval was just NA in every case...
# all are from ToxCast 2016

# Was there no AB data in the source file?
# I believe that I pulled all of the cytotoxicity data for ToxCast2016 from here: L:\Lab\NHEERL_MEA\MAESTRO SYSTEM\ToxCast Compounds\Phase I and II Con Response\Conc_Response_Log_JS.xlsx
# The alamar blue values are empty for 20151203 in the Conc_Response_Log_JS.xlsx
# (I also made references to a "flat file" in my data prep notes for ToxCast2016... but I'm assumign that whatever other manually-made wide format fiel would have pulled data form the conc resp log as well.)

# Furthermore, if there is any AB data for experiment date 20151203, it woudl be in the this foldeR: L:\Lab\NHEERL_MEA\MAESTRO SYSTEM\ToxCast Compounds\Phase I and II Con Response\MEA Cytotox\20151118 Culture
# but there is only LDH data for this experiment date. The AB data is only for 20121201.
# So I think there really is NO AB data for these 6 substances.