# ------------------------------------------------------------------------ #
# Confirming the LDH poor detection of half lysis wells has not been an issue
# with previous data sets
# 
# Summary of the issue:
# When I opened up the LDH_CellTitlerblue-functions.R script today,
# all of the ½ symbols were replaced with "?" symbols
# So, whhen the script would have looked for 1/2 lysis wells, it would have in effect grabbed ALL treatments@
# Beucase "?" indicates "whatever comes before me matches a most 1 time"
# I THINK this was not an issue when I ran these scripts previously, that somehow the ½ symbol
# was preserved. Because then if not, there would be other treatments like "2 * sethoxydim" 
# that would have been included in the lysis control wells.
# But want to circle back to this and confirm.
# And now new question with APCRA - did I multiple by 2 twice? Or did i just re-run certain parts of the run_me?
# 
# April 14, 2022
# ------------------------------------------------------------------------- #

dat4 <- get_latest_dat(lvl = "dat4")
# Getting data from folders APCRA2019, DNT2019, GF2019, ToxCast2016 
# APCRA2019_dat4_2020-07-27.RData 
# DNT2019_dat4_2020-07-27.RData 
# GF2019_dat4_2020-07-27.RData 
# ToxCast2016_dat4_2020-07-27.RData 

dat4[grepl('LDH',acnm) & !wllt %in% c('t','n'), .N, by = .(wllt, spid, treatment)]
# wllt         spid   treatment   N
# 1:    z   Picrotoxin       PICRO  96
# 2:    x Tetrodotoxin         TTX  96
# 3:    x   Tritonx100       Lysis 318
# 4:    p   Tritonx100 2 * ½ Lysis 267
# 5:    b        Media       Media  60
# 6:    z  Bicuculline         BIC 414
# 7:    p   Tritonx100       LYSIS 207

# More potential issues are arising...
# APCRA almost looks like I multiple the 1/2 lsysi wells before preapred LDH!!! then I would have mltiple by 4 in effect!!

# Also question - is the conc NA anywhere, is that okay?
load('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/lvl0_snapshots/mea_acute_lvl0_2020-07-29.RData')
mea_acute_lvl0[is.na(conc), .N, by = .(spid, wllt)]
#          spid wllt     N
# 1:      Media    b  3957
# 2: Tritonx100    p   771
# 3: Tritonx100    v 10793
# 4: Tritonx100    x   318
# okay, so I've done this before, so I think it's okay