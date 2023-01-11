# I was really surprised when I realized that abamectin and emmamectin Benzoate are not hits now
# Even though they have consistent -100% rval's at higher conc's, adn had to be re-tested at lower conc's
# Looking into why that might be
# 07/26/2020
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl")
load("sbox_dat/sbox_dat_2020-06-21.RData")
mc3_mthds

ls()

dat1 <- get_latest_dat("dat1")
dat1[, med_av = median(activity_value)]

head(mc3)

# MFR - aeid up: 2425, dn: 2442, acid: 2432

mc3[, med_cval := median(cval), by = c("conc","spid")]
va_spids <- mc3[acid == 2432, .(min_med_cval = min(med_cval)), by = "spid"][min_med_cval < -99, unique(spid)]

# how many of these very active spids are hits?
mc5_mc6[spid %in% va_spids & grepl("firing_rate_mean_dn",aenm), .(spid, chnm, hitc, modl_ga, flags)]
# only 1 of these is not a hit. 
# Abamectin is not on this list...

abamectin_spid <- mc5_mc6[chnm == "Abamectin", unique(spid)]
mc3[spid == abamectin_spid & acid == 2432, unique(med_cval), by = "conc"]
abamectin_spid %in% va_spids
# TRUE

# okay, so there are several va compounds that are not hits...
mc5_mc6[spid %in% va_spids & grepl("firing_rate_mean_dn",aenm), .(spid, chnm, hitc, modl_ga, flags)][hitc != 1]
# 18 of the 108 "very active" spids
# 2 say borderline inactive, 1 says noisy data
# The rest, I'm just not sure why and i'm a bit concerned.
# Is the prob that they never got above BMAD?

mc5_mc6[spid %in% va_spids & grepl("firing_rate_mean_dn",aenm), .(spid, chnm, hitc, modl_ga, flags, coff, max_med)][hitc != 1]
# yep, for all but the 2 borderline inactive ones, the max_med's are just barely below the coff.
# hey, maybe the coff will lower when we remove outlier DMSO wells? Actually ya, it totally would!

# still, why are the max_med's so low for these va spids?
mc3[spid %in% va_spids & acid == 2432, summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -88.83  -46.62  -32.36  -32.24  -18.04   30.29 
mc3[acid == 2432, summary(bval)]
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -88.83  -46.10  -31.00  -30.99  -16.79   30.29
# I would like to see this altogether...
lvl3_sum <- mc3[acid == 2432 & spid %in% va_spids, .(min_med_cval = min(med_cval), bval = paste0(unique(bval),collapse=",")), by = c("spid")]

merge(mc5_mc6[spid %in% va_spids & grepl("firing_rate_mean_dn",aenm), .(spid, chnm, hitc, modl_ga, flags, coff, max_med)][hitc != 1], lvl3_sum, by = c("spid"))
# spid                                chnm hitc  modl_ga               flags     coff  max_med min_med_cval                                bval
# 1: EPAPLT0154B01                           Capsaicin    0       NA                <NA> 50.90747 49.08171    -99.99890 -46.9523370205047,-50.9170136292038
# 2: EPAPLT0154B05            Butylated hydroxyanisole    0       NA                <NA> 50.90747 49.04965    -99.99461   -57.37849465428,-50.9170136292038
# 3: EPAPLT0154D10                           Bithionol    0       NA                <NA> 50.90747 41.00320    -99.55836                   -58.3990718234051
# 4: EPAPLT0154E06                    2,2'-Bisphenol F    0       NA                <NA> 50.90747 25.63719    -99.99826                   -74.3572558004821
#  5: EPAPLT0154F05            N-Phenyl-1-naphthylamine    0       NA                <NA> 50.90747 42.07285    -99.65470 -69.0557061522356,-46.0991252939363
#  6: EPAPLT0154H02               Denatonium saccharide    0       NA                <NA> 50.90747 33.42315   -100.00000                   -66.5768523058051
#  7: EPAPLT0154H10 4,4'-(9H-Fluorene-9,9-diyl)diphenol    0       NA                <NA> 50.90747 41.14842    -99.65486                   -58.7175344202344
# 8: EPAPLT0167A01                            Acephate    0       NA                <NA> 50.90747 11.16943    -99.99838                    -88.826984272589
# 9: EPAPLT0167A06                       5-Azacytidine    0       NA          Noisy data 50.90747 11.17121    -99.99847                    -88.826984272589
# 10:  TP0001411C11                       4-Octylphenol    0 0.467346 Borderline inactive 50.90747 52.64967    -99.96352                   -47.3320877010151
# 11:  TP0001412E10             Methadone hydrochloride    0       NA                <NA> 50.90747 45.42834    -99.99871                   -54.5710160645712
# 12:  TP0001412F03                           Flutamide    0       NA                <NA> 50.90747 41.89456    -99.97117                   -58.0766052811572
# 13:  TP0001412F08                 Fenpyroximate (Z,E)    0       NA                <NA> 50.90747 48.72085    -99.58391                   -50.9005918688803
# 14:  TP0001413H03                  Emamectin benzoate    0       NA                <NA> 50.90747 46.80591    -99.99475                   -53.1876738072959
# 15:  TP0001413H04                        Ketoconazole    0 1.007018 Borderline inactive 50.90747 52.04918    -99.99776 -47.9438285112154,-53.1876738072959
# 16:  TP0001414B05                           Bensulide    0       NA                <NA> 50.90747 44.01978    -99.99832                   -55.9778266062753
# 17:  TP0001414B07                       Tetraconazole    0       NA                <NA> 50.90747 44.02217   -100.00000                   -55.9778266062753
# 18:  TP0001414B08                           Abamectin    0       NA                <NA> 50.90747 44.02217   -100.00000 -55.9778266062753,-53.1876738072959

# Maybe this is telling us something real. 
# That DMSO Wells are so variable, that we can't get ... any info from some apid.
# That the effect of DMSO is so strong, that we can't differentiate between a DMSO-effect adn treatment-effect.
# oo, that might be it.

# It might be that on some apid, the affect of DMSO is so strong that we can't make a reliable hit call
# I am not confident that this normalization method can really tell us that,
# but it's possible
# I'm still hoping that removing all DMSO outliers will lower the coff substantially, and allow a few of these to become hits

# other potential coff options:
# bmad* 5, 6, or 10... no
maxmed20pct = function() {
  
  e1 <- bquote(coff <- c(coff, dat[ , max(max_med)*.20]))
  list(e1)
  
}
# that's an option

pc50 = function() {
  
  e1 <- bquote(coff <- c(coff, 50))
  list(e1)
  
}
# eh, about the same as now

pc30 = function() {
  
  e1 <- bquote(coff <- c(coff, 30))
  list(e1)
}
# maybe

bmad1 = function() {
  
  e1 <- bquote(coff <- c(coff, dat[ , unique(bmad)]))
  list(e1)
  
}
# probs too low

# I think that 3BMAD makes a lot of sense bc that's what we use for the single point screen.
# BUT, this is 3BMAD of different resp values...

# what is 20% of max_med compare to current coff?
mc5_mc6[, .(coff = unique(coff), bmad = unique(bmad), max(max_med)*0.20), by = "aenm"]
# huh, why does this look weird for dn aenm's?
# aenm      coff      bmad       V3
# 1: NHEERL_MEA_acute_firing_rate_mean_up 50.907473 16.969158 41.89997
# 2:     NHEERL_MEA_acute_burst_number_up 56.500518 18.833506 43.67338
# 3:  NHEERL_MEA_acute_synchrony_index_up 10.144071  3.381357 21.88357
# 4:              NHEERL_MEA_acute_LDH_up  5.822243  1.940748 26.05166
# 5:               NHEERL_MEA_acute_AB_up 39.276799 13.092266 20.00000
# 6: NHEERL_MEA_acute_firing_rate_mean_dn 50.907473 16.969158 21.44948
# 7:     NHEERL_MEA_acute_burst_number_dn 56.500518 18.833506 25.03429
# 8:  NHEERL_MEA_acute_synchrony_index_dn 10.144071  3.381357 20.18334

# !!! I just remembered - 
# TCPL won't fit a compounds if max_med is below 3BMAD, regardless of what "coff" is set to be!
# unless, you do force.fit, or fit all, somewhere
# I think I would have to make a really good argument to Katie to register these endpoints as fit all

# what would really be awesome, for dn endpoints:
# resp := (cval - bval) / (-100 - bval)
# Since -100 is the real lower limit, we would be dividing by the largest negative response possible, given bval
# I think this would be great where bval is large, since it would allow strong hits to shine thru
# I think this would capture the idea of - is the treated response signficantly/reasonably greater than the DMSO effect on this plate?
