# checking out negative values in LDH data.
library(data.table)
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts/get_latest_dat.R')

# these are the blank-corrected values.
test <- get_latest_dat(lvl = "dat3")
test[grepl("LDH",acsn) & rval < 0 & wllq == 1, summary(rval)]
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# -7.547e-02 -4.002e-02 -2.670e-02 -2.714e-02 -1.173e-02 -6.667e-05 
# -0.07547 to -0.00006
test[grepl("LDH",acsn) & rval < 0 & wllq == 1, .N] # 1359
test[grepl("LDH",acsn) & wllq == 1, .N] # 13748. Wow, so almost 10% are negative.
test[grepl("LDH",acsn) & wllq == 1, summary(rval)]
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -0.07547  0.04562  0.08967  0.18980  0.13605  3.91197       33 
stripchart(test[grepl("LDH",acsn) & wllq == 1, .(rval)], pch = 1, method = "jitter", vertical = T)
abline(h = 0)
#hmm. I think having the negative values might help add some real-ness. I would leave as-is.
# ugh, but standardize... it really doesn't matter.
# permission to be imperfect here.

test[grepl("AB",acsn) & rval == 0 & wllq == 1, .N] # 45. Wow, very few.

# 07/15/2020
dat4 <- get_latest_dat("dat4")
# the LDH assay measures how much LDH is outside the cells (versus how much is inside the cells)
# so if the blank-corrected value is negative, that just means that less LDH was found outside the cells in a well than in the controls

dat4[grepl("LDH",acsn) & rval < 0, .N, by = "wllq"] # 1359 wells would be affected. Wow.
# wllq    N
# 1:    1 1359
# 2:    0  249
dat4[grepl("LDH",acsn) & wllq == 1, .N] # out of 13589 LDH wells
1359/13589 # 0.1000074 wow, 10% of rval's are negative

dat4[grepl("LDH",acsn) & wllq == 1, summary(rval)]
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.14540  0.04637  0.09013  0.17827  0.13613  5.00620 
# hmm, this min is frmo a 2 * 1/2 lysis control well... this seems quite fishy
# will come back to this

dat4[grepl("LDH",acsn) & wllq == 1 & (wllt == "p" & rval < 0)]
# confirmed that these 3 control wells are just outliers. 
# Seline originally normalized these by the 1/2 Lysis wells from another plate
# now that I am usign the median of 3 controls, I think it is all good

# removing these 3 oddballs
dat4[grepl("LDH",acsn) & wllq == 1 & !(wllt == "p" & rval < 0), summary(rval)]
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -0.07547  0.04640  0.09013  0.17834  0.13616  5.00620 

dat4[grepl("LDH",acsn) & rval < 0, .N, by = "wllt"]
# wllt    N
# 1:    n   79
# 2:    t 1437
# 3:    z   30
# 4:    x   39
# 5:    p    3
# 6:    b   20

stripchart(dat4[grepl("LDH",acsn) & wllq == 1, .(rval)], pch = 1, method = "jitter", vertical = T)
abline(h = 0)

plot(density(dat4[grepl("LDH",acsn) & wllq == 1, rval]), xlim = c(-1, 2))
abline(v = 0)

hist(dat4[grepl("LDH",acsn) & wllq == 1, rval], breaks = 20)
# wow, so a lot of data is in that 0 - 0.2 ish range. Most negative values are in -0.2 - 0. This could make a significant difference

# mm, also, I could re-run with LDH, and see if the hit calls would change. Let's do it.