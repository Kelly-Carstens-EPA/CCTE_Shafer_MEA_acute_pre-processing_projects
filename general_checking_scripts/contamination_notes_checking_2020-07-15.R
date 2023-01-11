# 07/15/2020
# checking out if I have already addressed the wllq notes in 
# L:\Lab\NHEERL_MEA\MAESTRO SYSTEM\2019_Contamination_MEA.xlsx

setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/lvl0_snapshots")
library(data.table)
load("dat4_2020-06-22.RData")

# 20190403	MW 66-9613, well D6
dat4[plate.id == "MW66-9613" & rowi == 4 & coli == 6, unique(experiment.date)] # "20190416", makes sense
dat4[plate.id == "MW66-9613" & rowi == 4 & coli == 6, .(acsn, wllq, wllq_notes)] # all have wllq == 0, for other reasons
# will add this contamination note to the apcra run_me

# 20190410	MW 66-9817, well D6
dat4[plate.id == "MW66-9817" & rowi == 4 & coli == 6] # already has contamination note

# 20190424 MW 68-3714, well B3 (there is no plate like that, I think they mean 67)
dat4[plate.id == "MW67-3714" & rowi == 2 & coli == 3, .(acsn, wllq, wllq_notes)] # already have contamination note

# 20190501 MW 68-0719, well A7
dat4[plate.id == "MW68-0719" & rowi == 1 & coli == 7, .(acsn, wllq, wllq_notes)] # already have contamination note

# Culture: 20190508 DNT single point screen, not included

# 20190515 MW 68-0807, Well F1
dat4[plate.id == "MW68-0807" & rowi == 6 & coli == 1, .(acsn, wllq, wllq_notes)] # already has contamination note

# 20190515 MW 68-0809, Well E1
dat4[plate.id == "MW68-0809" & rowi == 5 & coli == 1, .(acsn, wllq, wllq_notes)] # already has contamination note



