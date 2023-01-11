# determining what data has been included in the kosnik 2019 data
library(data.table)
library(xlsx)
dat <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv",
                 stringsAsFactors = F)

length(unique(dat$EXPERIMENT_DATE))
#69

exp.dirs <- list.dirs(path = "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/Experiment Date", full.names = F, recursive = F)
length(exp.dirs)
# there are 71 date folders in the directory L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/Experiment Date

head(unique(dat$EXPERIMENT_DATE))
usedates <- sort(unique(dat$EXPERIMENT_DATE))

# convert exp.dirs to same date format
exp.dates <- format(as.Date(exp.dirs, format = "%m-%d-%y"), format = "%Y%m%d")

setdiff(exp.dates, usedates)
# [1] "20151202" "20160607" 

setdiff(usedates, exp.dates)
# integer(0)

# do a quick similar check with the plates
useplates <- sort(c(unique(dat$MEA_PLATE_ID_RUN1), unique(dat$MEA_PLATE_ID_RUN2), unique(dat$MEA_PLATE_ID_RUN3)))
length(useplates) # 194
length(useplates) == 3*length(usedates) # FALSE
# there are 13 plates that should not be used.

setDT(dat)
sum.dat <- unique(dat[, .(EXPERIMENT_DATE, MEA_PLATE_ID_RUN1, MEA_PLATE_ID_RUN2, MEA_PLATE_ID_RUN3)])
# oh right! some of the plates were likely repeated
sum.dat[, .N, by = "EXPERIMENT_DATE"]
sum.dat[EXPERIMENT_DATE == "20150618"] # ah, spacing
dat[MEA_PLATE_ID_RUN1 == "MW1068-24", MEA_PLATE_ID_RUN1 := "MW 1068-24"]

# see if any unexpected num of treatments
dat[, length(unique(EPA_SAMPLE_ID)), by = "EXPERIMENT_DATE"]
# looking at all dates with other than 9 unique treatments
dat[EXPERIMENT_DATE == "20150618", unique(EPA_SAMPLE_ID)] # just had 2 names for BIC (BC)
dat[EXPERIMENT_DATE == "20160303", unique(EPA_SAMPLE_ID)]
# "DMSO"     "TX014268" "TX001598" "BIC"      "TX014427" "TX006867" "LYSIS"    "TX009097"


# see if all expected number of rows
su.dat2 <- dat[!(EPA_SAMPLE_ID %in% c("DMSO","BIC","BC","LYSIS")), .N, by = c("EXPERIMENT_DATE", "EPA_SAMPLE_ID")]
su.dat2[N!=7]
