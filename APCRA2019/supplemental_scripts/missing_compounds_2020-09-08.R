library(data.table)

# source all function in folder 'mea-acute-neural-stats-to-mc0-scripts', except for the run_me.R template
scripts <- list.files(path = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T)
scripts <- scripts[!grepl("run_me\\.R",scripts) & !grepl("wllt_conc_formalization\\.R",scripts)]
sapply(scripts, source)

dat4 <- get_latest_dat(lvl = "dat4", "APCRA2019")

comps <- read.csv("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/apcra_pro_list_mea_avail_2020-09-08.csv")
setDT(comps)

missing_apcra <- setdiff(unique(comps$preferred_name), unique(dat4$treatment))
setdiff(comps[mea.acute == 0, unique(preferred_name)], missing_apcra) # character 0

dat4[treatment %in% missing_apcra, unique(spid)]

# Q1: Did I grab all of the compounds in the TSCA_APCRA folder?
sort(unique(dat4$apid))
length(unique(dat4$apid)) # 16. Since I do it by exp date, this is 8 cultures
dat4[, .(length(unique(treatment))), by = list(apid)] # 12 - 11...
# see other script, yet I did
dat4[wllt == "t", length(unique(spid))] # 84

# for everything in the 84-chem spid map file -> did I collect data for those?
apcra1 <- read.xlsx("L:\Lab\NHEERL_MEA\Project TSCA_APCRA\EPA_18235_EPA-Shafer_84_20181129.xlsx", sheet = T)

# Q2: Were the missing compounds tested in the single point screen?
# No - all 84 compounds were restested, and I have inlcuded all 84 in the data as expected

# Q3: Are the missing compounds in the TSCA folder?
# nope, just checked the spidmap and the missing dtxsid's, no overlap

# Q4: Where are teh missing compounds, then? Did we test them at all? 

# remove all functions
funs_to_Remove <- Filter(function(x) is.function(get(x)), ls())
rm(list = funs_to_Remove)

library(openxlsx)
?read.xlsx
