###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/"
dataset_title <- "" # e.g. "name2020"
root_output_dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/" # where the dataset_title folder will be created
select.neural.stats.files <- F # select new neural stats files, or use the files in the most recent neural_stats_files_log?
select.calculations.files <- F # select new calculations files, or use the files in the most recent calculations_files_log?
run.type.tag.location <- 5 # neural stats files should be named as "tag1_tag2_tag3_....csv". Which tag in the file names defines the run type?
spidmap_file <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/All Assays_list_toxcast_OECD 20190524.xlsx"
use_sheet <- "MEA Acute Conc Res" # sheet name in spidmap_file
# optional adjustsment; usually can use defaults:
parameter_set_type <- "post_july_2016" # use "pre_july_2016" if your endpoints include 'Half Width at Half Height of Cross Correlation' instead of 'Width...'
override_wllq_checks <- FALSE # set to TRUE only if you have already verified your wllq updates
plate.id.tag.location <- numeric(0) # only update this if you have to, if your dataset does not include plate.id.tag in file headers
###################################################################################
# END USER INPUT
###################################################################################

library(data.table)
library(readxl)
library(tcpl)

# set up folders and working directory
if (!dir.exists(file.path(root_output_dir,dataset_title))) dir.create(file.path(start.dir,dataset_title))
setwd(file.path(root_output_dir,dataset_title))
main.output.dir <- getwd()
if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# source all function in folder 'mea-acute-neural-stats-to-mc0-scripts', except for the run_me.R template
scripts <- list.files(path = "../mea-acute-neural-stats-to-mc0-scripts", pattern = "\\.R$", full.names = T)
scripts <- scripts[!grepl("run_me\\.R",scripts) & !grepl("wllt_conc_formalization\\.R",scripts)]
sapply(scripts, source)

# loading some information for the funtions to reference
get_acsn_map(type = parameter_set_type) # load the acsn map with the appropriate endpoints

# select input files to use, store files in .txt file
if (select.neural.stats.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
}
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}

# Check that at run.type.tag.location, there is one file with  _00 and 1 file with _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(run.type.tag.location, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# Check the neural stats files for common issues
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e } )  
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# extract all of the data from the files and transform into long data format (dat1)
extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# view dat1
load(file = paste0("output/",dataset_title,"_dat1_",as.character.Date(Sys.Date()),".RData"))
str(dat1)
dat1[, .N/length(unique(dat1$tcpl_acsn)), by = "wllq_notes"]
rm(dat1)

# collapse the plate data by calculating the percent change in activity (dat2)
collapsePlateData(main.output.dir)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 

# look at data so far
load(file = paste0("output/",dataset_title,"_dat2_",as.character.Date(Sys.Date()),".RData"))
dat2[wllq==1, summary(rval)]
# OUTPUT --------------------------------------------------------- 
# 
#
# ---------------------------------------------------------------- 
rm(dat2)

# get cytotox data
cytodat <- getAllCytoData(main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 
str(cytodat)
any(is.na(cytodat))

# combine the cytodat with dat2, add trt, conc, and wllq to ea (dat3)
combineNeuralAndCyto(cytodat, main.output.dir, dataset_title)
# OUTPUT --------------------------------------------------------- 
# 
# ---------------------------------------------------------------- 
rm(cytodat)

# load dat3 and finalize it
load(file = paste0("output/",dataset_title,"_dat3_",as.character.Date(Sys.Date()),".RData"))
dat4 <- dat3
rm(dat3)
str(dat4)

# FINALIZE WLLQ

# set wllq to zero where rval is NA
dat4[wllq==1 & is.na(rval),.N] 
# 
dat4[is.na(rval), `:=` (wllq = 0, wllq_notes = paste0(wllq_notes, "rval is NA; "))]

# do any other updates to wllq based on notes from lab notebook
# e.g. misdosed, recording too long, etc.
# for example, updateWllq(dat4, date = "20190530", plate = "MW68-0807", well = "C6", wllq_note = "Contamination", override_check = override_wllq_checks)


# VERIFY TREATMENT LABELS FOR CONTROLS IN NEURAL AND CYTOTOX ASSAYS

# view and standardize treatment names, so can compare all relevant values below
dat4[, .N, by = "treatment"]
dat4[grepl("DMSO",treatment), treatment := "DMSO"]

# visually confirm if the PICRO, TTX, LYSIS were added before the second recording for MEA endpoints
# varies across experiments, sometimes across days
# if not, the PICRO, TTX, LYSIS wells only contained media for the MEA endpoints
boxplot(rval ~ treatment, dat4[wllq == 1 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("firing_rate",acsn)], ylab = "percent change in mean firing rate")
stripchart(rval ~ treatment, dat4[wllq == 1 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("firing_rate",acsn)], 
           ylab = "percent change in mean firing rate", vertical = T, pch = 1, method = "jitter")
title(main = paste0("Percent Change in Mean Firing rate\nin Control wells of ",dataset_title," MEA acute Experiments"))
# RESPONSE:
# yes/no, it appears that the PICRO, TTX, LYSIS were added before the second treatment
# rename the treatment in the wells as needed

# for cytotoxicity assays, the "Media" wells at F1 should contain the LYSIS. Visually confirm if correct, adjust where needed

# for Cell Titer Blue assay:
stripchart(rval ~ treatment, dat4[wllq==1 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "CellTiter Blue Blank-Corrected Fluorescence Values for Control Compounds")
stripchart(rval ~ treatment, dat4[wllq==0 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", add=T, col = "red")

dat4[, AB.trt.finalized := FALSE] # set this to TRUE for individual plates as you update as needed

# for every other culture, the "Media" well in F1 contains Lysis at the time of the AB reading (or could change by well F1 vs by the name "Media"...)
dat4[AB.trt.finalized == FALSE & grepl("AB",acsn) & treatment == "Media", .(plate.id, experiment.date, rowi, coli, wllq, rval, wllq_notes)] # all are in row 6, col 1
dat4[AB.trt.finalized == FALSE & grepl("AB",acsn) & treatment == "Media", `:=`(treatment = "Lysis",conc = 10, AB.trt.finalized = TRUE)]

# view updated stripchart
stripchart(rval ~ treatment, dat4[wllq==1&treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH","TTX/Lysis","Media/Lysis","PICRO/Lysis") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "CellTiter Blue Blank-Corrected Fluorescence Values for Control Compounds")
stripchart(rval ~ treatment, dat4[wllq==0& treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH","TTX/Lysis","Media/Lysis","PICRO/Lysis") & grepl("(AB)",acsn)],
           vertical = T, pch = 1, method = "jitter", col = "red", add = T)


# for LDH assay:
stripchart(rval ~ treatment, dat4[wllq==1 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acsn)],
           vertical = T, pch = 1, method = "jitter", main = "LDH Blank-Corrected Optical Density Values for Control Compounds")
stripchart(rval ~ treatment, dat4[wllq==0 & treatment %in% c("DMSO","PICRO","TTX","Media","Lysis","½ Lysis","1:250 LDH","1:2500 LDH") & grepl("(LDH)",acsn)],
           vertical = T, pch = 1, method = "jitter", add=T, col = "red")
# note that the added stripchart does nto follow the same category bins, sadly

# looks like media wells really do just contain Media
# actually makes some sense based on the assay - 
# first, 50uL of culture Media from each well in MEA plate is transfered to LDH plate (so F1 just contains Media in LDH plate)
# then 20uL of Media is added to F1 in the MEA plate. Then (all?) of the contents are removed and mixed
# a Media/Alamar Blue mixture is added to eadch well of MEA plate
# then culture Media/Blue mixutre is taken from MEA plates to CTB plates
# all this to say, I understand now why the 
# The only time when well F1 would have Lysis in the LDH assay is the same time when Lysis is added before the second recording,
# which was already clearly marked in the Group 1 file. 
# So LDH well treatments should usually = neural stats well treatments, if Lysis, PICRO, etc. were added before the second recording



# CALCULATE PERCENT OF TOTAL LDH (verify wllq, treatments first)
dat4 <- processLDH(dat4, use_half_lysis = T)
# can look at values and plot to see if any 1/2 Lysis median control wells look concerning
plot(dat4[grepl("LDH",acsn) & wllq == 1 & !is.na(as.numeric(conc)), .(log10(as.numeric(conc)), rval)], xlab = "log10(conc)", main = "LDH Percent of Total LDH by conc")


# ASSIGN WLLT
# for neural stats endpoints:
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), unique(treatment)]
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & treatment == "DMSO", wllt := "n"]
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & treatment == "Lysis", wllt := "x"] # only positve control for cytotox assays
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & treatment == "PICRO", wllt := "p"] # gain of signal positive control
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & treatment == "TTX", wllt := "m"] # loss of signal control
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & treatment == "Media", wllt := ""]
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), unique(treatment)]
dat4[!(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), wllt := "t"]

# for cytotox endpoints
dat4[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")), unique(treatment)]
dat4[grepl("Lysis",treatment) & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "p"] # Lysis, 1/2 Lysis, TTX/Lysis can all be labelled "p"
dat4[treatment == "DMSO" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "n"]
dat4[treatment == "PICRO" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := "z"] # same for BIC
dat4[treatment == "TTX" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := ""]
dat4[treatment == "Media" & acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB"), wllt := ""]
dat4[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), unique(treatment)]
dat4[(acsn %in% c("NHEERL_MEA_acute_LDH","NHEERL_MEA_acute_AB")) & is.na(wllt), wllt := "t"]
dat4[,unique(wllt)]
# 


# CHECK CONC'S (conc's for controls can just make those follow the treatments)
dat4[, unique(conc)] # any NA's? any non-numeric? Any 0? does it look like conc correction was done?

# update conc for control wells
# dmso
dat4[treatment == "DMSO",unique(conc)]
# [1] "Control"
# I'm just gonna gues 0.002, as in Toxcast compounds. bc it doesn't really matter
dat4[treatment == "DMSO", conc := "0.002"]

# picro
dat4[treatment == "PICRO", .N, by = "conc"]
# conc   N
# 1:   25 594
# 2:   10  51
# 3:    1  17
# based on lab notebook, this should always be 25 (there can be mix ups in calc file)
dat4[treatment == "PICRO", conc := "25"]

# ttx
dat4[treatment == "TTX", .N, by = "conc"]
# conc   N
# 1:    1 628
# 2:   10  17
# 3:   25  17
# based on lab notebook, this should always be 1
dat4[treatment == "TTX", conc := "1"]

# media
dat4[treatment == "Media", .N, by = "conc"]
# conc   N
# 1:   10 873
# 2:   25  48
# 3:    1  16
# I am pretty sure Media should always be 10
dat4[treatment == "Media", conc := "10"]

# lysis
dat4[grepl("Lysis",treatment), .N, by = c("treatment","conc")]
#     treatment    conc   N
# 1:       Lysis      10  82
# 2:   TTX/Lysis      10   3
# 3: Media/Lysis      10   1
# 4: PICRO/Lysis      10   1
# 5:       Lysis   Lysis 117
# 6:     ½ Lysis ½ Lysis 117
# Any Lysis wells will be 10, 1/2 Lysis wells will be labelled 5 (since these wells are half Lysis half Media dilution)
dat4[grepl("Lysis",treatment) & !grepl("½",treatment), conc := "10"]
dat4[treatment == "½ Lysis", conc := "5"]

# any other compounds to update??

# make conc's numeric
dat4[, sort(unique(conc))]
dat4[, conc := as.numeric(conc)]


# ASSIGN SPIDS
spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
names(spidmap)
setnames(spidmap, old = "NCCT ID", new = "spid")
setnames(spidmap, old = "Chemical ID", new = "treatment")
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1]   
dat4 <- merge(x = dat4, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat4[is.na(spid),unique(treatment)]
# [1] 
dat4[grepl("DMSO",treatment), spid := "DMSO"]
dat4[treatment == "Media", spid := "Media"]
dat4[treatment == "PICRO", spid := "Picrotoxin"]
dat4[treatment == "TTX", spid := "Tetrodotoxin"]
dat4[grepl("Lysis",treatment), spid := "Tritonx100"]
dat4[grepl("Lysis",treatment), unique(conc), by = "treatment"]
unique(dat4$spid) # confirm no NA spids


# ASSIGN ACID
# holding off on this, until I get NHEERL_MEA_acute_cross_correlation_WHM registered
# # get acid, via invitrodb
# tcplConf(user = "***REMOVED***", pass = ***REMOVED***, db='invitrodb', drvr='MySQL', host = "ccte-mysql-res.epa.gov")
# shafer.assays <- tcplLoadAcid(fld = "asid",val=20,add.fld = "acsn")
# mea.acute <- shafer.assays[grepl("MEA_acute",acnm)]
# dat4 <- merge(dat4, mea.acute[, .(acsn,acid)], by = "acsn")


# check that all data is there, nothing is missing
unique(dat4$acsn)
# 
unique(dat4$wllq) # any NA?
# [1] 
length(unique(dat4$plate.id)) # correct number of plates?
# 
length(unique(dat4$experiment.date))
sort(unique(dat4$conc))
check.points <- dcast(dat4[, .N, by = c("acsn","plate.id")], plate.id ~ acsn, value.var = "N")
setnames(check.points, old = names(check.points), new = sub("NHEERL_MEA_acute_","",names(check.points)))
check.points
# 

# visualizations/confirmation
boxplot(rval ~ acsn, dat4[wllq == 1 & !grepl("AB",acsn)])
boxplot(rval ~ acsn, dat4[wllq == 1 & grepl("AB",acsn)]) # the only non-normalized endpoint at the moment
plot(dat4[wllq == 1 & !grepl("AB",acsn), .(log10(conc), rval)], xlab = "log10(conc)", ylab = "rval (percent change in activity)")
title(main = paste0("All Points (except AB) for ",dataset_title))

# create a nice summary of wllq assignments for each well
createWllqSummary(dat4, dataset_title)

# save dat4
dat4 <- dat4[, .(treatment, spid, experiment.date, plate.id, apid, rowi, coli, conc, acsn, wllt, wllq, wllq_notes, rval, srcf, files_log, dat2)]
save(dat4, file = file.path(main.output.dir, paste0("output/",dataset_title,"_dat4_",as.character.Date(Sys.Date()),".RData")))

# save a copy of dat4 with just the mc0 columns
assign(paste0(dataset_title,"_mc0"), value = dat4[, .(spid, apid, rowi, coli, conc, acsn, wllt, wllq, srcf, rval)])
save(list = c(paste0(dataset_title,"_mc0")), file = file.path(main.output.dir, paste0("output/",dataset_title,"_mc0_",as.character.Date(Sys.Date()),".RData")))

# you're done!