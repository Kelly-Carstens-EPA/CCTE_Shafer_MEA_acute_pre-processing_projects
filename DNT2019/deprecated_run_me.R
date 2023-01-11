# script to do the things!

###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA"
dataset_title <- "DNT2019"
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/DNT2019/")
select.neural.stats.files <- F # run selectInputFiles or no? else will read from most recent _files_log.txt file
select.calculations.files <- F
run.type.tag.location <- 5 # for files named as "tag1_tag2_tag3_....csv", Which tag in the file names defines the run_type?
###################################################################################
# END USER INPUT
###################################################################################

# load packages
library(data.table)
library(readxl)
library(pracma)

# source functions from master scripts
source('../mea-acute-neural-stats-to-mc0-scripts/gather_files-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/check-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/data_prep-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/collapsePlateData.R')
source('../mea-acute-neural-stats-to-mc0-scripts/acute_cytotox_prep06.R')

# loading some information for the funtions to reference
load("../mea-acute-neural-stats-to-mc0-scripts/standard_settings.RData")
get_acsn_map(type = "post_july_2016")

main.output.dir <- getwd()

if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# select input files to use and log files in log file
if (select.neural.stats.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "neural_stats")
}
if (select.calculations.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title, files_type = "calculations")
}

# before process any data, need to check that file names following the naming convention 
# specifically, at the specified run.type.tag.location, there should either be _00 or _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(main.output.dir)
# Reading from DNT2019_files_log_2020-05-18.txt...
# Got 78 files.
# The following files appear to be named incorrectly:
#   Empty data.table (0 rows and 2 cols): filenames,run.type.tags

# Check the selected files for common issues
# if you do not run these checks, fileToLongDat will throw an error later if any of these are an issue
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e
         })                                     

# extract all of the data from the files and transform into long data format
# If you have reviewed the checks above and are confident ignoring them, 
# you can set the corresponding "check.x" variables to F. Document why this is okay.
extractAllData(main.output.dir, dataset_title, run.type.tag.location, append = F, threshold = 30,
               check.settings = F, check.parameters = T, check.timing = F)

# output summary of wllq changes
load(file = "output/DNT2019_alldat1_2020-05-18.RData")
alldat1[wllq == 0, .(wllq_set_to_zero = paste0(sort(unique(well)),collapse=",")), by = "apid"]
object.size(alldat1)
rm(alldat1)

# create alldat2, with the collapsed plate data
collapsePlateData(main.output.dir)

load(file = "output/DNT2019_alldat2_2020-05-18.RData")
boxplot(rval ~ acsn, alldat2[wllq==1])
rm(alldat2)

# end May 18, 2020

# add cyto data
# files <- read_files(main.output.dir, files_type = "calculations")
saveCytoData(main.output.dir, dataset_title)
load(file = "output/DNT2019_cytodat_2020-05-18.RData")
str(cytodat)


# COMBINE the data!

# get trt, conc values for alldat2 from cytodat
alldat3 <- list()
endpoints <- unique(alldat2$acsn)
for (endpoint in endpoints) {
  add.dat <- merge(alldat2[acsn == endpoint], cytodat[, .(treatment, apid, rowi, coli, conc)])
  alldat3 <- rbind(alldat3, add.dat)
  rm(add.dat)
}

# get the wllq for cytodat - just use 1 (any) acsn to get the table
wllq_summary <- alldat2[acsn == "NHEERL_MEA_acute_burst_duration_mean", .(wllq), by = c("apid","rowi","coli")]
cytodat <- merge(wllq_summary, cytodat, all.y = T)

alldat4 <- rbind(alldat3, cytodat)

# assign wllt (will look a bit diff for ea dataset, with naming)
# check trt names either spid or DMSO, Bicuculline, or Tritonx100
# check conc's make sure it all looks good/is numeric (not "Control")

# check that all data is there, nothing is missing
# set wllq to 0 where needed
# any other checks

save(alldat4, file = file.path(main.output.dir, paste0("/output",dataset_title,"_alldat4_",as.character.Date(Sys.Date()),".RData")))
