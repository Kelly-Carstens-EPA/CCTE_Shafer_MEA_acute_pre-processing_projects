# script to do the things!

###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/dummy_test/"
dataset_title <- "dummy_test"
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/dummy_test/")
select.files <- F
# things to define re date and plate id collection:
# for files named as "tag1_tag2_tag3_....csv",
# If the files do not include the "Plate Serial Number" info, 
# what tag in the file name is the plate.id located?
# (the neural stats compiler file name, not the )
plate.id.location <- NULL # default is NULL - will first look in file header. If can't find, will look in file name tag
run.type.tag.location <- 5 # Which tag in the file names defines the run_type?
###################################################################################
# END USER INPUT
###################################################################################

# load packages
library(data.table)
library(xlsx)
library(pracma)

# source functions
source('../mea-acute-neural-stats-to-mc0-scripts/gather_files-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/data_prep-functions.R')
source('../mea-acute-neural-stats-to-mc0-scripts/collapsePlateData.R')
source('../mea-acute-neural-stats-to-mc0-scripts/acute_cytotox_prep06.R')
source('../mea-acute-neural-stats-to-mc0-scripts/check-functions.R')

# loading some information for the funtions to reference
load("../mea-acute-neural-stats-to-mc0-scripts/standard_settings.RData")

main.output.dir <- getwd()
if (!dir.exists(file.path(main.output.dir,"output"))) dir.create(file.path(main.output.dir,"output"))

# select input files to use and log files in log file
if (select.files) {
  selectInputFiles(start.dir, main.output.dir, dataset_title)
}

# before process any data, need to check that file names following the naming convention 
# specifically, at the specified run.type.tag.location, there should either be _00 or _01 for each plate
# this is a fallable check, thought, bc the plate or date names may be incorrect in the file names
checkFileNames(main.output.dir)
checkFileNames(files_log = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/APCRA2019_files_log_2020-05-13.txt")

# Check the selected files for 
# if you do not run these checks, will throw an error later... 
# oof, how to handle that? If there are errors that I want to ignore, but just for certain plates?
tryCatch(writeCheckSummary(main.output.dir, dataset_title), 
         error = function(e){
           closeAllConnections()
           e
         })            

# more tests
files <- read_files(main.output.dir)
filei <- files[1]

# a real file that is missing some network spike parameters
filei <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA/20190515 Culture DNT G1 G2/DNT Acute Group 1/Neural Statistics Compiler Deprecate/TC_20190508_MW68-0808_15_00(000).csv"
results <- runChecks(filei)
results
# $settings_summary
# [1] "TC_20190508_MW68-0808_15_00(000).csv has some different settings 'Column 'file_col2': 1 string mismatch'\nfile_col1\t\t\t\tfile_settings\t\t\t\tstandard_settings\n   Low Pass Cutoff Freq.\t\t\t\t5 kHz\t\t\t\t3 kHz"
# 
# $parameters_summary
# [1] "TC_20190508_MW68-0808_15_00(000).csv is missing Area Under Cross-Correlation, Width at Half Height of Cross-Correlation, Synchrony Index"
# 
# $timing_summary
# [1] "TC_20190508_MW68-0808_15_00(000).csv analysis starts at 1568s, "

# extract all of the data from the files and transform into long data format
extractAllData(main.output.dir, dataset_title, run.type.tag.location, append = F, plate.id.location = plate.id.location, threshold = 30,
               check.settings = F, check.timing = F, check.parameters = F)

# output summary of wllq changes
load(file = "output/dummy_test_alldat_2020-05-15.RData")
alldat[wllq == 0, .(wllq_set_to_zero = paste0(sort(unique(well)),collapse=",")), by = "apid"]

collapsePlateData(main.output.dir)

# testing with APCRA data - need to update this file
extractAllData(main.output.dir, dataset_title = "apcra_test", run.type.tag.location, append = F, files_log = "L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/APCRA2019/APCRA2019_files_log_2020-05-13.txt",
               plate.id.location = plate.id.location, threshold = 30,
               check.settings = T, check.timing = T, check.parameters = T)

# collapse the treated and baselien data acrosss plates


# summary of files that are 30 seconds above or below 40 min
# Warning message:
#   In fileToLongdat(filei, plate.id.location = plate.id.location, div.location = div.location,  :
#                      
#                      Analysis duration is (2434.5s) for TC_20190911_MW69-3920_15_00(000)(000).csv

# Other than this file, all files are within 30 seconds of 40 minutes of duration

# might check the file offset later, as Kathleeen suggested with recording time!!

# now collect all data
files <- files[order(basename(files))]
alldat <- list()
cat("\nReading data from files...")
# sink(file = log_file)
for (filei in files) {
  # cat("\n",basename(filei),sep="")
  add.dat <-  tryCatch({
    # The code you want run
    fileToLongdat(filei)
  }, error = function(err) {
    # Is returned if error encountered
    cat("\nProblem with ",filei,"\n",sep="")
    print(paste0(err))
  })
  # add.dat <- fileToLongdat(filei, plate.id.location = plate.id.location, div.location = div.location)
  alldat <- rbind(alldat, add.dat)
  rm(add.dat)
}

# !!! still need to finalize baseline/treated definitions!!

# plot data re mfr stuff
alldat[grepl("firing_rate_mean",tcpl_acsn) & run_type == "baseline", summary(activity_value*60)]
hist(alldat[grepl("firing_rate_mean",tcpl_acsn) & run_type == "baseline", activity_value*60], 
     breaks = 30, xlab = "Mean firing rate (spikes/min) in baseline wells")
abline(v=30)

# wells where nAE>10
hist(alldat[grepl("firing_rate_mean",tcpl_acsn) & run_type == "baseline" & wllq == 1, activity_value*60], 
     breaks = 30, xlab = "Mean firing rate (spikes/min) in baseline wells")
abline(v=30)

# just curious
hist(alldat[grepl("firing_rate_mean",tcpl_acsn) & run_type == "treated", activity_value*60], 
     breaks = 30, xlab = "Mean firing rate (spikes/min) in treated wells")
abline(v=30)


# add cyto data


# cleaning up, probs could streamline later
setnames(alldat3, old = "tcpl_acsn",new = "acsn")
usecols <- intersect(names(alldat3), names(cytodat))
alldat4 <- rbind(alldat3[, ..usecols], cytodat[,..usecols])

# for cyto data, remove wllq where baseline was too low??

# include looking it over, assisnging wllt and conc clean up
# check trt names either spid or DMSO, Bicuculline, or Tritonx100
# check conc's make sure it all looks good/is numeric (not "Control")

save(alldat4, file = file.path(output.dir, paste0("alldat4_",dataset_title,"_",as.character.Date(Sys.Date()),".RData")))
