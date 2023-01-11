# script to do the things!

###################################################################################
# USER INPUT
###################################################################################
start.dir <- "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/Experiment Date"
dataset_title <- "ToxCast2016"
setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016")

select.files <- FALSE # run selectInputFiles or no? else will read from most recent _files_log.txt file

# things to define re date and plate id collection:
# for files named as "tag1_tag2_tag3_....csv",
# If the files do not include the "Plate Serial Number" info, 
# what tag in the file name is the plate.id located?
# (the neural stats compiler file name, not the )
plate.id.location <- 2 # default is NULL - will first look in file header. If can't find, will look in file name tag
div.location <- 5 # default is 4 - will first look in file header. If can't find, will look in file name tag
###################################################################################
# END USER INPUT
###################################################################################

# load packages
library(data.table)
library(xlsx)
library(pracma)

# source functions
source('../mea-acute-neural-stats-to-mc0-scripts/gather_files-functions.R')
source('mea-acute-neural-stats-to-mc0-scripts/data_prep-functions_toxcast2016.R')
source('../mea-acute-neural-stats-to-mc0-scripts/collapsePlateData.R')
source('../mea-acute-neural-stats-to-mc0-scripts/acute_cytotox_prep06.R')

# additional function for this data set
source('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016/get_files_without_select.R')

main.output.dir <- getwd()

# select input files to use and log files in log file
if (select.files) {
  # selectInputFiles(start.dir, log_file, dataset_title, date.location)
  all_files <- getToxCastFiles()
  writeLogFile(all_files, main.output.dir, dataset_title)
  length(all_files) # 414
}

# read in files from most recent _files_log txt file in check.dir
files <- read_files(check.dir = main.output.dir)

# # confirming these are the same:
# c( setdiff(files, all_files), setdiff(all_files, files)) # character(0)
# rm(all_files)

checknames <- unlist(strsplit(basename(files), split = "_Neural Statistics Compiler\\(00[012]\\)\\.csv"))

# a few anamolies
checknames <- unlist(strsplit(checknames, split = "(_Batch Process \\(1 Files\\)\\(000\\))|(40min)"))

name_prefix <- sub("_[01]{2}\\([01]{3}\\)","",checknames)
# a few exceptions
name_prefix <- sub("_[01]{2}\\([01]\\)", "",name_prefix)

name.ending <- c()
for (i in 1:length(checknames)) {
  add.ending <- sub(name_prefix[i], "", checknames[i])
  name.ending <- c(name.ending, add.ending)
}
name_table <- data.table(full.name = checknames, name.prefix = name_prefix, name.ending = name.ending)
name_table[, c("file1","file2", "dummy_file3"):=list(sort(unique(name.ending))[1], sort(unique(name.ending))[2], sort(unique(name.ending))[3]), by = "name.prefix"]
unique(name_table[, .(file1, file2, dummy_file3)]) # no third files, check
summary.table <- name_table[, .(num_plates = .N), by = c("file1","file2")]
summary.table[order(-num_plates)]
# file1    file2 num_plates
# 1: _00(000) _01(000)        378
# 2: _00(000) _01(001)         16
# 3: _00(001) _01(000)         10
# 4:   _00(1)   _01(1)          6
# 5: _00(000) _00(001)          2
# 6: _00(001) _01(001)          2



# just checking for missing parameter data. Flags will go to console
# **  note that I am allowing an exception here for "Width at Half Height of Cross-Correlation" for this data set
checkForAllParameters(files, skip_param = "Width at Half Height of Cross-Correlation")

# end thr May 13, 2020


# just check/get summary of anlaysis start times
log_file <- file.path(getwd(), paste0(dataset_title, "_analysis_start_summary_",as.character.Date(Sys.Date()),".txt"))
writeFlagSummary(files, log_file, threshold = 10)

# transform the files into a long file format
# 1 - collpasePlateData calls fileToLongDat for each file, and binds results together into 1 table
# 2 - collapsePlateData calculates the 
output.dir <- file.path(getwd(),"output")
log_file <- file.path(getwd(), paste0(dataset_title, "_processing_log_",as.character.Date(Sys.Date()),".txt"))
collapsePlateData(files, log_file, output.dir, plate.id.location, div.location)


# read in data from flat file
dat <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv",
                stringsAsFactors = F)
basedat <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled.csv",
                    stringsAsFactors = F)
names(dat)

# adpated form source_to_lvl0_nheerl_mea_Acute.R
# check if data is same between files
diff <- sapply(intersect(names(basedat), names(dat)), function(x) all.equal(basedat[,x], dat[,x]))
(cols <- names(diff)[which(diff!="TRUE")])
# [1] "CONCENTRATION"                                  
# [2] "MEA_AREA_UNDER_CROSS_CORRELATION_BASELINE_RUN_2"
# [3] "MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_4"    
# [4] "MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_5"

# replace concentration in df's with cyto's. Concentration in cyto already has concentration correction from stock
setdiff(basedat$CONCENTRATION, dat$CONCENTRATION)
unique(dat$CONCENTRATION) # (shortened) - yes, I believe this could include conc corrections already
# [1]       NA 40.00000 10.00000  3.00000  1.00000  0.30000  0.10000  0.03000 36.60000  9.15000  2.75000  0.91500  0.27500  0.09150  0.02745 41.80000 10.45000
# [18]  3.14000  1.04500  0.31400  0.10450  0.03135 39.80000  9.95000  2.99000  0.99500  0.29900  0.09950  0.02985 41.00000 10.25000  3.08000  1.02500  0.30800
# [35]  0.10250  0.03075 41.60000 10.40000  3.12000  1.04000  0.31200  0.10400  0.03120 40.20000 10.05000  3.02000  1.00500  0.30200  0.10050  0.03015 24.60000
# [52]  6.15000  1.85000  0.61500  0.18500  0.06150  0.01845 25.80000  6.45000  1.94000  0.64500  0.19400  0.06450  0.01935 37.00000  9.25000  2.78000  0.92500
# [69]  0.27800  0.09250  0.02775 40.60000 10.15000  3.05000  1.01500  0.30500  0.10150  0.03045 20.00000  5.00000  1.50000  0.50000  0.15000  0.05000  0.01500
# [86] 34.40000  8.60000  2.58000  0.86000  0.25800  0.08600  0.02580 35.60000  8.90000  2.67000  0.89000  0.26700  0.08900  0.02670 38.20000  9.55000  2.86500
basedat$CONCENTRATION <- dat$CONCENTRATION
# Replace the remaining columns with differences
basedat$MEA_AREA_UNDER_CROSS_CORRELATION_BASELINE_RUN_2 <- dat$MEA_AREA_UNDER_CROSS_CORRELATION_BASELINE_RUN_2
basedat$MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_4 <- dat$MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_4
basedat$MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_5 <- dat$MEA_AREA_UNDER_CROSS_CORRELATION_DOSE_RUN_5

# corrections

#cyto[480:485, 143:148]; basedat[480:485, 143:148] #Inserted As and NAs shifting data
#Comparison: both datasets have data shift b/w rows 481-528 and columns 144-272, but only basedat has rest of data
#cyto[480:485, 268:274]; df[480:485, 268:274] #Cyto shifted data replaced with cytotox data
basedat[481:528,144:272] <- basedat[481:528,146:274] # now the last 2 columns are just NA's, without a title

# remove extra columns
basedat[,(ncol(basedat)-1):ncol(basedat)] <- NULL
# fix SampleIDs
basedat$EPA_SAMPLE_ID[basedat$EPA_SAMPLE_ID=='BC'] <- 'BIC'
# fix PlateIDs
basedat$MEA_PLATE_ID_RUN1[basedat$MEA_PLATE_ID_RUN1=='MW1068-24'] <- 'MW 1068-24'

# holding off on this for now
# # from "ToxCast MEA data Outliers and data check.docx" ( we do not have this file )
# # remove columns 7-8 for plateID 'MW 1076-37' on 20160317
# idx <- which(df$MEA_PLATE_ID_RUN1=='MW 1076-37' &
#                df$EXPERIMENT_DATE=='20160317' &
#                df$`MEA_WELL_ID _ALL_RUNS`%in%paste0(paste0(LETTERS[1:6],'0'),rep(7:8,each=6)))
# df[idx, 14 + c(0*43 + 1:43, (0+3)*43 + 1:43)] <- NA # Run 1 baseline and dose columns
# 
# # remove columns 1-2 for plateID 'MW 1048-15' on 20160531
# idx <- which(df$MEA_PLATE_ID_RUN3=='MW 1048-15' &
#                df$EXPERIMENT_DATE=='20160531' &
#                df$`MEA_WELL_ID _ALL_RUNS`%in%paste0(paste0(LETTERS[1:6],'0'),rep(1:2,each=6)))
# df[idx, 14 + c(2*43 + 1:43, (2+3)*43 + 1:43)] <- NA # Run 3 baseline and dose columns

# change plateID 'MW 1042-50' to 'MW 1044-1' for run 3 (said corrected in raw file, but still here)
basedat$MEA_PLATE_ID_RUN3[basedat$MEA_PLATE_ID_RUN3=='MW 1042-50'] <- 'MW 1044-1'

# add in cyto data
basedat <- cbind(df, cyto[,names(cyto)[!names(cyto)%in%names(df)]])

# remove earlier runs of any samples that are duplicated
x   <- unique(basedat[grepl("^TX", basedat$EPA_SAMPLE_ID) ,c('EPA_SAMPLE_ID','EXPERIMENT_DATE')])
x   <- x[order(x$EXPERIMENT_DATE, decreasing=T),]
dup <- x[duplicated(x$EPA_SAMPLE_ID),] # duplicated() assigns TRUE to the second, third, etc. repeats of the items.
df  <- df[!paste(df$EPA_SAMPLE_ID, df$EXPERIMENT_DATE)%in%paste(dup$EPA_SAMPLE_ID, dup$EXPERIMENT_DATE),]


# my summary of what is needed:
dat <- read.csv("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv",
                stringsAsFactors = F)

# change plateID 'MW 1042-50' to 'MW 1044-1' for run 3 (said corrected in raw file, but still here)
dat$MEA_PLATE_ID_RUN3[dat$MEA_PLATE_ID_RUN3=='MW 1042-50'] <- 'MW 1044-1'

# manipulation for my purposes:
setDT(dat)
dat.sm <- dat[, c(1:14, 273:280)]

# what's the deal with teh multiple well ID's?
dat.sm[, all.equal(MEA_WELL_ID._ALL_RUNS, LDH_WELL__ID, AB_WELL_ID)] # TRUE

id.cols <- c( "ALIQUOT_PLATE_BARCODE", "ALIQUOT_WELL_ID",       "ALIQUOT_CONC",          "ALIQUOT_CONC_UNIT",     "EPA_SAMPLE_ID",         "CONCENTRATION",        
              "ALIQUOT_VOLUME",        "ALIQUOT_VOLUME_UNIT",   "ALIQUOT_SOLVENT",       "EXPERIMENT_DATE", "MEA_WELL_ID._ALL_RUNS")

usecols <- c(id.cols, "MEA_PLATE_ID_RUN1","LDH_.DEAD_RUN1", "AB_.DEAD_RUN1")
run1 <- dat.sm[, ..usecols]
updatecols <- grep("_RUN1",names(run1),value=T)
setnames(run1, old = updatecols, new = sub("_RUN1", "", updatecols))

usecols <- c(id.cols, "MEA_PLATE_ID_RUN2","LDH_.DEAD_RUN2", "AB_.DEAD_RUN2")
run2 <- dat.sm[, ..usecols]
updatecols <- grep("_RUN2",names(run2),value=T)
setnames(run2, old = updatecols, new = sub("_RUN2", "", updatecols))

usecols <- c(id.cols, "MEA_PLATE_ID_RUN3","LDH_.DEAD_RUN3", "AB_.DEAD_RUN3")
run3 <- dat.sm[, ..usecols]
updatecols <- grep("_RUN3",names(run3),value=T)
setnames(run3, old = updatecols, new = sub("_RUN3", "", updatecols))

flatdat <- rbind(run1, run2, run3)
setnames(flatdat, old = c("MEA_PLATE_ID"), new = "plate.id")
flatdat[, apid := paste0(EXPERIMENT_DATE,"_",plate.id)]

# target cols: platedat[, .(tcpl_acsn, plate.id, apid, coli, rowi, wllq, rval, srcf)]

# assign acsn
flatdat.long <- melt(flatdat, measure.vars = c("LDH_.DEAD","AB_.DEAD"), variable.factor = F, value.name = "rval", variable.name = "file.acsn")
tcplLoadAcid(fld = "asid", val = 20) # confirming these are the correct acnms
flatdat.long[file.acsn == "LDH_.DEAD", tcpl_acsn := "NHEERL_MEA_acute_LDH"]
flatdat.long[file.acsn == "AB_.DEAD", tcpl_acsn := "NHEERL_MEA_acute_AB"]

# should I set negative percent change values to 0?
flatdat.long[rval < 0, .N] # this happens a bunch!! someone probably did 1-%viable
flatdat.long[, orval := 1 - rval]
flatdat.long[orval <0, .N, by = "tcpl_acsn"] # still, sevearl...

# assign coli and rowi
flatdat.long[, coli := as.numeric(sub("[[:alpha:]]*","",MEA_WELL_ID._ALL_RUNS))]
flatdat.long[, rowc := sub("[[:digit:]]*$","",MEA_WELL_ID._ALL_RUNS)]
flatdat.long[, rowi := sapply(rowc, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
flatdat.long[, rowc := NULL]

# add srcf
flatdat.long$srcf <- basename("L:/Lab/Toxcast_Data/toxcast_data/files/nheerl_mea_acute/source/ToxCast CC Burst Analysis_Network enabled with cytotoxicity data.csv")


# read in the neural stat + cyto data
neuralRData <- file.path(output.dir, paste0(dataset_title,"_alldat2_",as.character.Date(Sys.Date()),".RData"))
load(neuralRData)



# get the trt, conc data for alldat2 from cyto dat
alldat3 <- merge(alldat2, cytodat[, .(treatment, conc, apid, rowi, coli, wllt)])

# cleaning up, probs could streamline later
setnames(alldat3, old = "tcpl_acsn",new = "acsn")
usecols <- intersect(names(alldat3), names(cytodat))
alldat4 <- rbind(alldat3[, ..usecols], cytodat[,..usecols])

# for cyto data, remove wllq where baseline was too low??

# include looking it over, assisnging wllt and conc clean up
# check trt names either spid or DMSO, Bicuculline, or Tritonx100
# check conc's make sure it all looks good/is numeric (not "Control")

save(alldat4, file = file.path(output.dir, paste0("alldat4_",dataset_title,"_",as.character.Date(Sys.Date()),".RData")))
