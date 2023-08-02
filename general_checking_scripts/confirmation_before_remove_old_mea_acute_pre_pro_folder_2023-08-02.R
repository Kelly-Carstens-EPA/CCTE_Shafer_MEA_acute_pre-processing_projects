# I just redacted a password from the commit history of the mea acute pre-processing
# Using the BFG tool
# That means that all of the commit ids have changed
# What has been the mea acute pre-processing projects folder needs to be deleted and recloned from the remote
# https://github.com/Kelly-Carstens-EPA/CCTE_Shafer_MEA_acute_pre-processing_projects
# Doing soem checks here before I delete the deprecated pre-processign folder
# (currently named ../deprecated_pre-process_mea_acute_for_tcpl)
# Aug 2 2023

# I just cloned the updated remote, which is currently saved in teh folder "CCTE_Shafer_MEA_acute_pre-processing_projects"
# This folder will be renamed to "pre-process_mea_acute_for_tcpl"


# Checking changes in ../deprecated_pre-process_mea_acute_ --------

# Checking that the updates in the neural_stats_acsn_to_tcpl_acnm_map.xlsx from the deprecated_pre-processing_mea_acute_for_Tcpl
# are just excel-related updates to the .xlsx, and not actual chagnes to the cells
library(data.table)
library(openxlsx)

test <- as.data.table(read.xlsx('neural_stats_acsn_to_tcpl_acnm_map.xlsx'))
test2 <- as.data.table(read.xlsx('../neural_stats_acsn_to_tcpl_acnm_map.xlsx'))
all.equal(test, test2)

setdiff(colnames(test), names(test2)) # "Status_invitroDBv3.5"
setdiff(colnames(test2), names(test))

all.equal(test[, .SD, .SD = names(test2)], test2)
# TRUE!

# cool -> so I can delete '../neural_stats_acsn_to_tcpl_acnm_map.xlsx'


# Check all gitignored files migrated -------------------------------------

# Several files were marked as .gitignored in ../deprecated_pre-process_mea_acute_for_tcpl
# I have manually migrated these over to "CCTE_Shafer_MEA_acute_pre-processing_projects"
# And updated the .gitignore
# want to check that I successfully identified all files that needed to be manually migrated

deprecated.gitignore.files <- scan(file = '../deprecated_pre-process_mea_acute_for_tcpl/.gitignore', sep = '\n', what = character())
deprecated.gitignore.files

res.list <- c()
for (filei in deprecated.gitignore.files){
  res <- list.files(path = dirname(filei), 
             pattern = basename(filei))
  if (length(res) != 1)
    res.list <- c(res.list, FALSE)
  else
    res.list <- c(res.list, TRUE)
}

names(res.list) <- deprecated.gitignore.files

table(res.list)
# res.list
# FALSE  TRUE 
# 6    80 

cat(names(res.list[res.list == FALSE]), sep = '\n')
# .Rproj.user/ - this is fine, I dont' need to save hits
# Impedance2023/.Rhistory - hmm, it looks like this file is present... 
# maybe the leading . messed it up?
list.files(path = 'Impedance2023', pattern = 'Rhistory') # empty
list.files(path = 'Impedance2023', pattern = 'Rhistory', all.files = T) # ".Rhistory"
# ah, here it is! so hidden files are not included in list.files by default
# TSCA2019/output/TSCA2019_dat1_2021-05-10.RData - ugh, I'll save one of these in the "deprecated" folder under the password-redacted CCTE_Shafer_MEA_Acute...
# TSCA2019/output/TSCA2019_dat1_2021-12-16.RData
# single_point_screen/TSCA2019/output/TSCA2019_dat1_2020-11-16.RData - these files are present, I just moved them to deprecated/single_point_screen/TSCA2019/output
# single_point_screen/TSCA2019/output/TSCA2019_dat1_2020-11-17.RData - these files are present, I just moved them to deprecated/single_point_screen/TSCA2019/output

# Okay! So there are no files that are .gitignored on the folder deprecated_pre-process_mea_acute_for_tcpl that I have not successfully migrated over to the new
# cloen of MEA acute project repo in whic the passwords have been redacted

# SO, we should be save to delete the deprecated pre-process mea acute folder


# Other than the .gitignored object, any modifications in the deprecated_pre-process_mea_acute_for_tcpl shoudl be saved in the commit history!