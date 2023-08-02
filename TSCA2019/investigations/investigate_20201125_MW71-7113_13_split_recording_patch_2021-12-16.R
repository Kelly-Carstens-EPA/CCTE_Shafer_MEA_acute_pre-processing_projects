# ----------------------------------------------------------------------- #
# The recording for 20201125_MW71-7113_13 is broken up over 2 recordings
# (14min39 for the first, 25min21sec for the second)
# We need to combine the values from these 2 files 
# to get 1 set of values for this plate
# 
# Dec 16, 2021
# ----------------------------------------------------------------------- #

files <- file.path('L:/Lab/NHEERL_MEA','Project TSCA 2019','Acute TSCA Conc Response',
                   '20201125 Culture G2','Neural Statistic Compiler',c('AC_20201125_MW71-7113_13_00(001)(000).csv','AC_20201125_MW71-7113_13_00(002)(001).csv'))
files

# first, run teh checks on these 2 files
runChecks(files, check.settings = F, check.parameters = T, check.timing = T)
# $settings_summary
# NULL
# 
# $parameters_summary
# [1] TRUE TRUE
# 
# $timing_summary
# [1] "AC_20201125_MW71-7113_13_00(001)(000).csv analysis duration is 878.75s" 
#     "AC_20201125_MW71-7113_13_00(002)(001).csv analysis duration is 1521s"  

# okay, looks good!

# Now read in the data, then tinker with how to merge

extractAllData(main.output.dir, dataset_title, run.type.tag.location, plate.id.tag.location = plate.id.tag.location, append = T)

i <- 1
run.type.tag.location <- 5
dat1 <- fileToLongdat(files[i], run.type.tag.location, plate.id.tag.location = numeric(0), guess_run_type_later = F)
# AC_20201125_MW71-7113_13_00(001)(000).csv will be removed. Recording length is 878.75 
# Processed AC_20201125_MW71-7113_13_00(001)(000).csv 
dat1
# okay, so it isn't removed yet lol
i <- 2
dat2 <- fileToLongdat(files[i], run.type.tag.location, plate.id.tag.location = numeric(0), guess_run_type_later = F)
# Processed AC_20201125_MW71-7113_13_00(002)(001).csv 

cat(unique(dat1$acsn), sep = '\n')

# Question:
# Do we have data from other instances where the a recording was done in pieces, 
# and as a whole? Perhaps for some DMSO control analysis?

# Not seeing any... but it definitely wouldn't be all that difficult to create

# I'm just going to not include the data for this plate for now, will add in later


