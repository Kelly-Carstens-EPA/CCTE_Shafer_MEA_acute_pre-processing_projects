#-----------------------------------------------------------------------------------#
# Confirming which MEA Acute assay components selected in Kosnik, et al.
# Creating table of which enpdoints can be released in invitrodb snapshot
# Jan 27, 2022
#-----------------------------------------------------------------------------------#

library(data.table)
library(openxlsx)
library(stringi)

# getSheetNames('../Kosnik2019-Supplementary_file1.xlsx')
# test <- read.xlsx('../Kosnik2019-Supplementary_file1.xlsx', sheet = 'Table S2')
# test
# # Hmm, this shows 27 endpoints, ranked with the Gini index
# # But not the top 15 resulting from the 3 ML models with a hit count threshold of 3


# I'll just copy Table 1 with the 15 top parameters
# Copied from here: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7371233
table1 <- 'Category	Parameter	Description/definition
General activity (“Firing”)	Mean firing rate (Hz)	Total number of spikes over the duration of the analysis
Number of spikes	Total number of spikes over the duration of the analysis
Number of bursts	Total number of single-electrode bursts over the duration of the analysis
Burst structure	Burst duration (average)	Average time from the first to the last spike in a burst
Interburst interval (average)	Average time between the start of bursts
Burst percentage-avg	The number of spikes in a burst divided by the total number of bursts, times 100
Burst percentage-std	Standard deviation of the burst percentage
Number of spikes/burst (avg)	Average number of spikes in a burst
Connectivity	Network burst percentage	The number of spikes in network bursts divided by the total number of spikes, times 100
#Spikes/network burst-avg	The average number of spikes in a network burst
#Spikes/network burst-std	The standard deviation of the number of spikes in a network burst
#Electrodes participating in Burst-avg	The average number of electrodes participating in a network burst
Area under cross-correlation	Area under the well-wide pooled inter-electrode cross-correlation
Full-width at half-height of cross-correlation	Distance along the x-axis from the left half height to the center half height of the normalized cross correlogram
Synchrony Index	A unit measure of synchrony between 0 and 1'

# Slice and dice to transform to a data table
table1.split <- stri_split(table1, fixed = '\n')[[1]]
table1.split2 <- stri_split(table1.split, fixed = '\t')
# Add the category to each row
cati <- ''
for (i in 2:length(table1.split2)) {
  if (length(table1.split2[[i]]) < 3) {
    table1.split2[[i]] <- c(cati, table1.split2[[i]])
  } 
  else if (length(table1.split2[[i]]) == 3) {
    cati <- table1.split2[[i]][1]
  } 
}

# transform to a table
table1.mat <- Reduce(f = rbind, x = table1.split2[2:length(table1.split2)]) # result is a matrix!
table1.dt <- as.data.table(table1.mat)
colnames(table1.dt) <- table1.split2[[1]]
table1.dt


# Clean up parameters to match components names as outputted by neural stats compiler
table1.dt[, acsn_lower := tolower(Parameter)]
table1.dt[, acsn_lower := sub('-avg',' - avg',acsn_lower)]
table1.dt[, acsn_lower := sub('-std',' - std', acsn_lower)]
table1.dt[, acsn_lower := sub('average','avg', acsn_lower)]
table1.dt[, acsn_lower := sub('interburst','inter-burst', acsn_lower)]
table1.dt[, acsn_lower := sub('\\/',' per ', acsn_lower)]
table1.dt[, acsn_lower := sub('#','number of ', acsn_lower)]
table1.dt[, acsn_lower := sub('\\(avg\\)','- avg', acsn_lower)]

# Load map I created from neural stats compiler acsn to tcpl acnm
acsn.map <- as.data.table(read.csv('L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/neural_stats_acsn_to_tcpl_acnm_map.csv'))
acsn.map[, acsn_lower := tolower(acsn)]

# Check for components that still don't match
setdiff(table1.dt$acsn_lower, acsn.map$acsn_lower)
table1.dt[acsn_lower == 'burst duration - avg', acsn_lower := 'burst duration - avg (s)']
table1.dt[acsn_lower == 'inter-burst interval - avg', acsn_lower := 'inter-burst interval - avg (s)']
table1.dt[, acsn_lower := sub('electrodes','elecs',acsn_lower)]

# full width at half height of cross-correlation - this is not a neural stats component
# Options:
# Half Width at Half Height of Cross-Correlation
# Width at Half Height of Cross-Correlation
# Half Width at Half Height of Normalized Cross-Correlation
# Width at Half Height of Normalized Cross-Correlation

# Description in table 1:
"Distance along the x-axis from the left half height to the center half height of the normalized cross correlogram"
# So the description sounds like the half width half height of the normalized cross correlogram
# BUT, as of 07/17/2020, 
# CCTE_Shafer_MEA_acute_cross_correlation_HWHM_normalized was not registered, but
# CCTE_Shafer_MEA_acute_cross_correlation_HWHM was registered
# So that makes it seem like Marissa must have used the non-normalized version
# (at least, she would have registered it that way!)
# Ah, here's some evidence:
# Supplemental Figure 1
# She determined which endpoints are highly correlated and selected one of them
# between these 2 components:
# FULL WIDTH AT HALF HEIGHT OF CROSS CORRELATION
# FULL WIDTH AT HALF HEIGHT OF NORMALIZED CROSS CORRELATION
# She selected FULL WIDTH AT HALF HEIGHT OF CROSS CORRELATION
# So I think we don't want to normalized version

# From previous conversations with Tim, we know tha the "width at half height.." is the same as the "half width at half height"
# (was named incorrectly as width at half height by Axion, then eventually corrected)
table1.dt[acsn_lower == 'full-width at half-height of cross-correlation', 
          acsn_lower := 'half width at half height of cross-correlation']

# Any remaining mismatches?
setdiff(table1.dt$acsn_lower, acsn.map$acsn_lower)
# empty!

# Merge the tables
acsn.map <- merge(acsn.map, table1.dt, by = c('acsn_lower'), all.x = T)
acsn.map[, kosnik_top15 := as.numeric(!is.na(Parameter))]
# Read descriptions to confirm mapping is correct
View(acsn.map[!is.na(Parameter), .(acsn, Parameter, Description..adapted.from.AxISUserGuide.1.5..a.slightly.older.version., `Description/definition`)])
# Yep, all look okay!

# One more check - I'm guessing that all in kosnik top 15 should have been registered before 2020
acsn.map[, .N, by = .(kosnik_top15, Status_2020.07.17)]
#    kosnik_top15               Status_2020.07.17  N
# 1:            0                      registered  3
# 2:            1                      registered 14
# 3:            0                  not registered 31
# 4:            1 registered under different name  1
acsn.map[kosnik_top15 == 0 & Status_2020.07.17 == 'registered', .(acsn)]
#                                         acsn
# 1:                                        AB
# 2:                                       LDH
# 3: Width at Half Height of Cross-Correlation
# the viability endpoints, and the alternate name for half width at half height of cc
acsn.map[kosnik_top15 == 1 & Status_2020.07.17 == 'registered under different name', .(acsn)]
# acsn
# 1: Number of Elecs Participating in Burst - Avg
# I guess I probably proposed that we update the name for this endpoint?


# Create table with suggested of which endpoints to release ---------------
library(tcpl)
tcplConf(drvr = 'MySQL', user = Sys.getenv('INVITRODB_USER_RO'), pass = Sys.getenv('INVITRODB_PASS_RO'), host = Sys.getenv('INVITRODB_HOST_RO'), db = 'invitrodb')
aeid.tb <- tcplLoadAeid(fld = 'asid', val = 20, add.fld = c("acid","acnm"))
aeid.tb <- aeid.tb[!grepl('_dev_',aenm)]
setdiff(aeid.tb$acnm, acsn.map$acnm) # "CCTE_Shafer_MEA_MFR" "CCTE_Shafer_MEA_LDH" "CCTE_Shafer_MEA_AB" - these are the sc endpoints
setdiff(acsn.map$acnm, aeid.tb$acnm) # "CCTE_Shafer_MEA_acute_per_network_burst_interspike_interval_cv"
acsn.map[acnm == 'CCTE_Shafer_MEA_acute_per_network_burst_interspike_interval_cv']
# I'm not sure what's going on with this endpoint
# But, I don't see it in the neural stats compiler files
# And it isn't in the 15 from Kosnik et al
# So I'm not going to worry about it

# Merge together
aeid.tb <- merge(aeid.tb, acsn.map[, unique(.SD), .SDcols = c('acnm','kosnik_top15')], by = 'acnm', all.x = T)

# Label which columns are ready to be released
aeid.tb[, can_be_released := 0]
aeid.tb[kosnik_top15 == 1, can_be_released := 1]
aeid.tb[grepl('(LDH)|(AB)',aenm), can_be_released := 1]
aeid.tb[acnm %in% c("CCTE_Shafer_MEA_MFR", "CCTE_Shafer_MEA_LDH", "CCTE_Shafer_MEA_AB"), can_be_released := 1]
aeid.tb[can_be_released == 0]

# Add Notes on mc vs sc
aeid.tb[, notes := '']
aeid.tb[acnm %in% c("CCTE_Shafer_MEA_MFR", "CCTE_Shafer_MEA_LDH", "CCTE_Shafer_MEA_AB"), notes := 'release sc only; can delete mc data']
aeid.tb[aenm %in% c('CCTE_Shafer_MEA_acute_active_electrodes_number_up','CCTE_Shafer_MEA_acute_bursting_electrodes_number_up'),
        notes := 'delete this endpoints']

# Confirm results
aeid.tb[, .N, by = .(can_be_released, notes)]
#    can_be_released                               notes  N
# 1:               1 release sc only; can delete mc data  4
# 2:               1                                     32
# 3:               0              delete these endpoints  2
# 4:               0                                     56
# 15*2 from Kosnik + 2 cytotoxicity = 32 endpoints to be released

# save the result
aeid.tb <- aeid.tb[, .(asid, acid, acnm, aeid, aenm, can_be_released, notes)]
aeid.tb
write.csv(aeid.tb, file = 'mea_acute_status_by_aenm_2022-01-27.csv', row.names = F)


# Is this different than the main15 I identified previously? --------------

main15 <- as.data.table(read.csv('../mea_acute_main15_acnm_aenm_2020-12-08.csv'))
setdiff(main15$aenm, aeid.tb[can_be_released == 1, aenm])
# empty
setdiff(aeid.tb[can_be_released == 1, aenm], main15$aenm)
# "CCTE_Shafer_MEA_AB"     "CCTE_Shafer_MEA_LDH"    "CCTE_Shafer_MEA_MFR_up" "CCTE_Shafer_MEA_MFR_dn"
# This is the 4 sc endpoints, didn't expect them to be included
length(intersect(aeid.tb[can_be_released == 1, aenm], main15$aenm)) # 32, sweet!
