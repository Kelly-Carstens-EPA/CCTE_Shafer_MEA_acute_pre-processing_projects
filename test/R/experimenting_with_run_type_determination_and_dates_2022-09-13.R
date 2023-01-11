filei <- 'L:/Lab/NHEERL_MEA/Project TSCA 2019/Acute TSCA Conc Response/20210428 Culture G21/Neural Statistic Compiler/AC_20210428_MW75-8205_15_00(000)(000).csv'
filei <- 'test/test_neural_stats_header_experiment_date_12hour_pm.csv'
test <- read.csv(filei, header = F, nrows = 10)
test
# V1                             V2
# 1            Investigator:                                
#   2  Recording Name: 20210428                               
# 3             Description:                                
#   4                                                         
# 5                                                         
# 6      Maestro Pro Settings                               
# 7        Original File Time                5/13/2021 13:27
# 8        Sampling Frequency                       12.5 kHz
# 9             Voltage Scale -5.48486178148311E-08 V/sample
# 10    Experiment Start Time                5/13/2021 12:33
test2 <- scan(filei, what = character(), sep = '\n', n = 10)
test2
# [1] "Investigator: ,"                                 "Recording Name: 20210428,"                      
# [3] "Description: ,"                                  ","                                              
# [5] ","                                               "Maestro Pro Settings,"                          
# [7] "   Original File Time,5/13/2021 13:27"           "   Sampling Frequency,12.5 kHz"                 
# [9] "   Voltage Scale,-5.48486178148311E-08 V/sample" "   Experiment Start Time,5/13/2021 12:33" 

# Okay, I just changed teh format of that cell from "Custom" to "Time". So that now it appears in teh cell as "1:27:00 PM'
# Let's see what happens now...
test2 <- scan(filei, what = character(), sep = '\n', n = 10)
test2
# [1] "Investigator: ,"                                 "Recording Name: 20210428,"                      
# [3] "Description: ,"                                  ","                                              
# [5] ","                                               "Maestro Pro Settings,"                          
# [7] "   Original File Time,1:27:00 PM"                "   Sampling Frequency,12.5 kHz"                 
# [9] "   Voltage Scale,-5.48486178148311E-08 V/sample" "   Experiment Start Time,5/13/2021 12:33" 
# Okay, now it is different!!

# So let's play around with this...
as.POSIXlt('1:27:00 PM', format = '%I:%M:%S %p')
# "2022-09-13 13:27:00 EDT"
# (this just inserted today's date)

ifelse(grepl('[AP]M','1:27:00 PM'), format = '%I:%M:%S %p')

# So I think if am/pm does not appear in the text extracted by R from excel, then it's reading as a 24-hour time
# If the time shwon is a 12-hour, I think I can assume that "am/pm" will also be include din the string.

# So I think I can reliably assume that excel will default to representing the dates as 24-hour
# But I can include 2 checks:
# - that if Am/Pm is present, use a format along the lines of "'%I:%M:%S %p'"
# - do somethign to compare teh filenames. If the designated run types seem to disagree with teh logical sorting of the file names, then flag those for review
# - make it clear to someone liek Kelly what they would have to change if they came across a date that was formatted differently


# GAME PLan:
# - Get the dates, using teh ifelse check for am/pm shown above. Implement in "determine_run_types"
# - figure out how to handle that fact that sometimes the seconds are included in the string, sometimes not (I don't think I'll ever need seconds, just don't want that to mess up the formatting, esp if lookign for am/pm afterwards)
# - Add check for congruence with filename
# - If there is a miscongruence, output a helpful error message to show what would need to be chagned in teh code (or make it easy to override if teh file names are misleading)
