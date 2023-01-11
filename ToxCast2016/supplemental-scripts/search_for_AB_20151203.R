# search the drive for any AB files from 20151203

start.dir <- "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox"

folders <- list.dirs(start.dir, full.names = T, recursive = F)
for (folder in folders) {
  files <- list.files(path = folder, pattern = "1086", full.names = T)
  print(files)
}

# none have the plates
# 1086-19, 1086-24, 1086-25

# character(0)
# character(0)
# character(0)
# character(0)
# [1] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151103 Culture/TC_MW1086-20_20151103_20151117_14_AB.xls"
# [2] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151103 Culture/TC_MW1086-21_20151103_20151117_14_AB.xls"
# [3] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151103 Culture/TC_MW1086-23_20151103_20151117_14_AB.xls"
# [1] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151118 Culture/TC_MW1086-26_20151118_20151201_13_AB.xls"
# [2] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151118 Culture/TC_MW1086-34_20151118_20151201_13_AB.xls"
# [3] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151118 Culture/TC_MW1086-35_20151118_20151201_13_AB.xls"
# [1] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151125 Culture/TC_MW1086-36_20151125_201511208_14_AB.xls"
# [2] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151125 Culture/TC_MW1086-37_20151125_201511208_14_AB.xls"
# [3] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151125 Culture/TC_MW1086-38_20151125_201511208_14_AB.xls"
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# [1] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20160203 Culture/MW 1086-35_20160218.xls"
# [2] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20160203 Culture/MW 1086-36_20160218.xls"
# [3] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20160203 Culture/MW 1086-37_20160218.xls"
# [4] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20160203 Culture/MW 1086-38_20160217.xls"
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# [1] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20160427 Culture/TC_MW1086-34_20160427_20160510_AB.xls"
# [2] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20160427 Culture/TC_MW1086-41_20160427_20160510_AB.xls"
# [3] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20160427 Culture/TC_MW1086-91_20160427_20160510_AB.xls"
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)
# character(0)

# searchign for something else now
folders <- list.dirs(start.dir, full.names = T, recursive = F)
for (folder in folders) {
  files <- list.files(path = folder, pattern = "1089", full.names = T)
  print(files)
}
# [1] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151202 Culture/TC_MW1089-8_20151202_20151215_13_AB.xls"

for (folder in folders) {
  files <- list.files(path = folder, pattern = "1090-[67]", full.names = T)
  print(files)
}
# [1] "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/MEA Cytotox/20151202 Culture/TC_MW1090-7_20151202_20151215_13_AB.xls"