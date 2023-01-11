dat3 <- get_latest_dat(lvl = "dat3", dataset_title)
# ToxCast2016_dat3_2020-06-19.RData 
dat4 <- get_latest_dat(lvl = "dat4", dataset_title)
# ToxCast2016_dat4_2020-06-21.RData 
spid_map <- merge(dat3[, .(plate.id, experiment.date, rowi, coli, treatment, acsn)], dat4[, .(plate.id, experiment.date, rowi, coli, treatment, spid, acsn)], by = c("plate.id", "experiment.date","rowi","coli","acsn"), suffixes = c(".dat3",".dat4"))
spid_map[, .(org_treatment = unique(treatment.dat3)), by = .(treatment.dat4, spid)]
# treatment.dat4         spid org_treatment
# 1:           DMSO         DMSO          DMSO
# 2:       TX002474 TP0001414F03      TX002474
# 3:       TX006145 TP0001414F04      TX006145
# 4:       TX007998 TP0001414F05      TX007998
# 5:            BIC  Bicuculline           BIC
# ---                                          
#   383:       TX015415 TP0001413E05      TX015415
# 384:       TX015526 TP0001413E06      TX015526
# 385:       TX010369 TP0001413E08      TX010369
# 386:       TX003365 TP0001413E09      TX003365
# 387:       TX000926 TP0001413E10      TX000926
# okay, so it's not a simple TX to TP replacemnt.
# But, I clearly do have a map with teh old dat3 and dat4 here
# but if feel precarious

# I found a map on the L drive! Let's check that it's the same
spidmap_file <- "L:/Lab/NHEERL_MEA/MAESTRO SYSTEM/ToxCast Compounds/Phase I and II Con Response/EPA_11024_TShafer_384ph2_75ul_13May2015.xlsx" # to map TX treatments from flatfile to TP
use_sheet <- 1 # sheet name in spidmap_file
spidmap <- as.data.table(read_excel(spidmap_file, sheet = use_sheet))
names(spidmap)
setnames(spidmap, old = "EPA_SAMPLE_ID", new = "spid")
setnames(spidmap, old = "pd_sample_id", new = "treatment")
setdiff(unique(dat4$treatment), unique(spidmap$treatment))
# [1] "DMSO"  "BIC"   "LYSIS"
dat3 <- merge(x = dat3, y = spidmap[, c("spid", "treatment")], all.x = TRUE, by = "treatment")

# assign spids for the non-registered control compounds, e.g.: "Tritonx100" "Bicuculline"  "DMSO" "PICRO" "TTX" "MEDIA"
dat3[is.na(spid),unique(treatment)]
# [1] "BIC"   "DMSO"  "LYSIS"
dat3[grepl("DMSO",treatment), spid := "DMSO"]
dat3[treatment == "BIC", spid := "Bicuculline"]
dat3[treatment == "LYSIS", spid := "Tritonx100"]
unique(dat3$spid) # confirm no NA spids
if(any(is.na(unique(dat3$spid)))) {
  stop(paste0("The following treatments don't have a corresponding spid:", dat3[is.na(spid), unique(treatment)]))
} else {
  cat("No spids are NA.\n")
}
cat("Number of unique spids:",dat3[,length(unique(spid))],"\n")

# NEXT:
# - confirm that this new spid map file maps the same as before
# - confirm that conc-corrections were done by checking every compound whose stock conc is not 20, do those conc's look like they were conc-corrected?

# check that dat3's spids match up with dat4's
nrow(dat4) == nrow(dat3[!is.na(acsn)]) # TRUE
all.equal(dat3[!is.na(acsn), .(plate.id, experiment.date, rowi, coli, treatment, spid, acsn)], dat4[, .(plate.id, experiment.date, rowi, coli, treatment, spid, acsn)], check.attributes = F, ignore.row.order = T)
# TRUE
# check, the spid's appear to mapped the same

# check out conc-correction
spidmap[, unique(ALIQUOT_SOLVENT)] # all DMSO
spidmap[ALIQUOT_CONC != 20] # 49 instances
spidmap[ALIQUOT_CONC_UNIT != "mM"] # 1 case of this!!
check.spids <- spidmap[ALIQUOT_CONC != 20, unique(spid)]
compare_concs <- merge(dat4[spid %in% check.spids, .(paste0(sort(unique(signif(conc,3))),collapse=",")), by = "spid"], 
      spidmap[, .(ALIQUOT_CONC, guess_concs = paste0(signif(ALIQUOT_CONC/20*c(0.03,0.1,0.3,1,3,10,40),3),collapse=",")), by = "spid"])
compare_concs[V1 != guess_concs]
# spid                                                  V1 ALIQUOT_CONC                              guess_concs
# 1: TP0001411B01            0.0274,0.0915,0.275,0.915,2.75,9.15,36.6         18.3 0.0274,0.0915,0.274,0.915,2.74,9.15,36.6
# 2: TP0001411B06              0.0314,0.104,0.314,1.04,3.14,10.4,41.8         20.9   0.0313,0.104,0.313,1.04,3.14,10.4,41.8
# 3: TP0001411D01            0.0298,0.0995,0.299,0.995,2.99,9.95,39.8         19.9 0.0298,0.0995,0.298,0.995,2.98,9.95,39.8
# 4: TP0001411D07                0.0308,0.102,0.308,1.02,3.08,10.2,41         20.5     0.0307,0.102,0.307,1.02,3.08,10.2,41
# 5: TP0001411D11                     0.0302,0.1,0.302,1,3.02,10,40.2         20.1     0.0302,0.101,0.302,1.01,3.02,10,40.2
# 6: TP0001411E07                              0.03,0.1,0.3,1,3,10,40         10.0             0.015,0.05,0.15,0.5,1.5,5,20
# 7: TP0001411F01            0.0184,0.0615,0.185,0.615,1.85,6.15,24.6         12.3 0.0184,0.0615,0.184,0.615,1.84,6.15,24.6
# 8: TP0001412B01              0.0304,0.102,0.305,1.01,3.05,10.2,40.6         20.3   0.0304,0.102,0.305,1.02,3.05,10.2,40.6
# 9: TP0001412E07            0.0286,0.0955,0.286,0.955,2.86,9.55,38.2         19.1 0.0286,0.0955,0.287,0.955,2.86,9.55,38.2
# 10: TP0001412H03                     0.0302,0.1,0.302,1,3.02,10,40.2         20.1     0.0302,0.101,0.302,1.01,3.02,10,40.2
# 11: TP0001413D04 0.015,0.03,0.05,0.1,0.15,0.3,0.5,1,1.5,3,5,10,20,40         10.0             0.015,0.05,0.15,0.5,1.5,5,20
# 12: TP0001414A12            0.0298,0.0995,0.299,0.995,2.99,9.95,39.8         19.9 0.0298,0.0995,0.298,0.995,2.98,9.95,39.8

# the only real concern:
# 6: TP0001411E07                              0.03,0.1,0.3,1,3,10,40         10.0             0.015,0.05,0.15,0.5,1.5,5,20
# 11: TP0001413D04 0.015,0.03,0.05,0.1,0.15,0.3,0.5,1,1.5,3,5,10,20,40         10.0             0.015,0.05,0.15,0.5,1.5,5,20
# I just checked the flat file, and it looks like this was just missed - Ohter compounds where stock conc is not 20, Kathleen added a formulat to conc-correct
# It looks like this one was just missed.