# investigatin wllq stuff

# Group 5 compounds left out at room temperature overnight...
g5_compounds <- alldat3[grepl("20190409",apid) & !grepl("(DMSO)|(PICRO)|(LYSIS)|(TTX)",treatment), unique(treatment)]
alldat3[treatment %in% g5_compounds, unique(apid)] # these compounds were tested in 2 experiments
# [1] "20190409_MW1237-13" "20190409_MW1237-14" "20190409_MW1237-15" "20190514_MW67-3718" "20190514_MW67-3719"
# [6] "20190514_MW68-0701"
# compare activity in ea experiment
stripchart(rval ~ conc, alldat3[grepl("20190409",apid) & grepl("firing_rate",acsn) & wllq==1], vertical=T, pch = 1, method = "jitter")
stripchart(rval ~ conc, alldat3[grepl("20190514",apid) & grepl("firing_rate",acsn) &wllq==1], vertical=T, pch = 1, method = "jitter")

g5_names <- unique(sapply(g5_compounds, function(x) substring(x,1,3)))
# exp <- "20190409"
exp <- "20190514"
cplates <- unique(alldat3[grepl(exp,apid),apid])
plot(x= c(-4:4), y = c(-100, rep(300,8)), pch = "")
for (chem in g5_names) {
  for (apidi in cplates) {
    points(alldat3[grepl(chem, treatment) & apid == apidi & grepl("firing_rate",acsn) & wllq==1, .(log(as.numeric(conc)), rval)],
           type = "l")
    # print(alldat3[grepl(chem, treatment) & apid == apidi & grepl("firing_rate",acsn) & wllq==1, .(log(as.numeric(conc)), rval)])
  }
}