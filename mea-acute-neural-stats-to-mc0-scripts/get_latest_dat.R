# get most recent dat4 (or other) from all folders, or a specific folder

get_latest_dat <- function(lvl = "dat4", dataset_titles = NULL) {
  
  if (!(lvl %in% c(paste0("dat",1:4),"mc0"))) stop(paste0("'lvl' must be in ",paste0(c(paste0("dat",1:4),"mc0"),collapse=", ")))
  if (is.null(dataset_titles)) {
    # # kinda janky - all folders with no underscores and a "20" will be used
    # dirs <- list.dirs(full.names = F, recursive = F)
    # dirs <- dirs[grepl(pattern = "20",dirs) & !grepl("_",dirs)]
    # Actually, since some datasets are not designed to be added to TCPL,
    # I'm going to just manually list the ones that are tcpl-ready
    dirs <- c('APCRA2019','DNT2019','GF2019','ToxCast2016')
    cat(paste0("Getting data from folders ",paste0(dirs,collapse=", ")),"\n")
  }
  else {
    dirs <- dataset_titles
  }
  
  dat <- data.table()
  RData_files_used <- c()
  for (diri in dirs) {
    dat_files <- list.files(path = file.path(diri,"output"), pattern = paste0("_",lvl), full.names = T)
    dat_file <- dat_files[order(basename(dat_files), decreasing = T)[1]] # get the most recent file based on file name
    RData_files_used <- c(RData_files_used, dat_file)
    cat(basename(dat_file),"\n")
    dat_name <- load(dat_file, verbose = F)
    dati <- get(dat_name) 
    dati[, origin := diri] # is there a way to do this with an apply function?
    dat <- rbind(dat, dati, fill = T)
    rm(list = c(dat_name, "dati"))
  }
  assign("RData_files_used",RData_files_used,envir = parent.frame())
  
  return(dat)

}

# other ideas:
# something so it is easy to check the baseline values for a given plate/well
# or even better, for a given treatment/plate/spid (would need to get trt/conc from higher lvls, but vals from dat1)
# some trickiness with treatment/treatment_original/other updates at lvl 4
