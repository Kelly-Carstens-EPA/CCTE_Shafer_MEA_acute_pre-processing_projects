# new cytotox data collection scripts
# this is better because it is more robust to slight changes in the placement of the data in the sheet
# which is particularly important now that I need down to row H for LDH data

# uses the list of possible tab names to get the AB or LDH data from excel sheet
findTabData <- function(sourcefile, assay = c("AB", "LDH")) {
  
  ABnames <- c("CellTiter Blue","Alamar Blue", "AB", "CTB")
  LDHnames <- c("LDH", "Total LDH")
  
  tabNames <- switch(assay, 
                     AB = ABnames,
                     LDH = LDHnames)
  
  sourcefile_tabs <- getSheetNames(sourcefile)
  tabName <- intersect(tabNames, sourcefile_tabs)
  if (length(tabName) != 1) {
    tabName <- readline(prompt = paste0("Enter name of sheet for ",tabNames[1]," in ", basename(sourcefile)," : "))
  }
  # my_data <- as.data.table(read_excel(sourcefile, sheet = tabName, range = "A1:AX55", col_names = paste0("col",1:50)))
  my_data <- as.data.table(read.xlsx(sourcefile, sheet = tabName, rows = 1:55, skipEmptyRows = FALSE, cols = 1:50, skipEmptyCols = FALSE, colNames = FALSE))
  colnames(my_data) <- paste0("col",1:ncol(my_data)) # if last col is empty, will drop even with skipEmptyCols == FALSE
  
  return(my_data)
}


valuesUnderTagPhrase <- function(dat_dt, tagPhrase, value.name, cyto_type, min.tag.phrase.count = 3) {
  
  use_plate_rows <- switch(cyto_type, "LDH" = 10, "AB" = 8)
  
  tag_instances_cols <- dat_dt[, lapply(.SD, function(x) sum(grepl(tagPhrase,x))>=min.tag.phrase.count), .SDcols = names(dat_dt)]
  dat_tag_col <- which(unlist(tag_instances_cols,use.names=F))[1] # just look at the first column with 3 instances of tagPhrase
  if(length(dat_tag_col) == 0 || is.na(dat_tag_col)) {
    stop(paste0("Tag Phrase '",tagPhrase,"' not found.\n"))
  }
  use_rows <- head(grep(tagPhrase, unlist(dat_dt[, dat_tag_col, with = F])), n = 3) # just using first 3 instances, since other calculations may have been done below
  tag_dat <- data.table()
  for (tag_rowi in use_rows) {
    add.dat <- dat_dt[(tag_rowi+3):(tag_rowi+use_plate_rows), (dat_tag_col):(dat_tag_col+8), with = F]
    setnames(add.dat, old = names(add.dat), new = as.character(dat_dt[(tag_rowi+2), (dat_tag_col):(dat_tag_col+8), with = F]))
    add.dat[, names(add.dat)[!grepl("Row",names(add.dat))] := lapply(.SD, as.character), 
            .SDcols = names(add.dat)[!grepl("Row",names(add.dat))]] # standardize measure.vars type, for melt
    add.dat <- melt(add.dat, id.vars = "Row", value.name = value.name, variable.name = "coli", variable.factor = F)
    plate.id <- as.character(dat_dt[(tag_rowi+1), (dat_tag_col+2), with = F]) # this might not be consistent/reliable, if the plate ID did not get added or copied consistently...
    add.dat[, "plate.id" := paste0("MW",plate.id)]
    if (length(plate.id) == 0) stop(paste0("plate id not found for"))
    
    # check for NA values on the current plate (except for those we excpect to be NA)
    if (value.name == "treatment") {
      test_for_na <- any(is.na(add.dat[Row %in% LETTERS[1:6] & coli %in% 1:8]))
    }
    else {
      test_for_na <- any(is.na(add.dat[(Row %in% LETTERS[1:6] & coli %in% 1:8) | (Row=="G" & coli %in% 4:5) | (Row=="H" & coli %in% 1:6)]))
    }
    if (test_for_na) {
      cat(paste0("\n Some ",cyto_type," ",value.name," on ",plate.id," are NA:\n"))
      print(dcast(add.dat[,c("Row","coli",value.name), with = F], Row ~ coli, value.var = value.name))
      check <- readline(prompt = "Do you wish to continue anyways? (y/n): ")
      if (!check %in% c("Y","y","Yes","yes")) {
        stop("cytotox data collection aborted.")
      }
    }
    
    tag_dat <- rbind(tag_dat, add.dat)
    rm(add.dat)
  }
  return(tag_dat)
}

getAssayData <- function(cyto_type, sourcefile, min.tag.phrase.count = 3) {
  
  cat(cyto_type, "\n")
  
  assay_dat_dt <- findTabData(sourcefile, cyto_type)
  
  # get the treatment, concentration, and blank-corrected values
  treatment_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = "Chemical", "treatment", cyto_type, min.tag.phrase.count = min.tag.phrase.count)
  conc_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = "Concentration mM", "conc", cyto_type, min.tag.phrase.count = min.tag.phrase.count)
  value_tagPhrase <- switch(cyto_type, "LDH" = "Corrected Optical Denisty", "AB" = "Corrected Fluorescence")
  value_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = value_tagPhrase, "rval", cyto_type, min.tag.phrase.count = min.tag.phrase.count)
  
  # merge values based on plate, row, and column
  longdat <- merge(treatment_dat[!is.na(Row)], conc_dat[!is.na(Row)], by = c("Row","coli","plate.id"), fill = T, all = T)
  longdat <- merge(longdat, value_dat[!is.na(Row)], by = c("Row","coli","plate.id"), fill = T, all = T)
  
  # assign the acnm
  acnm <- acsn_map[acsn == cyto_type, acnm]
  if (length(acnm) == 0) {
    warning(paste0(cyto_type," not found in acsn_map. Setting acnm to ",cyto_type," as placeholder.\n"))
    acnm <- cyto_type
  }
  longdat[, "acnm" := acnm]
  
  # check that there are 3 unique plate id
  plates <- unique(longdat$plate.id) # this is important for correct merging of trt, conc, rvals
  cat(c(plates),"\n")
  if (length(plates) != 3) warning(paste0("Something is off with plates in ",basename(sourcefile), " ",cyto_type))
  
  num_negative <- longdat[rval < 0, .N]
  if(num_negative > 0) {
    cat(paste0("some values are negative. These will be set to 0\n"))
    longdat[rval < 0, rval := 0.0]
  }
  
  # check for NA values, in any field
  
  return(longdat)
}


getFileCytoData <- function(sourcefile, min.tag.phrase.count = min.tag.phrase.count) {
  
  # get the experiment date from Plate 1
  # plate1_dat <- as.data.frame(read_excel(sourcefile, sheet = "Plate 1", range = "A1:J10", col_names = paste0("col",1:10)))
  plate1_dat <- as.data.table(read.xlsx(sourcefile, sheet = "Plate 1", rows = 1:10, cols = 1:10, colNames = FALSE))
  colnames(plate1_dat) <- paste0("col",1:ncol(plate1_dat))
  exp_date_index <- which(plate1_dat == "Experiment ID", arr.ind = T) # returns a 2-element vector of the row and col index
  experiment_date <- plate1_dat[exp_date_index[1], unlist(.SD), .SDcols = exp_date_index[2] + 1]
  if (!('experiment_date' %in% ls())) {
    cat(paste0("Can't find experiment date in ",sourcefile,".\nSetting experiment_date to NA"))
    experiment_date <- NA_character_
  }
  
  AB_dat <- getAssayData("AB",sourcefile, min.tag.phrase.count = min.tag.phrase.count)
  LDH_dat <- getAssayData("LDH",sourcefile, min.tag.phrase.count = min.tag.phrase.count)
  
  allfiledat <- rbind(AB_dat, LDH_dat)
  
  # clean up
  # wherever treatment is NA, use what is listed under conc (e.g for Lysis wells)
  allfiledat[is.na(treatment), treatment := conc]
  
  # Exclude the blank wells, where wells outside of the main plate and have NA rval
  allfiledat <- allfiledat[!(!(Row %in% c("A","B","C","D","E","F") & coli %in% 1:8) & is.na(rval))]
  
  # finalize the columns
  allfiledat[, experiment.date := experiment_date]
  allfiledat[, apid := experiment_date]
  allfiledat[, srcf := basename(sourcefile)]
  allfiledat[, rowi := sapply(Row, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]
  allfiledat[, coli := as.numeric(coli)]
  
  allfiledat <- allfiledat[,c("treatment","apid","experiment.date","plate.id","rowi","coli","conc","rval","srcf","acnm")]

  return(allfiledat)
}

getAllCytoData <- function(main.output.dir, dataset_title, files_log = "", min.tag.phrase.count = 3) {
  
  cat("\n\nLoad Cytotoxicity Data:\n")
  
  # only need to specifiy files_log if you want to use a specific files_log
  # instead of just the most rect calculations files log
  calc_files <- read_files(check.dir = main.output.dir, files_log = files_log, files_type = "calculations")
  
  cat("\nReading data from files...\n")
  
  cytodat <- list()
  for (i in 1:length(calc_files)) {
    cat("\n",basename(calc_files[i]),"\n",sep="")
    add.dat <- getFileCytoData(calc_files[i], min.tag.phrase.count = min.tag.phrase.count)
    cytodat <- rbind(cytodat, add.dat)
    rm(add.dat)
  }
  
  cytodat[, "files_log" := basename(files_log)]
  
  # # save the data as .RData
  # filename <- file.path(main.output.dir, paste0("output/",dataset_title,"_cytodat_",as.character.Date(Sys.Date()),".RData"))
  # save(cytodat, file = filename)
  # rm(cytodat)
  # cat("\n\nRData is saved:",filename)
  
  # check for/summarize NA values
  na_indicies <- which(is.na(cytodat), arr.ind = TRUE)
  if (length(na_indicies) == 0) {
    cat("There are no NA values in cytodat.\n")
  }
  else {
    cat("There are some NA values in cytodat:\n")
    print(cytodat[na_indicies[, "row"], .SD, .SDcols = setdiff(names(cytodat),"files_log")])
  }
  
  cat("\ncytodat is ready\n")
  return(cytodat)
}