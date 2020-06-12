# new cytotox data collection scripts
# this is better because it is more robust to slight changes in the placement of the data in the sheet
# which is particularly important now that I need down to row H for LDH data

# function to  look up index of value in a data frame
#  there probably is a more efficient, data table way to do this...
returnindex = function(value, mydata) {
  for (i in 1:nrow(mydata)) {
    for (j in 1:ncol(mydata)) {
      if (is.na(mydata[i,j])) {
        next
      }
      if (as.character(mydata[i,j]) == as.character(value)) {
        return(c(i,j))
      }
    }
  }
  # print("could not find index in data frame")
  return(NULL)
}


# uses the list of possible tab names to get the AB or LDH data from excel sheet
findTabData <- function(sourcefile, assay = c("AB", "LDH")) {
  
  ABnames <- c("CellTiter Blue","Alamar Blue", "AB", "CTB")
  LDHnames <- c("LDH", "Total LDH")
  
  tabNames <- switch(assay, 
                     AB = ABnames,
                     LDH = LDHnames)
  
  sourcefile_tabs <- excel_sheets(sourcefile)
  tabName <- intersect(tabNames, sourcefile_tabs)
  if (length(tabName) != 1) {
    tabName <- readline(prompt = paste0("Enter name of sheet for ",tabNames[1]," in ", basename(sourcefile)," : "))
  }
  my_data <- as.data.table(read_excel(sourcefile, sheet = tabName, col_names = FALSE))
  return(my_data)
}


valuesUnderTagPhrase <- function(dat_dt, tagPhrase, value.name, cyto_type) {
  
  use_plate_rows <- switch(cyto_type, "LDH" = 10, "AB" = 8)
  
  tag_instances_cols <- dat_dt[, lapply(.SD, function(x) sum(grepl(tagPhrase,x))>=3), .SDcols = names(dat_dt)]
  dat_tag_col <- which(unlist(tag_instances_cols,use.names=F))[1] # just look at the first column with 3 instances of tagPhrase
  use_rows <- grep(tagPhrase, unlist(dat_dt[, dat_tag_col, with = F]))[1:3] # just using first 3 instances, since other calculations may have been done below
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
    
    # quick check for NA values on the current plate, in the main plate wells
    if (any(is.na(add.dat[Row %in% c("A","B","C","D","E","F") & coli %in% 1:8]))) {
      cat(paste0("\n Some ",cyto_type," ",value.name," on ",plate.id,"are NA\n"))
      print(dcast(add.dat[,.(Row,coli,rval)], Row ~ coli, value.var = "rval"))
      check <- readline(prompt = "Do you wish to continue anyways? (y/n): ")
      if (!check %in% c("Y","y","Yes","yes")) {
        stop()
      }
    }
    tag_dat <- rbind(tag_dat, add.dat)
    rm(add.dat)
  }
  return(tag_dat)
}

getAssayData <- function(cyto_type, sourcefile) {
  
  cat("\n",cyto_type, "")
  
  assay_dat_dt <- findTabData(sourcefile, cyto_type)
  
  # get the treatment, concentration, and blank-corrected values
  treatment_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = "Chemical", "treatment", cyto_type)
  conc_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = "Concentration mM", "conc", cyto_type)
  value_tagPhrase <- switch(cyto_type, "LDH" = "Corrected Optical Denisty", "AB" = "Corrected Fluorescence")
  value_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = value_tagPhrase, "rval", cyto_type)
  
  # merge values based on plate, row, and column
  longdat <- merge(treatment_dat[!is.na(Row)], conc_dat[!is.na(Row)], by = c("Row","coli","plate.id"), fill = T, all = T)
  longdat <- merge(longdat, value_dat[!is.na(Row)], by = c("Row","coli","plate.id"), fill = T, all = T)
  
  # assign the acsn
  acsn <- switch(cyto_type, "LDH" = "NHEERL_MEA_acute_LDH", "AB" = "NHEERL_MEA_acute_AB")
  longdat[, "acsn" := acsn]
  
  # check that there are 3 unique plate id
  plates <- unique(longdat$plate.id) # this is important for correct merging of trt, conc, rvals
  cat(plates)
  if (length(plates) != 3) stop(paste0("Something is off with plates in ",basename(sourcefile), " ",cyto_type))
  
  if(cyto_type == "AB") {
    num_negative <- longdat[rval < 0, .N]
    if(num_negative > 0) cat("\nsome values are negative. These will be set to 0")
    longdat[rval < 0, rval := 0.0]
  }
  
  return(longdat)
}


getFileCytoData <- function(sourcefile) {
  
  # get the experiment date from Plate 1
  plate1_dat <- as.data.frame(read_excel(sourcefile, sheet = "Plate 1", col_names = F))
  exp_date_index <- returnindex("Experiment ID", plate1_dat)
  experiment_date <- plate1_dat[exp_date_index[1], (exp_date_index[2] + 1)]
  if (experiment_date == "") stop(paste0("Can't find experiment date for ",sourcefile))
  
  AB_dat <- getAssayData("AB",sourcefile)
  LDH_dat <- getAssayData("LDH",sourcefile)
  
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
  
  allfiledat <- allfiledat[,c("treatment","apid","experiment.date","plate.id","rowi","coli","conc","rval","srcf","acsn")]

  return(allfiledat)
}

getAllCytoData <- function(main.output.dir, dataset_title, files_log = "") {
  
  # only need to specifiy files_log if you want to use a specific files_log
  # instead of just the most rect calculations files log
  calc_files <- read_files(check.dir = main.output.dir, files_log = files_log, files_type = "calculations")
  
  cat("\nReading data from files...\n")
  
  cytodat <- list()
  for (i in 1:length(calc_files)) {
    cat("\n",basename(calc_files[i]),sep="")
    add.dat <- getFileCytoData(calc_files[i])
    cytodat <- rbind(cytodat, add.dat)
    rm(add.dat)
  }
  
  cytodat[, "files_log" := basename(files_log)]
  
  # # save the data as .RData
  # filename <- file.path(main.output.dir, paste0("output/",dataset_title,"_cytodat_",as.character.Date(Sys.Date()),".RData"))
  # save(cytodat, file = filename)
  # rm(cytodat)
  # cat("\n\nRData is saved:",filename)
  
  cat("\ncytodat is ready")
  return(cytodat)
}