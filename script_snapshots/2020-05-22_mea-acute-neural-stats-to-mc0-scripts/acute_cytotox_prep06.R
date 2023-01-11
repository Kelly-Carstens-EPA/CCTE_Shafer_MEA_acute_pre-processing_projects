# This script will extract the percent of control values from the calculations files
# Script to process cytotoxicity data to prepare for tcpl
# Output is a long file with all the necessary columns for tcpl (except no spid, just treatment column)
# This script will extract the cytotoxicity data (LDH and Alamar Blue) from the input files
# Example input files:
# - "ON_20160720_MW1139-19_Summary.xlsx" - these contain data for 1 plate per sheet (LDH and Alamar Blue)
# - "20171011_ON G8_2 Calculations.xlsx" - these contain data for 3 plates per sheet (LDH and Alamar Blue)

# Notes:
# this script sets any negative percent of control values to zero XXX not any more - since that depends...
# The culture date (from file name) and plate.id are saved. But the plate.id will be created once merged with neural stats
# data, so that the experiment date can be used
# OR, get exp id from Plate 1!

###################################################################################
# THINGS THAT CAN BE TWEAKED
###################################################################################
# Do your individual input excel sheets contain data for one plates or three plates per sheet?
sheetdata = "three" # set to "one" or "three"
###################################################################################
# END THINGS
###################################################################################

###################### FUNCTIONS

# function to  look up index of value in a data frame
#  there probably is a more efficient, data table way to do this...
returnindex = function(value, mydata) {
  for (i in 1:nrow(mydata)) {
    for (j in 1:ncol(mydata)) {
      if (is.na(mydata[i,j])) {
        next
      }
      if (strcmp(as.character(mydata[i,j]),as.character(value))) {
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
  
  # get the source data, trying all common names
  i <- 1
  repeat {
    if (i > length(tabNames)) {
      # print(paste0("Could not find sheet for AB data in ", basename(sourcefile)))
      tabName <- readline(prompt = paste0("Enter name of tab in ", basename(sourcefile)," for Alamar Blue data: "))
    } 
    else {
      tabName <- tabNames[i]
    }
    
    my_data <-  tryCatch({
      # The code you want run
      as.data.frame(read_excel(sourcefile, sheet = tabName, col_names = FALSE))
    }, error = function(err) {
      # Is executed if error encountered
      NULL
    })
    
    if (!is.null(my_data)) {
      break
    }
    i <- i+1
  }
  return(my_data)
}


# use this function if the data from each plate comes from a separate file or sheet
createCytoTable = function(sourcefile,firstround) {
  
  AB_data <- findTabData(sourcefile, "AB")
  LDH_data <- findTabData(sourcefile, "LDH")
  
  # get source file name
  filenamesplit = strsplit(sourcefile, split = "/")
  srcname = tail(filenamesplit[[1]], n = 1)
  
  # look for plate name in file name (anything with -)
  namesplit = strsplit(srcname, split ="_")
  plate.id = namesplit[[1]][grep(pattern="-",namesplit[[1]])]
  if (!grepl("MW", plate.id)) plate.id <- paste0("MW",plate.id)
  
  # # get the date from file name:
  # culture_date = namesplit[[1]][grep(pattern="[0-9]{8}",namesplit[[1]])] # looks for 8-digit string in namesplit
  
  if (isempty(AB_data)) {
    print("AB data not found")
  } else {
    createCytoData(AB_data, "AB", firstround, plate.id, srcname)
  }
  if(isempty(LDH_data)) {
    print("LDH data not found")
  } else {
    createCytoData(LDH_data, "LDH", 0, plate.id, srcname)
  }
  
}


# use this function when data for 3 plates are on same sheet
createCytoTable2 = function(sourcefile) {
  
  # get source file name
  srcname = basename(sourcefile)
  
  # get the culture date from filename
  namesplit = strsplit(srcname, split ="_")
  # culture_date = namesplit[[1]][grep(pattern="[0-9]{8}",namesplit[[1]])] # looks for 8-digit string in namesplit
  
  # get the experiment date from Plate 1
  plate1_dat <- as.data.frame(read_excel(sourcefile, sheet = "Plate 1", col_names = F))
  exp_date_index <- returnindex("Experiment ID", plate1_dat)
  experiment_date <- plate1_dat[exp_date_index[1], (exp_date_index[2] + 1)]
  if (experiment_date == "") stop(paste0("Can't find experiment date for ",sourcefile))
  
  AB_data_all = findTabData(sourcefile, "AB")
  LDH_data_all = findTabData(sourcefile, "LDH")
  
  # split the data wherever there is a new occurrence of the word "Chemical" in the first column
  # Only want the first 3 - because these should correspond to the first 3 plates
  # Any occurences of "chemical" after that are probably other calculations
  AB_plate_indicies = which(AB_data_all[,1] == "Chemical")[1:3]
  LDH_plate_indicies = which(LDH_data_all[,1] == "Chemical")[1:3]
  
  allfiledat <- list()
  
  for (i in 1:length(AB_plate_indicies)) {
    
    # dealing with sometimes vertically offset Concentrations values
    AB_startrow <- max(1, (AB_plate_indicies[i]-2))
    LDH_startrow <- max(1, (LDH_plate_indicies[i]-2))
    
    # get the plate slice of the data
    AB_data = AB_data_all[AB_startrow:(AB_plate_indicies[i]+9),]
    LDH_data = LDH_data_all[LDH_startrow:(LDH_plate_indicies[i]+9),]
    
    # get plate.id. Should be same for AB and LDH
    plateindex = returnindex("Plate", AB_data)
    ABplate.id = paste("MW",AB_data[plateindex[1],(plateindex[2]+1)], sep = "")
    plateindex = returnindex("Plate", LDH_data)
    LDHplate.id = paste("MW",LDH_data[plateindex[1],(plateindex[2]+1)], sep = "")
    
    if (isempty(AB_data)) {
      print("AB data not found")
    } else {
      AB_longdat <- createCytoData(AB_data, "AB", ABplate.id, srcname, experiment_date)
    }
    if(isempty(LDH_data)) {
      print("LDH data not found")
    } else {
      LDH_longdat <- createCytoData(LDH_data, "LDH", LDHplate.id, srcname, experiment_date)
    }
    
    allfiledat <- rbind(allfiledat, AB_longdat, LDH_longdat)
  }
  return(allfiledat)
}

createCytoData = function(sourcedata, cyto_type, plate.id = NULL, srcname = "", experiment_date) {
  
  # cat(experiment_date,"_",plate.id,cyto_type,"\n")
  cat("\n",plate.id, " ",cyto_type,sep = "")
  
  # compound map
  compoundindex = returnindex("Chemical",sourcedata)
  chemrow = compoundindex[1] + 3
  chemcol = compoundindex[2] + 1
  compoundmap = sourcedata[chemrow:(chemrow+5),chemcol:(chemcol+7)]
  
  # concetrations
  concindex = returnindex("Concentration mM",sourcedata)
  concrow = concindex[1] + 3
  conccol = concindex[2] + 1
  concmap = sourcedata[concrow:(concrow+5),conccol:(conccol+7)]
  
  # Get desired values
  # if one-plate version, expected rval header is "Percent of Control"
  if (sheetdata == "one") {
    valueindex = returnindex("Percent of Control",sourcedata)
    # note that data start only 2 rows below, instead of 3
    valuerow = valueindex[1] + 2
    valuecol = valueindex[2] + 1
  }
  if (sheetdata == "three") {
    tagPhrases = c("Percent of Control","Percent of Control Dead")
    valueindex = NULL
    i = 1
    while(is.null(valueindex) ) {
      if (i > length(tagPhrases)) {
        stop(paste("no Percent of Control data found for",plate.id,cyto_type))
      }
      valueindex = returnindex(tagPhrases[i], sourcedata)
      i = i+1
    }
    # assuming that data starts 3 rows below
    valuerow = valueindex[1] + 3
    valuecol = valueindex[2] + 1
    
  }
  valuemap = sourcedata[valuerow:(valuerow+5),valuecol:(valuecol+7)]
  if (any(is.na(valuemap))) {
    cat("\nvaluemap contains NAs\n")
    print(valuemap)
    check <- readline(prompt = "Do you wish to continue anyways? (y/n): ")
    if (!check %in% c("Y","y","Yes","yes")) {
      stop()
    }
  }
  
  # if any Alamar Blue percent of control values are negative, then we should set these to zero
  if (cyto_type == "AB" && any(valuemap < 0, na.rm = T)) {
    valuemap[valuemap < 0] = 0.0
    cat(" some blank-corrected values are negative. Setting these to zero")
  }
  
  # now create a data frame with all the desired values
  
  compounds = c()
  concentrations = c()
  values = c()
  rowi = c()
  coli = c()
  for (i in 1:nrow(compoundmap)) {
    for (j in 1:ncol(compoundmap)) {
      # compounds = c(compounds, as.character(compoundmap[i,j]))
      # concentrations = c(concentrations, as.character(concmap[i,j]))
      compounds = c(compounds, compoundmap[i,j])
      concentrations = c(concentrations, concmap[i,j])
      values = c(values, as.numeric(valuemap[i,j]))
      rowi = c(rowi, i)
      coli = c(coli, j)
    }
  }
  
  # create data frame
  sourcedata = data.frame("treatment" = compounds, "rowi" = rowi, "coli" = coli, "conc" = concentrations, "rval" = values, stringsAsFactors = FALSE)
  
  if (isempty(plate.id) | length(plate.id)>1) {
    print("Plate cannot be determined from file name")
    plate.id = readline("Enter plate sn: ")
  }
  
  # determine correct assay component source name
  if (cyto_type == "LDH") {
    sourcedata$acsn = "NHEERL_MEA_acute_LDH"
  } else if (cyto_type == "AB" | cyto_type == "CTB") {
    sourcedata$acsn = "NHEERL_MEA_acute_AB"
  } else {
    stop("invalid cyto_type used")
  }
  
  sourcedata$srcf = srcname
  sourcedata$plate.id = plate.id
  sourcedata$experiment_date <- experiment_date
  sourcedata$apid <- paste0(experiment_date,"_",plate.id)
  sourcedata$coli <- as.character(sourcedata$coli)
  
  # reorder columns
  sourcedata = sourcedata[,c("treatment","apid","rowi","coli","conc","rval","srcf","acsn")]
  
  return(sourcedata)
  
}

getCytoData <- function(main.output.dir, dataset_title, files_log = "") {
  
  # only need to specifiy files_log if you want to use a specific files_log
  # instead of just the most rect calculations files log
  calc_files <- read_files(check.dir = main.output.dir, log_file = files_log, files_type = "calculations")
  
  cat("\nReading data from files...\n")
  
  cytodat <- list()
  for (i in 1:length(calc_files)) {
    cat("\n",basename(calc_files[i]),sep="")
    add.dat <- createCytoTable2(calc_files[i])
    # add.dat <- tryCatch(createCytoTable2(calc_files[i]),
    #                     error = function(e) {
    #                       warning(e)
    #                       return(list())
    #                     })
    cytodat <- rbind(cytodat, add.dat)
    rm(add.dat)
  }
  setDT(cytodat)
  
  # # save the data as .RData
  # filename <- file.path(main.output.dir, paste0("output/",dataset_title,"_cytodat_",as.character.Date(Sys.Date()),".RData"))
  # save(cytodat, file = filename)
  # rm(cytodat)
  # cat("\n\nRData is saved:",filename)
  
  # return cytodat
  return(cytodat)
}

# # debuggin
# sourcefile <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA/20190515 Culture DNT G1 G2/DNT Acute Group 1/20190515_Calculations_DNT Group_1-DONE.xlsx"
# sourcefile <- "L:/Lab/NHEERL_MEA/Project - DNT 2019/Project DNT 2019 Acute MEA/20190612 Culture  DNT G7 G8/Group 7/20190612_Calculations_DNT Group_7_resave_testing_encoding.xlsx"
# res <- read.xlsx(sourcefile, sheetName = "LDH", stringsAsFactors = F, header = F)
# res2 <- read.xlsx2(sourcefile, sheetName = "CellTiter Blue", stringsAsFactors = F, header = F) # this worked! but any formulas are just NA
# res <- read.xlsx(sourcefile, sheetIndex = 7, startRow = 1, stringsAsFactors = F, header = F) # still no work
# res2 <- read.xlsx2(sourcefile, sheetName = "CellTiter Blue", stringsAsFactors = F, header = F)
# # can't read anything from ehre...
# # > read.xlsx(sourcefile, sheetIndex = 1)
# # Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl,  :
# #                   org.apache.poi.POIXMLException: java.lang.reflect.InvocationTargetException
# library(readxl)
# ?read_excel
# read_xlsx(sourcefile, sheet = "LDH", col_names = F)
# guess_encod
# library(readr)
# guess_encoding(sourcefile)
# sourcefile <- calc_files[1]
# library(xlsx)
# 
# options(java.parameters = "-Xmx8g") # xlsx also uses java
# 
# # Replace file and sheetName with appropriate values for your file
# # keepFormulas=FALSE and header=TRUE are the defaults. I added them only for illustration.
# raw = read.xlsx(sourcefile, sheetName="CellTiter Blue", header=TRUE, keepFormulas=FALSE) # still get the following error
# Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl,  : 
#                   java.lang.IllegalStateException: Cannot get a numeric value from a text cell 

# I can read in with read.xlsx2, but the formulas all evaluate to NA