# experimenting with reading from JS' file, for 15 minutes
# 
# library(readxl)
# library(data.table)
get_ToxCast_cytodat <- function() {
  
  setwd("L:/Lab/NHEERL_MEA/Carpenter_Amy/pre-process_mea_acute_for_tcpl/ToxCast2016")
  sourcefile <- "copied_source_files/Conc_Response_Log_JS.xlsx"
  sheets <- excel_sheets(sourcefile) # this took a while
  sheets <- sheets[grepl("Experiment Date",sheets)]
  
  # remove this culture that should not be included
  sheets <- setdiff(sheets, c("Experiment Date 6-7-16"))
  
  alldat <- data.table()
  
  for (s in 1:length(sheets)) {
    sheet <- sheets[s]
    cat(sheet,"\n")
    
    pagei <- read_excel(sourcefile, sheet = sheet)
    
    experiment_date <- sub("Experiment Date ","",sheet)
    experiment_date <- format(as.Date(experiment_date, format = "%m-%d-%y"), "%Y%m%d")
    
    # get background values
    LDH_row <- grep("LDH", unlist(pagei[, 1], use.names=F))
    CTB_row <- grep("(CellTiter Blue)|(Cell Titer Blue)", unlist(pagei[, 1], use.names=F))
    if(length(LDH_row) == 0 | length(CTB_row) == 0) stop(paste0("LDH or CTB row not found for ",sheet))
    first_assay <- ifelse(LDH_row < CTB_row, "LDH","CTB")
    assay_assignment <- c(rep(first_assay, 3), rep(setdiff(c("LDH","CTB"),first_assay),3))
    background_correction_rows <- grep("Background Correction",unlist(pagei[, 11], use.names = F))
    
    page_dat <- data.table()
    for (i in 1:length(background_correction_rows)) {
      cyto_type <- assay_assignment[i]
      file_row <- background_correction_rows[i]
      plate.id <- sub(" ","",as.character(pagei[file_row, 1]))
      # find the next occurence of "A" below the Background Correction
      repeat {
        file_row <- file_row + 1
        test_char <- as.character(pagei[file_row,11])
        if (!is.na(test_char) && test_char == "A") break
      }
      dati <- as.data.table(pagei[file_row:(file_row+5),c(11:19)])
      setnames(dati, old = names(dati), new = c("Row",1:8))
      dati <- melt(dati, id.vars = "Row", vavariable.factor = T, variable.name = "coli", value.name = "rval")
      if(any(is.na(dati$rval))) cat("\nThere are some NAs",file_row)
      dati[, "plate.id" := plate.id]
      acsn <- switch(cyto_type, "LDH" = "NHEERL_MEA_acute_LDH", "CTB" = "NHEERL_MEA_acute_AB")
      dati[, "acsn" := acsn]
      
      page_dat <- rbind(page_dat, dati)
      rm(dati)
    }
    page_dat[, experiment.date := experiment_date]
    page_dat[, apid := experiment_date]
    
    alldat <- rbind(alldat, page_dat)
    rm(page_dat)
  }
  
  # set negative blank-corrected values to 0
  alldat[rval < 0, rval := 0.0]
  
  # add rowi, files_log
  alldat[, rowi := sapply(Row, function(x) utf8ToInt(x) - utf8ToInt("A") + 1)]

  
  return(alldat)
}
