valuesUnderTagPhrase <- function(dat_dt, tagPhrase, value.name, cyto_type) {
  
  use_plate_rows <- switch(cyto_type, "LDH" = 10, "AB" = 8)
  
  tag_instances_cols <- dat_dt[, lapply(.SD, function(x) sum(grepl(tagPhrase,x))>=3), .SDcols = names(dat_dt)]
  dat_tag_col <- which(unlist(tag_instances_cols,use.names=F))[1] # just look at the first column with 3 instances of tagPhrase
  if(length(dat_tag_col) == 0 || is.na(dat_tag_col)) {
    stop(paste0("Tag Phrase '",tagPhrase,"' not found.\n"))
  }
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
    
    # check for NA values on the current plate (except for those we excpect to be NA)
    if (value.name == "treatment") {
      test_for_na <- any(is.na(add.dat[Row %in% LETTERS[1:6] & coli %in% 1:8]))
    }
    else {
      test_for_na <- any(is.na(add.dat[(Row %in% LETTERS[1:6] & coli %in% 1:8) | (Row=="G" & coli %in% 4:5) | (Row=="H" & coli %in% 1:6)]))
    }
    if (test_for_na) {
      warning(paste0("\n Some ",cyto_type," ",value.name," on ",plate.id," are NA (may include some cells that are not needed for final cytodat)"))
    }
    
    tag_dat <- rbind(tag_dat, add.dat)
    rm(add.dat)
  }
  return(tag_dat)
}