getAssayData <- function(cyto_type, sourcefile) {
  
  cat(cyto_type, "\n")
  
  assay_dat_dt <- findTabData(sourcefile, cyto_type)
  
  # get the treatment, concentration, and blank-corrected values
  treatment_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = "Chemical", "treatment", cyto_type)
  conc_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = "Concentration mM", "conc", cyto_type)
  value_tagPhrase <- switch(cyto_type, "LDH" = "Corrected Optical Denisty", "AB" = "Corrected Fluorescence")
  value_dat <- valuesUnderTagPhrase(assay_dat_dt, tagPhrase = value_tagPhrase, "rval", cyto_type)
  
  # merge values based on plate, row, and column
  longdat <- merge(treatment_dat[!is.na(Row)], conc_dat[!is.na(Row)], by = c("Row","coli","plate.id"), fill = T, all = T)
  longdat <- merge(longdat, value_dat[!is.na(Row)], by = c("Row","coli","plate.id"), fill = T, all = T)
  
  # assign the acsn and acnm
  longdat[, acsn := cyto_type]
  acnm <- acsn_map[acsn == cyto_type, acnm]
  if (length(acnm) == 0) {
    warning(paste0(cyto_type," not found in acsn_map. Setting acnm to ",cyto_type," as placeholder.\n"))
    acnm <- cyto_type
  }
  longdat[, "acnm" := acnm]
  
  # check that there are 3 unique plate id
  plates <- unique(longdat$plate.id) # this is important for correct merging of trt, conc, rvals
  cat(c(plates),"\n")
  if (length(plates) != 3) stop(paste0("Something is off with plates in ",basename(sourcefile), " ",cyto_type))
  
  return(longdat)
}
