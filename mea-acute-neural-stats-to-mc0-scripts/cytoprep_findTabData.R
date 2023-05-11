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