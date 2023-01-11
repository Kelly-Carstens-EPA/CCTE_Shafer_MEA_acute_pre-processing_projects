# script to get the files for Kosnik data

# files are either structured as Burst Analysis _
# Or, subfolders with 2 choices

getToxCastFiles <- function(start.dir) {
  
  exp.dirs <- list.dirs(start.dir, recursive = F)
  
  all_files <- c()
  for (exp in exp.dirs) {
    
    # exclude the 12-2-15 folder, which does not contain a full analysis
    if (grepl("(12-2-15)", exp)) {
      print(paste0("skipping ",exp))
      next
    }
    exp.folders <- list.dirs(exp, recursive = F)
    burst.folder <- grep("[Bb]urst [Aa]nalysis", exp.folders, value = T)
    if (length(burst.folder) != 1) stop("problem")
    
    burst.subfolders <- list.dirs(burst.folder, recursive = F)
    if (length(burst.subfolders) == 0) {
      usefolder <- burst.folder
    } else {
      netburst.folder <- grep("[Nne]twork [Bb]urst", burst.subfolders, value = T)
      usefolder <- netburst.folder
    }
    add.files <- list.files(usefolder, pattern = ".csv", all.files = T, full.names = T, recursive = F)
    
    # exceptions
    # don't include this file - it does not include network spike data. (but there is another file in this folder that does)
    add.files <- add.files[basename(add.files) != "TC_MW 1048-10_20151007_20151022_15_00(000)_Neural Statistics Compiler(000).csv"]
    
    if (length(add.files) != 6) {
      stop(paste0("\nThere aren't 6 files in ",usefolder))
    }
    
    all_files <- c(all_files, add.files)
  }
  return(all_files)
}
