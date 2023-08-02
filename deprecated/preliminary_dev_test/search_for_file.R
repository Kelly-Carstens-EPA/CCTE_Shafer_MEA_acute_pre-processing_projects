# searchign for the file 'ToxCast MEA data Outliers and data check\\.docx' on the L drive

# if a dir was last modified before 5/31/2019, no need to check.
# it is rather unlikely that the file would be in a folder that was created after 8/16/2019... I think.
# let's get a survey of the ctime of all folders on the MEA drive
check_cdir <- function(cdir) {
  
  # if(basename(cdir) == "MAESTRO SYSTEM") return("done")
  
  print(basename(cdir))
  # check if there are any files in this dir
  thefile <- list.files(cdir, pattern = fname, include.dirs = F)
  if (length(thefile) > 0) {
    print("found it!!!!!!!!!!!!!!!!!!!!!!")
    print(cdir)
    print(thefile)
    stop("we can stop now")
  }
  
  # get all dirs in current layer
  dirs <- list.dirs(path = cdir,  full.names = T, recursive = F)
  
  # remove dirs last modified before 05/31/2016
  dirs <- dirs[file.mtime(dirs) > as.POSIXct(as.Date("20160531","%Y%m%d"))]
  
  # remove dirs created after 20190816
  if (length(dirs) > 0) {
    dirs <- dirs[sapply(dirs, function(d) file.info(d)$ctime < as.POSIXct(as.Date("20190816","%Y%m%d")))]
  }
  
  # print(length(dirs))
  
  # recurse!
  if (length(dirs) > 0) {
    sapply(dirs, function(x) check_cdir(x))
  }
  
  return(paste0("done"))
}

# # testing
# start.dir <- "C:/Users/ACARPE01/OneDrive - Environmental Protection Agency (EPA)/Profile/Documents"
# fname <- "5.19.doc1\\.txt"
# check_cdir(start.dir)

fname <- "ToxCast MEA data Outliers and data check\\.docx"
start.dir <- "L:/Lab/NHEERL_MEA"
Sys.time()
check_cdir(start.dir)
Sys.time()

# OUTPUT:
# ... (probs started around 7ish?)
# 1] "TxCstAsy_updated_tjs"
# [1] "01-MeanFiringRate"
# [1] "02-NeuralNeworkOntogeny"
# [1] "03-Apoptosis"
# [1] "04-NeuriteOutgrowth"
# [1] "05-Synaptogenesis"
# [1] "extra"
# [1] "done"
# > Sys.time()
# [1] "2020-06-13 19:36:18 EDT"

fname <- "ToxCast MEA data Outliers and data check\\.docx"
start.dir <- "L:/Lab/NCCT_ToxCast"
Sys.time()
check_cdir(start.dir)
Sys.time()
# search seeeeveral files. No result

start.dir <- "L:/Lab/Toxcast_Data"
Sys.time()
check_cdir(start.dir)
Sys.time()
# No luck here either

start.dir <- "L:/Lab/NHEERL_Mundy"
Sys.time()
check_cdir(start.dir)
Sys.time()
# nope

# I could now look in folder that were created after that date...

