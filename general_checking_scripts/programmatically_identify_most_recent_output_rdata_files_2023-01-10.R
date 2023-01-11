# ------------------------------------------------------------------------- #
# Programmatically determine which files I can .gitignore
# Jan 20, 2023
# ------------------------------------------------------------------------- #

library(data.table)
library(stringi)
dat.files <- list.files(pattern = '_dat.*\\.RData', full.names = T, recursive = T)
dat.files

files.tb <- data.table(fullname = dat.files)
files.tb[, basename := basename(dat.files)]
files.tb[, name_sans_date := stri_extract(basename, regex = '^.*_dat[0-9]+')]
files.tb

files.tb[, file_date := stri_extract()]

files.tb[, is_max := basename == max(basename), by = .(name_sans_date)]
View(files.tb)

# Note which files to git ignore
files.tb[, should_gitignore_file := !is_max]
files.tb[grepl('sbox',basename), should_gitignore_file := TRUE]

# cat out files to ignore so can save to .gitignore
files.tb[, fullnames_sans_dot := stri_replace(fullname, fixed = './', replacement = '')]
files.tb[should_gitignore_file == TRUE, cat(fullnames_sans_dot, sep = '\n')]
