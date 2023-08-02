# Update the well quality table for TSCA2019
# To inclue the cultre dates
# for the plates that were just refrenced by plate in the well quality table

# Get current wllq table
wllq.tb.by.well.file <- file.path(project.output.dir, paste0(project_name,'_well_quality_table_by_well.csv'))
wllq.tb.by.well <- as.data.table(read.csv(wllq.tb.by.well.file, colClasses = 'character'))
wllq.tb.by.well[, .N, by =.(is.na(culture.date), culture.date == '')]

# Load cytodat as a reference for the plate-date mapping
load(file.path(project_name,'output',paste0(project_name,"_cytodat.RData")))
plate.date.tb <- cytodat[, unique(.SD), .SDcols = c('culture.date','plate.id')]
plate.date.tb[is.na(culture.date) | is.na(plate.id)]
# empty

# Any plates reused in multipel cultures? (if so, then this might not work)
plate.date.tb[, .N, by = .(plate.id)][N > 1]
# empty, good

#Merge
wllq.tb.by.well <- merge(wllq.tb.by.well, plate.date.tb, all.x  = T, by = 'plate.id', suffixes = c('','.add'))
wllq.tb.by.well[is.na(culture.date) | culture.date == '', culture.date := culture.date.add]
wllq.tb.by.well[is.na(culture.date) | culture.date == '']
# empty!

# Saveupdated table
View(wllq.tb.by.well)
wllq.tb.by.well[, culture.date.add := NULL]
write.csv(wllq.tb.by.well, file = wllq.tb.by.well.file, row.names = F)
