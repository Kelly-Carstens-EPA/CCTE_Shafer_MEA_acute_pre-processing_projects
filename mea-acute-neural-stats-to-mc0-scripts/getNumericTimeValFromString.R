#' Get a numeric value corresponding to a string column
#'
#' @param dat1 
#' @param time_string_col 
#' @param new_cols_suffix 
#' @description Function to calculate apply as.POSIXlt to a time component of a character string containing the time in one of the following 4 formats:
#' * Decimal hours (24 hour):Decimal minute:Decimal second
#' * Decimal hours (24 hour):Decimal minute
#' * Decimal hours (12 hour):Decimal minute:Decimal second AM/PM
#' * Decimal hours (12 hour):Decimal minute AM/PM
#' (24-hour time is assumed if AM/PM not specified).
#' If the time is hour is 1 and AM/PM is not specified, function will throw an error and prompt use to clarify AM/PM or convert to 24-hour time.
#' 
#' Notes on development choices for this function:
#' * This function is useful because sorting based on the unconverted string column may produce undesireable results when there are inconsistencies in formatting (e.g. "05/13/2021" versus "5/13/2021").
#' * For simplicity, only the time component of the string column is utilized (not the date). Since all baseline-treated recordings would happen on the same date, the date part of the experiment_start_time is irrelevant for determining which file is treated and which is baseline.
#'
#' @return dat1
#' @export
#'
#' @examples
getNumericTimeValFromString <- function(dat1, time_string_col, new_cols_suffix) {
  
  org.ncols <- ncol(dat1)
  
  # Determine which files include seconds
  dat1[, time_includes_sec := grepl('[0-9]+\\:[0-9]{2}\\:[0-9]{2}',get(time_string_col))]
  
  # Determine if am/pm is given
  dat1[, meridiem_given := grepl('[AP]M',get(time_string_col))]
  
  # Get a character string of just the time (not the date)
  dat1[, time_char := stri_extract(get(time_string_col), regex = '[0-9\\:]+( [AP]M)*$')]
  
  # Get the hour so that can check if any files start at "1:__" but AM/PM not specified
  # (in this case, if I assume military time, and the baseline file was at 12:__, then the sorting would get thrown off
  # In all other times, it doesn't matter if the time is truly military or 12-hour, 
  # because baseline and treated will be sorted correctly regardless as long as we aren't crossing the we aren't cross the 12pm-1pm / 12:00-13:00 line)
  dat1[, hour := as.integer(stri_extract(time_char, regex = '^[0-9]{1,2}'))]
  stopifnot(nrow(dat1[is.na(hour)]) == 0)
  if (nrow(dat1[hour %in% 1 & meridiem_given == FALSE]) > 0) {
    stop('Hour is \'1\' but am/pm not specified.\nAdd am/pm or convert to military time to ensure sorting with baseline/treated files is correct.')
  }
  
  # Define format for every type of experiment start time
  dat1[meridiem_given == FALSE & time_includes_sec == TRUE, time_format := '%H:%M:%OS'] # format = Decimal hours (24 hour):Decimal minute:Decimal second
  dat1[meridiem_given == FALSE & time_includes_sec == FALSE, time_format := '%H:%M']
  dat1[meridiem_given == TRUE & time_includes_sec == TRUE, time_format := '%I:%M:%OS %p'] # format = Decimal hours (12 hour):Decimal minute:Decimal second AM/PM
  dat1[meridiem_given == TRUE & time_includes_sec == FALSE, time_format := '%I:%M %p']
  
  # Get sortable time for all (as numeric)
  dat1[, time_posix_num := as.numeric(as.POSIXlt(time_char, format = time_format))] 
  stopifnot(nrow(dat1[is.na(time_posix_num)]) == 0)
  
  # Add suffix to added column names
  added.cols <- names(dat1)[(org.ncols+1):ncol(dat1)]
  setnames(dat1, old = added.cols, new = paste0(added.cols,'_',new_cols_suffix))
  
  # message & return
  cat('The following columns have been added to dat1:\n',
      paste0(added.cols,'_',new_cols_suffix, collapse =", "), '\n')
  return(dat1)
}