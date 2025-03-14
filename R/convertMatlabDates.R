convertMatlabDates <- function(date_string) {
  month_abbreviations <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  month_no <- c("01", "02", "03", "04", "05", "06", 
                "07", "08", "09", "10", "11", "12")
  for (i in seq_along(month_abbreviations)) {
    date_string <- gsub(month_abbreviations[i], month_no[i], date_string)
  }
  return(date_string)
}