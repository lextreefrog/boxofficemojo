# Utility functions


# Date functions
getDecade <- function(years) {
  decadeNames = list("8" = "1980s",
                     "9" = "1990s",
                     "0" = "2000s",
                     "1" = "2010s")
  return(
    sapply(years, FUN = function(x) decadeNames[[substr(x, 3, 3)]])
  )
}
