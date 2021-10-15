# All data from boxofficmojo

addTconstToBoxOfficeDt <- function(rawBasicsDt, cleanBoxOfficeDt) {
  # We need the tconst for movies, we can get it form rawBasicsDt by selecting
  # logic for filtering:
  # its a left-join on box office data, so that I'm still counting sales of movies that I don't
  
  # 95% of rows in rawBasicsDt are TV episodes, remove them
  # and remove movies that came out before we have b.o. data
  movieBasicsDt <- rawBasicsDt %>%
    .[titleType == "movie"] %>%
    .[startYear %between% cleanBoxOfficeDt[, c(min(year), max(year))]]
  
  boxOfficeMovies <- cleanBoxOfficeDt[, .N, by = .(release, year)][, -3][, has_boxdata := TRUE]
  basicsMovies    <- movieBasicsDt[, N := .N, by = .(primaryTitle, startYear)] %>%
    .[, .(release = primaryTitle, year = as.integer(startYear), tconst, N)]
  boxOfficeMerged <- merge(boxOfficeMovies, basicsMovies, by = c("release", "year"),
                           all.x = TRUE)
  
  # Multiple movies can come out in the same year with the same title
  # ex: Brothers - 2015, Wilson 2017.
  # Need a rule to resolve these, but for now treat them like we don't know their tconst
  # $164B in sales to movies with tconst in output, $28B without, $5B of which from unresolved tconsts
  boxOfficeMerged[N > 1, tconst := NA]
  
  output <- merge(cleanBoxOfficeDt, boxOfficeMerged[, .(release, year, tconst)] %>% unique,
                  by = c("release", "year"),
                  all.x = TRUE)
  return(output)
  
}

createCrewDt <- function(rawCrewData, cleanTitleBasics) {
  # WIP
}

createDirectorFilmDt <- function()

library(data.table)
if (FALSE) {
  source("~/my_code/boxofficemojo/boxofficemojo.R")
  setwd("~/my_code/data")
  cleanBoxOfficeDt <- fread("domestic_weekend.csv") %>% cleanBoxOfficeDt()
  rawBasicsDt <- fread("title_basics_movie.csv")
}