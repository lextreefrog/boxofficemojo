# All data from boxofficmojo

addTconstToBoxOfficeDt <- function(rawBasicsDt, cleanBoxOfficeDt) {
  # We need the tconst for movies, we can get it form rawBasicsDt by selecting
  # logic for filtering:
  # its a left-join on box office data, so that I'm still counting sales of movies that I don't
  
  # 95% of rows in rawBasicsDt are TV episodes, remove them
  # and remove movies that came out before we have b.o. data
  movieBasicsDt <- rawBasicsDt %>%
    .[titleType == "movie"] %>%
    .[startYear %between% cleanBoxOfficeDt[, c(min(release_year), max(release_year))]]
  
  boxOfficeMovies <- cleanBoxOfficeDt[, .N, by = .(release, release_year)][, -3][, has_boxdata := TRUE]
  basicsMovies    <- movieBasicsDt[, N := .N, by = .(primaryTitle, startYear)] %>%
    .[, .(release = primaryTitle, 
          originalTitle, 
          runtimeMinutes,
          release_year = as.integer(startYear),
          tconst, N)]
  
  # To mitigate problem of duplicate tconsts, we prune out films that occur multiple times in the 
  # same year that don't have same original and primary titles
  # this is in effect removing foreign films, not a perfect solution.
  basicsSinglets <- basicsMovies[N == 1]
  basicsDupes    <- basicsMovies[N > 1]
  basicsResolved <- rbind(basicsSinglets,
                          basicsDupes[release == originalTitle])
  basicsResolved[, N := .N, by = .(release, release_year)]
  
  # We can also prune out movies without runtimes (affects some disney remakes like "Beauty and the Beast")
  basicsSinglets <- basicsResolved[N == 1]
  basicsDupes    <- basicsResolved[N > 1]
  basicsResolved <- rbind(basicsSinglets,
                          basicsDupes[runtimeMinutes != "\\N"])
  basicsResolved[, N := .N, by = .(release, release_year)]
  
  # Some movies (Taken, Taken 3, Paranormal Activity) had intl releases year earlier than domestic
  # So the join doesn't work because of the year. 
  # TODO: resolve this -- simulate the data with year + 1, and get the winners
  # - might want to only do this with movies with box office returns greater than X
  
  boxOfficeMerged <- merge(boxOfficeMovies, basicsResolved, by = c("release", "release_year"),
                           all.x = TRUE)
  
  # Multiple movies can have identical release_year + title
  # ex: Brothers - 2015, Wilson - 2017.
  # ONLY FOR THESE CASES: try to resolve to movies where primary==original title, else NA
  # $164B in sales to movies with tconst in output, $28B without, $5B of which from unresolved tconsts
  boxOfficeMerged[N > 1, tconst := NA]
  
  output <- merge(cleanBoxOfficeDt, boxOfficeMerged[, .(release, release_year, tconst)] %>% unique,
                  by = c("release", "release_year"),
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