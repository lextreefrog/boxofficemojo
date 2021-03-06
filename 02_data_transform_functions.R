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
  
  boxOfficeMerged <- merge(boxOfficeMovies, basicsResolved, by = c("release", "release_year"),
                           all.x = TRUE)
  # Some movies (Taken, Taken 3, Paranormal Activity) had intl releases year earlier than domestic
  # So the join doesn't work because of the year. 
  # - might want to only do this with movies with box office returns greater than X
  mergedNoTconst <- boxOfficeMerged[is.na(tconst)]
  mergedNoTconst[, potential_release_year := release_year - 1]
  backdatedReleased <- merge(mergedNoTconst, 
                             basicsResolved[, .(release, release_year, backdated_tconst = tconst)],
                             by.x = c("release", "potential_release_year"),
                             by.y = c("release", "release_year"),
                             all.x = TRUE, all.y = FALSE)
  backdatedReleased[, tconst := fcoalesce(tconst, backdated_tconst)] %>%
    .[, ':='(backdated_tconst = NULL,
             potential_release_year = NULL)]
  
  boxOfficeMerged <- rbind(boxOfficeMerged[!is.na(tconst)],
                           backdatedReleased)
  
  # Some movies we just can't join, will have to use the URL from scraping boxoffice records
  # To get the ID because string doesn't cut it
  boxOfficeMerged[N > 1, tconst := NA]
  
  # Final missing breakdown
  # has tconst:                            $186B
  # can't ID tconst because of duplicates: $3B
  # Re-releases and weird strings:         $1.3B
  # (think Star Wars IV 1998 special edition., Curse of La Llorona)
  # Few that could have been released 2 years ago (Paranormal Activity, ex.), but negligible amt
  
  output <- merge(cleanBoxOfficeDt, boxOfficeMerged[, .(release, release_year, tconst)] %>% unique,
                  by = c("release", "release_year"),
                  all.x = TRUE)
  return(output)
  
}

addNconstToKeyedDt <- function(rawCrewData, keyedDt) {
  return(merge(rawCrewData, keyedDt, by = "tconst"))
}

filterNamesDt <- function(keyedBoxOfficeDt, crewNamesDt) {
  # makes the file a lot smaller by only containing relevant people (i.e. no actors, TV directors)
  # get list of all relevant nm########'s from boxOfficeDt
  uniqueDirectors <- keyedBoxOfficeDt[, .(director = unique(directors))] %>% 
    .[, tstrsplit(director, ",")]
  directorVector <- foreach(i = seq(1, ncol(uniqueDirectors))) %do% {
    uniqueDirectors[, ..i] 
  } %>% unlist %>% c %>% unique
  filteredCrewNames <- crewNamesDt[nconst %in% directorVector]
  return(
    filteredCrewNames
  )
}

getDirectorData <- function(director = "Steven Spielberg", 
                            keyedBoxOfficeDt,
                            nameDt, sort_factor_by = "release") {

  directorNconsts <- nameDt[primaryName == director, unique(nconst)]
  if (length(directorNconsts) > 1) {
    # Any name ambiguity resolved by defaulting to person with highest
    # box office sales - this only matters for George Miller though
    searchPattern <- paste(directorNconsts, collapse = "|")
    relevantDts <- keyedBoxOfficeDt[grepl(pattern = searchPattern, x = directors)] %>%
      .[, .(career_gross = sum(gross, na.rm = TRUE)), by = .(directors)]
    decisionTable <- data.table(candidates = directorNconsts)
    getMyGross <- function(nconst) {relevantDts[grepl(pattern = nconst, x = directors), sum(career_gross, na.rm = TRUE)]}
    decisionTable[, career_gross := getMyGross(candidates), by = candidates]
    directorNconst <- decisionTable[which.max(career_gross), candidates]
  } else {
    directorNconst <- directorNconsts
  }
  filteredData <- keyedBoxOfficeDt[grepl(pattern = directorNconst, x = directors)]
  filteredData[, director_name := director]
  filteredData[, opening_week  := min(week_start_date), by = release] 
  
  # make factor levels for release
  # if (sort_factor_by == "release date") {
  #   levels <- filteredData[order(opening_week), release] %>% unique
  # } else {
  #   levels <- filteredData[order(-total_gross), release] %>% unique
  # }
  # 
  # filteredData[, release := factor(release, levels = levels)]
  
  return(filteredData)
}

getDirectorTotals <- function(directorDt) {
  return(
    directorDt %>%
      .[order(opening_week), .(domestic_total = max(total_gross, na.rm = TRUE)),
        by = .(director_name, release, release_year, opening_week)] %>%
      .[, ':='(share_of_total = domestic_total / sum(domestic_total),
               total_label    = paste0("$", round(domestic_total / 1000000, digits = 3), "M"),
               release_f      = factor(release, levels = release))] %>% 
      .[, cum_share := cumsum(share_of_total)] %>%
      .[order(domestic_total), ':='(barsplot_order = (.N :1),
                                    barsplot_label = paste0((.N : 1), ". ", release,
                                                            " (", release_year, ")"))]
    
  )
}

plotTotalsBar <- function(totalsDt, activeReleases = NULL) {
  # totalsDt comes in ordered in the order we want
  plotDt <- totalsDt %>%
    .[, active_a := ifelse(release %in% activeReleases, 1, .47)]
  thePlot <- plotDt %>%
    ggplot(aes(x = director_name, y = -share_of_total, fill = release_f, alpha = active_a, color = "#808080")) + 
    geom_bar(stat = "identity") + 
    coord_flip() +
    scale_color_manual(values = "#808080") + 
    theme_minimal() + 
    theme(axis.title = element_blank(), 
          axis.text = element_blank(),
          plot.background = element_blank(),panel.grid = element_blank(),
          legend.position = "none")
  return(
    thePlot
  )
}

plotTotalBars <- function(totalsDt, activeReleases) {
  plotDt <- totalsDt[order(domestic_total)][, x := seq_len(.N)]
  plotDt[, barsplot_label_f := factor(barsplot_label, levels = barsplot_label)]
  thePlot <- plotDt %>%
    ggplot(aes(x = barsplot_label_f, y = domestic_total, fill = release_f)) + 
    geom_bar(stat = "identity") + 
    scale_x_discrete(labels = ifelse(plotDt$release %in% activeReleases, plotDt$barsplot_label, "")) + 
    # xlim(c(0, pmax(plotDt[, .N], 10))) + 
    coord_flip() +
    ggtitle("Box office totals") + 
    scale_color_manual(values = "#808080") + 
    theme_minimal() + 
    theme(axis.title = element_blank(), 
          axis.text.x = element_blank(),
          plot.background = element_blank(),panel.grid = element_blank(),
          legend.position = "none")
  return(
    thePlot
  )
}

plotDirectorDataWalletShare <- function(directorDt) {
  plotDt <- directorDt %>%
    .[, decade := getDecade(release_year)] %>% # save this for the tool tip in plotly
    .[, opening_week_label := format(as.Date(opening_week), "%b-%d")]
  thePlot <- plotDt %>%
    ggplot(aes(x = weeks, y = share_of_wallet, color = release)) + 
    geom_line() + 
    # geom_label_repel(aes(x = 1, label = opening_week_label), hjust = 1) + 
    xlab("Number of Weeks since Box Office Debut") + 
    ylab("Share of Weekend Wallet") + 
    theme(legend.position = "none")
  thePlot
}

if (FALSE) {
  source("~/my_code/boxofficemojo/boxofficemojo.R")
  setwd("~/my_code/data")
  cleanBoxOfficeDt <- fread("domestic_weekend.csv") %>% cleanBoxOfficeDt()
  rawBasicsDt      <- fread("title_basics_movie.csv")
  rawCrewData      <- fread("crew_data.csv") 
  nameDt           <- fread("name_data.csv")
  
  keyedBoxOfficeDt <- rawBasicsDt %>%
    addTconstToBoxOfficeDt(cleanBoxOfficeDt = cleanBoxOfficeDt) %>%
    addNconstToKeyedDt(rawCrewData = rawCrewData)
  fwrite(keyedBoxOfficeDt, "domestic_weekend_keyed.csv")
}
