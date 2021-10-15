library(data.table)
library(httr)
library(rvest)
library(foreach)
library(iterators)

# Web scraping functions - boxofficemojo
makeTableFromUrl <- function(pageUrl) {
  
  pageHtml <- read_html(pageUrl)
  
  # Columns are grouped in these two calls or called individually
  # These get returned as one interleaved vector, explaining the modulo-weirdness below
  intFields <- html_nodes(pageHtml, css = ".mojo-field-type-positive_integer") %>% html_text
  estFields <- html_nodes(pageHtml, css = ".mojo-estimatable")                 %>% html_text()
  return(
    data.table(
      date        = html_node(pageHtml, css = "h4") %>% html_text(),
      rank        = html_nodes(pageHtml, css = ".mojo-sort-column") %>% html_text() %>% .[-1],
      rank_lw     = intFields[(seq(1, length(intFields)) %% 3) == 1][-1], 
      release     = html_nodes(pageHtml, css = ".mojo-cell-wide") %>% html_text() %>% .[-1],
      gross       = estFields[(seq(1, length(estFields)) %% 6) == 1][-1],
      gross_twlw  = estFields[(seq(1, length(estFields)) %% 6) == 2][-1],
      theaters    = intFields[(seq(1, length(intFields)) %% 3) == 2][-1], # appears in both groups
      change      = estFields[(seq(1, length(estFields)) %% 6) == 4][-1],
      average     = estFields[(seq(1, length(estFields)) %% 6) == 5][-1],
      total_gross = estFields[(seq(1, length(estFields)) %% 6) == 0][-1],
      weeks       = intFields[(seq(1, length(intFields)) %% 3) == 0][-1],
      distributor = html_nodes(pageHtml, css = ".mojo-field-type-release_studios") %>% html_text() %>% .[-1]
    )
  )
}

generateValidUrls <- function(market = "domestic", snapshot = "weekend",
                              start_year = 1982, end_year = 2021) {
  # Outputs all URLs as a table
  # market + snapshot vars not implemented
  allUrlTable   <- data.table(
    expand.grid(year = start_year : end_year,
                week = 1 : 53)
  )
  allUrlTable[, full_url := paste0("https://boxofficemojo.com/weekend/",
                                   year, "W", formatC(week, width = 2, flag = "0"))]
  # Next lines to brute force which years have a week 53 + find missing weeks
  # Missing weeks only known to exist pre-1982. Future weeks for current year gives empty tables.
  allUrlTable[, should_check := fifelse(week > 50 | year < 1982, TRUE, FALSE)]
  allUrlTable[should_check==TRUE, is_error := sapply(full_url, FUN = function(x) http_error(x))]
  validUrls <- allUrlTable[should_check == FALSE | is_error == FALSE]
  return(validUrls)
}

saveYearTable <- function(year) {
  urlTable <- generateValidUrls(start_year = year, end_year = year)
  allWeekends <- foreach(url = iter(urlTable, by = "row")) %do% {
    print(url$week)
    thisWeekend <- makeTableFromUrl(url$full_url)
    print(nrow(thisWeekend))
    thisWeekend
  } %>% rbindlist
  allWeekends[, year := year]
  fwrite(allWeekends, paste0("domestic_weekend_", year, ".csv"))
}

saveAllYears <- function(start_year = 1982, end_year = 2021) {
  for (i in start_year:end_year) {
    print(paste0("Doing year: ", year));
    tryCatch({
      saveYearTable(i)
    },
    error = function(e) {print("oops!")})
  }
}

cleanBoxOfficeDt <- function(bdt) {
  # clean date
  bdt[, week_start_date := paste(gsub("-.*", "", date), year) %>% as.Date(format = "%B %d %Y")]
  # make number columns better
  number_cols <- c("rank", "rank_lw", "gross", "gross_twlw", "theaters", 
                   "change", "average", "total_gross", "weeks")
  bdt[, (number_cols) := lapply(.SD, FUN = function(x) as.numeric(gsub("[\\$,%]", "", x))),
      .SDcols = number_cols]
  # calculated vars
  bdt[, weekly_total := sum(gross), by = date]
  bdt[, share_of_wallet := gross / weekly_total]
  return(bdt)
}


if (FALSE) {
  setwd("~/my_code/boxofficemojo/")
  source("boxofficemojo.R")
  # saveAllYears()
  setwd("../data/yearly_files")
  megaDt <- list.files() %>% lapply(fread) %>% rbindlist %>% cleanBoxOfficeDt()
  exDt <- list.files()[10] %>% fread %>% cleanBoxOfficeDt
  exDt[, weekly_total := sum(gross), by = date]
  exDt[, share_of_wallet := gross / weekly_total]
  topMovies <- exDt[, .(top1 = sum(gross, na.rm = TRUE),
                        top2 = max(total_gross, na.rm = TRUE)),
                    by = release]
}
