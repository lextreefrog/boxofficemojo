cat('\14')
rm(list = ls(all.names = TRUE)); gc()
setwd("~/my_code/boxofficemojo/")

source("utility_Functions.R")
source("02_data_transform_functions.R")
source("boxofficemojo.R")

setwd("~/my_code/data/")

keyedBoxOfficeDt <- fread("domestic_weekend_keyed.csv")
rawBasicsDt      <- fread("title_basics_movie.csv")
rawCrewData      <- fread("crew_data.csv") 
nameDt           <- fread("name_data.csv")
