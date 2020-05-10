# Merge census data with point data
# make sure all of your packages are up to date



# set working directory -------------
#setwd("/Users/MVERAS/Documents/Data Driven Eval/Data/")
setwd("C:/Users/Claud/Box/ddpe/stx/dta")

# load packages -----------------------------------------------------------

library(tigris)
library(sf)
library(magrittr)
library(tidycensus)
library(dplyr)
library(rlang)
options(tigris_use_cache = TRUE)
library(data.table)
library(tableone)
library(stringr)
library(quanteda)

# load data ---------------------------------------------------------------

load("Texas1MSampleClean.Rdata")
load("txracepopinformationbytract.rdata")
load("txincomeinformationbytract.rdata")

# reduce data file  -------------------------------------------------------

tx_latlon <- tx[,c("raw_row_number", "lat", "lng")]
remove(tx)

# get census tract for point data! ------------------------------------------------------------------

tidycensus::census_api_key("a065c441353b16389007cc799ea246324b6827cd", install = T)
tx <- tidycensus::get_acs(state = "TX", geography = "tract",
                          variables = "B19013_001", geometry = TRUE)
# Now to sim some data:

bbox <- st_bbox(tx)

my_points <- data.frame(
  x = tx_latlon$lng,
  y = tx_latlon$lat
) %>%
  # convert the points to same CRS
  st_as_sf(coords = c("x", "y"),
           crs = st_crs(tx))


#this takes about 10 mins on my computer!
my_points$tract <- as.numeric(st_within(my_points, tx))
# tract is the row-index for overlapping census tract record in 'tx'

# save(my_points, file = "LatLongCensusRow.Rdata") # save points with the census tract infromation
# load("LatLongCensusRow.Rdata")
tx_latlon$tractrow <- my_points$tract #only works if sets are in same order!

# save(tx, file = "txtracts.Rdata")
# load("txtracts.Rdata")

tx$tractrow <- rownames(tx)
tx <- as.data.frame(tx)
tx$NAME <- NULL
tx$geometry <- NULL


# merge all census tract data ---------------------------------------------

#first, clean census data

income_tract <- data.frame( GEOID =substr(income_popul$GEOID, 8, 18),
                            income_acs_2015 = income_popul$median_income)

race_tract <- data.frame(population_dec_2010 = race_popul$population,
                         hisp_pop_dec_2010 = race_popul$hisp_popul,
                         black_pop_dec_2010 = race_popul$black_popul,
                         white_pop_dec_2010 = race_popul$white_popul,
                         urban_pop_dec_2010 = race_popul$urban,
                         median_age_dec_2010 = race_popul$median_age,
                         GEOID =substr(race_popul$GEOID, 8, 18)) 

# merge income, race, and tractrow information 
fullcensusdata <- merge(income_tract, race_tract, by = "GEOID")
fullcensusdata <- merge(fullcensusdata, tx, by = "GEOID")
# fullcensusdata$median_hh_income_acs_2017 <- fullcensusdata$estimate
# fullcensusdata$median_hh_income_acs_2017_moe <- fullcensusdata$moe
# income estimate is from acs 2018 that is why it doesn't match the income_acs_2015 estimate ALL IS WELL

fullcensusdata$estimate <- NULL
fullcensusdata$moe <- NULL
fullcensusdata$variable <- NULL

fullcensusdata <- fullcensusdata %>% mutate(prop_black = black_pop_dec_2010 / population_dec_2010,
                                            prop_hisp = hisp_pop_dec_2010/population_dec_2010,
                                            prop_white = white_pop_dec_2010/population_dec_2010,
                                            prop_urban = urban_pop_dec_2010/population_dec_2010)

# merge to get row numbers
tx_with_tract <- merge(fullcensusdata, tx_latlon, all.y = T, by = "tractrow")

# bring back og file 
load("Texas1MSampleClean.Rdata")
#merge!
tx_merged <- merge(tx, tx_with_tract, by = "raw_row_number")

#save !!!! WOOO
save(tx_merged, file = "Texas1MSampleClean_Merged.Rdata")
