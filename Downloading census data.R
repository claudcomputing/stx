#2
# download needed census data for texas 

# packages ----------------------------------------------------------------

# install.packages("totalcensus")
library(totalcensus)

# pull data ---------------------------------------------------------------

#must set path first to save data
#set_path_to_census("/Users/MVERAS/Documents/Data Driven Eval/Data/CensusData")
set_path_to_census("C:/Users/Claud/Box/Data/CensusData")
setwd('C:/Users/Claud/Box/stx')

#find the needed data
#search_tablecontents("dec", years = 2010)
#search_tablecontents("dec", years = 2010, keywords = "median age")
#search_tablecontents("acs5", years = 2010, keywords = "income")



race_popul <- read_decennial(
  year = 2010,
  states = "TX",
  table_contents = c(
    "black_popul = P0030003",
    "hisp_popul = P0040003",
    "white_popul = P0030002",
    "urban = P0020002",
    "urbanized = P0020003",
    "median_age = P0130001" #,
    # "median_income = PCT0500001"
  ),
  #areas = "South Bend city, IN",
  summary_level = "tract"
)

save(race_popul, file = "txracepopinformationbytract.rdata")

income_popul <-
  read_acs5year(
    year = 2015,
    states = "TX",
    table_contents = c("median_income = B19013_001"), 
    summary_level = "tract")

search_tablecontents("acs5", years = 2015, keywords = "B19013_001")
search_tablecontents("acs5", years = 2015, keywords = "median")
#DP03_0062E median household income dp03 table
# B19013_001 B06011PR_001 B19013_001
save(income_popul, file = "C:/Users/Claud/Box/ddpe/stx/dta/txincomeinformationbytract.rdata")

library(tidycensus)

tx_hh_income <- get_acs(
  geography = "tract",
  year = 2015,
  state = "TX",
  survey = "acs5",
  variables = "B19013_001",
  key = Sys.getenv("CENSUS_API_KEY")
) 
# save(tx, file = "/Volumes/mbv221/Test Area/tx.rdata")
income_popul<-tx_hh_income
income_popul$median_income<-tx_hh_income$estimate
save(income_popul, file = "C:/Users/Claud/Box/ddpe/stx/dta/txincomeinformationbytract.rdata")
