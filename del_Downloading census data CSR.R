# download needed census data for texas 
#
# packages ----------------------------------------------------------------

# install.packages("totalcensus")
library(totalcensus)

# pull data ---------------------------------------------------------------

#must set path first to save data
#set_path_to_census("/Users/MVERAS/Documents/Data Driven Eval/Data/CensusData")
set_path_to_census("C:/Users/csr315")
#C:/Users/csr315/Box Sync/ddpe/stx/dta Box Sync/dta/census
race_popul <- read_decennial(
  year = 2010,
  states = "TX",
  table_contents = c("black_popul = P0030003", "hisp_popul = P0040003", "white_popul = P0030002", "urban = P0020002", "median_age = P0130001"),
  #areas = "South Bend city, IN",
  summary_level = "tract"
)

