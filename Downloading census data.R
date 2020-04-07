#2
# download needed census data for texas 

# packages ----------------------------------------------------------------

# install.packages("totalcensus")
library(totalcensus)

# pull data ---------------------------------------------------------------

#must set path first to save data
set_path_to_census("/Users/MVERAS/Documents/Data Driven Eval/Data/CensusData")


#find the needed data
search_tablecontents("dec", years = 2010)
search_tablecontents("dec", years = 2010, keywords = "median age")

search_tablecontents("acs5", years = 2010, keywords = "income")

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

income_popul <-
  read_acs5year(
    year = 2015,
    states = "TX",
    table_contents = c(median_income = "B22008_001"),
    summary_level = "tract")


save(race_popul, file = "txracepopinformationbytract.rdata")
save(income_popul, file = "txincomeinformationbytract.rdata")



# save(tx, file = "/Volumes/mbv221/Test Area/tx.rdata")
