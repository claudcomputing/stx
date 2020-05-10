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

#find the needed data - see "A - Locate Census Variables.R"
#Note - totalcensus appears to have codes crossed for race/ethnicity populations

race_popul <- read_decennial(
  year = 2010,
  states = "TX",
  table_contents = c(
    "white_popul = P0050004",
    "black_popul = P0050010",
    "hisp_popul = P0050003",
    "urban = P0020002",
    "urbanized = P0020003",
    "median_age = P0130001"
    # "median_income = PCT0500001"
  ),
  #areas = "South Bend city, IN",
  summary_level = "tract"
)
#state level test stats look good & match reference tables
#https://data.census.gov/cedsci/table?d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P5&y=2010&t=Populations%20and%20People&hidePreview=true&vintage=2010&g=0400000US48.140000
#https://data.census.gov/cedsci/table?g=0400000US48&d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P2&y=2010&t=Populations%20and%20People&hidePreview=false&vintage=2010
#GEOID  NAME      population      median_income black_popul hisp_popul  white_popul     GEOCOMP SUMLEV state STUSAB       lon     lat
#04000US48 Texas   26538614         53,207       3,070,821     10,196,367    11,635,757    total    040    TX     TX -99.28182 31.4347
save(race_popul, file = "C:/Users/Claud/Box/ddpe/stx/dta/txracepopinformationbytract.rdata")


income_popul <-
  read_acs5year(
    year = 2015,
    states = "TX",
    table_contents = c("median_income = B19013_001"), 
    summary_level = "tract")
#search_tablecontents("acs5", years = 2015, keywords = "B19013_001")
#DP03_0062E median household income dp03 table
#matches tidy census income
# B19013_001 B06011PR_001 B19013_001
save(income_popul, file = "C:/Users/Claud/Box/ddpe/stx/dta/txincomeinformationbytract.rdata")

