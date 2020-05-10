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
search_tablecontents("acs5", years = 2010, keywords = "B03002_006")
search_tablecontents("acs5", years = 2010, keywords = "HISPANIC OR LATINO ORIGIN BY RACE")


#search_tablecontents("dec", years = 2010, keywords = "P0130001")
#search_tablecontents("dec", years = 2010, keywords = "origin")
search_tablecontents("dec", years = 2010, keywords = "P0030003") #originally black
search_tablecontents("dec", years = 2010, keywords = "P0040003") #originally hisp
search_tablecontents("dec", years = 2010, keywords = "P0030002") #originally white

#"black_popul = P0030003",
#"hisp_popul = P0040003",
#"white_popul = P0030002",
#GEOID  NAME acs_NAME population        black_popul   hisp_popul    white_popul GEOCOMP SUMLEV state STUSAB       lon     lat
#04000US48 Texas    Texas   25145561    17,701,552    2,979,598     9,460,921   total    040    TX     TX -99.28182 31.4347

#"allrace = P0050001",
#"black_popul = P0050004",
#"hisp_popul = P0050010",
#"white_popul = P0050003",
#GEOID  NAME acs_NAME       population    allrace     black_popul     hisp_popul    white_popul    urban urbanized median_age GEOCOMP SUMLEV state STUSAB       lon     lat
#04000US48 Texas    Texas   25,145,561    25,145,561    11,397,345    2,886,825     9,460,921        21,298,039  18,947,957       33.6   total    040    TX     TX -99.28182 31.4347

#https://www.census.gov/prod/cen2010/briefs/c2010br-02.pdf
#says Texas total population 2010 census is 25,145,561, non-hisp white is 11,397,345, minority is 13,748,216 WTFFF
#"Racial classification issues continue to persist among those who identify as Hispanic, resulting in a substantial 
#proportion of that population being categorized as Some Other Race."
#https://www.census.gov/quickfacts/TX
#https://demographics.texas.gov/Resources/Presentations/OSD/2012/2012_09_27_Hispanic_Leadership_Summit.pdf

search_tablecontents("dec", years = 2010, keywords = "white alone")
search_tablecontents("dec", years = 2010, keywords = "HISPANIC OR LATINO AND NOT HISPANIC OR LATINO BY RACE")
search_tablecontents("dec", years = 2010, keywords = "B03002_001")
#	B03002_001	Total:	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population	
#	2	B03002_002	Not Hispanic or Latino:	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population

#	3	B03002_003	White alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	4	B03002_004	Black or African American alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	5	B03002_005	American Indian and Alaska Native alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	6	B03002_006	Asian alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	7	B03002_007	Native Hawaiian and Other Pacific Islander alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	8	B03002_008	Some other race alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	9	B03002_009	Two or more races:	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	10	B03002_010	Two races including Some other race	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	11	B03002_011	Two races excluding Some other race, and three or more races	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population

#	12	B03002_012	Hispanic or Latino:	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	13	B03002_013	White alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	14	B03002_014	Black or African American alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	15	B03002_015	American Indian and Alaska Native alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	16	B03002_016	Asian alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	17	B03002_017	Native Hawaiian and Other Pacific Islander alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	18	B03002_018	Some other race alone	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	19	B03002_019	Two or more races:	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	20	B03002_020	Two races including Some other race	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population
#	21	B03002_021	Two races excluding Some other race, and three or more races	HISPANIC OR LATINO ORIGIN BY RACE	unknown	Universe: Total Population

state_test <- read_decennial(
  year = 2010,
  states = "TX",
  table_contents = c(
    "black_popul = B03002_004",
    "hisp_popul = B03002_012",
    "white_popul = B03002_003"
  ),
  summary_level = "state"
)
#> search_tablecontents("dec", years = 2010, keywords = "B03002_001")
#Be aware that the same reference may point to different table content in census 2000 and 2010.

#ERROR !!!!!
#Error in read_decennial_geoheaders_(year, states, table_contents, geo_headers,  : 
#The table content reference B03002_004 does not exist.

print(state_test)
#2010 P0090005 ----- White alone HISPANIC OR LATINO AND NOT HISPANIC OR LATINO BY RACE Total population
#2010 P0090006 ----- Black or African American alone HISPANIC OR LATINO AND NOT HISPANIC OR LATINO BY RACE Total population
#2010 P0090002 - Hispanic or Latino HISPANIC OR LATINO AND NOT HISPANIC OR LATINO BY RACE Total population

#GEOID  NAME acs_NAME             population    black_popul   hisp_popul    white_popul   GEOCOMP     SUMLEV   state STUSAB       lon     lat
#04000US48 Texas    Texas         25,145,561    17,701,552    2,979,598     9,460,921     total    040    TX     TX -99.28182 31.4347
#GEOID  NAME acs_NAME           population      black_popul  hisp_popul    white_popul     GEOCOMP SUMLEV state STUSAB       lon     lat
#04000US48 Texas    Texas         25145561      9460921       11397345      2886825       total    040    TX     TX -99.28182 31.4347

#OK so these references are wrong. let's find the correct ones.
#https://data.census.gov/cedsci/table?g=0400000US48&d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P12I&y=2010&t=Populations%20and%20People&hidePreview=false&vintage=2010 
#11,397,345 Should be white alone, not hispanic
#https://data.census.gov/cedsci/table?g=0400000US48&d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P12H&y=2010&t=Populations%20and%20People&hidePreview=false&vintage=2010
#9,460,921 Should be total hispanic
#https://data.census.gov/cedsci/table?g=0400000US48&d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P12B&y=2010&t=Populations%20and%20People&hidePreview=false&vintage=2010
#2,979,598 Is black but might include hispanics
#see output above - none of the vars tested provide hisp counts above 3M, and black popul is pointing to what should be the hisp population
#https://data.census.gov/cedsci/table?g=0400000US48&d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P1&y=2010&t=Populations%20and%20People&hidePreview=false&vintage=2010
#25,145,561  is total population
#https://data.census.gov/cedsci/table?g=0400000US48&d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P2&y=2010&t=Populations%20and%20People&hidePreview=false&vintage=2010
#Urban 21,298,039
#Urbanized 18,947,957
#RACE TABLE!!! https://data.census.gov/cedsci/table?g=0400000US48&d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P3&y=2010&t=Populations%20and%20People&hidePreview=false&vintage=2010
#ETHNICITY TABLE!!! https://data.census.gov/cedsci/table?g=0400000US48&d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P4&y=2010&t=Populations%20and%20People&hidePreview=false&vintage=2010
#YAAAAAASSSSS https://data.census.gov/cedsci/table?d=DEC%20Summary%20File%201&tid=DECENNIALSF12010.P5&y=2010&t=Populations%20and%20People&hidePreview=true&vintage=2010&g=0400000US48.140000

#From the data table downloaded from census
#(Inserted a zero to make it work in total census!!)
#P005003
#Total!!Not Hispanic or Latino!!White alone	
#P005004
#Total!!Not Hispanic or Latino!!Black or African American alone
#P005010
#Total!!Hispanic or Latino
#P005003	Total!!Not Hispanic or Latino!!White alone	11,397,345
#P005004	Total!!Not Hispanic or Latino!!Black or African American alone	2,886,825
#P005010	Total!!Hispanic or Latino	9,460,921


#THESE RESULTS ARE CLEARLY SCRAMBLED!

#> state_test <- read_decennial(
#  +   year = 2010,
#  +   states = "TX",
#  +   table_contents = c(
#    +     "black_popul = P0050004",
#    +     "hisp_popul = P0050010",
#    +     "white_popul = P0050003"
#    +   ),
#  +   summary_level = "state"
#  + )
#print(state_test)
#GEOID  NAME acs_NAME           population black_popul hisp_popul white_popul GEOCOMP SUMLEV state STUSAB       lon     lat
#1: 04000US48 Texas    Texas   25145561    11,397,345    2,886,825     9460921   total    040    TX     TX -99.28182 31.4347
#so the black population code is pointing to white, hisp is pointing to black, and white is pointing to hisp.

#OKay then, whatever, use it.

state_test <- read_decennial(
  year = 2010,
  states = "TX",
  table_contents = c(
    "white_popul = P0050004",
    "black_popul = P0050010",
    "hisp_popul = P0050003",
    "urban = P0020002",
    "urbanized = P0020003",
    "median_age = P0130001"
  ),
  summary_level = "state"
)
print(state_test)
#GEOID  NAME acs_NAME         population white_popul    black_popul hisp_popul    urban     urbanized median_age GEOCOMP SUMLEV state STUSAB       lon     lat
#1: 04000US48 Texas    Texas   25145561    11397345     2886825     9460921       21298039  18947957       33.6   total    040    TX     TX -99.28182 31.4347
 
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
save(race_popul, file = "C:/Users/Claud/Box/ddpe/stx/dta/txracepopinformationbytract.rdata")


state_test <-
  read_acs5year(
    year = 2015,
    states = "TX",
    table_contents = c("median_income = B19013_001"
                       ,"black_popul = B03002_004"
                       ,"hisp_popul = B03003_003"
                       ,"white_popul = B03002_003"), 
    summary_level = "state")
print(state_test)
#GEOID  NAME      population      median_income black_popul hisp_popul  white_popul     GEOCOMP SUMLEV state STUSAB       lon     lat
#04000US48 Texas   26538614         53,207       3,070,821     10,196,367    11,635,757    total    040    TX     TX -99.28182 31.4347

income_popul <-
  read_acs5year(
    year = 2015,
    states = "TX",
    table_contents = c("median_income = B19013_001"
                       ,"black_popul = B03002_004"
                       ,"hisp_popul = B03003_003"
                       ,"white_popul = B03002_003"), 
    summary_level = "tract")

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
