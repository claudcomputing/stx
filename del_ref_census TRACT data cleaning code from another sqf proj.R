# File to create summaries of census data by precinct or borough
# Output of file is summary .csv files, which are uploaded in the Shiny app

if(!requireNamespace("here"))
  install.packages("here", repos = "https://cloud.r-project.org")
library("here")
here::here()
library(proj4)
library(shiny)
library(dplyr)
library(haven)
library(RSQLite)
library(tidycensus)
library(sqldf)
#library(xlsx)
#require("openintro")

#Census Mapping Section################################################################################################

#################### crosswalk precinct to census ########################################
#census - get crosswalks to link precincts to census tract and county data
census<-read.csv("data/NYC_Blocks_2010CensusData_Plus_Precincts.csv")
geoidcw<-read.csv("data/precinct_blocks_key.csv")

#get data from team cleaned dataset
dta1<-data

#align data types for subsequent merge
typeof(census$precinct)
dta1 <- dta1%>%
  mutate(precinct = as.integer(round(precinct, 0)))
typeof(dta1$precinct)

#create precinct/year level data 
precinct<-unique(dta1$precinct) #dta1$year both unique
year<-unique(dta1$year)
precincts_level<-data.frame(precinct)
precincts_year_level<-data.frame(expand.grid(precinct,year)) %>%
  mutate (precinct=Var1, year=Var2) %>%
  select (precinct,year)%>%as_tibble()
wcensus = dplyr::left_join(precincts_level, census, by = "precinct")
wgeoid = dplyr::left_join(precincts_year_level, geoidcw, by = "precinct")

#note that the GEOIDs on this dataset are 4 longer than the tract geoids in the acs 5 year estimates,
# so create a geoid that takes only the first 11 characters (rather than the 15 it has at start)
wgeoid2 <- wgeoid%>%
  mutate(GEOID = substr(as.character(geoid10), 1, 11)) %>%
  select(-geoid10) %>%
  distinct()

wgeoid%>%distinct()%>%nrow() #193055, before we truncate tract
wgeoid2%>%nrow() #12415, after we truncate tract

#################### census data \ ########################################
#connect to census using data key acc671144aa7860d8154e1fc4f97eee2ae0cc0b6
census_api_key("acc671144aa7860d8154e1fc4f97eee2ae0cc0b6",install= TRUE)

#view available variables by census dataset
#v12 <- load_variables(2012, "acs12", cache = TRUE)
#v17 <- load_variables(2017, "acs5", cache = TRUE)

#Names of census variables identified in v17

#total="B03002_001" = the sum of:
#non-hisp asian = "B03002_006",
#non-hisp black = "B03002_004",
#hispanic="B03002_012",
#non-hisp white = "B03002_003",
#non-hisp amerindian.alaska ="B03002_005",
#non-hisp nathawaii.othpac ="B03002_007",
#non-hisp other = "B03002_008",
#non-hisp tworaces ="B03002_009",
#non-hisp other and tworaces together ="B03002_010",

#2012-2016

#################### 2012 ########################################

ny12 <- get_acs(geography = "tract", 
                variables = c(white = "B03002_003",
                              black = "B03002_004",
                              asian = "B03002_006",
                              native.american ="B03002_005",
                              hispanic="B03002_012",
                              other ="B03002_007", #hawaiian pacific islander
                              other = "B03002_010"), #2 races and other
                state = "NY", year = 2012, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

#get pop estimates to tract-race level
ny12.1<-ny12 %>%group_by(GEOID,NAME, suspect.race)%>% summarize(population = sum(population))%>%arrange(GEOID)
#head(ny12.1)
#View(ny12.1)
#ny12.2 <- ny12 %>% group_by(GEOID, suspect.race) %>% summarize(population = sum(population))

#join to tract-precinct crosswalk & limit to NYC Precint only tracts
wgeoid12 = dplyr::left_join(filter(wgeoid2, year == 2012), ny12.1, by = "GEOID")
#head(wgeoid12)

#################### 2013 ########################################
ny13 <- get_acs(geography = "tract", 
                variables = c(white = "B03002_003",
                              black = "B03002_004",
                              asian = "B03002_006",
                              native.american ="B03002_005",
                              hispanic="B03002_012",
                              other ="B03002_007", #hawaiian pacific islander
                              other = "B03002_010"), #2 races and other
                state = "NY", year = 2013, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

#get pop estimates to tract-race level
ny13.1 <- ny13 %>%group_by(GEOID,NAME, suspect.race)%>% summarize(population = sum(population))%>%arrange(GEOID)

#join to tract-precinct crosswalk & limit to NYC Precint only tracts
wgeoid13 = dplyr::left_join(filter(wgeoid2, year == 2013), ny13.1, by = "GEOID")

#################### 2014 ########################################
ny14 <- get_acs(geography = "tract", 
                variables = c(white = "B03002_003",
                              black = "B03002_004",
                              asian = "B03002_006",
                              native.american ="B03002_005",
                              hispanic="B03002_012",
                              other ="B03002_007", #hawaiian pacific islander
                              other = "B03002_010"), #2 races and other
                state = "NY", year = 2014, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

#get pop estimates to tract-race level
ny14.1 <- ny14 %>%group_by( GEOID,NAME, suspect.race)%>% summarize(population = sum(population))%>%arrange(GEOID)

#ny14%>%summarize(population=sum(population))
#ny14.1%>% mutate(state="NY") %>% group_by(state) %>% summarize(population=sum(population))

#join to tract-precinct crosswalk & limit to NYC Precint only tracts
wgeoid14 = dplyr::left_join(filter(wgeoid2, year == 2014), ny14.1, by = "GEOID")
wgeoid14 = wgeoid14 %>%
  filter(!is.na(population)) %>%
  filter(!is.na(suspect.race)) 
#wgeoid14%>%group_by(year) %>%summarize(population=sum(population))

#################### 2015 ########################################
ny15 <- get_acs(geography = "tract", 
                variables = c(white = "B03002_003",
                              black = "B03002_004",
                              asian = "B03002_006",
                              native.american ="B03002_005",
                              hispanic="B03002_012",
                              other ="B03002_007", #hawaiian pacific islander
                              other = "B03002_010"), #2 races and other
                state = "NY", year = 2015, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

#get pop estimates to tract-race level
ny15.1 <- ny15 %>%group_by(GEOID,NAME, suspect.race)%>% summarize(population = sum(population))%>%arrange(GEOID)

#join to tract-precinct crosswalk & limit to NYC Precint only tracts
wgeoid15 = dplyr::left_join(filter(wgeoid2, year == 2015), ny15.1, by = "GEOID")



#################### 2016 ########################################
ny16 <- get_acs(geography = "tract", 
                variables = c(white = "B03002_003",
                              black = "B03002_004",
                              asian = "B03002_006",
                              native.american ="B03002_005",
                              hispanic="B03002_012",
                              other ="B03002_007", #hawaiian pacific islander
                              other = "B03002_010"), #2 races and other
                state = "NY", year = 2016, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

#get pop estimates to tract-race level
ny16.1 <- ny16 %>%group_by(GEOID,NAME, suspect.race)%>% summarize(population = sum(population))%>%arrange(GEOID)

#join to tract-precinct crosswalk & limit to NYC Precint only tracts
wgeoid16 = dplyr::left_join(filter(wgeoid2, year == 2016), ny16.1, by = "GEOID")

####################################################################

#join all years
census_geoid <- bind_rows(wgeoid12,wgeoid13,wgeoid14,wgeoid15,wgeoid16)

#summarize to precinct level
census_precinct <- census_geoid %>%
  group_by(year, precinct, suspect.race) %>%
  summarize(population = sum(population)) %>%
  ungroup() %>%
  data.frame() %>%
  na.omit()

#gut check race precinct sum/dist
census_precinct %>% group_by(year, suspect.race) %>% summarize(population=sum(population))

#see NYC total pop
census_precinct %>% na.omit()%>%group_by(year) %>% summarize(population=sum(population)) 

#share dataset
write.csv(census_precinct, "data/census_precinct.csv")

#make crosswalk from sqf data
boroxprecinct <- data %>% select(city, precinct) %>% mutate(borough=city) %>% select(borough, precinct, city) %>%distinct()

#join precincts to borough
census_prexcity = dplyr::left_join(census_precinct, boroxprecinct, by = "precinct")
#census_precinct %>% nrow()
#boroxprecinct %>% nrow()
#census_prexcity %>% nrow() #issue
#census_prexcity2 <- census_prexcity %>% mutate(suspectrace=suspect.race)
#sqldf("select * from census_prexcity2 group by precinct, suspectrace, year having count(*) >1") 

#census_precinct = census_precinct %>% na.omit()
census_boro <- census_prexcity %>%
  mutate(city <- as.factor(city))%>%
  group_by(year, borough, city, suspect.race) %>%
  summarize(population = sum(population)) %>%
  ungroup() %>% na.omit() %>%
  data.frame()
census_prexcity$race<-ifelse(census_prexcity$suspect.race=="white", "white", ifelse(census_prexcity$suspect.race == "black","black",ifelse(census_prexcity$suspect.race=="asian", "asian", ifelse(census_prexcity$suspect.race=="hispanic","hispanic", "other"))))

#make a data set summarizing population by race and borough
census_boro_race2 <- census_prexcity %>%
  mutate(city <- as.factor(city))%>%
  group_by(year, borough, city, race) %>%
  summarize(population = sum(population)) %>%
  ungroup() %>% na.omit() %>%
  data.frame()
#census_boro_race2%>% mutate(state="NY") %>% group_by(state,year) %>% summarize(population=sum(as.numeric(population))) #issue
write.csv(census_precinct, file = "data/census_precinct.csv")
write.csv(census_boro, "data/census_boro.csv")
write.csv(census_boro_race2, "data/census_boro_race2.csv")

####################################################################

############# combine with sqf and clean ############################

# load census data

census_precinct <- read.csv("data/census_precinct.csv")
census_boro <- read.csv("data/census_boro.csv")
census_boro_race2 <- read.csv("census_boro_race2.csv")

# joining stop and frisk data with census data excluding missing in both datasets
census_precinct$suspect.race <- as.factor(census_precinct$suspect.race)
census_precinct <- census_precinct %>%
  filter(!is.na(population))
census_precinct%>% mutate(state="NY") %>% group_by(state,year) %>% summarize(population=sum(as.numeric(population)))
data_census <- inner_join((subset(data, (!is.na(data[,79])))), census_precinct, by = c("year","precinct","suspect.race"))
data_census$suspect.race <- as.factor(data_census$suspect.race)
data_census%>% mutate(state="NY") %>% group_by(state,year) %>% summarize(population=sum(as.numeric(population)))
#note that now all of the variables are at the stop level, EXCEPT the population estimates which are at the precinct level.

#Joining SQF data with census data with population at borough level
census_boro$race <- as.factor(census_boro$suspect.race)
#Make race 2
census_boro$race<-ifelse(census_boro$suspect.race=="white", "white", ifelse(census_boro$suspect.race == "black","black",ifelse(census_boro$suspect.race=="asian", "asian", ifelse(census_boro$suspect.race=="hispanic","hispanic", "other"))))
census_boro$race <- as.factor(census_boro$race)
census_boro <- census_boro %>%
  filter(!is.na(population))
data_census_boro <- inner_join(data, census_boro, by = c("year","city","race"))
data_census_boro$race <- as.factor(data_census_boro$race)

#table(data_census_boro$race)
#table(data_census_boro$city)
#table(data_census_boro$race, data_census_boro$city)
#head(data_census_boro$population)

#using data summarized by race and borough
census_boro_race2$race <- as.factor(census_boro_race2$race)
census_boro_race2 <- census_boro_race2 %>%
  filter(!is.na(population))
data_census_boro2 <- inner_join(data, census_boro_race2, by = c("year","city","race"))
data_census_boro2$race <- as.factor(data_census_boro2$race)
