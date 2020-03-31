## Script pulls ACS census data by county, summarizes to the NYC county/boroughs, and joins with data cleaning data object

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

NYC_subdiv <- c("Bronx borough, Bronx County, New York","Manhattan borough, New York County, New York", "Staten Island borough, Richmond County, New York","Queens borough, Queens County, New York","Brooklyn borough, Kings County, New York")

#Census API key if needed ########################################

census_api_key("acc671144aa7860d8154e1fc4f97eee2ae0cc0b6",install= TRUE)

#################### 2012 ########################################

ny12 <- get_acs(geography = "county subdivision", 
                variables = c(white = "B03002_003",
                              black = "B03002_004",
                              asian = "B03002_006",
                              native.american ="B03002_005",
                              hispanic="B03002_012",
                              other ="B03002_007", # hawaiian pacific islander
                              other = "B03002_010"), # 2 races and other
                state = "NY", year = 2012, cache = TRUE) %>%
  mutate(population = estimate,
         suspect.race = variable)

  
ny12.sum <- ny12 %>%
            filter(NAME %in% NYC_subdiv) %>%
            group_by(NAME, suspect.race) %>%
            summarize(population = sum(population))

ny12.sum$year <- 2012
sum(ny12.sum$population)

#################### 2013 ########################################
ny13 <- get_acs(geography = "county subdivision", 
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

ny13.sum <- ny13 %>%
            filter(NAME %in% NYC_subdiv) %>%
            group_by(NAME, suspect.race) %>%
            summarize(population = sum(population))

ny13.sum$year <- 2013
sum(ny13.sum$population)

#################### 2014 ########################################
ny14 <- get_acs(geography = "county subdivision", 
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

ny14.sum <- ny14 %>%
            filter(NAME %in% NYC_subdiv) %>%
            group_by(NAME, suspect.race) %>%
            summarize(population = sum(population))

ny14.sum$year <- 2014
sum(ny14.sum$population)

#################### 2015 ########################################
ny15 <- get_acs(geography = "county subdivision", 
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

ny15.sum <- ny15 %>%
            filter(NAME %in% NYC_subdiv) %>%
            group_by(NAME, suspect.race) %>%
            summarize(population = sum(population))

ny15.sum$year <- 2015
sum(ny15.sum$population)

#################### 2016 ########################################
ny16 <- get_acs(geography = "county subdivision", 
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

ny16.sum <- ny16 %>%
            filter(NAME %in% NYC_subdiv) %>%
            group_by(NAME, suspect.race) %>%
            summarize(population = sum(population))

ny16.sum$year <- 2016
sum(ny16.sum$population)

# Combine all census years together ########################################
census_summary <- bind_rows(ny12.sum, ny13.sum, ny14.sum, ny15.sum, ny16.sum)
census_summary$city <- ifelse(census_summary$NAME == NYC_subdiv[1], "BRONX", 
                              ifelse(census_summary$NAME == NYC_subdiv[2], "MANHATTAN",
                                     ifelse(census_summary$NAME == NYC_subdiv[3], "STATEN ISLAND",
                                            ifelse(census_summary$NAME == NYC_subdiv[4], "QUEENS",
                                                   ifelse(census_summary$NAME == NYC_subdiv[5], "BROOKLYN",NA)))))

census_summary <- census_summary %>%
                  ungroup() %>%
                  group_by(city, year, suspect.race) %>%
                  select(-NAME)

# Summarize stop data by year, city ########################################

## Summary of SQF data by year and city (# of stops)
sqf_yr_boro <- data %>%
               group_by(city, year, suspect.race) %>%
               summarize(stopped = n())

## Left join census data population to the summarized SQF data
sqf_to_pop <- left_join(sqf_yr_boro, census_summary, by = c("city", "year", "suspect.race"))
## Remove unknown race observations
sqf_to_pop <- sqf_to_pop[complete.cases(sqf_to_pop),]
## Sanity check of population (note: will not include )
sum(sqf_to_pop$population[sqf_to_pop$year==2012])
sum(sqf_to_pop$population[sqf_to_pop$year==2013])
sum(sqf_to_pop$population[sqf_to_pop$year==2014])
sum(sqf_to_pop$population[sqf_to_pop$year==2015])
sum(sqf_to_pop$population[sqf_to_pop$year==2016])

## Add probability column
## Remove other column - It is unknown whether the census 'other' corresponds with SQF 'other' categories
sqf_to_pop <- sqf_to_pop %>%
              mutate(probability = stopped / population) %>%
              filter(suspect.race != "other")

