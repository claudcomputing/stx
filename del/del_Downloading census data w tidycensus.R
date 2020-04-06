# download needed census data for texas 
# try joining by stop data "county_name" and "county subdivision?" 
#
# packages ----------------------------------------------------------------

# install.packages("tidycensus")
library(tidycensus)
library(dplyr)
census_api_key("acc671144aa7860d8154e1fc4f97eee2ae0cc0b6",install= TRUE)
# get varnames ---------------------------------------------------------------
d10<-load_variables(2010, "sf1", cache = TRUE)
findpop<-d10[grepl('Hisp', d10$label) == T,]
findpop[1:2]
write.csv(findpop,"C:/Users/csr315/Box Sync/ddpe/stx/dta/findpop.csv")

#name    label  
#P009002	Total!!Hispanic or Latino	HISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE
#P009003 Total!!Not Hispanic or LatinoHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE
#P009004 Total!!Not Hispanic or Latino!!Population of one raceHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE

#P009005 Total!!Not Hispanic or Latino!!Population of one race!!White aloneHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE

#P009006 Total!!Not Hispanic or Latino!!Population of one race!!Black or African American aloneHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE

#P009007 Total!!Not Hispanic or Latino!!Population of one race!!American Indian and Alaska Native aloneHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE

#P009008 Total!!Not Hispanic or Latino!!Population of one race!!Asian aloneHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE

#P009009 Total!!Not Hispanic or Latino!!Population of one race!!Native Hawaiian and Other Pacific Islander aloneHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE

#P009010 Total!!Not Hispanic or Latino!!Population of one race!!Some Other Race aloneHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE

#P009011 Total!!Not Hispanic or Latino!!Two or More RacesHISPANIC OR LATINO, AND NOT HISPANIC OR LATINO BY RACE

# pull data ---------------------------------------------------------------

TX_subdiv <- c() # enter county names of TX, e.g. "Bronx borough, Bronx County, New York"

inctry <- get_decennial(geography = "state", variables="PCT0500001", state = "TX",year=2000, cache = TRUE)

tx_dec10 <- get_decennial(geography = "tract", 
                variables = c(white = "P009005",
                              black = "P009006",
                              asian = "P009008",
                              nonhisptotal ="P009003",
                              hispanic="P009002",
                              median_age ="P013001" 
                              median_income=""
                              urban_pop="P002003"),
                state = "TX", year = 2010, cache = TRUE) %>%
  mutate(population = value,
         race = variable)
#check total is 25.1M
tx_dec10 %>% select(race,population)%>%filter(race %in% c("nonhisptotal","hispanic")) %>% summarize(population = sum(population))

head(tx_dec10)
#if we want to group to bigger than a tract
#tx_dec10.sum <- tx_dec10 %>%
  #group_by(NAME, acs_race_pop) %>%
  #summarize(population = sum(population))

## deleted ------------------------------------------------------

findage<-d10[grepl('age', d10$label,ignore.case=TRUE) == T,]
findage[1:2]
write.csv(findage,"C:/Users/csr315/Box Sync/ddpe/stx/dta/findage.csv")

findinc<-d10[grepl('income', d10$label,ignore.case=TRUE) == T,]
findinc[1:2]
write.csv(findinc,"C:/Users/csr315/Box Sync/ddpe/stx/dta/findinc.csv")
findurb<-d10[grepl('urban', d10$label,ignore.case=TRUE) == T,]
findurb[1:2]
write.csv(findurb,"C:/Users/csr315/Box Sync/ddpe/stx/dta/findurb.csv")


findage<-d10[grepl('age', d10$label,ignore.case=TRUE) == T,]
findage[1:2]
write.csv(findage,"C:/Users/csr315/Box Sync/ddpe/stx/dta/findage.csv")

findinc<-d10[grepl('income', d10$label,ignore.case=TRUE) == T,]
findinc[1:2]
write.csv(findinc,"C:/Users/csr315/Box Sync/ddpe/stx/dta/findinc.csv")
findurb<-d10[grepl('urban', d10$label,ignore.case=TRUE) == T,]
findurb[1:2]
write.csv(findurb,"C:/Users/csr315/Box Sync/ddpe/stx/dta/findurb.csv")
