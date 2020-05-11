#map attempt 

# set working directory -------------
setwd("/Users/MVERAS/Documents/Data Driven Eval/Data/")

# load packages -----------------------------------------------------------
library(data.table)
library(tableone)
library(stringr)
library(magrittr)
library(tidycensus)
library(tidyverse)
library(sf)
library(ggplot2)
library(ggmap)
library(scales)
library(broom)
# load data ---------------------------------------------------------------

load("Texas1MSampleClean_Merged.Rdata")

# preprocessing -----------------------------------------------------------

tx_merged[mc!= "H-O", misid := 1*(mc == "H-W")]

tx<-tx_merged %>% 
  filter(year(date)<2016 & year(date)> 2008 & mc %in% c("H-W", "H-H")) %>%
  mutate(year=year(date))

prop <- tx %>% 
  group_by(GEOID) %>% 
  summarise(total = n(),
            misidtotal = sum(misid),
            prop_white = mean(prop_white),
            prop_black = mean(prop_black),
            prop_hisp = mean(prop_hisp)) %>%
  mutate(phw = misidtotal/total)


# mapping -----------------------------------------------------------------
txbase <- tidycensus::get_acs(state = "TX", geography = "tract",
                              variables = "B19013_001", geometry = TRUE)

# plot(txbase) #slow- makes all the maps 

# merge

m <- merge(prop, txbase, by = "GEOID")
m <- st_as_sf(m)

# plot(m)

justtexas <- ggplot() +
  geom_sf(data = m) + theme(axis.line =  element_blank(),
                            axis.text =  element_blank(),
                            axis.ticks =  element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.border = element_blank(),
                            panel.background = element_blank()) 


# plot(m["total"])

# plot(m["phw"], 
# main = "Percent of individuals with hispanic last names identified as white", 
# breaks = "quantile", nbreaks = 4)


# ggplot(m) + 
#   geom_sf(aes(fill=total)) 

#total number of stops
ggplot(m) + 
  geom_sf(aes(fill=total)) +theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=5)) 



#proportion of the hispanic stops labeled as white 
ggplot(m) + 
  geom_sf(aes(fill=phw)) +theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) 

#proportion of census tract that is white
ggplot(m) + 
  geom_sf(aes(fill=prop_white)) +theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Greens", breaks=pretty_breaks(n=10)) 


#proportion of census tract that is white
ggplot(m) + 
  geom_sf(aes(fill=prop_hisp)) +theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Greens", breaks=pretty_breaks(n=10)) 

