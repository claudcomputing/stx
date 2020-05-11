#tx_maps1

# env ----
library(table1)
library(ggplot2)
library(forcats)
require(gridExtra)
library(ggpubr)
library(choroplethr)
library(choroplethrMaps)
library(mapproj)
library(RColorBrewer)
library(lubridate)


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

prop_year<-tx %>%
  group_by(GEOID,year) %>% 
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
myear <- merge(prop_year, txbase, by = "GEOID")
myear <- st_as_sf(myear)
m09<-myear %>%
  filter(year==2009)
m15<-myear %>%
  filter(year==2015)

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
m1<-ggplot(m) + geom_sf(aes(fill=total)) +theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Greys", breaks=pretty_breaks(n=5)) +ggtitle('Number of Vehicle Stops by Texas Troopers, 2009-2015')

#proportion of the hispanic stops labeled as white 
m2<- ggplot(m) + geom_sf(aes(fill=phw)) + theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +ggtitle('Proportion of Hispanic Stops Labeled as White, 2009-2015')
#maybe do a 2009 map compared to a 2015 map
m2_09<- ggplot(m09) + geom_sf(aes(fill=phw)) + theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +ggtitle('Proportion of Hispanic Stops Labeled as White, 2009')
m2_15<- ggplot(m15) + geom_sf(aes(fill=phw)) + theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) +ggtitle('Proportion of Hispanic Stops Labeled as White, 2015')

#proportion of census tract that is white
m3<-ggplot(m) + 
  geom_sf(aes(fill=prop_white)) +theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Purples", breaks=pretty_breaks(n=10)) +ggtitle('Proportion of Population that is White, 2009-2015')

#proportion of census tract that is hispanic
m4<-ggplot(m) + 
  geom_sf(aes(fill=prop_hisp)) +theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Blues", breaks=pretty_breaks(n=10)) +ggtitle('Proportion of Population that is Hispanic, 2009-2015')

#proportion of census tract that is black
m5<-ggplot(m) + 
  geom_sf(aes(fill=prop_black)) +theme_nothing(legend=TRUE) + scale_fill_distiller(type="seq", trans="reverse", palette = "Purples", breaks=pretty_breaks(n=10)) +ggtitle('Proportion of Population that is Black, 2009-2015')

m1
m2
m3
m4
m5
m2_09
m2_15

#Other notes on other attempts--------
#select and reshape ----
require(data.table)
tx<-tx %>% 
  filter(year(date)<2016 & year(date)> 2008) %>%
  mutate(year=year(date))
table(tx$year, tx$mc)
require(tidyverse)


#TX misclassification plot

#https://learn.r-journalism.com/en/mapping/ggplot_maps/mapping-census/
#http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#get-the-spatial-data-tigris
#https://walker-data.com/tigris-webinar/#18 

#reshape to get census level data for maps
library(scales)
library(rgdal)
library(tigris)
tracts <- tracts(state = 'TX', cb=TRUE)
co20 <- counties("TX", cb = TRUE, resolution = "20m")
try <- geo_join(co20, tx, "GEOID", "tractrow")

df4 <- tx %>%
  filter(!is.na(mc)&year==2009&mc=="H-W") %>%
  select(tract,mc,population_dec_2010) %>%
  group_by(tract,mc) %>%
  summarize(total = n()) %>%
  mutate(HWrate=mc/population_dec_2010)%>%
  ungroup() %>%
  data.frame()



#####################NOTES ON OTHER STUFF I TRIED


# Get rid of the census tracts with no data
by_tract <- by_tract[!is.na(by_tract$GEOID10),]
income_merged<- geo_join(tracts, tx, "tract", "tractrow")



#maybe later
#https://andrewbtran.github.io/NICAR/2017/maps/mapping-census-data.html
m3 <-ggplot()
m3 <- m3 +  geom_polygon(data = tx, aes(x=lng, y=lat, group=group, fill=total), color = "black", size=0.2) 
m3 <- m3 + coord_map() 
m3 <- m3 + scale_fill_distiller(type="seq", trans="reverse", palette = "Reds", breaks=pretty_breaks(n=10)) 
m3 <- m3 + theme_nothing(legend=TRUE) 
m3 <- m3 + labs(title="Where TX police traffic stops misclassify", fill="")
print(m3)

#qmplot(lon, lat, data = tx, colour = I('red'), size = I(3), darken = .3)
#qmap(baylor, zoom = 14, source = "stamen", maptype = "watercolor")
#qmap(baylor, zoom = 14, source = "stamen", maptype = "toner")