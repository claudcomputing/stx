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

load("C:/Users/Claud/Box/ddpe/stx/dta/Texas1MSampleClean_Merged.Rdata")
head(tx_merged)
tx<-tx_merged

#select and reshape ----
require(data.table)
tx<-tx %>% 
  filter(year(date)<2016 & year(date)> 2008) %>%
  mutate(year=year(date))
table(tx$year, tx$mc)
require(tidyverse)

#TX misclassification plot

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