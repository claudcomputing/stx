#tx_maps1



#Other notes on other attempts--------
#TX County Percent Hispanic
data(df_county_demographics)
df_county_demographics$value = df_county_demographics$percent_hispanic
col.pal<-brewer.pal(9,"Blues")
m1<-county_choropleth(df_county_demographics, 
                      state_zoom = "texas",
                      title      = "Texas County Percent Hispanic Estimates, 2012",
                      num_colors = 9) + coord_map() + scale_fill_manual(name="Percent Hispanic",values=col.pal, drop=FALSE)
#TX County Average Per Capita Income 
df_county_demographics$value = df_county_demographics$per_capita_income 
col.pal<-brewer.pal(7,"Greens")
m2<-county_choropleth(df_county_demographics, 
                      state_zoom = "texas",
                      title      = "Texas County Income Per Capita Estimates, 2012",
                      num_colors = 7) + coord_map() +scale_fill_manual(name="Per Capita Income",values=col.pal, drop=FALSE)
#TX County Percent White
data(df_county_demographics)
df_county_demographics$value = df_county_demographics$percent_white
col.pal<-brewer.pal(9,"Greys")
m4<-county_choropleth(df_county_demographics, 
                      state_zoom = "texas",
                      title      = "Texas County Percent White Estimates, 2012",
                      num_colors = 9) + coord_map() + scale_fill_manual(name="Percent White",values=col.pal, drop=FALSE)
#TX County Percent Black
data(df_county_demographics)
df_county_demographics$value = df_county_demographics$percent_black
col.pal<-brewer.pal(9,"Blues")
m5<-county_choropleth(df_county_demographics, 
                      state_zoom = "texas",
                      title      = "Texas County Percent Black Estimates, 2012",
                      num_colors = 9) + coord_map() + scale_fill_manual(name="Percent Black",values=col.pal, drop=FALSE)


#https://www.r-bloggers.com/advanced-choroplethr-changing-color-scheme-2/
#https://www.r-bloggers.com/learn-to-map-census-data-in-r/

m1_county_hispanic<-m1
m2_county_income<-m2
m3_county_white<-m3
m4_county_black<-m4
rm(m1,m2,m3,m4)



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