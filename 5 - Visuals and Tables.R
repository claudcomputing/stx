# run this after you have the merged file
# makes some tables & charts
# csr315@nyu.edu

# dta: https://openpolicing.stanford.edu/data/  
# doc: https://github.com/stanford-policylab/opp/blob/master/data_readme.md  

# env ----
library(table1)
library(ggplot2)
library(forcats)
library(tidyverse)
require(gridExtra)
library(ggpubr)
library(choroplethr)
library(choroplethrMaps)
library(mapproj)
library(RColorBrewer)
library(data.table)
library(effects)

library(stargazer)

# set working directory ---------------------------------------------------

#setwd("/Users/MVERAS/Documents/Data Driven Eval/Data/")
setwd('C:/Users/Claud/Box/stx')

# load data ---------------------------------------------------------------


#p<-readRDS("C:/Users/csr315/Box/ddpe/stx/dta/kx738rc7407_tx_plano_2019_12_17.rds")
#load("C:/Users/csr315/Box Sync/ddpe/stx/dta/Texas1MSample.Rdata")
load("Texas1MSampleClean_Merged.rdata")
# head(tx$raw_HA_RACE_SEX)
# names(tx)

#clean label reshape ----

#subsample
#years 2009-2015
tx[mc!= "H-O", misid := 1*(mc == "H-W")]
tx<-tx_merged %>% 
  filter(year(date)<2016 & year(date)> 2008) %>%
  mutate(year=year(date))
table(year(tx$date), tx$mc) #good!  

#make raw race labels
#labels middle east as white. Very few M (~40 in rand samp)
levels(tx$subject_race)
levels(as.factor(tx$rawrace))
tx$raw_race <- ifelse(tx$rawrace=="A", "Asian", #/Pacific Islander
                      ifelse(tx$rawrace=="B", "Black", 
                             ifelse(tx$rawrace=="H", "Hispanic",
                                    ifelse(tx$rawrace %in% c("I","O"), "Other",
                                           ifelse(tx$rawrace == "U", "Unknown",
                                                  ifelse(tx$rawrace %in% c("M","W"), "White", "Missing"))))))
table(tx$raw_race,tx$rawrace)
head(tx$rawrace)
head(tx$raw_race)

tx$cleaned_race <- ifelse(tx$subject_race=="asian/pacific islander", "Asian", #/Pacific Islander
                          ifelse(tx$subject_race=="black", "Black", 
                                 ifelse(tx$subject_race=="hispanic", "Hispanic",
                                        ifelse(tx$subject_race == "other", "Other",
                                               ifelse(tx$subject_race == "unknown", "Unknown",
                                                      ifelse(tx$subject_race == "white", "White", "Missing"))))))
table(tx$subject_race,tx$raw_race)
table(tx$subject_race,tx$cleaned_race)

#reshape for charts ----


#reshape to make by year charts
df1 <- tx %>%
  filter(!is.na(mc)) %>%
  group_by(year, mc) %>%
  summarize(stopped = n()) %>%
  ungroup() %>%
  data.frame()
df2 <- tx %>%
  filter(!is.na(raw_race)) %>%
  group_by(year, raw_race) %>%
  summarize(stopped = n()) %>%
  ungroup() %>%
  data.frame()
df3 <- tx %>%
  filter(!is.na(cleaned_race)) %>%
  group_by(year, cleaned_race) %>%
  summarize(stopped = n()) %>%
  ungroup() %>%
  data.frame()

df5 <- tx %>%
  filter(!is.na(misid)) %>%
  mutate(citation_dummy=as.integer(citation_issued)) %>%
  group_by(year, misid, citation_issued, citation_dummy) %>%
  summarize(citation_issued_freq = n()) %>%
  ungroup() %>%
  group_by(year, misid) %>%
  summarize(citation_issued_rate=sum(citation_issued_freq*citation_dummy)/sum(citation_issued_freq)) %>%
  data.frame() 


#main plots ----
# plot race - raw and recoded, raw by recoded, and mcs
#mc over time
p1<-ggplot(data=df1, aes(x=year, y=stopped, color = mc))
p1<-p1 + geom_line(size = 1) +
  xlab('Year') +
  ylab('Frequency') + 
  ggtitle('Police Stops with Hispanic/Race Surnames by Race Reported Over Time') + 
  theme(text = element_text(size = 10)) +
  geom_point() + 
  labs(color = 'Misclassification Category') +
  scale_color_manual(values = c("#00AFBB", "#E7B800","purple"),
                     labels = c('Reported as Hispanic', 'Reported as Other', 'Reported as White'))
p2<-ggplot(data=df2, aes(x=year, y=stopped, color = raw_race))
p2<-p2 + geom_line(size = 1) +
  xlab('Year') +
  ylab('Frequency, Race Reported') + 
  ggtitle('Police Stops by Race Classifications Over Time') + 
  theme(text = element_text(size = 10)) +
  geom_point() + 
  labs(color = 'Race Classification')
p3<-ggplot(data=df3, aes(x=year, y=stopped, color = cleaned_race))
p3<-p3+ geom_line(size = 1) +
  xlab('Year') +
  ylab('Frequency, Race with Hispanic Imputed') + 
  ggtitle(' ') + #Imputed Race Category of Police Stops Over Time 
  theme(text = element_text(size = 10)) +
  geom_point() + 
  labs(color = 'Imputed Race')
p2_p3<-ggarrange(p2, p3, ncol=2,common.legend = TRUE, legend="bottom")

# plot citation over time by misclass
p5<-ggplot(data=df5, aes(x=year, y=citation_issued_rate, color = as.factor(misid)))
p5<-p5 + geom_line(size = 1) +
  xlab('Year') +
  ylab('Citations Issued Rated (Percent of Total Stops)') + 
  ggtitle('Rate of Citations Issued as a Share of Stops Reported Over Time') + 
  theme(text = element_text(size = 10)) +
  geom_point() + 
  labs(color = 'Misclassification Category') +
  scale_color_manual(values = c("#00AFBB", "#E7B800"),
                     labels = c('Reported as Hispanic', 'Reported as White'))
p5

#explore more plots without time ----
ggplot(tx, aes(x=mc)) + geom_histogram(stat="count")
ggplot(tx, aes(x=raw_race,fill= subject_race,color = raw_race)) + geom_histogram(stat="count") 
ggplot(tx, aes(x=subject_race,fill= subject_race,color = subject_race)) + geom_histogram(stat="count")
ggplot(tx, aes(x=raw_race,fill= subject_race)) + geom_bar(position="dodge")
ggplot(tx, aes(x=mc_wmissing)) + geom_histogram(stat="count")


# main tables ----

#mc descriptive stats table
#demogs and stop stats

# explore tables w different race classifications ----
#counts over time
table(year(tx$date), tx$mc)
#demogs
#label(tx$subject_age) <-"Age"
label(tx$subject_sex) <-"Sex"
label(tx$subject_race) <-"Race/Ethnicity Recoded"
label(tx$raw_race) <-"Race/Eth Raw Recorded"
label(tx$mc) <-"Hispanic ReClassification 1"
label(tx$misid_label) <-"Hispanic Coded as White Identification"
# label(tx$mc_wmissing) <-"Hispanic ReClassification 2" #removed 
#table1(~ subject_sex + subject_race + raw_race + mc
       # +mc_wmissing
       #,data=tx)
#stop stats label
label(tx$search_basis) <-"Search Basis"
label(tx$search_conducted) <-"Search Conducted"
label(tx$contraband_found) <-"Contraband Found"
label(tx$contraband_weapons) <-"Contraband Weapons"
label(tx$contraband_drugs) <-"Contraband Drugs"
label(tx$outcome)<-"Outcome"
#table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome,data=tx)
t1<-table1(~ subject_sex + raw_race | misid,data=tx,Overall="Total")

#main tables
tx$misid_label <- 
  factor(tx$misid, levels=c(1,0),
         labels=c("H-H", 
                  "H-W"))

t2<-table1(~ search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | misid_label,data=tx,Overall="Total")
#point in time - mc in 2009 and 2015
#reshape to look at 2009 versus 2015 mcs
df4 <- tx %>%
  filter(!is.na(misid_label)&(year==2009|year==2015))

label(df4$search_basis) <-"Search Basis"
label(df4$outcome)<-"Outcome"
t3<-table1(~ search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | misid_label*year, data=df4,Overall="Total")



# more stratified demogs
table1(~ subject_sex + raw_race | raw_race,data=tx,Overall="Total")
# table1(~ subject_sex + raw_race | subject_race,data=tx,Overall="Total")
table1(~ subject_sex + raw_race | mc,data=tx,Overall="Total")
# table1(~ subject_sex + raw_race | mc_wmissing,data=tx,Overall="Total")

#more stratified stop stats
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | raw_race,data=tx,Overall="Total")
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | suspect_race,data=tx,Overall="Total")
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | misid,data=tx,Overall="Total")
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | mc_wmissing,data=tx,Overall="Total")



# checking out violations ----------

#stop stats label
label(tx$viol_alcohol) <-"Alcohol in Vehicle"
label(tx$viol_belt) <-"Seatbelt"
label(tx$viol_drug) <-"Drug in Vehicle"
label(tx$viol_dui) <-"Driving Under Influence (DUI)"
label(tx$viol_lamp) <-"Lamp"
label(tx$viol_license)<-"Drivers License"
label(tx$viol_mod)<-"Vehicle Modification"
label(tx$viol_plate)<-"License Plate"
label(tx$viol_registration)<-"Registration"
label(tx$viol_speed)<-"Speeding"
label(tx$viol_traffic)<-"Traffic"


tx$misid_label <- 
  factor(tx$misid, levels=c(1,0),
         labels=c("H-H", 
                  "H-W"))

tx$viol_alcohol <- factor(tx$viol_alcohol, levels=c(1,0),
                          labels=c("H-H", 
                                   "H-W"))
tx$viol_belt <-factor(tx$viol_belt, levels=c(1,0))
tx$viol_drug <-factor(tx$viol_drug, levels=c(1,0))
tx$viol_dui <-factor(tx$viol_dui, levels=c(1,0))
tx$viol_lamp <-factor(tx$viol_lamp, levels=c(1,0))
tx$viol_license <-factor(tx$viol_license, levels=c(1,0))
tx$viol_mod <-factor(tx$viol_mod, levels=c(1,0))
tx$viol_plate <-factor(tx$viol_plate, levels=c(1,0))
tx$viol_registration <-factor(tx$viol_registration, levels=c(1,0))
tx$viol_speed <-factor(tx$viol_speed, levels=c(1,0))
tx$viol_traffic <-factor(tx$viol_traffic, levels=c(1,0))

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (%s)", MEAN, SD)))
}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

table1(  ~ viol_alcohol + viol_belt  + viol_drug + viol_dui + viol_lamp + viol_license + viol_mod + viol_plate + viol_registration + viol_speed + viol_traffic | misid_label, data = tx, render.categorical=my.render.cat,render.continuous=my.render.cont)




#TX maps ----
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
#name plots for rmd report / easy export ----
p1_mc_stops_over_time <-p1
p2_race_reported_imputed<-p2_p3
#t1_mc_demographics <-t1
t2_mc_stopstats<-t2
t3_mc_09n15_stopstats<-t3
rm(df,df1,df2,df3,df4,p1,p2,p3,p2_p3,t1,t2,t3)
