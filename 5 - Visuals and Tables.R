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

#reshape to look at 2009 versus 2015 mcs
df4 <- tx %>%
  filter(!is.na(mc)&(year==2009|year==2015))

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
t1<-table1(~ subject_sex + raw_race | mc,data=tx,Overall="Total")
t2<-table1(~ search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | mc,data=tx,Overall="Total")
#point in time - mc in 2009 and 2015
t3<-table1(~ search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | mc*year,data=df4,Overall="Total")
# explore tables w different race classifications ----
#counts over time
table(year(tx$date), tx$mc)
#demogs
#label(tx$subject_age) <-"Age"
label(tx$subject_sex) <-"Sex"
label(tx$subject_race) <-"Race/Ethnicity Recoded"
label(tx$raw_race) <-"Race/Eth Raw Recorded"
label(tx$mc) <-"Hispanic ReClassification 1"
# label(tx$mc_wmissing) <-"Hispanic ReClassification 2" #removed 
table1(~ subject_sex + subject_race + raw_race + mc
       # +mc_wmissing
       ,data=tx)
#stop stats label
label(tx$search_basis) <-"Search Basis"
label(tx$search_conducted) <-"Search Conducted"
label(tx$contraband_found) <-"Contraband Found"
label(tx$contraband_weapons) <-"Contraband Weapons"
label(tx$contraband_drugs) <-"Contraband Drugs"
label(tx$outcome)<-"Outcome"
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome,data=tx)
#stratified demogs
table1(~ subject_sex + raw_race | raw_race,data=tx,Overall="Total")
# table1(~ subject_sex + raw_race | subject_race,data=tx,Overall="Total")
table1(~ subject_sex + raw_race | mc,data=tx,Overall="Total")
# table1(~ subject_sex + raw_race | mc_wmissing,data=tx,Overall="Total")

#stratified stop stats
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | raw_race,data=tx,Overall="Total")
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | suspect_race,data=tx,Overall="Total")
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | mc,data=tx,Overall="Total")
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

table1(
  ~ viol_alcohol + viol_belt  + viol_drug + viol_dui + viol_lamp + viol_license + viol_mod + viol_plate + viol_registration + viol_speed + viol_traffic |
    mc,
  data = tx,
  Overall = "Total"
)



#TX maps ----
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
df_county_demographics$value = df_county_demographics$percent_black
col.pal<-brewer.pal(9,"Greys")
m4<-county_choropleth(df_county_demographics, 
                      state_zoom = "texas",
                      title      = "Texas County Percent White Estimates, 2012",
                      num_colors = 9) + coord_map() + scale_fill_manual(name="Percent White",values=col.pal, drop=FALSE)


#https://www.r-bloggers.com/advanced-choroplethr-changing-color-scheme-2/
#https://www.r-bloggers.com/learn-to-map-census-data-in-r/

#name plots for rmd report / easy export ----
p1_mc_stops_over_time <-p1
p2_race_reported_imputed<-p2_p3
t1_mc_demographics <-t1
t2_mc_stopstats<-t2
t3_mc_09n15_stopstats<-t3
m1_county_hispanic<-m1
m2_county_income<-m2
rm(df,df1,df2,df3,df4,p1,p2,p3,p2_p3,t1,t2,t3,m1,m2)
