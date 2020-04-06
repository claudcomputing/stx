# accompanies tx1.Rmd
# makes some tables & charts
# csr315@nyu.edu

# dta: https://openpolicing.stanford.edu/data/  
# doc: https://github.com/stanford-policylab/opp/blob/master/data_readme.md  

# env ----
library(table1)
library(ggplot2)
library(forcats)
library(tidyverse)

#p<-readRDS("C:/Users/csr315/Box/ddpe/stx/dta/kx738rc7407_tx_plano_2019_12_17.rds")
#load("C:/Users/csr315/Box Sync/ddpe/stx/dta/Texas1MSample.Rdata")
load("C:/Users/Claud/Box/ddpe/dta/Texas1MSample.Rdata")
head(tx$raw_HA_RACE_SEX)
names(tx)

#clean label reshape ----

#subsample
#years 2009-2015
tx<-tx %>% 
  filter(year(tx$date)<2016 & year(tx$date)> 2008) %>%
  mutate(year=year(tx$date))
table(year(tx$date), tx$mc)

#define labels  



#make raw race labels  ----
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
df <- tx %>%
  select(year, mc, uempmed) %>%
  gather(key = "variable", value = "value", -date)
head(df, 3)

#plot ----
# plot race - raw and recoded, raw by recoded, and mcs
ggplot(data=tx, aes(x=year, y=mc)) + geom_line(aes(color = mc))

# explore ----
ggplot(tx, aes(x=mc)) + geom_histogram(stat="count")
ggplot(tx, aes(x=raw_race,fill= subject_race,color = raw_race)) + geom_histogram(stat="count") 
ggplot(tx, aes(x=subject_race,fill= subject_race,color = subject_race)) + geom_histogram(stat="count")
ggplot(tx, aes(x=raw_race,fill= subject_race)) + geom_bar(position="dodge")
ggplot(tx, aes(x=mc_wmissing)) + geom_histogram(stat="count")

#desc tables ----
table(year(tx$date), tx$mc)

# describe 

#demogs
#label(tx$subject_age) <-"Age"
label(tx$subject_sex) <-"Sex"
label(tx$subject_race) <-"Race/Ethnicity Recoded"
label(tx$raw_race) <-"Race/Eth Raw Recorded"
label(tx$mc) <-"Hispanic ReClassification 1"
label(tx$mc_wmissing) <-"Hispanic ReClassification 2"
table1(~ subject_sex + subject_race + raw_race + mc+ mc_wmissing,data=tx)
#stop stats
label(tx$search_basis) <-"Search Basis"
label(tx$search_conducted) <-"Search Conducted"
label(tx$contraband_found) <-"Contraband Found"
label(tx$contraband_weapons) <-"Contraband Weapons"
label(tx$contraband_drugs) <-"Contraband Drugs"
label(tx$outcome)<-"Outcome"
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome,data=tx)
#stratified demogs
table1(~ subject_sex + raw_race | raw_race,data=tx,Overall="Total")
table1(~ subject_sex + raw_race | suspect_race,data=tx,Overall="Total")
table1(~ subject_sex + raw_race | mc,data=tx,Overall="Total")
table1(~ subject_sex + raw_race | mc_wmissing,data=tx,Overall="Total")
#stratified stop stats
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | raw_race,data=tx,Overall="Total")
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | suspect_race,data=tx,Overall="Total")
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | mc,data=tx,Overall="Total")
table1(~search_basis + contraband_found + search_conducted + contraband_weapons + contraband_drugs + outcome | mc_wmissing,data=tx,Overall="Total")

############## in process
# do some stuff by time

################Notes and deleted
#found contraband (numerator of hit rate)
data$found.true <- NA
data$found.true <- ifelse(data$contraband_found == T, T, ifelse(data$found.pistol ==T, T, ifelse(data$found.rifle==T, T, ifelse(data$found.assault == T, T, ifelse(data$found.knife== T, T, ifelse(data$found.machinegun==T, T, ifelse(data$found.other== T, T, ifelse(data$found.gun == T, T, ifelse(data$found.weapon==T, T, F)))))))))
names(tx)


#Data frame for Predicted Probability Plot
glm_data <- data %>% filter(suspected.crime == "cpw") %>% mutate(year.f = factor(year, level = c(2012:2016)))

glm.found.t <- glm(found.weapon ~ suspect.age + suspect.height + suspect.weight + suspect.build + suspect.sex + year.f*suspect.race, data = glm_data, family = "binomial")

glm.found <- glm(found.weapon ~ suspect.age + suspect.height + suspect.weight + suspect.build + suspect.sex + year*suspect.race, data = glm_data, family = "binomial")

glm_data <- glm_data %>% 
  mutate(predicted = predict(glm.found.t, type = "response"))

# Save the logistic regression result
result <- tidy(glm.found.t) %>% kable()

#time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")
#forcats::fct_inorder(Day44$Sample)



tx[rawrace == "H", raw_race := "hispanic"]
tx[rawrace == "B", raw_race := "black"]
tx[rawrace == "I", raw_race := "indigenous"]
tx[rawrace == "M", raw_race := "missing"]
tx[rawrace == "O", raw_race := "other"]
tx[rawrace == "U", raw_race := "unknown"]
tx[rawrace == "W", raw_race := "white"]
levels(tx$raw_race)<-racelabels_i
tx$raw_race<- ifelse(tx$rawrace == "H", "hispanic",
                     ifelse(tx$rawrace == "B", "black",
                            ifelse(tx$rawrace == "I", "indigenous",
                                   ifelse(tx$rawrace == "M", "missing",
                                          ifelse(tx$rawrace == "O", "other",
                                                 ifelse(tx$rawrace == "U", "unknown",
                                                        ifelse(tx$rawrace == "W", "white",
                                                               "NA")))))))