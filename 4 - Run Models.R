# run some prelim models 



# set working directory -------------
setwd("/Users/MVERAS/Documents/Data Driven Eval/Data/")

# load packages -----------------------------------------------------------
library(data.table)
library(tableone)
library(stringr)
library(quanteda)
library(tigris)
library(sf)
library(magrittr)
library(tidycensus)
library(dplyr)
library(car)

library(geepack)
library(lme4)
library(lmerTest)
library(broom)
# load data ---------------------------------------------------------------

load("Texas1MSampleClean_Merged.Rdata")


# preprocessing -----------------------------------------------------------

tx_merged[mc!= "H-O", misid := 1*(mc == "H-W")]

tx<-tx_merged %>% 
  filter(year(date)<2016 & year(date)> 2008 & mc %in% c("H-W", "H-H")) %>%
  mutate(year=year(date))



# predict misid -----------------------------------------------------------

model1 <-
  glm(
    misid ~ prop_black + prop_hisp + 
      prop_white + prop_urban + median_age_dec_2010 +
      income_acs_2015 + subject_sex + year,
    data = tx,
    family = "binomial"
  )

model2 <-
  glm(
    misid ~ prop_black + prop_hisp + 
      prop_white + prop_urban + median_age_dec_2010 +
      income_acs_2015 + subject_sex + year + officer_last_name_hisp,
    data = tx,
    family = "binomial"
  )


model3 <-
  glm(
    misid ~ prop_black + prop_hisp + 
      prop_white + prop_urban + median_age_dec_2010 +
      income_acs_2015 + subject_sex + year + officer_last_name_hisp +
      viol_belt + viol_drug + viol_alcohol + viol_dui + viol_lamp + 
      viol_license + viol_plate + viol_registration + viol_speed + 
      viol_traffic + viol_mod,
    data = tx,
    family = "binomial"
  )

# vif(model3)
anova(model1, model2, test = "Rao")
anova(model1, model2, model3, test = "Rao")
AIC(model1)
AIC(model2)
AIC(model3)

model3table <- tidy(model3)
write.csv(model3table, file = "logitmodelforppt.csv")
exp(coef(model3))

# predict contraband ------------------------------------------------------


model4 <-
  glm(
    contraband_weapons ~ prop_black + prop_hisp + misid +
      prop_white + prop_urban + median_age_dec_2010 +
      income_acs_2015 + subject_sex ,
    data = tx,
    family = "binomial"
  )

summary(model4)

model5 <-
  glm(
    contraband_drugs ~ prop_black + prop_hisp + misid +
      prop_white + prop_urban + median_age_dec_2010 +
      income_acs_2015 + subject_sex ,
    data = tx,
    family = "binomial"
  )

summary(model5) # minimal difference here running binomial'/ gaussian

model6 <-
  glm(
    citation_issued ~ prop_black + prop_hisp + misid +
      prop_white + prop_urban + median_age_dec_2010 +
      income_acs_2015 + subject_sex ,
    data = tx,
    family = "binomial"
  )
exp(coef(model6))


model7 <-
  glm(
    citation_issued ~ prop_black + prop_hisp + misid +
      prop_white + prop_urban + median_age_dec_2010 +
      income_acs_2015 + subject_sex +
      viol_belt + viol_drug + viol_alcohol + viol_dui + viol_lamp + 
      viol_license + viol_plate + viol_registration + viol_speed + 
      viol_traffic + viol_mod,
    data = tx,
    family = "binomial"
  )
exp(coef(model7))
