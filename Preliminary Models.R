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

# load data ---------------------------------------------------------------

load("Texas1MSample_Merged.Rdata")

tx_merged[mc!= "H-O", misid := 1*(mc == "H-W")]

model1 <-
  glm(
    misid ~ prop_black + prop_hisp + 
      prop_white + prop_urban + median_age_dec_2010 +
      median_hh_income_acs_2017 + subject_sex + factor(district),
    data = tx_merged,
    family = "binomial"
  )

model2 <-
  glm(
    misid ~ prop_black + prop_hisp + 
      prop_white + prop_urban + median_age_dec_2010 +
      median_hh_income_acs_2017 + subject_sex + factor(county),
    data = tx_merged,
    family = "binomial"
  )
