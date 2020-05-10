# this code is for a preliminary looksy at the texas state trooper data   

# set working directory -------------
setwd("/Users/MVERAS/Documents/Data Driven Eval/Data/")

# load packages -----------------------------------------------------------
library(data.table)
library(tableone)
library(stringr)
library(quanteda)

# read in dataset ---------------------------------------------------------

# statewide dataset is huge - careful loading and running code 

# texas_raw <- readRDS("texas_race_sex.rds")

# keep only data
# tx <- as.data.table(texas_raw$data)
# remove(texas_raw)

# save file
# save(tx, file = "texas_raw_data_only.rdata")


#keep only cases with lat and long
# tx_geo <- tx[!(is.na(lat)| is.na(lng)) , ] 
# remove(tx) 
#note that cases without lat long can be geocoded by milepost  

# save file
# save(tx_geo, file = "texas_raw_lat_long_only.rdata")

# load texas file
# load("texas_raw_lat_long_only.rdata")


# sample texas file -------------------------------------------------------

set.seed(11)
s <- sample(nrow(tx_geo), 1000000, replace = F)
tx <- tx_geo[s]

remove(tx_geo)

# basic descriptives ------------------------------------------------------------

dput(names(tx))
varlist <- (names(tx))

# c("raw_row_number", "date", "time", "location", "lat", "lng", 
#   "county_name", "district", "precinct", "region", "subject_race", 
#   "subject_sex", "officer_id", "officer_id_hash", "officer_last_name", 
#   "type", "violation", "citation_issued", "warning_issued", "outcome", 
#   "contraband_found", "contraband_drugs", "contraband_weapons", 
#   "search_conducted", "search_vehicle", "search_basis", "vehicle_color", 
#   "vehicle_make", "vehicle_model", "vehicle_type", "vehicle_year", 
#   "raw_HA_RACE_SEX", "raw_HA_SEARCH_PC_boolean", "raw_HA_SEARCH_CONCENT_boolean", 
#   "raw_HA_INCIDTO_ARREST_boolean", "raw_HA_VEHICLE_INVENT_boolean"
# )

#these are all vehicle stops
#no summons or arrests in file
#note violation is a really messy text field - could put some work into clean if worht it

#taking out variables from the descriptive table that have too many categories (e.g. violation)
#note file includes the officer last name which could be useful... 
#vehicle make includes peoples addresses.... 

catvars <- c(
  "county_name", "district", "precinct", "region", "subject_race", 
  "subject_sex", 
  "type", "citation_issued", "warning_issued", "outcome", 
  "contraband_found", "contraband_drugs", "contraband_weapons", 
  "search_conducted", "search_vehicle", 
  "search_basis", 
  "vehicle_type", 
  "raw_HA_RACE_SEX", "raw_HA_SEARCH_PC_boolean", "raw_HA_SEARCH_CONCENT_boolean", 
  "raw_HA_INCIDTO_ARREST_boolean", "raw_HA_VEHICLE_INVENT_boolean"
)

#these are the excluded variables
varlist[!(varlist %in% catvars)]
# [1] "raw_row_number"    "date"              "time"             
# [4] "location"          "lat"               "lng"              
# [7] "officer_id"        "officer_id_hash"   "officer_last_name"
# [10] "violation"         "vehicle_color"     "vehicle_make"     
# [13] "vehicle_model"     "vehicle_year"   

CreateTableOne(c(catvars, "date", "time", "lat", "lng"), data = tx, factorVars = catvars)


# Look at misclassification cases -----------------------------------------
tx[, rawrace := substring(raw_HA_RACE_SEX, 1,1)]
table(tx$rawrace)

tx[, rawsex := substring(raw_HA_RACE_SEX, 2,2)]
table(tx$rawsex)

table(tx$rawrace, tx$subject_race)

# asian/pacific islander  black hispanic  white  other unknown
# A                  16296      0        0      0      0       0
# B                      0  96222        0      0      0       0
# H                      0      0   143757      0      0       0
# I                      0      0      388      0   1368       0
# M                      0      0       20    926      0       0
# O                      0      0      117      0    337       0
# U                      0      0    26404      0      0   29728
# W                      0      0   139862 544569      0       0

# code hispanic by name cases 
tx[subject_race == "hispanic" & rawrace == "H", misclass := "Hisp - coded Hisp" ]
tx[subject_race == "hispanic" & rawrace == "I", misclass := "Hisp - coded Indig" ]
tx[subject_race == "hispanic" & rawrace == "M", misclass := "Hisp - coded M" ]
tx[subject_race == "hispanic" & rawrace == "O", misclass := "Hisp - coded Other" ]
tx[subject_race == "hispanic" & rawrace == "U", misclass := "Hisp - coded Unknown" ]
tx[subject_race == "hispanic" & rawrace == "W", misclass := "Hisp - coded White" ]
table(tx$misclass)


#collapsed categories - ignore the I and M cases - join the U and O cases
tx[subject_race == "hispanic" & rawrace == "W", mc := "H-W" ]
tx[subject_race == "hispanic" & rawrace == "H", mc := "H-H" ]
tx[subject_race == "hispanic" & rawrace %in% c("O", "U"), mc := "H-O" ]
table(tx$mc)

options(max.print = 3000)
#all three cats
CreateTableOne(c(catvars, "date", "time", "lat", "lng"), strata = "mc", data = tx, factorVars = catvars)

#just two cats
CreateTableOne(c(catvars, "date", "time", "lat", "lng"), strata = "mc", data = tx[mc != "H-O"], factorVars = catvars)


# looking at the time piece -----------------------------------------------

table(year(tx$date), tx$mc)

#       H-H   H-O   H-W
# 2007   112     0     0
# 2008  6711     0     0
# 2009 11028  4481 28103
# 2010 10574  4363 26547
# 2011 13797  4201 24848
# 2012 15092  3597 20069
# 2013 14669  2628 14904
# 2014 17260  2528 13179
# 2015 18893  4723 10021
# 2016 35621     0  2191

# save interim tx sample file -----------------------------------------------------
# save(tx, file = "Texas1MSample.Rdata")

# continue cleaning -------------------------------------------------------
load("Texas1MSample.Rdata")

# officer last name hispanic indicator -------------------------------------
surnames <- fread("Top1000LastNames.csv")

View(table(tx$officer_last_name))

tx$ln <- sapply(strsplit(tx$officer_last_name, " "), function(x) x[which.max(nchar(x))])
tx[ , ln := gsub('[.,]', "", toupper(ln))]

surnames$name <- toupper(surnames$name)
surnames[, hispname := 1*(pcthispanic > 75)]
surnames <- surnames[hispname == 1, c("name", "hispname")]

tx <- merge(tx, surnames, by.x = "ln", by.y = "name", all.x = T)
tx[, officer_last_name_hisp := 1*(!is.na(hispname))]

# recode violations --------------------------------------------------------
# tx$violation
# violationtext <- dfm(tx$violation, remove = stopwords("english"), remove_punct = TRUE, stem = T)  
# topfeatures(violationtext)
# 
# violationcolloc <- (textstat_collocations(tx$violation))
# violationcolloc3 <- (textstat_collocations(tx$violation, size =3))

violationtable <- str_split(tx$violation, "[|]")
# violationtable <- as.vector(unlist(violationtable))

# i used this violation table to recode violations

tx[, viol_alcohol := 1 *(grepl("Open Container",violation))]
tx[, viol_belt := 1 * (grepl("Safety Belt|Safety Seat",violation))]
tx[, viol_drug := 1*(grepl("drug|controlled substance", violation, ignore.case = T))]
tx[, viol_dui := 1*(grepl("dwi", violation, ignore.case = T))]
tx[, viol_lamp := 1*(grepl("FMVSS 571.108|Lamp", violation, ignore.case = T)) ]
tx[, viol_license := 1*(grepl("DL|license invalid|expired license|Fail To Report Change |failure to display driver|Expired Driver License", violation, ignore.case = T)) ]
tx[, viol_plate := 1*(grepl("license plate", violation, ignore.case = T)) ]
tx[, viol_registration := 1*(grepl("registration|register|financial responsibility|inspection", violation, ignore.case = T)) ]
tx[, viol_speed := 1*(grepl("speed", violation, ignore.case = T)) ]
tx[, viol_traffic := 1*(grepl("passing|following|signal|stop sign|shoulder|single lane|move over|without lights|disregard|change lane|fail to yield|fail to signal|impeding|controlled access highway|fail to keep|fail to dim|fail to move", violation, ignore.case = T)) ]
tx[, viol_mod := 1*(grepl("vision reducing|glass coating|mud flap|unauthorized sunscreening|tow unregistered trailer|safety chains|obstructed view|equipment not approved", violation, ignore.case = T)) ]

save(tx, file = "Texas1MSampleClean.Rdata")
