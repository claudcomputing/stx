library(shiny)
library(tidyverse)
library(ggmap)
library(proj4)
library(sf)
library(rgdal)
library(sp)
library(leaflet)
library(scales)
library(ggplot2)
library(broom)
library(knitr)
#Read-in Data. Make sure data is saved in a folder 'data'
data <- read_csv("data/sqf_08_16.csv")
data <- data %>% filter(year >= 2012 & year <= 2016)

#Convert original data coordinates to longitude and latitude
nyc.proj = "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0"

coords_all <- proj4::project(list(data$xcoord, data$ycoord), nyc.proj, inverse=TRUE)
data$lat <- coords_all$y
data$lon <- coords_all$x
rm(coords_all)

#Filter to data points in official NYC limits, remove data points without longitude & latitude coordinates
data <- data %>% 
  filter(lon >= -74.257159 & lon <= -73.699215,
         lat >= 40.495992 & lat <= 40.915568,
         is.na(lon) != TRUE,
         is.na(lat) != TRUE)

#Replace NAs in suspect.sex and suspect.race columns as 'unknown'
#Factor race so that it can be used as colors in the Rshiny app;
#Convert Staten Island borough indicator to common notation ("STATEN ISLAND")

data$suspect.sex[is.na(data$suspect.sex)] <- 'unknown'
data$suspect.race[is.na(data$suspect.race)] <- 'unknown'
data$suspect.weight[is.na(data$suspect.weight)] <- mean(data$suspect.weight, na.rm = T)

data <- data %>%
        mutate(suspect.race = as.factor(suspect.race),
               suspect.sex = as.factor(suspect.sex),
               found.weapon = as.factor(data$found.weapon),
               suspect.build.f = as.factor(suspect.build),
               suspect.age.sc = as.numeric(scale(suspect.age, center=TRUE, scale =TRUE)),
               suspect.height.sc = as.numeric(scale(suspect.height, center=TRUE, scale=TRUE)),
               suspect.weight.sc = as.numeric(scale(suspect.weight, center=TRUE, scale=TRUE)),
               city = ifelse(city == "STATEN IS", "STATEN ISLAND",city))

#found contraband (numerator of hit rate)
data$found.true <- NA
data$found.true <- ifelse(data$found.contraband == T, T, ifelse(data$found.pistol ==T, T, ifelse(data$found.rifle==T, T, ifelse(data$found.assault == T, T, ifelse(data$found.knife== T, T, ifelse(data$found.machinegun==T, T, ifelse(data$found.other== T, T, ifelse(data$found.gun == T, T, ifelse(data$found.weapon==T, T, F)))))))))

#type of contraband found
data$contraband <- NA
data$contraband <- ifelse(data$found.contraband == T, "other.contraband", ifelse(data$found.pistol ==T, "pistol", ifelse(data$found.rifle==T, "rifle", ifelse(data$found.assault == T, "assault", ifelse(data$found.knife== T, "knife", ifelse(data$found.machinegun==T, "machine.gun", ifelse(data$found.other== T, "other.weapon", ifelse(data$found.gun == T, "gun", ifelse(data$found.weapon==T, "other.weapon", "none.found")))))))))

## contraband T/F
data$contraband_count <- data$found.contraband + data$found.pistol + data$found.rifle + data$found.assault + data$found.knife + data$found.machinegun + data$found.other + data$found.gun

#type of force used
data$force.used <- NA
data$force.used <- ifelse(data$force.hands == T, "hands", ifelse(data$force.wall ==T, "wall", ifelse(data$force.ground==T, "ground", ifelse(data$force.drawn == T, "weapon.drawn", ifelse(data$force.pointed== T, "weapon.pointed", ifelse(data$force.baton=="baton", T, ifelse(data$force.handcuffs== T, "handcuffs", ifelse(data$force.pepper == T, "pepper.spray", ifelse(data$force.other==T, "other.force", "no.force.used")))))))))

#why stopped
data$why.stopped <- NA
data$why.stopped <- ifelse(data$stopped.bc.object == T, "object", ifelse(data$stopped.bc.desc ==T, "desc", ifelse(data$stopped.bc.casing==T, "casing", ifelse(data$stopped.bc.lookout == T, "lookout", ifelse(data$stopped.bc.clothing== T, "clothing", ifelse(data$stopped.bc.drugs=="drugs", T, ifelse(data$stopped.bc.furtive== T, "furtive", ifelse(data$stopped.bc.violent == T, "violent", ifelse(data$stopped.bc.bulge==T, "bulge", ifelse(data$stopped.bc.other==T,"other.reason", "no.force.used"))))))))))

#why frisked
data$why.frisked <- NA
data$why.frisked <- ifelse(data$frisked.bc.suspected.crime == T, "suspected.crime", ifelse(data$frisked.bc.weapons ==T, "weapons", ifelse(data$frisked.bc.attire==T, "attire", ifelse(data$frisked.bc.actual.crime == T, "actual.crime", ifelse(data$frisked.bc.noncompliance== T, "noncompliance", ifelse(data$frisked.bc.threats==T, "threats", ifelse(data$frisked.bc.prior== T, "prior", ifelse(data$frisked.bc.furtive == T, "furtive", ifelse(data$frisked.bc.bulge==T, "bulge", ifelse(data$stopped.bc.other==T,"other.reason",ifelse(data$frisked==T,"frisked.no.force","no.force.used")))))))))))

#race option 2
data$race<-data$suspect.race
data$race<-ifelse(data$suspect.race=="white", "white", ifelse(data$suspect.race == "black","black",ifelse(data$suspect.race=="asian", "asian", ifelse(data$suspect.race=="hispanic","hispanic", "other"))))
table(data$race)
table(data$city)
table(data$race,data$city)

#CPW or not
data$cpw.ind <- ifelse(data$suspected.crime == "cpw", "CPW", "Not CPW")

####################################### make datasets for widgets ###############################

#Data Frames for Tab Boxes (Counts & Rates) excluding missing suspect.race data

data_output1 <- data %>%
  filter(!is.na(suspect.race)) %>%
  group_by(year, city, suspect.race) %>%
  summarize(frisked_rate = sum(frisked)/n(),
            arrested_rate = sum(arrested)/n()) %>%
  ungroup() %>%
  data.frame()

rate_frisk <- data.frame(year = data_output1$year, city = data_output1$city, 
                         suspect.race = data_output1$suspect.race, frisked_rate = data_output1$frisked_rate)

rate_arrest <- data.frame(year = data_output1$year, city = data_output1$city, 
                          suspect.race = data_output1$suspect.race, arrested_rate = data_output1$arrested_rate)

#Data frame for Predicted Probability Plot
glm_data <- data %>% filter(suspected.crime == "cpw") %>% mutate(year.f = factor(year, level = c(2012:2016)))

glm.found.t <- glm(found.weapon ~ suspect.age + suspect.height + suspect.weight + suspect.build +
                     suspect.sex + year.f*suspect.race, data = glm_data, family = "binomial")

glm.found <- glm(found.weapon ~ suspect.age + suspect.height + suspect.weight + suspect.build +
                   suspect.sex + year*suspect.race, data = glm_data, family = "binomial")

glm_data <- glm_data %>% 
  mutate(predicted = predict(glm.found.t, type = "response"))

# Save the logistic regression result
result <- tidy(glm.found.t) %>% kable()

# Save colors from ggplot defaults

g <- ggplot_build(ggplot(data, aes(fill = suspect.race, x = force.used), width = 4)+
  geom_bar(position = 'stack', stat = 'count') + 
  labs(x = "Types of Force", y = "Force Used (Count)", fill = "Race") + 
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)))

colors <- unique(g$data[[1]]["fill"])
