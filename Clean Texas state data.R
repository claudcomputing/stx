
#prepare datafile for running regressions 

# set working directory -------------
setwd("/Users/MVERAS/Documents/Data Driven Eval/Data/")


# load data ---------------------------------------------------------------

load("Texas1MSample.Rdata")
surnames <- fread("Top1000LastNames.csv")

# load packages -----------------------------------------------------------
library(data.table)
library(tableone)
library(stringr)
library(quanteda)

# create additional variables ---------------------------------------------

# officer last name hispanic indicator 
View(table(tx$officer_last_name))

tx$ln <- sapply(strsplit(tx$officer_last_name, " "), function(x) x[which.max(nchar(x))])
tx[ , ln := gsub('[.,]', "", toupper(ln))]

surnames$name <- toupper(surnames$name)
surnames[, hispname := 1*(pcthispanic > 75)]
surnames <- surnames[hispname == 1, c("name", "hispname")]

tx <- merge(tx, surnames, by.x = "ln", by.y = "name", all.x = T)
tx[, officer_last_name_hisp := 1*(!is.na(hispname))]

# recode violations
# tx$violation
violationtext <- dfm(tx$violation, remove = stopwords("english"), remove_punct = TRUE, stem = T)  
topfeatures(violationtext)

violationcolloc <- (textstat_collocations(tx$violation))
violationcolloc3 <- (textstat_collocations(tx$violation, size =3))

violationtable <- str_split(tx$violation, "[|]")
violationtable <- as.vector(unlist(violationtable))
