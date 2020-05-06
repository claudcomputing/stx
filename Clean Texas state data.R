#4

#prepare datafile for running regressions 

# set working directory -------------
setwd("/Users/MVERAS/Documents/Data Driven Eval/Data/")
setwd("C:/Users/Claud/Box/ddpe/stx")

# load data ---------------------------------------------------------------

load("C:/Users/Claud/Box/ddpe/stx/dta/Texas1MSample.Rdata")
surnames <- fread("C:/Users/Claud/Box/ddpe/stx/dta/Top1000.xls")
#load("Texas1MSample.Rdata")
#surnames <- fread("Top1000LastNames.csv")

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
set.seed(100)
textplot_wordcloud(violationtext, min_count = 6, random_order = FALSE,
                   rotation = .25, 
                   color = RColorBrewer::brewer.pal(8,"Dark2"))
violationcolloc <- (textstat_collocations(tx$violation))
violationcolloc3 <- (textstat_collocations(tx$violation, size =3))

violationtable <- str_split(tx$violation, "[|]")
violationtable <- as.vector(unlist(violationtable))

write.csv(violationtable, 'C:/Users/Claud/Box/ddpe/stx/dta/Violation Codes.csv')
