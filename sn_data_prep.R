## Install packages if needed
##install.packages("bibliometrix")
##install.packages("tidyverse")

## Load needed packages
library(bibliometrix)
library(tidyverse)

## Read in data files downloaded from Web of Science
filePathAB <- dir("./WoS_data", pattern = "*.bib", recursive = TRUE, full.names = TRUE)
DAB <- do.call("readFiles", as.list(filePathAB))
MAB <- convert2df(DAB, dbsource = "wos", format = "bibtex")

## Read in data files downloded from Scopus
filePathSc <- dir("./Scopus_data", pattern = "*.bib", recursive = TRUE, full.names = TRUE)
DSc <- do.call("readFiles", as.list(filePathSc))
MSc <- convert2df(DSc, dbsource = "scopus", format = "bibtex")

## Read in TR_J1 report
raw_usage <- read.csv(file = "usage_stats.csv", skip = 13, header = TRUE) 
usage <- filter(raw_usage, Metric_Type == "Total_Item_Requests") %>%
    filter(Platform != "nature.com") %>%
    group_by(Title, Publisher) %>%
    summarise(Reporting_Period_Total = sum(Reporting_Period_Total))
usage$Title <- trimws(str_to_lower(usage$Title))

nature_titles <- filter(raw_usage, Metric_Type == "Total_Item_Requests", Platform == "nature.com") %>%
    select(Title)
nature_titles$Title <- trimws(str_to_lower(nature_titles$Title))

## Read in list of Springer Titles for filtering
SN <- read.csv("2019SpringerNature_title_list.csv", skip = 8, header = TRUE) 
SN$Title <- trimws(str_to_lower(SN$Title, locale = "en"))
Springer_list <- filter(SN, !(Title %in% nature_titles$Title))

## Read in list of abbreviated titles
AbTitles <- read.csv("Abbreviations.csv", header = TRUE)

## Convert abbreviations to lower case and trim whitespace
AbTitles$Abbreviated.title <- trimws(str_to_lower(AbTitles$Abbreviated.title, locale = "en"))
AbTitles$Title <- trimws(str_to_lower(AbTitles$Title, locale = "en"))

filtered_AbTitles <- filter(AbTitles, Abbreviated.title!="not found") %>%
    filter(!(Title %in% nature_titles$Title))

## Get list of Titles in which UAlberta authors publish (WoS)
pubs_WoS <- select(MAB, UT, SO, PY, DT)
colnames(pubs_WoS) <- c("DI", "SO", "PY", "DT")

## Convert Source in publications list to lower case
pubs_WoS$SO <- trimws(str_to_lower(pubs_WoS$SO, locale = "en"))

## Filter to keep only those WoS pubs in SN titles
filtered_pubs_WoS <- filter(pubs_WoS, SO %in% Springer_list$Title)

## Get list of Scopus-indexed publications
pubs_Scopus <- select(MSc, DI, SO, PY, DT)
pubs_Scopus$SO <-trimws(str_to_lower(pubs_Scopus$SO, locale = "en"))
pubs_Scopus <- filter(pubs_Scopus, !(SO %in% nature_titles$Title))


## Add Scopus publications to Web of Science publications
pubs <- bind_rows(filtered_pubs_WoS, pubs_Scopus)
titles <- unique(pubs$SO)

## Create list of pub counts by year
pubcount <- pubs %>%
    count(SO, PY) %>%
    spread(PY, n)

save(filtered_AbTitles, Springer_list, MAB, filtered_pubs_WoS, pubs_Scopus, pubs, pubcount, titles, usage, nature_titles, file = "SNdata.RData")