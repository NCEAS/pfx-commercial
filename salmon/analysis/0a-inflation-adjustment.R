# Read in, cleanup, and write out a version of the data

library(dplyr)
load("data/cfecCleaned2_salmon.Rdata")
names(cfec) <- tolower(names(cfec))
cfec <- as_data_frame(cfec)
names(cfec) <- gsub("\\.", "_", names(cfec))
cfec$year <- as.numeric(cfec$year)
cfec$specn_orig <- as.numeric(cfec$specn_orig)

deflationTable <- read.csv("/users/eric.ward/documents/CFEC/data/deflation.csv")

# Adjust price and g_earn for inflation
cfec$year <- as.numeric(cfec$year)
cfec$day <- substr(cfec$landdate, 6, 10)
cfec <- inner_join(cfec, deflationTable)
cfec <- mutate(cfec, g_price = g_price / defl, g_earn = g_earn / defl)
cfec$defl <- NULL

groupings <- read.csv("data/species-groupings.csv", stringsAsFactors = FALSE)
names(groupings) <- tolower(names(groupings))
names(groupings) <- gsub(" \\(spec\\)", "", names(groupings))
groupings <- select(groupings, specn, group1)
groupings <- rename(groupings, taxa_broad = group1)
cfec <- left_join(cfec, groupings)
cfec <- mutate(cfec, salmon = ifelse(spec %in%
  c("CHNK", "SOCK", "COHO", "PINK", "CHUM"), TRUE, FALSE))
cfec <- mutate(cfec, pollock = ifelse(spec == "PLCK", TRUE, FALSE))

region <- read.csv("data/regions.csv", stringsAsFactors = FALSE)

region <- region[,c("stat6","final")] #don’t include “assigned by” column
names(region)[2] = "region"
cfec$region <- NULL
cfec$stat6 = as.numeric(cfec$stat6)
cfec <- left_join(cfec, region)

# devtools::install_github("wesm/feather/R")
feather::write_feather(cfec, "portfolio/data-generated/salmon.feather")
