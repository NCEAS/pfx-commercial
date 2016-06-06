# Read in, cleanup, and write out a version of the data

library(dplyr)
load("data/cfecCleaned_new")
names(cfec) <- tolower(names(cfec))
cfec <- as_data_frame(cfec)
names(cfec) <- gsub("\\.", "_", names(cfec))
cfec$year <- as.numeric(cfec$year)
cfec$specn_orig <- as.numeric(cfec$specn_orig)

deflationTable = data.frame(year = 1985:2014,
  defl = c(0.57236, 0.58393, 0.59879, 0.61974,
    0.64388, 0.66774, 0.68993, 0.70564, 0.72244, 0.73781,
    0.75321, 0.76695, 0.78009, 0.78855, 0.80061,
    0.81883, 0.83753, 0.85038, 0.86729, 0.89114,
    0.91981, 0.94812, 0.97334, 0.9925, 1,
    1.01217, 1.03307, 1.05213, 1.06926, 1.08682))

# Adjust price and g_earn for inflation
cfec$year <- as.numeric(cfec$year)
cfec$day <- substr(cfec$landdate, 6, 10)
cfec <- inner_join(cfec, deflationTable)
cfec <- mutate(cfec, g_price = g_price / defl, g_earn = g_earn / defl)
cfec$defl <- NULL


regions <- read.csv("data/regions.csv", stringsAsFactors = FALSE)
cfec <- left_join(cfec, regions)

groupings <- read.csv("data/species-groupings.csv", stringsAsFactors = FALSE)
names(groupings) <- tolower(names(groupings))
names(groupings) <- gsub(" \\(spec\\)", "", names(groupings))
groupings <- select(groupings, specn, group1)
groupings <- rename(groupings, taxa_broad = group1)
cfec <- left_join(cfec, groupings)
cfec <- mutate(cfec, salmon = ifelse(spec %in%
  c("CHNK", "SOCK", "COHO", "PINK", "CHUM"), TRUE, FALSE))
cfec <- mutate(cfec, pollock = ifelse(spec == "PLCK", TRUE, FALSE))

# devtools::install_github("wesm/feather/R")
feather::write_feather(cfec, "data/cfec.feather")

# saveRDS(cfec, file = "../data/cfec.rds")
# system.time({d <- readRDS("../data/cfec.rds")})
# system.time({d <- feather::read_feather("../data/cfec.feather")})
