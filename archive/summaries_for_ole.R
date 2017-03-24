

cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

cfec = cfec[which(substr(cfec$p_fshy,1,1) %in% c("M","B","C")),]

cfec = cfec[which(cfec$region%in%c("Kodiak","PWS","Alaska Peninsula","Cook Inlet")),]

# 99.4% of the records are made up of B/C/M permits, 361619 / 363789
library(dplyr)
g = group_by(cfec, year, stat6) %>%
  summarize(pounds = sum(g_pounds))

# roll these stat6 areas into Ole's regions
#names(areas)[which(names(areas)=="STAT6")] = "stat6"
#g = left_join(g, areas)
#totPounds = group_by(g, AREA, year) %>%
#  summarize(totPounds = sum(pounds))

write.table(g, "pounds_by_stat6.csv", sep=",", row.names=FALSE)
