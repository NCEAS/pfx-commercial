library(dplyr)
revenueThreshold = 10000

dat.diff = readRDS(file="portfolio/data-generated/cfec-diff-for-modeling.rds")

length(unique(dat.diff$p_holder))

dat.diff = group_by(dat.diff, p_holder) %>%
  mutate(medRev = median(revenue, na.rm=T)) %>%
  filter(medRev >= revenueThreshold) %>%
  select(-medRev)

length(unique(dat.diff$p_holder))

dat.salmon = dat.diff[dat.diff$strategy_permit%in%c("S01A", "S01E", "S01K", "S03A", "S03E","S01M"),]
length(unique(dat.salmon$p_holder))

# Restrict analysis to people who keep same strategy - complicates days, etc
dat.salmon = dat.salmon[which(dat.salmon$strategy_permit==dat.salmon$strategy_permit.prev),]

dat.salmon$days.change = log((dat.salmon$days_permit+1) / (dat.salmon$days_permit.prev+1))
dat.salmon$year_permit = as.factor(paste(dat.salmon$year, dat.salmon$strategy_permit))

length(unique(dat.salmon$p_holder))

saveRDS(dat.salmon, file="salmon/data-generated/salmon.rds")
