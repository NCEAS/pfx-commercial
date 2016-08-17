library(dplyr)

cullDat = function(rev_threshold = 10000, npholders_thres1 = 50,
  npholders_thres2 = 100) {

dat <- readRDS(file="portfolio/data-generated/cfec-annual-for-modeling.rds")

### Eric's culling:
#1. We restricted our analysis to p_holders who fished for 5 or more years
#dat <- group_by(dat, p_holder) %>%
#  mutate(nyr = length(unique(year))) %>%
#  mutate(range_div = diff(range(specDiv))) %>%
#  as_data_frame() %>%
#  filter(nyr >= 5)

  # Filters: remove people-year combinations making < $rev_threshold

  ndat <- list()
  ndat$rev_b4_large_only <- sum(dat$revenue)
  # dat3 = dat[which(dat$revenue >= rev_threshold), ]
  dat3 <- as_data_frame(dat) %>% group_by(p_holder) %>%
    mutate(med_rev = median(revenue)) %>%
    filter(med_rev >= rev_threshold) %>% # as in K and H PNAS
    select(-med_rev) %>%
    as_data_frame()

  ndat$large_only <- nrow(dat)
  ndat$rev_large_only <- sum(dat$revenue)

  # note: grouping here is based on strategies defined by permits
  dat$strategy = dat$strategy_permit
  dat <- group_by(dat, strategy) %>%
    mutate(npeople = length(unique(p_holder)))

  dat[dat$length<15 & !is.na(dat$length), ]$length <- NA

  dat <- dat %>% group_by(boat) %>%
    mutate(median_length_boat = median(length, na.rm = T))
  # filter(dat, is.na(length) & !is.na(median_length_boat))$p_holder %>% unique

  dat <- dat %>% group_by(strategy) %>%
    mutate(median_length_permit = median(length, na.rm = T))
  # filter(dat, is.na(length) & is.na(median_length))$p_holder %>% unique

  dat <- dat %>% mutate(length = ifelse(is.na(length), median_length_boat, length))
  dat <- dat %>% mutate(length = ifelse(is.na(length), median_length_permit, length))

  #2. We created 'strategies' by concatenating
  # all permits fished, and only retaining 'strategies' with >= 50 people
  top_strategies = group_by(dat, strategy) %>%
    summarize(n = length(unique(p_holder)),
      earn=sum(revenue)) %>%
    filter(n>=npholders_thres1)
  nstrat <- list()
  nstrat$enough_pholders <- nrow(top_strategies)
  ndat$rev_enough_pholders <- sum(top_strategies$earn)

  # A few of these can be deleted (small in river fisheries, experimental, etc)
  top_strategies = top_strategies[-which(top_strategies$strategy %in% c("S04X","S04Y","Z12B","R18B", "Z12B")),]
  nstrat$no_river_exp <- nrow(top_strategies)
  ndat$rev_no_river_exp <- sum(top_strategies$earn)

  #3. We then tabulated the permits that make up these strategies...
  top_permits = data.frame("orig"=names(table(unlist(lapply(top_strategies$strategy, strsplit, " ")))))
  nstrat$top_permits <- nrow(top_permits)
  print("n permits raw:")
  print(nrow(top_permits))

  # 4. of these top permits, we proceeded to group permits that were targeting a single species
  # and other than area, were otherwise the same. In other words, someone fishing herring roe
  # in kodiak (G34K) and alaska peninsula (G34M) have a herring roe strategy. This combining
  # reduces permits to 59
  top_permits$new = as.character(top_permits$orig)
  top_permits$new[which(substr(top_permits$new,1,3)%in%c("D09","D9C","D9D"))] = "D09" # dungeness
  top_permits$new[which(substr(top_permits$new,1,2)%in%c("D9"))] = "D09" # dungeness
  top_permits$new[which(substr(top_permits$new,1,3)=="G01")] = "G01" # herring roe, purse seine
  top_permits$new[which(substr(top_permits$new,1,3)=="G34")] = "G34" # herring roe, gillnet
  top_permits$new[which(substr(top_permits$new,1,3)=="K91")] = "K91" # king crab
  top_permits$new[which(substr(top_permits$new,1,3)=="L12")] = "L12" # herring, hand pick
  top_permits$new[which(substr(top_permits$new,1,3)=="L21")] = "L21" # herring, pound
  top_permits$new[which(substr(top_permits$new,1,3)=="P09")] = "P09" # shrimp pot gear
  top_permits$new[which(substr(top_permits$new,1,3)=="Q11")] = "Q11" # sea cucumber
  top_permits$new[which(substr(top_permits$new,1,3)=="T09")] = "T09" # tanner crab
  top_permits$new[which(substr(top_permits$new,1,3)=="T91")] = "T09" # tanner crab (larger boats)

  # 5. Combining multispecies finfish permits (M). There are 8 miscellaneous finfish permits (M)
  # remaining, and only 2 can be combined: M26B/G (mechanical jig, GOA/statewide)
  top_permits$new[which(substr(top_permits$new,1,3)=="M26")] = "M26" # mechanical jig

  # combine northern Southeast and statewide sablefish permits:
  top_permits$new[top_permits$new=="C61A"] = "C61B"

  # combine vessel sizes across King crab:
  top_permits$new[top_permits$new=="K09Z"] = "K91"

  # combine vessel sizes across shrimp:
  top_permits$new[top_permits$new=="P91A"] = "P09"

  # combine vessel sizes for otter trawl finfish statewide:
  top_permits$new[top_permits$new=="M7IB"] = "M7HB"

  # combine vessel sizes for longline finfish statewide:
  top_permits$new[top_permits$new=="M6AB"] = "M61B"

  # combine vessel sizes for pot gear statewide:
  top_permits$new[top_permits$new=="M09B"] = "M91B"

  # merge in GOA pot gear finfish:
  top_permits$new[top_permits$new=="M09G"] = "M91B"

  # combine vessel sizes for longline finfish statewide:
  top_permits$new[top_permits$new=="M06B"] = "M61B"

  # combine otter trawl finfish sizes:
  top_permits$new[top_permits$new=="M07B"] = "M7HB"

  # combine sea urchin permits:
  top_permits$new[top_permits$new=="U12B"] = "U11A"

  # 6. Combine some of the longline categories based on vessel size.
  top_permits$new[top_permits$new=="C06B"] = "C61B"
  top_permits$new[top_permits$new=="B06B"] = "B61B"
  top_permits$new[top_permits$new=="S05B"] = "S15B"

  # 7. Combining salmon permits is a little more difficult, because the species composition
  # varies widely based on geography. For the drift gillnet (S03) permits, we
  # combined S03 permits from Cook Inlet, Bristol Bay, and Alaska Peninsula because sockeye
  # represents > 92% of species revenue
  top_permits$new[which(top_permits$new%in%c("S03H","S03M","S03T"))] = "S03" # drift gillnet
  # For the purse seine permits (S01), again there's wide variety in species landings by
  # area (85% pink in PWS to 85% sockeye on alaska peninsula). Of the 5 permits, only 2 had
  # similar species compositions, so we grouped Kodiak and Cook Inlet
  top_permits$new[which(top_permits$new%in%c("S01H","S01K"))] = "S01" # purse seine
  # For the set gillnet, S04 permits, there's some variety but also clear sockeye specialists. We
  # grouped permits that had > 80% sockeye
  top_permits$new[which(top_permits$new%in%c("S04E","S04H","S04K","S04M","S04T"))] = "S04a" # purse seine
  # We grouped permits from norton sound and kuskokwim because of similar species %
  top_permits$new[which(top_permits$new%in%c("S04W","S04Z"))] = "S04b"
  # There's a few remaining permits (S04D, S04X, S04P, S04Y) - but they're so different they can't be grouped

#8. combine the new permit groupings into new strategies
top_permits$orig = as.character(top_permits$orig)


top_strategies$new.strategy = NA
for(i in 1:nrow(top_strategies)) {
  top_strategies$new.strategy[i] = paste(top_permits$new[match(lapply(lapply(top_strategies$strategy, strsplit, " "), unlist)[[i]],
  top_permits$orig)], collapse=" ")
}
top_strategies$new.strategy <- sub("K91 K91", "K91", top_strategies$new.strategy)


# join this into the data frame -- this is leaving ~ 70 strategies
dat = left_join(dat, top_strategies) %>% select(-c(n, earn))
dat$strategy=dat$new.strategy

ndat$b4_no_na_strat <- nrow(dat)
ndat$rev_b4_no_na_strat <- sum(dat$revenue)
dat = dat[is.na(dat$strategy)==FALSE,]
ndat$after_no_na_strat <- nrow(dat)
ndat$rev_after_no_na_strat <- sum(dat$revenue)

# 9. Remove data points where people only did a strategy in one year. Below
# we try to include people-strategy random effects
# ndat$before_nosingles <- nrow(dat)
# dat = group_by(dat, strategy, p_holder) %>%
#   mutate(nsp = n()) %>% filter(nsp > 1) %>% select(-nsp)
# ndat$final <- nrow(dat)
# ndat$rev_final <- sum(dat$revenue)

# and now calculate previous year statistics:
# Also merge in previous revenue. Idx stores index of people last year
idx = match(paste(dat$p_holder,dat$year-1), paste(dat$p_holder,dat$year))
dat$revenue.prev = dat$revenue[idx]
dat$specdiv.prev = dat$specDiv[idx]
dat$strategy.prev = dat$strategy[idx]
dat_diff = dat[!is.na(dat$specdiv.prev), ] # remove rows without a previous value
nrow(dat_diff)
ndat$diff <- nrow(dat_diff)
ndat$rev_diff <- sum(dat_diff$revenue)

# remove people that switch strategies from one year to the next:
dat_diff = dat_diff[dat_diff$strategy == dat_diff$strategy.prev, ]
ndat$diff_same_strategy <- nrow(dat_diff)
ndat$rev_diff_same_strategy <- sum(dat_diff$revenue)

# and after all the aggregating, remove strategies with less than one hundred people:
dat_diff <- as_data_frame(dat_diff) %>% group_by(strategy) %>%
  mutate(n_pholders = length(unique(p_holder))) %>%
  filter(n_pholders >= npholders_thres2) %>%
  as_data_frame()
ndat$after_pholder_thresh2 <- nrow(dat_diff)
ndat$rev_after_pholder_thresh2 <- sum(dat_diff$revenue)

dat_diff <- filter(dat_diff, !is.na(specDiv)) # 2 rows slipped through somehow

# if species diversity changes in less than one percent
# of the people-year combinations, then assume these may be data entry errors
# this should affect about 2 of the strategies:
dat_diff <- mutate(dat_diff, spec_change = log(specDiv/specdiv.prev))
dat_diff <- group_by(dat_diff, strategy) %>%
  mutate(n0 = sum(spec_change == 0)/n()*100) %>%
  filter(!(n0 > 99 & spec_change != 0))

# clean up:
dat_diff <- as_data_frame(dat_diff) %>%
  select(p_holder, year, days_permit, days_permit.prev, revenue, revenue.prev,
    length, strategy, specDiv, specdiv.prev)

nstrat$final <- length(unique(dat_diff$strategy))

saveRDS(ndat, file = "portfolio/data-generated/ndat-cull.rds")
saveRDS(nstrat, file = "portfolio/data-generated/nstrat-cull.rds")

dat_diff
}
