cullDat = function(diff = FALSE) {

if(diff==FALSE) dat = readRDS(file="portfolio/data-generated/cfec-annual-for-modeling.rds")
if(diff==TRUE) dat = readRDS(file="portfolio/data-generated/cfec-diff-for-modeling.rds")

### Eric's culling:
#1. We restricted our analysis to p_holders who fished for 5 or more years
#dat <- group_by(dat, p_holder) %>%
#  mutate(nyr = length(unique(year))) %>%
#  mutate(range_div = diff(range(specDiv))) %>%
#  as_data_frame() %>%
#  filter(nyr >= 5)

# Filters: remove people-year combinations making < $5000
dat = dat[which(dat$revenue >= 10000), ]

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


#2. For each person-year combination, we created 'strategies' by concatenating
# all permits fished, and only retaining 'strategies' with >= 200 data points. Previously,
# this was done by people-year, but Eric changed this to be including > 200 people / strategy
top_strategies = group_by(dat, strategy) %>%
  summarize(n = length(unique(p_holder)),
    earn=sum(revenue)) %>%
  filter(n>=100) # note -- the code should work fine with this >= 100 too

# A few of these can be deleted (small in river fisheries, experimental, etc)
top_strategies = top_strategies[-which(top_strategies$strategy %in% c("S04X","S04Y","Z12B","R18B")),]

#3. We then tabulated the permits that make up these strategies, and there are only
# 56.
top_permits = data.frame("orig"=names(table(unlist(lapply(top_strategies$strategy, strsplit, " ")))))

# 4. of these top permits, we proceeded to group permits that were targeting a single species
# and other than area, were otherwise the same. In other words, someone fishing herring roe
# in kodiak (G34K) and alaska peninsula (G34M) have a herring roe strategy. This combining
# reduces permits to 59
top_permits$new = as.character(top_permits$orig)
top_permits$new[which(substr(top_permits$new,1,3)%in%c("D09","D9C","D9D"))] = "D09" # dungeness
top_permits$new[which(substr(top_permits$new,1,3)=="G01")] = "G01" # herring roe, purse seine
top_permits$new[which(substr(top_permits$new,1,3)=="G34")] = "G34" # herring roe, gillnet
top_permits$new[which(substr(top_permits$new,1,3)=="K91")] = "K91" # king crab
top_permits$new[which(substr(top_permits$new,1,3)=="L12")] = "L12" # herring, hand pick
top_permits$new[which(substr(top_permits$new,1,3)=="L21")] = "L21" # herring, pound
top_permits$new[which(substr(top_permits$new,1,3)=="P09")] = "P09" # shrimp pot gear
top_permits$new[which(substr(top_permits$new,1,3)=="Q11")] = "Q11" # sea cucumber
top_permits$new[which(substr(top_permits$new,1,3)=="T09")] = "T09" # tanner crab

# 5. Combining multispecies finfish permits (M). There are 8 miscellaneous finfish permits (M)
# remaining, and only 2 can be combined: M26B/G (mechanical jig, GOA/statewide)
top_permits$new[which(substr(top_permits$new,1,3)=="M26")] = "M26" # mechanical jig

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
top_strategies$new.strategy[which(top_strategies$new.strategy=="K91 K91 T91Q")] = "K91 T91Q"

# join this into the data frame -- this is leaving ~ 70 strategies
dat = left_join(dat, top_strategies) %>% select(-c(n, earn))
dat$strategy=dat$new.strategy
dat = dat[is.na(dat$strategy)==FALSE,]

# 9. Remove data points where people only did a strategy in one year. Below
# we try to include people-strategy random effects
dat = group_by(dat, strategy, p_holder) %>%
  mutate(nsp = n()) %>% filter(nsp > 1) %>% select(-nsp)

# Derived variables
scale2 <- function(x) {
  x <- x - mean(x, na.rm = TRUE)
  x / (2 * sd(x, na.rm = TRUE))
}
dat$log_spec_div <- scale2(log(dat$specDiv))
dat$scaled_spec_div <- scale2(dat$specDiv)
dat$log_length <- scale2(log(dat$length + 1))
dat$log_weight <- scale2(log(dat$weight + 1))
dat$log_days <- scale2(log(dat$days + 1))
dat$log_npermit <- scale2(log(dat$npermit))
dat$log_days_permit <- scale2(log(dat$days_permit+1))

return(dat)
# ggplot(dat, aes(strategy, log10(length))) + geom_boxplot() + coord_flip()
}
