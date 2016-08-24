library(dplyr)
library(knitr)
library(ggplot2)
library(date)

# crab yield versus high value habitat for whales. TRadeoff between crab catch + whale.
# Observed

cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

# pull out herring permits by area.
# A, E, H, K, M, T
# sac, bait, pound fisheries
# L21 TACE = pound fisheries, spawn on kelp

herring = cfec[substr(cfec$p_fshy,1,1) %in% c("L","G"),]

# We restrict the analysis to (1) fisheries with 10 or more years of data, and
# (2) fisheries that at their max had at least 25 people participating
group_by(herring, p_fshy, year) %>%
  summarize(g = sum(g_earn), p = mean(g_price,na.rm=T), lbs = sum(g_pounds), people = length(unique(p_holder))) %>%
  group_by(p_fshy) %>%
  mutate(n = length(unique(year)), maxpeople = max(people,na.rm=T)) %>%
  filter(n > 9) %>%
  filter(maxpeople > 25) %>%
  ggplot(aes(year, people)) + geom_line() + facet_wrap(~ p_fshy, scale="free_y")
# people generally decline to 0, except G01A, G34A, and L21
# total landed catch has either remained flat or increased, so as people exit, the people remaining have
# increased trends in landed catch

group_by(herring, p_fshy, year) %>%
  summarize(g = sum(g_earn), p = mean(g_price,na.rm=T), lbs = sum(g_pounds), people = length(unique(p_holder))) %>%
  group_by(p_fshy) %>%
  mutate(n = length(unique(year)), maxpeople = max(people,na.rm=T)) %>%
  filter(n > 9) %>%
  filter(maxpeople > 25) %>%
  ggplot(aes(p, log(lbs/2000), colour = year)) + geom_point() + facet_wrap(~ p_fshy, scale="free")


roe = cfec[substr(cfec$p_fshy,1,1) %in% c("G"),]

# We restrict the analysis to (1) fisheries with 10 or more years of data, and
# (2) fisheries that at their max had at least 25 people participating
group_by(roe, p_fshy, year) %>%
  summarize(g = sum(g_earn), p = mean(g_price,na.rm=T), lbs = sum(g_pounds), people = length(unique(p_holder))) %>%
  group_by(p_fshy) %>%
  mutate(n = length(unique(year)), maxpeople = max(people,na.rm=T)) %>%
  filter(n > 9) %>%
  filter(maxpeople > 25) %>%
  ggplot(aes(year, p, group = p_fshy, color=p_fshy)) + geom_line()
