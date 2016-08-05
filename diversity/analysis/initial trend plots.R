library(dplyr)
library(knitr)
library(ggplot2)
library(date)

# source strategies
strategies = read.csv("diversity/portfolio_strategies.csv", header=F)

# source data file
source("portfolio/analysis/cull-dat.r")
dat = cullDat(diff=FALSE)

# limit strategies to those in portfolio paper
dat = dat[which(dat$strategy%in%strategies$V1),]

# make time series plot of strategies over time
pdf("diversity/strategies_over_time.pdf")
group_by(dat, year) %>%
  summarize(nstrategy = length(unique(strategy))) %>%
  ggplot(aes(year, nstrategy)) + geom_line() + xlab("Year") +
  ylab("# Strategies") + ggtitle("Strategies from Portfolio Paper")
dev.off()

pdf("diversity/participation_by_strategy.pdf")
group_by(dat, year, strategy) %>%
  summarize(n = n()) %>%
  ggplot(aes(year, n)) + geom_line() + xlab("Year") +
  ylab("People") + ggtitle("Strategies from Portfolio Paper") + facet_wrap(~strategy, scale="free")
dev.off()

# people v boats:

# cfec.feather already inflation adjusted
cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)
# add indicator for single permit landings
cfec = group_by(cfec, year, p_holder) %>%
  mutate(n.permit = length(unique(p_fshy))) %>%
  mutate(permits = ifelse(n.permit==1, "Single","Multiple")) %>%
  select(-n.permit)

# filter out single-salmon permit specialists
cfec = group_by(cfec, year, p_holder) %>%
  mutate(ss = ifelse(length(unique(p_fshy))==1 & substr(p_fshy,1,1)=="S", 1, 0)) %>%
  filter(ss==0)

# diversity function
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

effSpec_people = group_by(cfec, year, p_holder, specn) %>%
  summarize(earn = sum(g_earn)) %>%
  group_by(year, p_holder) %>%
  summarize(effSpec_people = simp.div(earn)) %>%
  group_by(year) %>%
  summarize(effSpec_people = median(effSpec_people,na.rm=T))
effSpec_boat = group_by(cfec, year, cadfg, specn) %>%
  summarize(earn = sum(g_earn)) %>%
  group_by(year, cadfg) %>%
  summarize(effSpec_boat = simp.div(earn)) %>%
  group_by(year) %>%
  summarize(effSpec_boat = median(effSpec_boat,na.rm=T))

effSpec = left_join(effSpec_people, effSpec_boat)
pdf("Trends in diversity by people and boats.pdf")
ggplot(effSpec, aes(year, effSpec_boat, colour="boat")) +
  geom_line() + geom_line(aes(year, effSpec_people, colour="vessel")) +
  xlab("Year") + ylab("Effective species diversity") + ggtitle("Diversity (single permit salmon fishers removed)")
dev.off()

# facet plot of boats / people by strategy
cfec$taxa = substr(cfec$p_fshy,1,1)
pdf("Trends in boats per people by taxa.pdf")
group_by(cfec, year, taxa) %>%
  summarize(nBoat = length(unique(cadfg)),
    nPeople = length(unique(p_holder))) %>%
  ggplot(aes(year, nBoat/nPeople)) + geom_line() +
  facet_wrap(~ taxa, scale="free") + xlab("Year") + ylab("Diversity") + ggtitle("Boats/people by taxa")
dev.off()





