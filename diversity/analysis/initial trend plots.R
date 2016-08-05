library(ggplot)
library(dplyr)

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
