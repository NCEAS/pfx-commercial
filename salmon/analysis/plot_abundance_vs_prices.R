library(dplyr)
library(knitr)
library(ggplot2)
library(date)
library(ggtern)
library(reshape2)
library(mgcv)
library(fpc)
library(gridExtra)
library(glmmTMB)

# Plot abundance vs prices for PWS

# Also load in raw CFEC data for plots
cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
cfec = cfec[cfec$p_fshy%in%c("S 01E", "S 03E", "S 04E"),]
cfec$year = as.numeric(substr(cfec$landdate, 1, 4))

group_by(cfec[cfec$spec%in%c("CHUM","PINK","SOCK"),], year, p_fshy, spec) %>% summarize(m = sum(g_earn)) %>% ggplot(aes(year, m, colour = spec), group=spec, colour = spec) + geom_line() + facet_wrap(~p_fshy)

# don't include coho because don't initially have run sizes
prices = group_by(cfec[cfec$spec%in%c("CHUM","CHNK","PINK","SOCK"),], spec, year) %>%
  summarize(meanPrice = mean(g_price)) %>%
  filter(year <= 2007)

# read in abundance data per Rich
returns = read.csv("salmon/data-generated/totalReturnByBY.csv")
returns = returns[returns$BroodYearReturn <= 2007 & returns$BroodYearReturn >= 1985 & is.na(returns$BroodYearReturn)==F,]

sock = data.frame("spec"="SOCK", "year"=seq(1985,2007), "totalrun"=returns$TotalSockeye)
pink = data.frame("spec"="PINK", "year"=seq(1985,2007), "totalrun"=returns$PWSPink)
chnk = data.frame("spec"="CHNK", "year"=seq(1985,2007), "totalrun"=returns$CopperChinook)
chum = data.frame("spec"="CHUM", "year"=seq(1985,2007), "totalrun"=returns$PWSChum)
returns = rbind(sock,pink,chum,chnk)


salmon = left_join(prices, returns)
salmon$totalrun = as.numeric(salmon$totalrun)
salmon$meanPrice = as.numeric(salmon$meanPrice)

salmon = group_by(salmon, spec) %>%
  mutate(scaledRun = scale(log(totalrun), scale=FALSE), scaledPrice = scale(log(meanPrice),scale=FALSE))
pdf("Returns_v_prices.pdf")
ggplot(salmon, aes(scaledRun, scaledPrice, color = spec, label=year)) +
  geom_text(size=3) + facet_wrap(~spec) +
  xlab("Centered ln run size") + ylab("Centered ln price") + geom_hline(aes(yintercept=0),color="grey") + geom_vline(aes(xintercept=0),color="grey")
dev.off()



