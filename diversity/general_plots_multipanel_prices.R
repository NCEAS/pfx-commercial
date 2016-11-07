library(dplyr)
library(ggplot2)
library(reshape2)

cfec = feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

yourfishery <- c("G 01E")
mydat <- cfec[which(cfec$p_fshy%in%yourfishery),]
g1 = group_by(mydat,year) %>%
  summarize(price = sum(g_earn)/sum(g_pounds))
g1$spec="HERR"

yourfishery <- c("S 01E")
mydat <- cfec[which(cfec$p_fshy%in%yourfishery & cfec$spec=="PINK"),]
g2 = group_by(mydat,year) %>%
  summarize(price = sum(g_earn)/sum(g_pounds))
g2$spec="PINK"

yourfishery <- c("S 03T")
mydat <- cfec[which(cfec$p_fshy%in%yourfishery & cfec$spec=="SOCK"),]
g3 = group_by(mydat,year) %>%
  summarize(price = sum(g_earn)/sum(g_pounds))
g3$spec="SOCK"

yourfishery <- c("Q 11A","Q 11B")
mydat <- cfec[which(cfec$p_fshy%in%yourfishery & cfec$specn==895),]
g4 = group_by(mydat,year) %>%
  summarize(price = sum(g_earn)/sum(g_pounds))
g4$spec="CUKE"

dat = bind_rows(g1,g2)
dat = bind_rows(dat,g3)
dat = bind_rows(dat,g4)

pdf("general_plots_multipanel_prices.pdf")
ggplot(dat, aes(year,price)) + geom_line() +
  facet_wrap(~spec,scale="free_y") +
  xlab("Year") + ylab("Price per pound")
dev.off()
