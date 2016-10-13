

cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

species = c("CHNK","COHO","PINK","SOCK","CHUM","PHLB","SABL","PLCK","KCRB")

library(dplyr)
pdf("prices_revenue.pdf")
prices = group_by(cfec[cfec$spec%in%species,], year, spec) %>%
  summarize(priceDollars = sum(g_earn)/sum(g_pounds), revenueMillions = sum(g_earn)/1000000)

%>%
  ungroup() %>%
  group_by(spec) %>%
  mutate(scaleP = price/price[year==1985])

  ggplot(scaled, aes(year, scaleP, color=spec)) + geom_line() +
  xlab("Year") + ylab("Price ($)") + geom_point(aes(size=million))
dev.off()
