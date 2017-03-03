library(dplyr)
library(ggplot2)

d = read.csv("data/SEAK_pemitValues_CFEC.csv")

d = d[,c("PERIOD","VALUE","Permit")]
names(d)[which(names(d)=="PERIOD")]="year"

defl = read.csv("data/deflation.csv")

d = left_join(d, defl)
d$value = d$VALUE/d$defl

d = group_by(d, Permit) %>%
  mutate(rel = value/value[which(year==2000)])

g1 = ggplot(d, aes(year, value/1000, color = Permit)) + geom_line() + xlab("Year") + ylab("Estimated value ($1000s)") + theme_bw()

# Also calculate version of relative prices, relative to 2000
g2 = ggplot(d, aes(year, rel, color = Permit)) + geom_line() + xlab("Year") + ylab("% change relative to 2000 value") + theme_bw()

png("portfolio/figs/permit_values.png")
gridExtra::grid.arrange(g1, g2, ncol=1)
dev.off()


d = read.csv("data/halibut_prices.csv")
names(defl)[which(names(defl)=="year")]="Year"
d = left_join(d, defl)
d$value = d$Price/d$defl

d = group_by(d, Quota) %>%
  mutate(rel = value/value[which(Year==2000)])

g1 = ggplot(d, aes(Year, value, color = Quota)) + geom_line() + xlab("Year") + ylab("Average price ($)") + theme_bw()

# Also calculate version of relative prices, relative to 2000
g2 = ggplot(d, aes(Year, rel, color = Quota)) + geom_line() + xlab("Year") + ylab("% change relative to 2000 value") + theme_bw()

png("portfolio/figs/halibut_quota_price.png")
gridExtra::grid.arrange(g1, g2, ncol=1)
dev.off()
