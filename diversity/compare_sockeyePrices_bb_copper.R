library(dplyr)
library(ggplot2)
library(reshape2)

cfec = feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)

sock = filter(cfec, spec=="SOCK")

copper = filter(sock, stat6 %in% c(20020, 21235, 20010, 21231, 21221, 21211, 21215, 21225))
copper = group_by(copper, year) %>%
  summarize(p = sum(g_earn)/sum(g_pounds))
copper$area = "copper"

bb = filter(sock, stat6 %in% c(32610, 32611, 32671, 32670,
  32672, 32620, 32621, 32630, 32631, 32640, 32641, 32105,
  32180, 32140, 32160, 32170, 32100, 32120, 32130, 32140,
  32100, 32110))
bb = group_by(bb, year) %>%
  summarize(p = sum(g_earn)/sum(g_pounds))
bb$area = "bristol"

dat = bind_rows(copper, bb)

ggplot(dat, aes(year, p, group = area, color = area)) +
  geom_line() + xlab("Year") + ylab("Sockeye price ($/lb)")
