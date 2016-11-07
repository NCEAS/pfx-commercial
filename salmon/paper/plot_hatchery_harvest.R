library(ggplot2)
library(dplyr)
library(reshape2)

seak = read.csv("salmon/data-generated/stopha_table32.csv")
pws = read.csv("salmon/data-generated/stopha_table33.csv")
kodiak = read.csv("salmon/data-generated/stopha_table35.csv")

seak = melt(seak, id.vars = c("Year"))
pws = melt(pws, id.vars = c("Year"))
kodiak = melt(kodiak, id.vars = c("Year"))

seak$region = "Southeast"
pws$region = "Prince William Sound"
kodiak$region = "Kodiak"

seak$value[which(is.na(seak$value))] = 0
pws$value[which(is.na(pws$value))] = 0
kodiak$value[which(is.na(kodiak$value))] = 0

dat = bind_rows(seak, pws)
dat = bind_rows(dat, kodiak)

names(dat)[which(names(dat)=="variable")]="species"
dat$value = dat$value / 1000000

pdf("hatchery_harvest.pdf")
ggplot(dat, aes(Year, value)) +
  geom_area(aes(fill = species), position = "stack", colour = 1) +
  xlab("Year") + ylab("Harvest of hatchery produced salmon (millions)") +
  facet_wrap(~region, scale="free_y")
dev.off()

