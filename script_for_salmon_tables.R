library(dplyr)

dat <- readRDS(file="portfolio/data-generated/cfec-annual-for-modeling.rds")
dat$permit_group = NA
dat$permit_group[dat$permit%in%c("S05B","S15B")] = "salmon_troll"
dat$permit_group[which(dat$permit%in%c("S03H","S03M","S03T"))] = "drift_gillnet" 
dat$permit_group[which(dat$permit%in%c("S01H","S01K"))] = "purse_seine"
dat$permit_group[which(dat$permit%in%c("S04E","S04H","S04K","S04M","S04T"))] = "set_gillnet"
dat$permit_group[which(dat$permit%in%c("S04W","S04Z"))] = "set_gillnet2"

salmon_permit_table = group_by(dat[which(!is.na(dat$permit_group)),], permit_group) %>% 
  summarize(tot = sum(chnk) + sum(coho) + sum(pink) + sum(sock) + sum(chum),
    chnk = round(sum(chnk)/tot,2), coho = round(sum(coho)/tot,2), pink = round(sum(pink)/tot,2), 
    sock = round(sum(sock)/tot,2), chum = round(sum(chum)/tot,2)) %>% 
  select(-tot)

# alternatively do it by permit
salmon_permit_table2 = group_by(dat[which(dat$permit %in% c("S05B","S15B","S03H",
  "S03M","S03T","S01H","S01K","S04E","S04H","S04K","S04M","S04T","S04W","S04Z")),], permit, year) %>% 
  summarize(tot = sum(chnk,na.rm=T) + sum(coho,na.rm=T) + sum(pink,na.rm=T) + sum(sock,na.rm=T) + sum(chum,na.rm=T),
    chnk = sum(chnk)/tot, coho = sum(coho)/tot, pink = sum(pink)/tot, 
    sock = sum(sock)/tot, chum = sum(chum)/tot,
    permit_group = permit_group[1]) %>% 
  select(-tot) %>% 
  group_by(permit) %>% 
  summarize(chnk = round(mean(chnk,na.rm=T),2), pink = round(mean(pink,na.rm=T),2), coho = round(mean(coho,na.rm=T),2),
    chum = round(mean(chum,na.rm=T),2), sock = round(mean(sock,na.rm=T),2), permit_group = permit_group[1])




