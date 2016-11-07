library(dplyr)
library(ggplot2)
library(reshape2)

cfec = feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

#mydat <- cfec[which(cfec$p_fshy%in%yourfishery),]
# Identify dominant strategies associated with this permit
g2 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%c("PWS")))>0, 1, 0)) %>%
  filter(pws>0) %>%
  select(-pws) %>%
  group_by(p_holder, year, p_fshy) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(p_holder, year) %>%
  mutate(totRev = sum(g)) %>%
  group_by(p_holder, year, p_fshy) %>%
  mutate(g = g / totRev) %>%
  select(-totRev)

# use reshape 2 to cast this to wide format
aql <- melt(g2, id.vars = c("year", "p_fshy", "p_holder"))
aqw <- dcast(aql, year + p_holder ~ p_fshy)
# replace NAs w/0s
for(i in 1:ncol(aqw)) {
  aqw[which(is.na(aqw[,i])),i] = 0
}

indx = sample(seq(1,nrow(aqw)),size=1000,replace=F)
sub = aqw[indx,-c(1,2)]

sub = aqw[,-c(1,2)]
coarse_df = aqw[,-c(1,2)]
coarse_df$S = apply(coarse_df[,which(substr(names(sub),1,1)=="S")],1,sum)
coarse_df$B = apply(coarse_df[,which(substr(names(sub),1,1)=="B")],1,sum)
coarse_df$GHL = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("G","H","L"))],1,sum)
coarse_df$KTD = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("K","T","D"))],1,sum)
coarse_df$M = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("M"))],1,sum)
coarse_df$P = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("P"))],1,sum)
#coarse_df$Q = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("Q"))],1,sum)
coarse_df$OTHER = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("S","B","G","H","L","K","T","D","M","P")==FALSE)],1,sum)
coarse_df = coarse_df[,c("S","B","GHL","KTD","M","P","OTHER")]

clust = fpc::pamk(coarse_df[sample(seq(1,nrow(aqw)),size=1000,replace=F),])# cluster based on subset to pick medoids
medoids = clust$pamobject$medoids

names_medoids = ""
for(i in 1:nrow(medoids)) {
  names_medoids[i] = paste0(colnames(medoids)[which(medoids[i,] > 0.05)], collapse=":")
}

clust = kmeans(coarse_df, centers = medoids)

aqw$clusters = clust$cluster
totals = group_by(aqw, clusters, year) %>%
  summarize(n = n())
totals$group = names_medoids[totals$clusters]

p1.totals = group_by(totals, year, group) %>%
  summarize(n = sum(n))

#p1 = ggplot(totals, aes(year, n)) + geom_area(aes(fill = group), position = "stack", colour = 1) + xlab("Year") + ylab("People")

g2 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%c("Cook Inlet")))>0, 1, 0)) %>%
  filter(pws>0) %>%
  select(-pws) %>%
  group_by(p_holder, year, p_fshy) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(p_holder, year) %>%
  mutate(totRev = sum(g)) %>%
  group_by(p_holder, year, p_fshy) %>%
  mutate(g = g / totRev) %>%
  select(-totRev)

# use reshape 2 to cast this to wide format
aql <- melt(g2, id.vars = c("year", "p_fshy", "p_holder"))
aqw <- dcast(aql, year + p_holder ~ p_fshy)
# replace NAs w/0s
for(i in 1:ncol(aqw)) {
  aqw[which(is.na(aqw[,i])),i] = 0
}

indx = sample(seq(1,nrow(aqw)),size=1000,replace=F)
sub = aqw[indx,-c(1,2)]

sub = aqw[,-c(1,2)]
coarse_df = aqw[,-c(1,2)]
coarse_df$S = apply(coarse_df[,which(substr(names(sub),1,1)=="S")],1,sum)
coarse_df$B = apply(coarse_df[,which(substr(names(sub),1,1)=="B")],1,sum)
coarse_df$GHL = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("G","H","L"))],1,sum)
coarse_df$KTD = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("K","T","D"))],1,sum)
coarse_df$M = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("M"))],1,sum)
coarse_df$P = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("P"))],1,sum)
coarse_df$Q = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("Q"))],1,sum)
coarse_df$OTHER = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("S","B","G","H","L","K","T","D","M","P","Q")==FALSE)],1,sum)
coarse_df = coarse_df[,c("S","B","GHL","KTD","M","P","OTHER","Q")]

clust = fpc::pamk(coarse_df[sample(seq(1,nrow(aqw)),size=1000,replace=F),])# cluster based on subset to pick medoids
medoids = clust$pamobject$medoids

names_medoids = ""
for(i in 1:nrow(medoids)) {
  names_medoids[i] = paste0(colnames(medoids)[which(medoids[i,] > 0.05)], collapse=":")
}

clust = kmeans(coarse_df, centers = medoids)

aqw$clusters = clust$cluster
totals = group_by(aqw, clusters, year) %>%
  summarize(n = n())
totals$group = names_medoids[totals$clusters]

p2.totals = group_by(totals, year, group) %>%
  summarize(n = sum(n))

#p2 = ggplot(totals, aes(year, n)) + geom_area(aes(fill = group), position = "stack", colour = 1) + xlab("Year") + ylab("People")


g2 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%c("Kodiak")))>0, 1, 0)) %>%
  filter(pws>0) %>%
  select(-pws) %>%
  group_by(p_holder, year, p_fshy) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(p_holder, year) %>%
  mutate(totRev = sum(g)) %>%
  group_by(p_holder, year, p_fshy) %>%
  mutate(g = g / totRev) %>%
  select(-totRev)

# use reshape 2 to cast this to wide format
aql <- melt(g2, id.vars = c("year", "p_fshy", "p_holder"))
aqw <- dcast(aql, year + p_holder ~ p_fshy)
# replace NAs w/0s
for(i in 1:ncol(aqw)) {
  aqw[which(is.na(aqw[,i])),i] = 0
}

indx = sample(seq(1,nrow(aqw)),size=1000,replace=F)
sub = aqw[indx,-c(1,2)]

sub = aqw[,-c(1,2)]
coarse_df = aqw[,-c(1,2)]
coarse_df$S = apply(coarse_df[,which(substr(names(sub),1,1)=="S")],1,sum)
coarse_df$B = apply(coarse_df[,which(substr(names(sub),1,1)=="B")],1,sum)
coarse_df$GHL = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("G","H","L"))],1,sum)
coarse_df$KTD = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("K","T","D"))],1,sum)
coarse_df$M = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("M"))],1,sum)
coarse_df$P = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("P"))],1,sum)
coarse_df$Q = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("Q"))],1,sum)
coarse_df$OTHER = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("S","B","G","H","L","K","T","D","M","P","Q")==FALSE)],1,sum)
coarse_df = coarse_df[,c("S","B","GHL","KTD","M","P","OTHER","Q")]

clust = fpc::pamk(coarse_df[sample(seq(1,nrow(aqw)),size=1000,replace=F),])# cluster based on subset to pick medoids
medoids = clust$pamobject$medoids

names_medoids = ""
for(i in 1:nrow(medoids)) {
  names_medoids[i] = paste0(colnames(medoids)[which(medoids[i,] > 0.05)], collapse=":")
}

clust = kmeans(coarse_df, centers = medoids)

aqw$clusters = clust$cluster
totals = group_by(aqw, clusters, year) %>%
  summarize(n = n())
totals$group = names_medoids[totals$clusters]

p3.totals = group_by(totals, year, group) %>%
  summarize(n = sum(n))

#p3 = ggplot(totals, aes(year, n)) + geom_area(aes(fill = group), position = "stack", colour = 1) + xlab("Year") + ylab("People")

g2 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%c("Southeast Inside","Southeast Outside")))>0, 1, 0)) %>%
  filter(pws>0) %>%
  select(-pws) %>%
  group_by(p_holder, year, p_fshy) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(p_holder, year) %>%
  mutate(totRev = sum(g)) %>%
  group_by(p_holder, year, p_fshy) %>%
  mutate(g = g / totRev) %>%
  select(-totRev)

# use reshape 2 to cast this to wide format
aql <- melt(g2, id.vars = c("year", "p_fshy", "p_holder"))
aqw <- dcast(aql, year + p_holder ~ p_fshy)
# replace NAs w/0s
for(i in 1:ncol(aqw)) {
  aqw[which(is.na(aqw[,i])),i] = 0
}

indx = sample(seq(1,nrow(aqw)),size=1000,replace=F)
sub = aqw[indx,-c(1,2)]

sub = aqw[,-c(1,2)]
coarse_df = aqw[,-c(1,2)]
coarse_df$S = apply(coarse_df[,which(substr(names(sub),1,1)=="S")],1,sum)
coarse_df$B = apply(coarse_df[,which(substr(names(sub),1,1)=="B")],1,sum)
coarse_df$GHL = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("G","H","L"))],1,sum)
coarse_df$KTD = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("K","T","D"))],1,sum)
coarse_df$M = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("M"))],1,sum)
coarse_df$P = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("P"))],1,sum)
coarse_df$Q = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("Q"))],1,sum)
#coarse_df$U = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("U"))],1,sum)
coarse_df$OTHER = apply(coarse_df[,which(substr(names(sub),1,1)%in%c("S","B","G","H","L","K","T","D","M","P","Q")==FALSE)],1,sum)
coarse_df = coarse_df[,c("S","B","GHL","KTD","M","P","OTHER","Q")]

clust = fpc::pamk(coarse_df[sample(seq(1,nrow(aqw)),size=1000,replace=F),])# cluster based on subset to pick medoids
medoids = clust$pamobject$medoids

names_medoids = ""
for(i in 1:nrow(medoids)) {
  names_medoids[i] = paste0(colnames(medoids)[which(medoids[i,] > 0.05)], collapse=":")
}

clust = kmeans(coarse_df, centers = medoids)

aqw$clusters = clust$cluster
totals = group_by(aqw, clusters, year) %>%
  summarize(n = n())
totals$group = names_medoids[totals$clusters]

p4.totals = group_by(totals, year, group) %>%
  summarize(n = sum(n))

#p4 = ggplot(totals, aes(year, n)) + geom_area(aes(fill = group), position = "stack", colour = 1) + xlab("Year") + ylab("People")

ggplot(p5.totals, aes(year, n)) + geom_area(aes(fill = group), position = "stack", colour = 1) + xlab("Year") + ylab("People")

p1.totals$fishery = "PWS"
p2.totals$fishery = "Cook Inlet"
p3.totals$fishery = "Kodiak"
p4.totals$fishery = "Southeast"

dat = bind_rows(p1.totals,p2.totals)
dat = bind_rows(dat, p3.totals)
dat = bind_rows(dat, p4.totals)

pdf("general_plots_multipanel_anne.pdf")
ggplot(dat, aes(year, n)) +
  geom_area(aes(fill = group), position = "stack", colour = 1) +
  xlab("Year") + ylab("People") +
  facet_wrap(~fishery, scale="free_y")
dev.off()
