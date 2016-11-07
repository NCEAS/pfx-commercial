library(dplyr)
library(ggplot2)
library(reshape2)

cfec = feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}
casestudies = c("PWS", "Cook Inlet", "Kodiak", "Alaska Peninsula")

# subsets by region: kodiak, prince william sound, cook inlet, alaska peninsula
# all fisheries

for(cc in 1:length(casestudies)) {

casestudy = casestudies[cc]
#  Change the "yourfishery" to include whichever fishery(ies) you want. And then the rest of the code should be runnable. I didn't put this in a function because I didn't know how you'd want to output the figures.

mydat <- cfec[which(cfec$region%in%casestudy),]

pdf(paste0("diversity/",casestudy,"_generalPlots",".pdf"))
#  Create a summary dataset per year.
p1 <- mydat %>% group_by(year) %>%
  summarise(fisheries=length(unique(p_fshy)),permits=length(unique(p_fshy)),fishers=length(unique(p_holder)),rev=sum(g_earn))

# Permit fisheries by year
ggplot(p1,aes(x=year,y=fisheries)) + geom_point() + geom_line()

#  Total permits per year
ggplot(p1,aes(x=year,y=permits)) + geom_point() + geom_line()

#  Active permit holders per year
ggplot(p1,aes(x=year,y=fishers)) + geom_point() + geom_line()

#  Gross revenues per year
ggplot(p1,aes(x=year,y=rev)) + geom_point() + geom_line()

#  Gross revenues per permit holder per year - not sure how you want to visualize this. boxplots?
p2 <- mydat %>% group_by(year,p_holder) %>% summarise(rev=sum(g_earn),pph=length(unique(p_fshy)))
ggplot(p2,aes(x=factor(year),y=log(rev))) + geom_boxplot(outlier.shape=NA)

#  Number of permits per permit holder. There are some outliers,
#so I have created a plus group where anything greater than 5 permits becomes a 5
p2a <- p2 %>% mutate(pph.plus=ifelse(pph>5,5,pph)) %>% group_by(year) %>% count(pph.plus)
ggplot(p2a,aes(x=year,y=n,linetype=factor(pph.plus))) + geom_line() + scale_linetype_discrete(guide_legend(title="# of permits"))

#  Number of entrants and quitters per year (will need to delete the first year of the dataset for entrants and the last year for quitters)
p3 <- mydat %>% group_by(p_holder) %>% summarise(year1=min(year),year2=max(year))

#  Look at entrants
p3a <- p3 %>% group_by(year1) %>% summarise(entrants=n())
ggplot(p3a[p3a$year1>1985,],aes(x=year1,y=entrants)) + geom_point() + geom_line()

#  Look at quitters.
p3b <- p3 %>% group_by(year2) %>% summarise(quitters=n())
ggplot(p3b[p3b$year2<2014,],aes(x=year2,y=quitters)) + geom_point() + geom_line()

g1 = group_by(mydat, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%casestudy))>0, 1, 0)) %>%
  filter(pws>0) %>%
  select(-pws) %>%
  group_by(p_holder, year, p_fshy) %>%
  summarize(g = sum(g_earn)) %>%
  group_by(p_holder, year) %>%
  summarize(div = simp.div(g)) %>%
  group_by(year) %>%
  summarize(mean = mean(div), lower=quantile(div,0.025), upper=quantile(div,0.975)) %>%
  ggplot(aes(year, mean)) + geom_line() + ggtitle(casestudy) + ylab("Mean diversity")
print(g1)

# Identify dominant strategies associated with this permit
g2 = group_by(cfec, p_holder) %>%
  mutate(pws = ifelse(length(which(region%in%casestudy))>0, 1, 0)) %>%
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

totals = group_by(totals, year, group) %>%
  summarize(n = sum(n))

ggplot(totals, aes(year, n)) + geom_area(aes(fill = group), position = "stack", colour = 1) + xlab("Year") + ylab("People")

dev.off()

}
