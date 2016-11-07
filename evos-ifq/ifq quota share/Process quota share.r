#### 
library(dplyr)
library(ggplot2)


setwd("/Users/ole.shelton/GitHub/ifq quota share")
source("multiplot.r")
dat <- NULL

setwd("/Users/ole.shelton/GitHub/ifq quota share/ifq qs csv")
DIR <- dir()
dat.pre.94 <- read.csv("/Users/ole.shelton/GitHub/ifq quota share/pre94 data/halibut participation iphc pre-1994.csv")

dat <- NULL
for(i in 1:length(DIR)){
  temp <- read.csv(DIR[i])
  if(as.numeric(substr(DIR[i],1,2)) < 90){ year <- as.numeric(paste("20",substr(DIR[i],1,2),sep=""))     }
  if(as.numeric(substr(DIR[i],1,2)) > 90){ year <- as.numeric(paste("19",substr(DIR[i],1,2),sep=""))     }
  #print(year)
  
  temp.1 <- temp[,c("SPECIES","AREA","CAT","CDQ","CITY","STATE")]
    
  if(year <= 1999){
    nom <- temp[,"NAME"]
    A   <- gregexpr(pattern= ", ",nom)
    
    for(j in 1:length(nom)){
        if(A[[j]][1] > 0 ){
          temp$LAST[j]  <- substr(nom[j],1,A[[j]][1]-1)
          temp$FIRST[j] <- substr(nom[j],A[[j]][1]+1,A[[j]][1]+12)
        }
        if(A[[j]][1] < 0 ){
          temp$LAST[j]  <- nom[j]
          temp$FIRST[j] <- ""
        }
    }
  }
    
    temp.2 <- temp[,c(grep("ZIP",colnames(temp)),
                      grep("LAST",colnames(temp)),
                      grep("FIRST",colnames(temp)),
                      grep("UNIT",colnames(temp))
    )]
  
  colnames(temp.2) <- c("ZIP","LAST","FIRST","UNITS")
  temp.2$YEAR <- year
  temp.3 <- cbind(temp.1,temp.2)
  dat <- rbind(dat,temp.3)
}

### Consolidate by individual by area

hal.dat.ind <- summarise(group_by(dat,YEAR,SPECIES,AREA,CAT,STATE,LAST,FIRST),N.UNITS=length(UNITS),UNITS=sum(UNITS))
hal.dat.ind <- hal.dat.ind[hal.dat.ind$SPECIES=="halibut",]
hal.dat.ind$N <- 1

all.qs <- summarise(group_by(dat,YEAR,SPECIES,AREA),TOT.Q = sum(UNITS))
hal.dat.ind <- merge(hal.dat.ind, all.qs)
hal.dat.ind$PROP.Q <- hal.dat.ind$UNITS / hal.dat.ind$TOT.Q

hal.dat.sum <- summarise(group_by(hal.dat.ind,YEAR,AREA,CAT),N=sum(N))

hal.dat.ind <- hal.dat.ind[hal.dat.ind$AREA == "2C" | hal.dat.ind$AREA == "3A" |hal.dat.ind$AREA == "3B",]

hal.dat.across <-  summarize(group_by(hal.dat.ind,YEAR,CAT,STATE,LAST,FIRST),N.AREAS=sum(N))
hal.dat.B      <-  summarize(group_by(hal.dat.across,YEAR,CAT),TOT.IND=length(N.AREAS))
hal.dat.C      <-  summarize(group_by(hal.dat.across,YEAR,CAT,N.AREAS),IND=length(N.AREAS))
hal.dat.ind.across       <- merge(hal.dat.B,hal.dat.C)
hal.dat.ind.across$PROP <- hal.dat.ind.across$IND/ hal.dat.ind.across$TOT.IND
#####################################################################
#####################################################################

dat.pre.94$CAT <- NULL
dat.pre.94$CAT[dat.pre.94$max.vessel >= 0 & dat.pre.94$max.vessel <= 35] <- "D"
dat.pre.94$CAT[dat.pre.94$max.vessel >= 35 & dat.pre.94$max.vessel <= 55] <- "C"
dat.pre.94$CAT[dat.pre.94$max.vessel >= 56] <- "B"
dat.pre.94$CAT[dat.pre.94$max.vessel <= 0] <- "OTHER"

hal.dat.pre <- dat.pre.94[,c("YEAR","AREA","CAT","N")]
hal.dat.pre$YEAR <- as.numeric(as.character(hal.dat.pre$YEAR))
hal.dat.pre<- hal.dat.pre %>% group_by(YEAR,AREA,CAT) %>% summarize(. , N= sum(N))

# hal.dat.sum <- data.frame(hal.dat.sum)
# hal.dat.sum$CAT <- as.character(hal.dat.sum$CAT)
hal.dat.sum <- rbind(hal.dat.sum,hal.dat.pre)

hal.dat.sum$CAT[hal.dat.sum$CAT=="A"] <- "OTHER"

hal.dat.sum <- hal.dat.sum[order(hal.dat.sum$YEAR,hal.dat.sum$AREA,hal.dat.sum$CAT),]


#####################################################################
#####################################################################
# START PLOT

part.2C<- ggplot(hal.dat.sum[hal.dat.sum$AREA == "2C" ,]) +
              geom_area(aes(YEAR,N,fill=CAT)) +
              labs(x="Year",y="Quota owners") +
              theme_bw() +
              theme(legend.position="none") +
              scale_fill_discrete(guide_legend(title = "2C"))
part.3A<- ggplot(hal.dat.sum[hal.dat.sum$AREA == "3A" ,]) +
  geom_area(aes(YEAR,N,fill=CAT)) +
  labs(x="Year",y="Quota owners") +
  theme_bw() +
  theme(legend.position="none") +
  scale_fill_discrete(guide_legend(title = "3A"))
part.3B<- ggplot(hal.dat.sum[hal.dat.sum$AREA == "3B" ,]) +
  geom_area(aes(YEAR,N,fill=CAT)) +
  labs(x="Year",y="Quota owners") +
  theme_bw() +
  theme(legend.position="none") +
  scale_fill_discrete(guide_legend(title = "3B"))

START <- 1995
STOP  <- 2015

dist.2C <- ggplot()+
  geom_boxplot(data=hal.dat.ind[hal.dat.ind$AREA == "2C" & hal.dat.ind$YEAR == START ,],
              aes(y=PROP.Q,x=1,fill=CAT),outlier.shape=NA)+
  geom_boxplot(data=hal.dat.ind[hal.dat.ind$AREA == "2C" & hal.dat.ind$YEAR == STOP ,],
               aes(y=PROP.Q,x=2,fill=CAT),outlier.shape=NA)+
  scale_y_continuous(trans="log10") +
  scale_x_continuous(breaks=c(1,2),labels = c(START,STOP)) +
  labs(x="Year",y="Prop. Quota") +
  theme_bw() +
  #theme(legend.position="none") +
  scale_fill_discrete(guide_legend(title = "2C"))

dist.3A <- ggplot()+
  geom_boxplot(data=hal.dat.ind[hal.dat.ind$AREA == "3A" & hal.dat.ind$YEAR == START ,],
               aes(y=PROP.Q,x=1,fill=CAT),outlier.shape=NA)+
  geom_boxplot(data=hal.dat.ind[hal.dat.ind$AREA == "3A" & hal.dat.ind$YEAR == STOP ,],
               aes(y=PROP.Q,x=2,fill=CAT),outlier.shape=NA)+
  scale_y_continuous(trans="log10") +
  scale_x_continuous(breaks=c(1,2),labels = c(START,STOP)) +
  labs(x="Year",y="Prop. Quota") +
  theme_bw() +
  #theme(legend.position="none") +
  scale_fill_discrete(guide_legend(title = "3A"))

dist.3B <- ggplot()+
  geom_boxplot(data=hal.dat.ind[hal.dat.ind$AREA == "3B" & hal.dat.ind$YEAR == START ,],
               aes(y=PROP.Q,x=1,fill=CAT),outlier.shape=NA)+
  geom_boxplot(data=hal.dat.ind[hal.dat.ind$AREA == "3B" & hal.dat.ind$YEAR == STOP ,],
               aes(y=PROP.Q,x=2,fill=CAT),outlier.shape=NA)+
  scale_y_continuous(trans="log10") +
  scale_x_continuous(breaks=c(1,2),labels = c(START,STOP)) +
  labs(x="Year",y="Prop. Quota") +
  theme_bw() +
  #theme(legend.position="none") +
  scale_fill_discrete(guide_legend(title = "3B"))

## Make Plot
multiplot(part.2C,part.3A,part.3B,dist.2C,dist.3A,dist.3B,cols = 2)
########################################################################
DAT <-hal.dat.ind.across
p.1995 <- ggplot() +
          geom_bar(data=DAT[DAT$YEAR==START ,],stat="identity",position="dodge",
              aes(y=PROP,x=N.AREAS,fill=CAT)) +
              labs(x="",y="Prop. individuals",title=START) +            
              theme_bw() +
              theme(legend.position="none")
p.2000 <- ggplot() +
  geom_bar(data=DAT[DAT$YEAR==2000 ,],stat="identity",position="dodge",
           aes(y=PROP,x=N.AREAS,fill=CAT)) +
            theme(legend.position="none") +
            labs(x="Number of Areas",y="",title=2000) +
            theme_bw() +
            theme(legend.position="none")

p.2015 <- ggplot() +
          geom_bar(data=DAT[DAT$YEAR==STOP ,],stat="identity",position="dodge",
          aes(y=PROP,x=N.AREAS,fill=CAT)) +
          labs(x="",y="",title=STOP) +            
          theme_bw() +
          theme(legend.position="none")

multiplot(p.1995,p.2000,p.2015,cols=3)
