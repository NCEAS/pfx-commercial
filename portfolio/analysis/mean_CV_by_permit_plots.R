library(feather)
library(dplyr)
library(ggplot2)

# Load differenced data
cfecAnnual.diff = readRDS(file="data/cfec-diff-for-modeling.rds")

# restrict analysis to people who don't change strategies (keeps ~ 90%)
cfecAnnual.diff = cfecAnnual.diff[which(cfecAnnual.diff$strategy == cfecAnnual.diff$strategy.prev), ]

# calcualte variance versus mean across strategies 
strategy = lapply(lapply(strsplit(as.character(cfecAnnual.diff$strategy), " "), 
  substr, 1, 1), paste, collapse=" ")
cfecAnnual.diff$strategy = unlist(strategy)

# Add pollock + cod 
cfecAll= read_feather("data/cfec.feather")

#pollockCod = group_by(cfecAll, p_holder, year, p_fshy) %>% 
#  summarize(totalEarn = sum(g_earn), 
#    pCod = sum(g_earn[spec=="PCOD"])/totalEarn, 
#    pPlk = sum(g_earn[spec=="PLCK"])/totalEarn) %>% 
#  filter(pCod > 0.6 | pPlk > 0.6) %>%
#  filter(totalEarn > 5000)

m_fishers = group_by(cfecAnnual.diff[cfecAnnual.diff$strategy=="M",], p_holder, year)
codPollock = group_by(cfecAll[which(cfecAll$p_holder%in%m_fishers$p_holder & 
    cfecAll$year%in%m_fishers$year),], p_holder, year) %>% 
  summarize(totalEarn = sum(g_earn), 
    pCod = sum(g_earn[spec=="PCOD"])/totalEarn, 
    pPlk = sum(g_earn[spec=="PLCK"])/totalEarn)

cfecAnnual.diff$mStrategy = ""
cfecAnnual.diff$mStrategy[which(codPollock$p_holder %in% cfecAnnual.diff$p_holder & 
    codPollock$year %in% cfecAnnual.diff$year & codPollock$pPlk > 0.6)] = "plck"
cfecAnnual.diff$mStrategy[which(codPollock$p_holder %in% cfecAnnual.diff$p_holder & 
    codPollock$year %in% cfecAnnual.diff$year & codPollock$pCod > 0.6)] = "cod"

# Calculate some differenced/derived variables
cfecAnnual.diff$specDiv.pctChange = log(cfecAnnual.diff$specDiv / cfecAnnual.diff$specdiv.prev)
cfecAnnual.diff$days.pctChange = log((cfecAnnual.diff$days+1) / (cfecAnnual.diff$days.prev+1))
cfecAnnual.diff$rev.pctChange = log(cfecAnnual.diff$revenue / cfecAnnual.diff$revenue.prev)

# also filter out people making < 1000 / year
cfecAnnual.diff = cfecAnnual.diff[which(cfecAnnual.diff$revenue >= 5000), ]

# 
cfecAnnual.diff$strategy = paste(cfecAnnual.diff$strategy, cfecAnnual.diff$mStrategy)

# 4200 different strategies, need to model only most common, 
top.strategies = names(rev(sort(table(cfecAnnual.diff$strategy)))[1:200])
cfecAnnual.diff = cfecAnnual.diff[cfecAnnual.diff$strategy%in%top.strategies, ]
testData = cfecAnnual.diff

#plot_meanAcrossYears = function(testData, permit, title) {
#  group_by_(testData[grep(permit,testData$strategy),], "strategy", "year") %>% 
#    summarize(mean = log(mean(revenue)), sd = sd(rev.pctChange)) %>%
#    data.frame() %>%
#    group_by(strategy) %>% 
#    summarize(mean = mean(mean, na.rm=T), sd = mean(sd, na.rm=T), n = nchar(strategy[1])) %>% 
#    ggplot(aes(x = mean, y = sd, col=as.factor(strategy))) + geom_point(aes(size=n/4), alpha=0.5) +
#    xlab("Log (mean revenue across people in a year)") + 
#    ylab("CV of revenue across people") + 
#    ggtitle(title)
#}

#plot_meanByYears = function(testData, permit, title) {
#  group_by_(testData[grep(permit,testData$strategy),], "strategy", "year") %>% 
#    summarize(mean = log(mean(revenue)), sd = sd(rev.pctChange),
#      n = nchar(strategy[1])) %>%
#    ggplot(aes(x = mean, y = sd, col=as.factor(strategy))) + 
#    geom_point(aes(size=n/4), alpha=0.5) +
#    xlab("Log (mean revenue across people in a year)") + 
#    ylab("CV of revenue across people") + 
#    ggtitle(title)
#}

plot_meanAcrossYears2 = function(testData, permit, title) {
  centerMeans = group_by(testData, npermit) %>% 
  summarize(mean = mean(log(revenue)), sd = sd(rev.pctChange))
  
  group_by_(testData[grep(permit, testData$strategy),], "strategy") %>% 
    summarize(mean = mean(log(revenue), na.rm=T), sd = sd(rev.pctChange, na.rm=T),
      permits = as.character(npermit[1])) %>%
    ggplot(aes(x = mean, y = log(sd), col=permits)) + 
    geom_point(alpha=0.3) +
    geom_point(data = centerMeans, aes(x = mean, y = log(sd), col=as.factor(npermit)), alpha=0.7, size=5) +
    xlab("Mean (log revenue)") + 
    ylab("LogCV across people") + 
    geom_smooth(method = 'lm',size = 1, colour = 'grey70', se = F) + 
    ggtitle(title)
}

plot_meanByYears2 = function(testData, permit, title) {
    centerMeans = group_by(testData, npermit) %>% 
    summarize(mean = mean(log(revenue)), sd = sd(rev.pctChange))
    
    testData$maxN = max((group_by_(testData[grep(permit, testData$strategy),], "strategy", "year") %>% 
        summarize(n = n()))$n)
        
    group_by_(testData[grep(permit, testData$strategy),], "strategy", "year") %>% 
    summarize(mean = mean(log(revenue)), sd = sd(rev.pctChange),
      permits = as.character(npermit[1]), n = n()/maxN[1]) %>% 
    ggplot(aes(x = mean, y = log(sd), col=permits)) + 
    geom_point(aes(alpha=sqrt(n))) +
    geom_point(data = centerMeans, aes(x = mean, y = log(sd), col=as.factor(npermit)), alpha=0.7, size=5) +
    xlab("Mean (log revenue across people in a year)") + 
    ylab("LogCV across people") +
    geom_smooth(method = 'lm',size = 1, colour = 'grey70', se = F) + 
    ggtitle(title)
}

# Multiple plot function -- http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
      ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
        layout.pos.col = matchidx$col))
    }
  }
}


#### Make plots including points for each year
### Note: (1) this is mean of log(revenue) rather than log(mean) -- I experimented with both
### (2) M permits or S permits might need to be broken out into sockeye/cod/pollock
pdf("Mean_CV_allYears.pdf")
p1 = plot_meanByYears2(testData, "B|C", "Sablefish-Halibut")
p2 = plot_meanByYears2(testData, "G|L", "Herring")
p3 = plot_meanByYears2(testData, "S", "Salmon")
p4 = plot_meanByYears2(testData, "D|E|T|K", "Crab")
p5 = plot_meanByYears2(testData, "M", "Misc. finfish")
p6 = plot_meanByYears2(testData, "O|P|Q|R|U|W", "Inverts")
multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()

pdf("Mean_CV_acrossYears.pdf")
p1 = plot_meanAcrossYears2(testData, "B|C", "Sablefish-Halibut")
p2 = plot_meanAcrossYears2(testData, "G|L", "Herring")
p3 = plot_meanAcrossYears2(testData, "S", "Salmon")
p4 = plot_meanAcrossYears2(testData, "D|E|T|K", "Crab")
p5 = plot_meanAcrossYears2(testData, "M", "Misc. finfish")
p6 = plot_meanAcrossYears2(testData, "O|P|Q|R|U|W", "Inverts")
multiplot(p1, p2, p3, p4, p5, p6, cols=2)
dev.off()



