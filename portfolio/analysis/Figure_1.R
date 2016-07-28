
# This figure is meant to illustrate increased specialization

# number of permits / person
library(dplyr)
library(knitr)
library(ggplot2)
library(date)


cfec <- feather::read_feather("portfolio/data-generated/cfec.feather")

# panel 1 = something like proportion of single permit holders, broken out by salmon
# this calculates number of salmon and non-salmon permits held per person-year
permits_person_year = group_by(cfec, p_holder, year) %>%
  summarize(n.otherpermit = length(unique(p_fshy[g_earn>0 & substr(p_fshy,1,1) != "S"])),
    n.salpermit = length(unique(p_fshy[g_earn>0 & substr(p_fshy,1,1) == "S"])))
permits_person = group_by(year) %>%
  summarize(n.other = length(which(n.otherpermit==1 & n.salpermit==0)),
    n.sal = length(which(n.salpermit==1 & n.otherpermit==0)), nTot = n())

p1 = ggplot(permits_person, aes(x = year, y = (n.sal)/nTot)) +
  geom_line(colour="salmon1",size=2) +
  geom_line(aes(x = year, y = (n.sal+n.other)/nTot), colour = "black",size=2) +
    xlab("Proportion of permit holders fishing single permit") + ylab("Year")

# panel 2 = something like species diversity, maybe again splitting out salmon effect?
simp.div = function(x) {
  1/sum((x/sum(x))^2)
}

effSpec = group_by(cfec, year, p_holder, specn) %>%
  summarize(earn = sum(g_earn)) %>%
  group_by(year, p_holder) %>%
  summarize(effSpec = simp.div(earn), effSpec.nosal = simp.div(earn[specn%in%c(seq(400,450,10))==FALSE]))

# add salmon specialist label -- basically anyone with single salmon permit
salmon_specialists = filter(permits_person_year, n.salpermit ==1 & n.otherpermit==0) %>%
  select(-n.otherpermit, -n.salpermit)
effSpec$salmon = 0 # ugly, wasn't working with dplyr
effSpec$salmon[paste(effSpec$p_holder,effSpec$year)%in%paste(salmon_specialists$p_holder,salmon_specialists$year)] = 1

# showing uncertainty on this plot isn't very useful (variability across people)
p2 = group_by(effSpec, year) %>%
  summarize(mean = mean(effSpec,na.rm=T),
    mean.other = mean(effSpec.nosal[is.finite(effSpec.nosal)],na.rm=T),
    sd = sd(effSpec,na.rm=T),
    sd.other = sd(effSpec.nosal[is.finite(effSpec.nosal)],na.rm=T),
    lower = mean - sd/mean, upper = mean + sd/mean,
    lower.other = mean.other - sd.other/mean.other,
    upper.other = mean.other + sd.other/mean.other) %>%
  ggplot(aes(x = year, y = mean)) + geom_line() + geom_line(aes(x = year, y = mean), col="blue", size=2) +
  geom_line(aes(x = year, y = mean.other), col = "green", size=2) + xlab("Year") + ylab("Effective diversity")
  #geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
  #geom_ribbon(aes(ymin = lower.other, ymax = upper.other), alpha=0.2)
pdf("portfolio/figs/Fig1.pdf")
multiplot(p1, p2, cols=2)
dev.off()

# Multiple plot function
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
