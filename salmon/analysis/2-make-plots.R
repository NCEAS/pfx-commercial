library(rstan)

# grab original data to look at permit names
diffdat = readRDS(file="salmon/data-generated/salmon.rds")
years = seq(min(diffdat$year), max(diffdat$year))
permits = levels(as.factor(diffdat$permit))

yearpermit = data.frame("yrpermit"=levels(diffdat$year_permit))
yearpermit$year = as.numeric(substr(yearpermit$yrpermit,1,4))
yearpermit$permit = substr(yearpermit$yrpermit,6,9)

load("salmon/analysis/model-linear.Rdata")

est = extract(mod, permute="TRUE")

yearpermit$b0 = apply(est$b0_str_yr,2,median)
yearpermit$b0_low = apply(est$b0_str_yr,2,quantile,0.25)
yearpermit$b0_hi = apply(est$b0_str_yr,2,quantile,0.75)

ggplot(data=yearpermit) +
  geom_ribbon(aes(x=year,ymin=b0_low,ymax=b0_hi),alpha=0.3) +
  geom_line(aes(x=year,y=b0)) +
  facet_wrap(~permit) + xlab("Year") + ylab("Intercepts on mean")

# start with making time series of estimates on mean
# challenge is that they're time varying quadratic functions
# do facet_wrap of estimates by year

# effect of adding more 1 species from 1. specdiv.prev = 1

specdiv.prev = seq(1, 5, by=0.1)
specdiv = specdiv.prev + 1 # always add 1 more in these comparisons
x1 = log(specdiv/specdiv.prev)
x2 = log(specdiv/specdiv.prev) * specdiv.prev
x3 = log(specdiv/specdiv.prev) * specdiv.prev^2

df = expand.grid("year"=1:29, "diversity" = seq(1,length(x1)), "permit"=1:6)
df$permits = permits[df$permit]
df$years = years[df$year]
df$x = specdiv.prev[df$diversity]
df$pred = 0
df$low = 0
df$high = 0
for(i in 1:nrow(df)) {
  tmp = x1[df$diversity[i]] * (est$b_1[,df$year[i], df$permit[i]]+est$b1_str_yr_mu) +
    x2[df$diversity[i]] * (est$b_2[,df$year[i], df$permit[i]]+est$b2_str_yr_mu)# +
    #x3[df$diversity[i]] * (est$b_3[,df$year[i], df$permit[i]]+est$b3_str_yr_mu)
  df$pred[i] = median(tmp)
  df$low[i] = quantile(tmp,0.25)
  df$high[i] = quantile(tmp,0.75)
}

ggplot(df[df$permits=="S01E",], aes(x=x,y=pred,group=years)) +
  #geom_ribbon(aes(ymin=low,ymax=high), alpha=0.4) +
  geom_line() +
  facet_wrap(~years) + xlab("Previous species diversity") + ylab("Predicted benefit of adding +1 spp (ln $)")

ggplot(df[df$diversity==1,], aes(x=years,y=pred,group=permits)) +
  geom_line() +
  geom_ribbon(aes(ymin=low,ymax=high), alpha=0.4) +
  facet_wrap(~permits) + xlab("Year") + ylab("Predicted benefit of adding +1 spp (ln $)")

df = expand.grid("year"=1:29, "diversity" = seq(1,length(x1)), "permit"=1:6)
df$permits = permits[df$permit]
df$years = years[df$year]
df$x = specdiv.prev[df$diversity]
df$pred = 0
df$low = 0
df$high = 0
for(i in 1:nrow(df)) {
  tmp = x1[df$diversity[i]] * (est$g_1[,df$year[i], df$permit[i]]+est$g1_str_yr_mu) +
    x2[df$diversity[i]] * (est$g_2[,df$year[i], df$permit[i]]+est$g2_str_yr_mu)# +
  #x3[df$diversity[i]] * (est$b_3[,df$year[i], df$permit[i]]+est$b3_str_yr_mu)
  df$pred[i] = median(tmp)
  df$low[i] = quantile(tmp,0.25)
  df$high[i] = quantile(tmp,0.75)
}

ggplot(df[df$diversity==1,], aes(x=years,y=pred,group=permits)) +
  geom_line() +
  geom_ribbon(aes(ymin=low,ymax=high), alpha=0.4) +
  facet_wrap(~permits) + xlab("Year") + ylab("Predicted benefit of adding +1 spp (ln $)")
