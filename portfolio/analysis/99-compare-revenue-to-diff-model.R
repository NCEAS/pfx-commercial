# subset
dat = cullDat()
diff.data = cullDat(TRUE)
unique_holders <- unique(diff.data$p_holder)
n_sample <- round(length(unique_holders)*0.3)
set.seed(1)
diff.data <- dplyr::filter(diff.data, p_holder %in% base::sample(unique_holders, n_sample))
nrow(diff.data)

dat = inner_join(dat, unique(select(diff.data, p_holder, strategy)))
dat = filter(dat, ! is.na(revenue.prev))

dat = group_by(dat, p_holder, strategy) %>% mutate(centeredRev = log(revenue) - mean(log(revenue),na.rm=T))

mod <- glmmTMB(centeredRev ~ (scaled_spec_div + log_days_permit)^2 +
    (scaled_spec_div + log_days_permit|strategy), data = dat)
mod.1 <- glmmTMB(log(revenue) ~ log(specDiv/specdiv.prev) * log((days_permit+1)/(days_permit.prev+1)) +
    (log(specDiv/specdiv.prev) + log((days_permit+1)/(days_permit.prev+1))|strategy), data = dat, offset = log(revenue.prev))

diff.data$logdiff = log(diff.data$revenue/diff.data$revenue.prev)
diff.data$days.change = log((diff.data$days_permit+1)/(diff.data$days_permit.prev + 1))

diff.mod <- glmmTMB(logdiff ~ specDiv.change*days.change  +
    (specDiv.change + days.change|strategy), data = diff.data)

spec.re = ranef(mod)$cond$strategy[,2] + summary(mod)$coefficients$cond[2,1]
diff.re = ranef(diff.mod)$cond$strategy[,2] + summary(diff.mod)$coefficients$cond[2,1]

plot(spec.re, diff.re, type="n")
text(spec.re, diff.re, rownames(ranef(mod)$cond$strategy))

p1 = filter(dat,strategy=="B61B") %>%
  ggplot(aes(scaled_spec_div, log(revenue), color = log_days_permit),alpha=0.3) + geom_point()
p2 = filter(diff.data,strategy=="B61B") %>%
  ggplot(aes(specDiv.change, logdiff, color = days.change),alpha=0.3) + geom_point()

gridExtra::grid.arrange(p1,p2)

qqq<-filter(dat,strategy=="B61B")
pholders <- unique(qqq$p_holder)[100:160]

p1 = filter(dat,strategy=="B61B"&p_holder%in%pholders) %>%
  ggplot(aes(scaled_spec_div, centeredRev, color = log_days_permit),alpha=0.3) +
  geom_point() +
  facet_wrap(~p_holder)
p1

p1 = filter(dat, p_holder == 8977) %>%
  ggplot(aes(scaled_spec_div, centeredRev,label=year,colour=year))+ geom_text() +geom_path()
p2 = filter(diff.data, p_holder == 8977) %>%
  ggplot(aes(specDiv.change, logdiff, label=year,colour=year))+ geom_text()+geom_path()
gridExtra::grid.arrange(p1,p2)

d1 = filter(dat, p_holder == 8977)
d2 = filter(diff.data, p_holder == 8977)

library(nlme)
m1.1 <- gls(centeredRev~scaled_spec_div, data = d1)
m1.2 <- gls(centeredRev~scaled_spec_div, data = d1, correlation=corAR1())
