library(dplyr)
library(ggplot2)
#load("portfolio/data-generated/diff-dat-stan.rda")
#load("portfolio/data-generated/m.rda")
devtools::load_all("pfxr")
library(lme4)

source("portfolio/analysis/3.0-prep-model-matrix.R")
load("portfolio/data-generated/diff-dat-stan.rda")

b1 <- function(x, bp = 0) ifelse(x < bp, x, 0)
b2 <- function(x, bp = 0) ifelse(x < bp, 0, x)

d_B61BC61B <- filter(dat, strategy == "B61B C61B")
d_B61BC61B <- filter(dat, strategy == "K91 T09")
d_B61BC61B <- filter(dat, strategy == "C61B")

m_B61BC61B <- lmer(
  log(revenue/revenue.prev) ~ -1 + b1(spec_change) + b2(spec_change) +
    days_change + b1(spec_change):days_change + b2(spec_change):days_change +
    (1|strategy_year),
  data = d_B61BC61B)

a_B61BC61B <- broom::augment(m_B61BC61B)
d_B61BC61B <- cbind(d_B61BC61B, select(a_B61BC61B, .resid, .fitted))

# residual plot
group_by(d_B61BC61B, year) %>%
  summarize(sd = sd(.resid)) %>%
  ggplot(aes(year,sd)) + geom_line()

library(tidyr)

g = group_by(d_B61BC61B, p_holder) %>%
  summarize(n = length(unique(year))) %>%
  filter(n >= 10)

s = sample(g$p_holder, size=30, replace=F)

filter(d_B61BC61B, p_holder%in%s) %>%
  ggplot(aes(year, log(revenue/revenue.prev))) + geom_line(aes(y = .fitted)) +
  geom_point(aes(color = spec_change), size=2) +
  facet_wrap(~ p_holder, scale="free_y") +
  scale_color_gradient2()

881, 32568, 25210

cfec = feather::read_feather("portfolio/data-generated/cfec.feather")
cfec$year <- as.numeric(cfec$year)
sub = cfec[cfec$p_holder == 25210,]

group_by(sub, year, spec) %>%
  summarize(tot = sum(g_earn)) %>%
  ggplot(aes(year, tot, color = spec)) +
  geom_area(aes(fill = spec), position = "stack", colour = 1)

# for crab, 6376



