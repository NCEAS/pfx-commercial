library(dplyr)
library(ggplot2)
load("portfolio/data-generated/diff-dat-stan.rda")
load("portfolio/data-generated/m.rda")
devtools::load_all("pfxr")
library(lme4)

b1 <- function(x, bp = 0) ifelse(x < bp, x, 0)
b2 <- function(x, bp = 0) ifelse(x < bp, 0, x)

d_B61BC61B <- filter(dat, strategy == "B61B C61B")

m_B61BC61B <- lmer(
  log(revenue) ~ -1 + b1(spec_change) + b2(spec_change) +
    days_change + b1(spec_change):days_change + b2(spec_change):days_change +
    (1|strategy_year),
  data = d_B61BC61B, offset = log(revenue.prev))

a_B61BC61B <- broom::augment(m_B61BC61B)
d_B61BC61B <- cbind(d_B61BC61B, a_B61BC61B)

re_lmer <- coef(m_lmer)$strategy
re_lmer$strategy <- row.names(re_lmer)
row.names(re_lmer) <- NULL
library(tidyr)
