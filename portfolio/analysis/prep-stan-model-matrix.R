library(dplyr)

source("portfolio/analysis/cull-dat.R")
dat <- cullDat(diff = TRUE)

# downsample for fast testing
unique_holders <- unique(dat$p_holder)
n_sample <- round(length(unique_holders)*0.20)
set.seed(1)
dat <- dplyr::filter(dat, p_holder %in% base::sample(unique_holders, n_sample))
nrow(dat)

dat <- dat %>% group_by(strategy) %>% mutate(n=n()) %>% filter(n > 0)
nrow(dat)
length(unique(dat$strategy))

dat <- mutate(dat,
  revenue_change = log(revenue/revenue.prev),
  days_change = log((days_permit+1)/(days_permit.prev+1)),
  spec_change = log(specDiv/specdiv.prev))
dat$strategy_year <- paste(dat$strategy, dat$year, sep = ":")
dat$strategy_id <- as.numeric(as.factor(as.character(dat$strategy)))
dat$str_yr_id <- as.numeric(as.factor(as.character(dat$strategy_year)))
dat <- dat %>% group_by(strategy) %>%
  mutate(strategy_mean_div = mean(specDiv)) %>% as_data_frame

b1 <- function(x, bp = 0) ifelse(x < bp, x, 0)
b2 <- function(x, bp = 0) ifelse(x < bp, 0, x)

# NOTE: first and second coefficients must be species diversity slopes:
mm <- model.matrix(log(revenue) ~ -1 + b1(spec_change) + b2(spec_change) +
    days_change + b1(spec_change):days_change + b2(spec_change):days_change,
  data = dat)
mm2 <- mm

md <- select(dat, strategy_id, strategy_mean_div, strategy) %>% unique %>%
  arrange(strategy_id)

labels <- readr::read_csv("data/strategies-labels.csv")
md$str_label <- NULL
md <- left_join(md, select(labels, strategy, str_label))

look_str <- function(str) {
  x <- strsplit(str, " ")[[1]]
  x <- sapply(x, function(xx) labels$str_label[labels$strategy == xx])
  paste(x, collapse = ", ")
}

md$str_label <- sapply(md$strategy, function(x) look_str(x))
md$str_label <- gsub(" - ", " ", md$str_label)
