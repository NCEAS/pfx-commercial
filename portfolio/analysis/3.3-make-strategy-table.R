library(dplyr)
d <- readr::read_csv("data/strategies-labels.csv")

load("portfolio/data-generated/diff-dat-stan.rda")
d <- d %>% mutate(modeled = ifelse(strategy %in% md$strategy, "Yes", "No"))

# spp <- group_by(dat, strategy) %>%
  # summarise(
    # spp_l = round(quantile(specDiv, prob = 0.25), 1),
    # spp_h = round(quantile(specDiv, prob = 0.75), 1))

# d <- inner_join(d, spp)
d <- inner_join(d, select(md, strategy, strategy_mean_div))

d <- select(d, str_label, description, strategy_mean_div, modeled) %>%
  rename(Label = str_label, Description = description,
    `Over 100 permit holders` = modeled,
    `Mean spp. diversity` = strategy_mean_div) %>%
  arrange(`Mean spp. diversity`) %>%
  mutate(`Mean spp. diversity` = round(`Mean spp. diversity`, 1))

d <- d %>% filter(`Over 100 permit holders` == "Yes") %>%
  select(-`Over 100 permit holders`)

xtable::print.xtable(
  xtable::xtable(d,
    caption = "", digits = 1),
  include.rownames = FALSE,
  file = "portfolio/figs/strategy-table.tex",
  booktabs = TRUE,
  caption.placement = "top",
  size = "small",
  sanitize.text.function = identity,
  only.contents = TRUE,
  timestamp = NULL
)

n_grouped_permits <- nrow(d)
saveRDS(n_grouped_permits, "portfolio/data-generated/n_grouped_permits.rds")
