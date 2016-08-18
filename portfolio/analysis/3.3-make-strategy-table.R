library(dplyr)
d <- readr::read_csv("data/strategies-labels.csv")

load("portfolio/data-generated/diff-dat-stan.rda")
d <- d %>% mutate(modeled = ifelse(strategy %in% md$strategy, "Yes", "No"))

d <- select(d, str_label, description, modeled) %>%
  rename(Label = str_label, Description = description,
    `Over 100 permit holders` = modeled) %>%
  arrange(Label)
d <- d %>% filter(`Over 100 permit holders` == "Yes") %>%
  select(-`Over 100 permit holders`)

xtable::print.xtable(
  xtable::xtable(d,
    caption = ""),
  include.rownames = FALSE,
  file = "portfolio/figs/strategy-table.tex",
  booktabs = TRUE,
  caption.placement = "top",
  size = "small",
  sanitize.text.function = identity,
  only.contents = TRUE,
  timestamp = NULL
)

