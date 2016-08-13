library(dplyr)
d <- readr::read_csv("data/strategies-labels.csv")
d <- select(d, str_label, description) %>%
  rename(Label = str_label, Description = description) %>%
  arrange(Label)

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

