# convenience script

f <- list.files(path = file.path("portfolio", "analysis"), pattern = "^3\\.3-*")
f <- c(f, list.files(path = file.path("portfolio", "analysis"), pattern = "*check-*"))
f <- file.path("portfolio", "analysis", f)

lapply(f, source)
