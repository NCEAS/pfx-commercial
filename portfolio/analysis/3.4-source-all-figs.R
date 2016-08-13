# convenience script

plot_fig1 <- FALSE

f <- list.files(path = file.path("portfolio", "analysis"), pattern = "^3\\.3-*")
if (!plot_fig1) f <- f[!grepl("map", f)]
f <- c(f, list.files(path = file.path("portfolio", "analysis"), pattern = "*check-*"))
f <- file.path("portfolio", "analysis", f)

lapply(f, source)
