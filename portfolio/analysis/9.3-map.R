library(maps)
library(mapdata)
map("worldHires",".", xlim=c(-178,-130), ylim=c(51,73), col="gray80",
  fill=TRUE, mar = c(1,1,1,1), boundary = F, border = "grey80")
box(col = "grey40")

p <- readr::read_csv("data/map-points.csv")

for (i in 1:nrow(p)) {
  points(-1*p$long[i], p$lat[i], col = "red", pch = 19, cex = 1)
  graphics::text(-1*p$long[i], p$lat[i], labels = p$name[i], pos = 4)
}
