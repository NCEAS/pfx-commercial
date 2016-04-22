semi_variance <- function(x, mar = mean(x, na.omit = TRUE), full = TRUE) {
  x <- na.omit(x)
  x_mar <- x[x < mar]
  if (full)
    len <- length(x)
  else
    len <- length(x_mar)
  sum((x_mar - mar)^2)/len
}
semi_deviation <- function(x, ...) {
  sqrt(semi_variance(x, ...))
}
# Variance at Risk:
var_ <- function(x, prob = 0.05, ...) {
  quantile(x, probs = prob, ...)
}
# Conditional Value at Risk:
cvar <- function(x, prob = 0.05, ...) {
  mean(x[x < var_(x, prob = prob, ...)])
}
