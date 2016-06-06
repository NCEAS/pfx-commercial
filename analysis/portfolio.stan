data {
  int<lower=0> N; // rows of data
  vector[N] y; // vector to hold observations
  vector[N] x; // vector to hold predictor
}
parameters {
  real b0;
  real b1;
  real sigma0;
  real sigma1;
}
transformed parameters {
  vector[N] mu;
  vector[N] sigma;
  for (i in 1:N) {
    mu[i] <- b0 + b1*x[i];
    sigma[i] <- sqrt(exp(2*(sigma0 + sigma1*x[i])));
  }
}
model {
  b0 ~ normal(0, 100);
  b1 ~ normal(0, 5);
  sigma0 ~ normal(0, 5);
  sigma1 ~ normal(0, 5);
  y ~ normal(mu, sigma);
}

