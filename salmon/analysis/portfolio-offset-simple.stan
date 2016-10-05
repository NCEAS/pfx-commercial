data {
  int<lower=1> N; // n rows of data

  vector[N] y_i; // vector to hold observations
  vector[N] offset; // revenue in the previous year

  vector[N] cov; // revenue in the previous year

  int<lower=1> n_strategy;
  int<lower=1,upper=n_strategy> strategy_i[N]; // vector of IDs for strategy
  int<lower=1> n_yr;
  int<lower=1,upper=n_yr> year_i[N]; // vector of years
}
parameters {
  real<lower=0> b0_tau; # sd of random walk in intercept
  real b_offset;
  real<lower=0> g0_tau; # sd of random walk in intercept
  real g_offset;
  real b_cov;
  real g_cov;

  # random effects on mean
  vector[n_yr] mu_yr;
  vector[n_yr] sd_yr;
}
transformed parameters {
  vector[N] mu;
  vector[N] sigma;

  mu = offset;

  for (i in 1:N) {
    mu[i] = mu[i] + mu_yr[year_i[i]] + (strategy_i[i]-1)*b_offset + cov[i] * b_cov;
    sigma[i] = sd_yr[year_i[i]] + (strategy_i[i]-1)*g_offset + cov[i] * g_cov;
  }

  sigma = exp(sigma);
}
model {
  b0_tau ~ student_t(3, 0, 2); # sd of random walk in intercept
  b_offset ~ normal(0,1);
  b_cov ~ normal(0,1);
  g0_tau ~ student_t(3, 0, 2); # sd of random walk in intercept
  g_offset ~ normal(0,1);
  g_cov ~ normal(0,1);
  mu_yr[1] ~ student_t(3, 0, 2);
  sd_yr[1] ~ student_t(3, 0, 2);

  for(i in 2:n_yr) {
    mu_yr[i] ~ normal(mu_yr[i-1],b0_tau);
    sd_yr[i] ~ normal(sd_yr[i-1],g0_tau);
  }

  y_i ~ normal(mu, sigma);
}
