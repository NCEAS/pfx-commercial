data {
  int<lower=1> N; // n rows of data

  vector[N] y_i; // vector to hold observations
  vector[N] offset; // revenue in the previous year

  int<lower=1> n_strategy;
  int<lower=1,upper=n_strategy> strategy_i[N]; // vector of IDs for strategy

  int<lower=1> n_str_yr;
  int<lower=1,upper=n_str_yr> str_yr_i[N]; // vector of IDs for strategy/years

  int<lower=1> n_yr;
  int<lower=1,upper=n_yr> year_i[N]; // vector of years

  vector[N] b1_cov_i; // predictor data for random slope
  vector[N] b2_cov_i; // predictor data for random slope
  vector[N] b3_cov_i; // predictor data for random slope
  vector[N] b4_cov_i; // predictor data for random slope

  vector[N] g1_cov_i; // predictor data for random slope
  vector[N] g2_cov_i; // predictor data for random slope
  vector[N] g3_cov_i; // predictor data for random slope
  vector[N] g4_cov_i; // predictor data for random slope

}
parameters {
  real b0_str_yr[n_str_yr];
  real<lower=0> b0_str_yr_tau;
  vector[n_strategy] b_1[n_yr];
  real<lower=0> b1_str_yr_tau;
  vector[n_strategy] b_4;
  real<lower=0> b4_str_yr_tau;
  #real b1_str_yr_mu;
  real b4_str_yr_mu;
  real g0;
  vector[n_strategy] g0_strategy;
  real<lower=0> g0_strategy_tau;
  # random effects on sigma
  vector[n_strategy] g_1[n_yr];
  real<lower=0> g1_str_yr_tau;
  vector[n_strategy] g_4;
  real<lower=0> g4_str_yr_tau;

  #real g1_str_yr_mu;
  real g4_str_yr_mu;
}
transformed parameters {
  vector[N] mu;
  vector[N] sigma;

  mu = offset;

  for (i in 1:N) {
    mu[i] = mu[i] +
            b0_str_yr[str_yr_i[i]] +
            b1_cov_i[i] * b_1[year_i[i], strategy_i[i]]+
            b4_cov_i[i] * b_4[strategy_i[i]];
    sigma[i] = g0 +
            g0_strategy[strategy_i[i]] +
            g1_cov_i[i] * g_1[year_i[i], strategy_i[i]] +
            g4_cov_i[i] * g_4[strategy_i[i]];
  }

  sigma = exp(sigma);
}
model {
  b0_str_yr ~ normal(0, b0_str_yr_tau); # strategy-year intercept
  b0_str_yr_tau ~ student_t(3, 0, 2);
  b_4 ~ normal(b4_str_yr_mu, b4_str_yr_tau); # strategy-year intercept
  b4_str_yr_mu ~ student_t(3, 0, 2);
  b4_str_yr_tau ~ student_t(3, 0, 2);

  # b_1 is array nYear x nPermit
  b1_str_yr_tau ~ student_t(3, 0, 2);
  #b1_str_yr_mu ~ student_t(3, 0, 2);
  for(i in 1:n_strategy) {
    b_1[1,i] ~ normal(0, 2);# year 1
    for(t in 2:n_yr) {
      b_1[t,i] ~ normal(b_1[t-1,i], b1_str_yr_tau);
    }
  }

  g0 ~ normal(0, 2);
  g0_strategy ~ normal(0, g0_strategy_tau);
  g0_strategy_tau ~ student_t(3, 0, 2);
  g_4 ~ normal(g4_str_yr_mu, g4_str_yr_tau); # strategy-year intercept
  g4_str_yr_mu ~ student_t(3, 0, 2);
  g4_str_yr_tau ~ student_t(3, 0, 2);

  # b_1 is array nYear x nPermit
  g1_str_yr_tau ~ student_t(3, 0, 2);
  #g1_str_yr_mu ~ student_t(3, 0, 2);
  for(i in 1:n_strategy) {
    g_1[1,i] ~ normal(0, 2);# year 1
    for(t in 2:n_yr) {
      g_1[t,i] ~ normal(g_1[t-1,i], g1_str_yr_tau);
    }
  }

  y_i ~ normal(mu, sigma);
}
