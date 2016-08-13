data {
  int<lower=1> N; // n rows of data
  int<lower=1> J; // n fixed covariates on mean
  int<lower=1> K; // n fixed covariates on sigma

  matrix[N,J] X_ij; // fixed covariate matrix for mean
  matrix[N,K] X_sigma_ik; // fixed covariate matrix for sigma
  vector[N] y_i; // vector to hold observations

  int<lower=1> n_strategy;
  int<lower=1,upper=n_strategy> strategy_i[N]; // vector of IDs for strategy

  int<lower=1> n_person;
  int<lower=1,upper=n_person> person_i[N]; // vector of IDs for people

  vector[N] b1_cov_i; // predictor data for random slope
//  vector[N] b2_cov_i; // predictor data for random slope
//  vector[N] b3_cov_i; // predictor data for random slope
  vector[N] g1_cov_i; // predictor data for random slope (sigma)

  real b0_prior_mean;
  real b0_prior_sd;
  real b0_strategy_tau_prior_sd;

  vector[N] mean_div;
}
parameters {
  real b0;

  real b0_strategy[n_strategy];
  real<lower=0> b0_strategy_tau;

  real b0_pholder[n_person];
  real<lower=0> b0_pholder_tau;

  vector[J] b_j;
  vector[n_strategy] b1_strategy;
//  vector[n_strategy] b2_strategy;
//  vector[n_strategy] b3_strategy;
  real<lower=0> b1_strategy_tau;
//  real<lower=0> b2_strategy_tau;
//  real<lower=0> b3_strategy_tau;

  real g0;
  vector[n_strategy] g0_strategy;
  real<lower=0> g0_strategy_tau;

  vector[K] g_k;
  vector[n_strategy] g1_strategy;
  real<lower=0> g1_strategy_tau;

  real h1;
}
transformed parameters {
  vector[N] mu;
  vector[N] sigma;

  mu = b0 + X_ij * b_j;
  sigma = g0 + X_sigma_ik * g_k;

  for (i in 1:N) {
    mu[i] = mu[i] + b0_strategy[strategy_i[i]] +
                    b0_pholder[person_i[i]] +
                    b1_cov_i[i] * b1_strategy[strategy_i[i]];
                    // b2_cov_i[i] * b2_strategy[strategy_i[i]];
                    // b3_cov_i[i] * b3_strategy[strategy_i[i]];

    sigma[i] = sigma[i] + (g0_strategy[strategy_i[i]] + h1 * mean_div[i]) +
                          g1_cov_i[i] *g1_strategy[strategy_i[i]];
  }

  sigma = exp(sigma);
}
model {
  b0 ~ normal(b0_prior_mean, b0_prior_sd);
  b0_strategy ~ normal(0, b0_strategy_tau*2);
  b0_strategy_tau ~ student_t(3, 0, b0_strategy_tau_prior_sd);
  b0_pholder ~ normal(0, b0_pholder_tau);
  b0_pholder_tau ~ student_t(3, 0, 2);

  b_j ~ normal(0, 1);
  b1_strategy ~ normal(0, b1_strategy_tau);
  b1_strategy_tau ~ student_t(3, 0, 2);
//  b2_strategy_tau ~ student_t(3, 0, 2);
//  b3_strategy_tau ~ student_t(3, 0, 2);

  g0 ~ normal(0, 3);
  g0_strategy ~ normal(0, g0_strategy_tau);
  g0_strategy_tau ~ student_t(3, 0, 2);

  h1 ~ normal(0, 1);

  g_k ~ normal(0, 1);
  g1_strategy ~ normal(0, g1_strategy_tau);
  g1_strategy_tau ~ student_t(3, 0, 2);

  y_i ~ normal(mu, sigma);
}
