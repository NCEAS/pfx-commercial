data {
  int<lower=1> N; // rows of data
  int<lower=1> J; // # fixed covariates on mean
  int<lower=1> J_sig; // # fixed covariates on sigma

  matrix[N,J] X_ij; // fixed covariate matrix for mean
  matrix[N,J_sig] X_sig_ij; // fixed covariate matrix for var
  vector[N] y; // vector to hold observations

  int<lower=1> n_strategy;
  int<lower=1,upper=n_strategy> strategy_i[N]; // vector of IDs for strategy

  int<lower=1> n_person;
  int<lower=1,upper=n_person> person_i[N]; // vector of IDs for people

  vector[N] z1_i; // predictor data for random slope
//  vector[N] z2_i; // predictor data for random slope
//  vector[N] z3_i; // predictor data for random slope
  vector[N] z1_sig_i; // predictor data for random slope
//  vector[N] z2_sig_i; // predictor data for random slope
//  vector[N] z3_sig_i; // predictor data for random slope
}
parameters {
  real b0;

  vector[n_strategy] e_b0_dev_str;
  real<lower=0> b0_tau_str;

  vector[n_person] e_b0_dev_per;
  real<lower=0> b0_tau_per;

  vector[J] b1;
  vector[n_strategy] e_b1_dev;
//  real b2_dev[n_strategy];
//  real b3_dev[n_strategy];
  real<lower=0> b1_tau;
//  real<lower=0> b2_tau;
//  real<lower=0> b3_tau;

  real sigma0;
  vector[n_strategy] e_sigma0_dev;
  real<lower=0> sigma0_tau;

  vector[J_sig] sigma1;
  vector[n_strategy] e_b1_sig_dev;
//  real b2_sig_dev[n_strategy];
//  real b3_sig_dev[n_strategy];
  real<lower=0> b1_sig_tau;

//  real<lower=0> sigma;
}
transformed parameters {
  vector[N] mu;
  vector[N] sigma;
  vector[n_strategy] b0_dev_str;
  vector[n_person] b0_dev_per;
  vector[n_strategy] b1_dev;
  vector[n_strategy] sigma0_dev;
  vector[n_strategy] b1_sig_dev;

  mu = b0 + X_ij * b1;
  sigma = sigma0 + X_sig_ij * sigma1;

  b0_dev_str = e_b0_dev_str * b0_tau_str;
  b0_dev_per = e_b0_dev_per * b0_tau_per;
  b1_dev = e_b1_dev * b1_tau;
  sigma0_dev = e_sigma0_dev * sigma0_tau;
  b1_sig_dev = e_b1_sig_dev * b1_sig_tau;

  for (i in 1:N) {
    mu[i] = mu[i] + b0_dev_str[strategy_i[i]] +
                    b0_dev_per[person_i[i]] +
                    z1_i[i]*b1_dev[strategy_i[i]];
//                    z2_i[i]*b2_dev[strategy_i[i]];
//                    z3_i[i]*b3_dev[strategy_i[i]];

    sigma[i] = sigma[i] + sigma0_dev[strategy_i[i]] +
                     z1_sig_i[i]*b1_sig_dev[strategy_i[i]];
//                     z2_sig_i[i]*b2_sig_dev[strategy_i[i]] +
 //                    z3_sig_i[i]*b3_sig_dev[strategy_i[i]];

    sigma[i] = exp(sigma[i]);
  }
}
model {
  b0 ~ normal(0, 10);
  e_b0_dev_str ~ normal(0, 1);
  b0_tau_str ~ student_t(3, 0, 2);
  e_b0_dev_per ~ normal(0, 1);
  b0_tau_per ~ student_t(3, 0, 2);

  b1 ~ normal(0, 1);
  e_b1_dev ~ normal(0, b1_tau);
  b1_tau ~ student_t(3, 0, 2);
//  b2_tau ~ student_t(3, 0, 3);
//  b3_tau ~ student_t(3, 0, 3);

  sigma0 ~ normal(0, 5);
  e_sigma0_dev ~ normal(0, sigma0_tau);
  sigma0_tau ~ student_t(3, 0, 2);

  sigma1 ~ normal(0, 1);
  e_b1_sig_dev ~ normal(0, b1_sig_tau);
  b1_sig_tau ~ student_t(3, 0, 2);

  y ~ normal(mu, sigma);
}
