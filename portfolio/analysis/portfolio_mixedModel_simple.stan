data {
  int<lower=0> N; // rows of data
  int<lower=0> K_mean; // # fixed covariates on mean
  int<lower=0> K_sig; // # fixed covariates on sigma2
  int<lower=0> K_mean_re; // # random covariates on mean
  int<lower=0> K_sig_re; // # random covariates on sigma2  
  matrix[N,1] b1_cov; // fixed covariate on mean
  matrix[N,1] sigma1_cov; // fixed covariate on mean
  matrix[N,1] b1_cov_re; // fixed covariate on mean
  matrix[N,1] sigma1_cov_re; // fixed covariate on mean  
  int<lower=0> b0_re_n; // random effects on intercept
  int<lower=0> b0_re_levels[b0_re_n+1]; // number of levels of each. Add +1 for case where n=1
  int b0_re[N,b0_re_n];

  int<lower=0> b1_re_n; // random effects on intercept
  int<lower=0> b1_re_levels[b1_re_n+1]; // number of levels of each. Add +1 for case where n=1
  int b1_re[N,b1_re_n];

  int<lower=0> sigma0_re_n; // random effects on intercept
  int<lower=0> sigma0_re_levels[sigma0_re_n+1]; // number of levels of each. Add +1 for case where n=1
  int sigma0_re[N,sigma0_re_n];

  int<lower=0> sigma1_re_n; // random effects on intercept
  int<lower=0> sigma1_re_levels[sigma1_re_n+1]; // number of levels of each. Add +1 for case where n=1
  int sigma1_re[N,sigma1_re_n];
  
  vector[N] y; // vector to hold observations
}
parameters {
  real b0_mu;
  real b0_dev[max(b0_re_levels)];
  real<lower=0> b0_tau; 

  vector[K_mean] b1_mu;
  real b1_dev[max(b1_re_levels)];
  real<lower=0> b1_tau; 

  real sigma0_mu;
  real sigma0_dev[max(sigma0_re_levels)];
  real<lower=0> sigma0_tau; 

  vector[K_sig] sigma1_mu;
  real sigma1_dev[max(sigma1_re_levels)];
  real<lower=0> sigma1_tau; 

}
transformed parameters {
  vector[N] mu;
  vector[N] sigma;
  for (i in 1:N) {
    // mean includes global mean, fixed effects, and random effects in intercept / slope
    mu[i] <- b0_mu + b0_dev[b0_re[i,1]]; // add random effects in intercept
    mu[i] <- mu[i] + b1_cov[i,]*b1_mu + b1_cov_re[i,1]*b1_dev[b1_re[i,1]]; // add random effects

    sigma[i] <- sigma0_mu + sigma0_dev[sigma0_re[i,1]]; // add random effects in intercept
    sigma[i] <- sigma[i] + sigma1_cov[i,]*sigma1_mu + sigma1_cov_re[i,1]*sigma1_dev[sigma1_re[i,1]]; // add random effects
    sigma[i] <- sqrt(exp(2*sigma[i]));
  }
}
model {
  b0_mu ~ normal(0, 100); # mean intercept
  b0_dev ~ normal(0,b0_tau); # random effects for intercept
  b0_tau ~ cauchy(0, 5);
  
  b1_mu ~ normal(0, 100); # mean slope
  b1_dev ~ normal(0,b1_tau); # random effects for slope
  b1_tau ~ cauchy(0, 5);  

  sigma0_mu ~ normal(0, 100); # mean variance
  sigma0_dev ~ normal(0,sigma0_tau); # random effects for intercept
  sigma0_tau ~ cauchy(0, 5);

  sigma1_mu ~ normal(0, 100); # mean slope on variance
  sigma1_dev ~ normal(0,sigma1_tau); # random effects for slope
  sigma1_tau ~ cauchy(0, 5);
  
  y ~ normal(mu, sigma);
}

