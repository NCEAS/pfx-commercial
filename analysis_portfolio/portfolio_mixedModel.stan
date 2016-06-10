data {
  int<lower=0> N; // rows of data
  #int<lower=0> n.xcov; // number covariates on mean
  #int<lower=0> n.xre; // number re on mean
  #int<lower=0> n.sigcov; // number covariates on variance
  #int<lower=0> n.sigre; // number re on variance
  real x_cov[N,1]; // covariate on mean
  int x_re[N,1]; // re on mean
  int<lower=0> nx_levels; // rows of data
  real sig_cov[N,1];// covariate on sigma2
  int sig_re[N,1];  // re on sigma2
  vector[N] y; // vector to hold observations
}
parameters {
  real b0_mu;
  real b0_dev[nx_levels]; 
  real<lower=0> b0_tau; 
  
  real b1_mu;
  real b1_dev[nx_levels]; 
  real<lower=0> b1_tau; 
  
  real sigma0_mu;
  real sigma0_dev[nx_levels]; 
  real<lower=0> sigma0_tau;
  
  real sigma1_mu;
  real sigma1_dev[nx_levels]; 
  real<lower=0> sigma1_tau;  
}
transformed parameters {
  vector[N] mu;
  vector[N] sigma;
  for (i in 1:N) {
    // mean includes global mean, deviations for each permit, shared slope + permit specific deviations
    mu[i] <- (b0_mu + b0_dev[x_re[i,1]]) + (b1_mu + b1_dev[x_re[i,1]])*x_cov[i,1];
    sigma[i] <- sqrt(exp(2*((sigma0_mu + sigma0_dev[x_re[i,1]]) + (sigma1_mu + sigma1_dev[x_re[i,1]])*x_cov[i,1])));
  }
}
model {
  b0_mu ~ normal(0, 100); # mean 
  b0_dev ~ normal(0,b0_tau); # random effects
  b0_tau ~ cauchy(0, 5);
  b1_mu ~ normal(0, 100); # mean 
  b1_dev ~ normal(0,b1_tau); # random effects
  b1_tau ~ cauchy(0, 5);  
  
  sigma0_mu ~ normal(0, 100); # mean 
  sigma0_dev ~ normal(0,sigma0_tau); # random effects
  sigma0_tau ~ cauchy(0, 5);
  sigma1_mu ~ normal(0, 100); # mean 
  sigma1_dev ~ normal(0,sigma1_tau); # random effects
  sigma1_tau ~ cauchy(0, 5);

  y ~ normal(mu, sigma);
}

