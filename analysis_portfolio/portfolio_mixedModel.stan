data {
  int<lower=0> N; // rows of data
  int<lower=0> K_mean; // # fixed covariates on mean
  int<lower=0> K_sig; // # fixed covariates on sigma2
  int<lower=0> K_mean_re; // # random covariates on mean
  int<lower=0> K_sig_re; // # random covariates on sigma2  
  matrix[N,K_mean] b1_cov; // fixed covariate on mean
  matrix[N,K_sig] sigma1_cov; // fixed covariate on mean
  matrix[N,K_mean_re] b1_cov_re; // fixed covariate on mean
  matrix[N,K_sig_re] sigma1_cov_re; // fixed covariate on mean  
  int<lower=0> b0_re_n; // random effects on intercept
  int<lower=0> b0_re_levels[b0_re_n]; // number of levels of each
  int b0_re[N,b0_re_n];

  int<lower=0> b1_re_n; // random effects on intercept
  int<lower=0> b1_re_levels[b1_re_n]; // number of levels of each
  int b1_re[N,b1_re_n];

  int<lower=0> sigma0_re_n; // random effects on intercept
  int<lower=0> sigma0_re_levels[sigma0_re_n]; // number of levels of each
  int sigma0_re[N,sigma0_re_n];

  int<lower=0> sigma1_re_n; // random effects on intercept
  int<lower=0> sigma1_re_levels[sigma1_re_n]; // number of levels of each
  int sigma1_re[N,sigma1_re_n];
  
  vector[N] y; // vector to hold observations
}
parameters {
  real b0_mu;
  real b0_dev[max(b0_re_levels),b0_re_n];
  real<lower=0> b0_tau[b0_re_n]; 

  vector[K_mean] b1_mu;
  real b1_dev[max(b1_re_levels),b1_re_n];
  real<lower=0> b1_tau[b1_re_n]; 

  real sigma0_mu;
  real sigma0_dev[max(sigma0_re_levels),sigma0_re_n];
  real<lower=0> sigma0_tau[sigma0_re_n]; 

  vector[K_sig] sigma1_mu;
  real sigma1_dev[max(sigma1_re_levels),sigma1_re_n];
  real<lower=0> sigma1_tau[sigma1_re_n]; 

}
transformed parameters {
  vector[N] mu;
  vector[N] sigma;
  for (i in 1:N) {
    // mean includes global mean, fixed effects, and random effects in intercept / slope
    mu[i] <- b0_mu + b1_cov[i,]*b1_mu; // fixed effect linear model
    for(j in 1:b0_re_n) {
      mu[i] <- mu[i] + b0_dev[b0_re[i,j],j]; // add random effects in intercept
    }
    for(j in 1:K_mean_re) {
      for(k in 1:b1_re_n) {
        mu[i] <- mu[i] + b1_cov_re[i,j]*b1_dev[b1_re[i,k],k]; // add random effects in slope on mean
      }
    }    

    sigma[i] <- sigma0_mu + sigma1_cov[i,]*sigma1_mu; // fixed effect linear model for variance
    for(j in 1:sigma0_re_n) {
      sigma[i] <- sigma[i] + sigma0_dev[sigma0_re[i,j],j]; // add random effects in intercept on var
    }
    for(j in 1:K_sig_re) {
      for(k in 1:sigma1_re_n) {
        sigma[i] <- sigma[i] + sigma1_cov_re[i,j]*sigma1_dev[sigma1_re[i,k],k]; // add random effects in slope on var
      }
    }       
    sigma[i] <- sqrt(exp(2*sigma[i]));
  }
}
model {
  b0_mu ~ normal(0, 100); # mean intercept
  for(i in 1:b0_re_n) {
    b0_tau[i] ~ cauchy(0, 5);
    b0_dev[i] ~ normal(0,b0_tau[i]); # random effects for intercept
  }

  b1_mu ~ normal(0, 100); # mean slope
  for(i in 1:b1_re_n) {
    b1_tau[i] ~ cauchy(0, 5);
    b1_dev[i] ~ normal(0,b1_tau[i]); # random effects for slope
  }  

  sigma0_mu ~ normal(0, 100); # mean variance
  for(i in 1:sigma0_re_n) {
    sigma0_tau[i] ~ cauchy(0, 5);
    sigma0_dev[i] ~ normal(0,sigma0_tau[i]); # random effects for intercept
  }

  sigma1_mu ~ normal(0, 100); # mean slope on variance
  for(i in 1:sigma1_re_n) {
    sigma1_tau[i] ~ cauchy(0, 5);
    sigma1_dev[i] ~ normal(0,sigma1_tau[i]); # random effects for slope
  }  
  
  y ~ normal(mu, sigma);
}

