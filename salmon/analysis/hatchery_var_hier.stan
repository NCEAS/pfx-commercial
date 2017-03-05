data {
  int<lower=0> N; # number of time steps
  int fishery_species[N];
  int fishery[N];
  int species[N];
  vector[N] y;
  vector[N] x;
  vector[N] x2;
  vector[N] yprev;
  #vector[N] yprev2;
  #vector[N] prev_pct_hatchery;
  int M; # number of random effects
}
parameters {
  real sigma1[M];
  real sigma0[M];
  real musigma1;
  #real musigma0;
  real<lower=0> sigsigma1;
  #real<lower=0> sigsigma0;
  real b1[M];
  #real<lower=-0.99,upper=0.99> phi[M];
  real mub1;
  real<lower=0> sigb1;
}
transformed parameters {
  vector[N] pred;
  vector[N] pred_sig;
  real effect_sig[M];
  for(i in 1:N) {
    # (yprev[i] - (yprev2[i] + b1[fishery_species[i]]*prev_pct_hatchery[i]))
    pred[i] = yprev[i] + b1[fishery_species[i]]*x[i];# + phi[fishery_species[i]]*(yprev[i] - (yprev2[i] + b1[fishery_species[i]]*prev_pct_hatchery[i]));
    pred_sig[i] = exp(sigma0[fishery_species[i]] + sigma1[fishery_species[i]]*x[i]);
  }
  for(i in 1:M) {
  effect_sig[i] = log(exp(sigma0[i] + 0.2*sigma1[i] ) / exp(sigma0[i]));
  }
}
model {
  musigma1 ~ normal(0,1);
  mub1 ~ normal(0,1);
  sigsigma1 ~ student_t(5,0,2);
  sigb1 ~ student_t(5,0,2);

  # M is fishery - species terms
  for(i in 1:M) {
    b1[i] ~ normal(mub1,sigb1);
    sigma1[i] ~ normal(musigma1,sigsigma1); # random slopes
    sigma0[i] ~ normal(0,2); # fixed effect
    #phi[i] ~ normal(0,5);
  }

  # likelihood
  for(i in 1:N) {
    y[i] ~ normal(pred[i], pred_sig[i]);
  }

}
generated quantities {
  vector[N] log_lik;
  # regresssion example in loo() package
  for (n in 1:N) log_lik[n] = normal_lpdf(y[n] | pred[n], pred_sig[n]);
}

