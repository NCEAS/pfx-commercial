#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
// data:
DATA_MATRIX(x_ij); // fixed effect model matrix
DATA_VECTOR(y_i); // response vector
DATA_IVECTOR(pholder_k_i); // vector of IDs for strategy
DATA_IVECTOR(strategy_k_i); // vector of IDs for permit holder
DATA_INTEGER(n_pholder_k); // number of IDs for pholder
DATA_INTEGER(n_strategy_k); // number of IDs for strategy
DATA_INTEGER(n_j); // fixed effect column position of diversity
DATA_VECTOR(b1_cov_re_i); // predictor data for random slope
DATA_VECTOR(sigma1_cov_re_i); // predictor data for random slope

// parameters:
PARAMETER_VECTOR(b_j)
PARAMETER_VECTOR(sigma_j);
PARAMETER(log_b0_pholder_tau);
PARAMETER(log_b0_strategy_tau);
PARAMETER(log_b1_tau);
PARAMETER_VECTOR(b0_pholder_k);
PARAMETER_VECTOR(b0_strategy_k);
PARAMETER_VECTOR(b1_k);

PARAMETER(log_sigma0_pholder_tau);
PARAMETER(log_sigma0_strategy_tau);
PARAMETER(log_sigma1_tau);
PARAMETER_VECTOR(sigma0_pholder_k);
PARAMETER_VECTOR(sigma0_strategy_k);
PARAMETER_VECTOR(sigma1_k);

int n_data = y_i.size(); // get number of data points to loop over

// Linear predictor
vector<Type> linear_predictor_i(n_data);
vector<Type> linear_predictor_sigma_i(n_data);
linear_predictor_i = x_ij*b_j;
linear_predictor_sigma_i = x_ij*sigma_j;

Type nll = 0.0; // initialize negative log likelihood

for(int i = 0; i < n_data; i++){
  nll -= dnorm(
      y_i(i),

      b0_pholder_k(pholder_k_i(i)) +
      b0_strategy_k(strategy_k_i(i)) +
      b1_k(strategy_k_i(i)) * b1_cov_re_i(i) +
      linear_predictor_i(i),

      sqrt(exp(
          sigma0_pholder_k(pholder_k_i(i)) +
          sigma0_strategy_k(strategy_k_i(i)) +
          sigma1_k(strategy_k_i(i)) * sigma1_cov_re_i(i) +
          linear_predictor_sigma_i(i))),

      true);
}
for(int k = 0; k < n_pholder_k; k++){
  nll -= dnorm(b0_pholder_k(k), Type(0.0), exp(log_b0_pholder_tau), true);
  nll -= dnorm(sigma0_pholder_k(k), Type(0.0), exp(log_sigma0_pholder_tau), true);
}
for(int k = 0; k < n_strategy_k; k++){
  nll -= dnorm(b1_k(k), Type(0.0), exp(log_b1_tau), true);
  nll -= dnorm(sigma1_k(k), Type(0.0), exp(log_sigma1_tau), true);
  nll -= dnorm(b0_strategy_k(k), Type(0.0), exp(log_b0_strategy_tau), true);
  nll -= dnorm(sigma0_strategy_k(k), Type(0.0), exp(log_sigma0_strategy_tau), true);
}

// Reporting
/* Type b0_pholder_tau = exp(log_b0_pholder_tau); */
/* Type b0_strategy_tau = exp(log_b0_strategy_tau); */
Type b1_tau = exp(log_b1_tau);
/* Type sigma0_pholder_tau = exp(log_sigma0_pholder_tau); */
/* Type sigma0_strategy_tau = exp(log_sigma0_strategy_tau); */
Type sigma1_tau = exp(log_sigma1_tau);

vector<Type> b1_b1_k(n_strategy_k);
vector<Type> sigma1_sigma1_k(n_strategy_k);
for(int k = 0; k < n_strategy_k; k++){
  b1_b1_k(k) = b_j(n_j) + b1_k(k);
  sigma1_sigma1_k(k) = sigma_j(n_j) + sigma1_k(k);
}

/* REPORT(b0_pholder_k); */
REPORT(b0_strategy_k);
REPORT(b1_k);
REPORT(b_j);
/* REPORT(sigma0_pholder_k); */
REPORT(sigma0_strategy_k);
REPORT(sigma1_k);
/* REPORT(b0_tau); */
REPORT(b1_tau);
/* REPORT(sigma0_tau); */
REPORT(sigma1_tau);
REPORT(b1_b1_k);
REPORT(sigma1_sigma1_k);

/* ADREPORT(b0_pholder_k); */
ADREPORT(b0_strategy_k);
ADREPORT(b1_k);
ADREPORT(b_j);
/* ADREPORT(sigma0_pholder_k); */
ADREPORT(sigma0_strategy_k);
ADREPORT(sigma1_k);
/* ADREPORT(b0_tau); */
ADREPORT(b1_tau);
/* ADREPORT(sigma0_tau); */
ADREPORT(sigma1_tau);
ADREPORT(b1_b1_k);
ADREPORT(sigma1_sigma1_k);

return nll;
}
