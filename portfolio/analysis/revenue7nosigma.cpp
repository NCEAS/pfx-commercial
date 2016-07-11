#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
// data:
DATA_MATRIX(x_ij); // fixed effect model matrix
DATA_VECTOR(y_i); // response vector
DATA_IVECTOR(pholder_i); // vector of IDs for strategy
DATA_IVECTOR(strategy_i); // vector of IDs for permit holder
DATA_INTEGER(n_pholder); // number of IDs for pholder
DATA_INTEGER(n_strategy); // number of IDs for strategy
DATA_INTEGER(diversity_column); // fixed effect column position of diversity
DATA_VECTOR(b1_cov_re_i); // predictor data for random slope
// DATA_VECTOR(g1_cov_re_i); // predictor data for random slope

// parameters:
PARAMETER_VECTOR(b_j);
PARAMETER(sigma);
PARAMETER(log_b0_pholder_tau);
PARAMETER(log_b1_pholder_tau);
PARAMETER(log_b0_strategy_tau);
PARAMETER(log_b1_strategy_tau);
PARAMETER_VECTOR(b0_pholder);
PARAMETER_VECTOR(b1_pholder);
PARAMETER_VECTOR(b0_strategy);
PARAMETER_VECTOR(b1_strategy);

// PARAMETER(log_g0_pholder_tau);
// PARAMETER(log_g0_strategy_tau);
// PARAMETER(log_g1_tau);
// PARAMETER_VECTOR(g0_pholder);
// PARAMETER_VECTOR(g0_strategy);
// PARAMETER_VECTOR(g1_strategy);

int n_data = y_i.size();

// Linear predictor
vector<Type> linear_predictor_i(n_data);
vector<Type> linear_predictor_sigma_i(n_data);
linear_predictor_i = x_ij*b_j;
// linear_predictor_sigma_i = x_ij*sigma_j;

Type nll = 0.0; // initialize negative log likelihood

for(int i = 0; i < n_data; i++){
  nll -= dnorm(
      y_i(i),

      b0_pholder(pholder_i(i)) +
      b1_pholder(pholder_i(i)) * b1_cov_re_i(i) +
      b0_strategy(strategy_i(i)) +
      b1_strategy(strategy_i(i)) * b1_cov_re_i(i) +
      linear_predictor_i(i),

      sqrt(exp(sigma)),
          // g0_pholder(pholder_i(i)) +
          // g0_strategy(strategy_i(i)) +
          // g1_strategy(strategy_i(i)) * g1_cov_re_i(i) +
          // linear_predictor_sigma_i(i))),

      true);
}
for(int k = 0; k < n_pholder; k++){
  nll -= dnorm(b0_pholder(k), Type(0.0), exp(log_b0_pholder_tau), true);
  // nll -= dnorm(g0_pholder(k), Type(0.0), exp(log_g0_pholder_tau), true);
  nll -= dnorm(b1_pholder(k), Type(0.0), exp(log_b1_pholder_tau), true);
}
for(int k = 0; k < n_strategy; k++){
  nll -= dnorm(b1_strategy(k), Type(0.0), exp(log_b1_strategy_tau), true);
  // nll -= dnorm(g1_strategy(k), Type(0.0), exp(log_g1_strategy_tau), true);
  nll -= dnorm(b0_strategy(k), Type(0.0), exp(log_b0_strategy_tau), true);
  // nll -= dnorm(g0_strategy(k), Type(0.0), exp(log_g0_strategy_tau), true);
}

// Reporting
/* Type b0_pholder_tau = exp(log_b0_pholder_tau); */
/* Type b0_strategy_tau = exp(log_b0_strategy_tau); */
// Type b1_strategy_tau = exp(log_b1_strategy_tau);
/* Type g0_pholder_tau = exp(log_g0_pholder_tau); */
/* Type g0_strategy_tau = exp(log_g0_strategy_tau); */
// Type g1_tau = exp(log_g1_tau);

// vector<Type> combined_b1_strategy(n_strategy);
// // vector<Type> combined_g1_strategy(n_strategy);
// for(int k = 0; k < n_strategy; k++){
//   // these are fixed-effect slopes + random-effect slopes
//   combined_b1_strategy(k) = b_j(diversity_column) + b1_strategy(k);
//   // combined_g1_strategy(k) = sigma_j(diversity_column) + g1_strategy(k);
// }

REPORT(b0_pholder);
REPORT(b0_strategy);
REPORT(b1_strategy);
REPORT(b_j);
/* REPORT(g0_pholder); */
// REPORT(g0_strategy);
// REPORT(g1_strategy);
/* REPORT(b0_tau); */
// REPORT(b1_tau);
/* REPORT(g0_tau); */
// REPORT(g1_tau);
// REPORT(combined_b1_strategy);
// REPORT(combined_g1_strategy);

// /* ADREPORT(b0_pholder); */
// ADREPORT(b0_strategy);
// ADREPORT(b1_strategy);
// ADREPORT(b_j);
// /* ADREPORT(g0_pholder); */
// ADREPORT(g0_strategy);
// ADREPORT(g1_strategy);
// /* ADREPORT(b0_tau); */
// ADREPORT(b1_tau);
// /* ADREPORT(g0_tau); */
// ADREPORT(g1_tau);
// ADREPORT(combined_b1_strategy);
// ADREPORT(combined_g1_strategy);

return nll;
}
