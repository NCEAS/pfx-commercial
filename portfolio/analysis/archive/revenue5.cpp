#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
// data:
DATA_MATRIX(x_ij);
DATA_VECTOR(y_i);
DATA_IVECTOR(k_i); // vector of IDs
DATA_INTEGER(n_k); // number of IDs
DATA_VECTOR(x); // predictor data for random slope

// parameters:
PARAMETER_VECTOR(b_j)
PARAMETER_VECTOR(sigma_j);
PARAMETER(log_b0_sigma);
PARAMETER(log_b1_sigma);
PARAMETER_VECTOR(b0_k);
PARAMETER_VECTOR(b1_k);

PARAMETER(log_sigma0_sigma);
PARAMETER(log_sigma1_sigma);
PARAMETER_VECTOR(sigma0_k);
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
      b0_k(k_i(i)) +
      b1_k(k_i(i)) * x(i) +
      linear_predictor_i(i),
      sqrt(exp(sigma0_k(k_i(i)) +
      sigma1_k(k_i(i)) * x(i) +
      linear_predictor_sigma_i(i))),
      true);
}
for(int k = 0; k < n_k; k++){
  nll -= dnorm(b0_k(k), Type(0.0), exp(log_b0_sigma), true);
  nll -= dnorm(b1_k(k), Type(0.0), exp(log_b1_sigma), true);
  nll -= dnorm(sigma0_k(k), Type(0.0), exp(log_sigma0_sigma), true);
  nll -= dnorm(sigma1_k(k), Type(0.0), exp(log_sigma1_sigma), true);
}

REPORT( b0_k );
REPORT( b1_k );
REPORT( b_j );
REPORT( sigma0_k );
REPORT( sigma1_k );

ADREPORT( b0_k );
ADREPORT( b1_k );
ADREPORT( b_j );
ADREPORT( sigma0_k );
ADREPORT( sigma1_k );

return nll;
}
