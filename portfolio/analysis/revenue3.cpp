#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
// data:
DATA_MATRIX(x_ij);
DATA_VECTOR(y_i);
DATA_IVECTOR(k_i); // vector of IDs
DATA_INTEGER(n_k); // number of IDs

// parameters:
PARAMETER_VECTOR(b_j)
PARAMETER_VECTOR(sigma_j);
PARAMETER(log_b0_sigma);
PARAMETER_VECTOR(b0_k);

int n_data = y_i.size(); // get number of data points to loop over

// Linear predictor
vector<Type> linear_predictor_i(n_data);
vector<Type> linear_predictor_sigma_i(n_data);
linear_predictor_i = x_ij*b_j;
linear_predictor_sigma_i = sqrt(exp(x_ij*sigma_j));

Type nll = 0.0; // initialize negative log likelihood

for(int i = 0; i < n_data; i++){
  nll -= dnorm(y_i(i), b0_k(k_i(i)) + linear_predictor_i(i) , linear_predictor_sigma_i(i), true);
}
for(int k = 0; k < n_k; k++){
  nll -= dnorm(b0_k(k), Type(0.0), exp(log_b0_sigma), true);
}

REPORT( b0_k );
REPORT(b_j );

ADREPORT( b0_k );
ADREPORT( b_j );

return nll;
}
