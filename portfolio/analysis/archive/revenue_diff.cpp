#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
// data:
DATA_MATRIX(x_ij);
DATA_VECTOR(y_i);
DATA_IVECTOR(k_i); // vector of IDs
DATA_INTEGER(n_k); // number of IDs
DATA_INTEGER(n_j); // number of IDs
DATA_VECTOR(b1_cov_re_i); // predictor data for random slope
DATA_VECTOR(sigma1_cov_re_i); // predictor data for random slope
DATA_VECTOR(offset); // offset
//DATA_VECTOR(sigma2_cov_re_i); // predictor data for random slope

// parameters:
PARAMETER_VECTOR(b_j)
PARAMETER_VECTOR(sigma_j);
PARAMETER(log_b1_sigma);
PARAMETER_VECTOR(b1_k);
PARAMETER(log_sigma0_sigma);
PARAMETER(log_sigma1_sigma);
PARAMETER_VECTOR(sigma0_k);
PARAMETER_VECTOR(sigma1_k);
//PARAMETER(log_b0_sigma);
//PARAMETER_VECTOR(b0_k);
//PARAMETER(log_sigma2_sigma);
//PARAMETER_VECTOR(sigma2_k);

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

      b1_k(k_i(i)) * b1_cov_re_i(i) +
      linear_predictor_i(i) + offset(i),

      sqrt(exp(
          sigma0_k(k_i(i)) +
          sigma1_k(k_i(i)) * sigma1_cov_re_i(i) +
          linear_predictor_sigma_i(i))),

      true);
}
for(int k = 0; k < n_k; k++){
  //nll -= dnorm(b0_k(k), Type(0.0), exp(log_b0_sigma), true);
  nll -= dnorm(b1_k(k), Type(0.0), exp(log_b1_sigma), true);
  nll -= dnorm(sigma0_k(k), Type(0.0), exp(log_sigma0_sigma), true);
  nll -= dnorm(sigma1_k(k), Type(0.0), exp(log_sigma1_sigma), true);
  //nll -= dnorm(sigma2_k(k), Type(0.0), exp(log_sigma2_sigma), true);  
}

// Reporting
//Type b0_sigma = exp(log_b0_sigma);
Type b1_sigma = exp(log_b1_sigma);
Type sigma0_sigma = exp(log_sigma0_sigma);
Type sigma1_sigma = exp(log_sigma1_sigma);
//Type sigma2_sigma = exp(log_sigma2_sigma);

vector<Type> b1_b1_k(n_k);
vector<Type> sigma1_sigma1_k(n_k);
for(int k = 0; k < n_k; k++){
  // these are fixed-effect slopes + random-effect slopes
  b1_b1_k(k) = b_j(n_j) + b1_k(k);
  sigma1_sigma1_k(k) = sigma_j(n_j) + sigma1_k(k);
}

//REPORT( b0_k );
REPORT( b1_k );
REPORT( b_j );
REPORT( sigma0_k );
REPORT( sigma1_k );
//REPORT( sigma2_k );
//REPORT(b0_sigma);
REPORT(b1_sigma);
REPORT(sigma0_sigma);
REPORT(sigma1_sigma);
//REPORT(sigma2_sigma);
REPORT(b1_b1_k);
REPORT(sigma1_sigma1_k);

//ADREPORT( b0_k );
//ADREPORT( b1_k );
//ADREPORT( b_j );
//ADREPORT( sigma0_k );
//ADREPORT( sigma1_k );
//ADREPORT( sigma2_k );
//ADREPORT(b0_sigma);
//ADREPORT(b1_sigma);
//ADREPORT(sigma0_sigma);
//ADREPORT(sigma1_sigma);
//ADREPORT(sigma2_sigma);
//ADREPORT(b1_b1_k);
//ADREPORT(sigma1_sigma1_k);

return nll;
}
