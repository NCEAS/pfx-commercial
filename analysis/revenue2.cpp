#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
// data:
DATA_MATRIX(x_ij);
DATA_VECTOR(y_i);

// parameters:
PARAMETER_VECTOR(b_j)
PARAMETER_VECTOR(sigma_j);

int n_data = y_i.size(); // get number of data points to loop over

// Linear predictor
vector<Type> linear_predictor_i(n_data);
vector<Type> linear_predictor_sigma_i(n_data);
linear_predictor_i = x_ij*b_j;
linear_predictor_sigma_i = sqrt(exp(x_ij*sigma_j));

Type nll = 0.0; // initialize negative log likelihood

for(int i = 0; i < n_data; i++){
  nll -= dnorm(y_i(i), linear_predictor_i(i) , linear_predictor_sigma_i(i), true);
}

return nll;
}
