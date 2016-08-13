#include <TMB.hpp>

template<class Type>
Type objective_function<Type>::operator() () {
// data:
DATA_VECTOR(x);
DATA_VECTOR(y);

// parameters:
PARAMETER(b0);
PARAMETER(b1);
PARAMETER(sigma0);
PARAMETER(sigma1);

int n = y.size(); // get number of data points to loop over

Type nll = 0.0; // initialize negative log likelihood

for(int i = 0; i < n; i++){
  Type sigma = sqrt(exp(sigma0 + sigma1*x[i]));
  nll -= dnorm(y[i], b0 + b1 * x[i], sigma, true);
}

return nll;
}
