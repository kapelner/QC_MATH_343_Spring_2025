//see the reference manual:
//https://mc-stan.org/docs/reference-manual/index.html

data {
  int<lower=0> n;             //the sample size (we need this for the sum in the log kernel)
  real<lower=0> y_bar;        //a constant we need in the log kernel
  real<lower=0> sum_xi_yi;    //a constant we need in the log kernel
  vector[n] x;                //the covariate measurements
}

parameters {
  real beta_0; 
  real beta_1; 
}

model {
  target += n * y_bar * beta_0 + sum_xi_yi * beta_1;
  real intermediate_sum = 0;
  for (i in 1 : n){
    intermediate_sum += exp(beta_1 * x[i]);
  }
  target += -exp(beta_0) * intermediate_sum; //this sign error took me ~1hr to find!
}
