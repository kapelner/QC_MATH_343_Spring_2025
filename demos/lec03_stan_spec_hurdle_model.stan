
data {
  int<lower=0> n_0;                          //the number of zeroes in the sample
  int<lower=0> n_not_0;                      //the number of nonzeroes in the sample
  int<lower=0> sum_x_minus_1_greater_than_0; //the sum of nonzero x's minus 1
}

parameters {
  real<lower=0, upper=1> theta_1;            //the probability parameter of the hurdle
  real<lower=0> theta_2;                     //the parameter of the count model, the poisson shifted back by one
}

model {
  target += n_0 * log(theta_1) +
            n_not_0 * log(1 - theta_1) +
            sum_x_minus_1_greater_than_0 * log(theta_2) -
            n_not_0 * theta_2;
}
