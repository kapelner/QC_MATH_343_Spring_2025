
data {
  int<lower=0> n;         //the sample size
  vector[n] t_time;       //the times of the measurements
  real<lower=0> sum_time; //the sum of all time variables
  vector[n] x;            //the count measurements
}

parameters {
  real theta_0; 
  real theta_1; 
}

model {
  target += -n * theta_0 - theta_1 * sum_time;
  for (i in 1 : n){
    target += x[i] * log(theta_0 + theta_1 * t_time[i]);
  }
}
