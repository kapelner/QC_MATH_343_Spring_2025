data {
  int<lower=1> n;           //sample size
  real x[n];                //data
}

parameters {
  simplex[2] rho;           // mixing proportions, must add up to 1 (hence the simplex)
  ordered[2] theta;         // means (ordered so theta0 < theta1 by definition)
  vector<lower=0>[2] sigsq; // variances
}

model {
  vector[2] lps = log(rho);
  for (i in 1 : n) {
    lps[0] = normal_lpdf(x[i] | theta[0], sqrt(sigsq[0]));
    lps[1] = normal_lpdf(x[i] | theta[1], sqrt(sigsq[1]));
  }
  target += log_sum_exp(lps);
}
