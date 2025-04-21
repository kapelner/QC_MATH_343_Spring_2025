data {
  int<lower=1> n;           //sample size
  real x[n];                //data
}

parameters {
  simplex[2] rho;           // mixing proportions, must add up to 1 (hence the simplex)
  ordered[2] theta;         // the two means (ordered so theta_1 < theta_2 by definition)
  vector<lower=0>[2] sigsq; // the two variances
}

model {
  for (i in 1 : n) {
    vector[2] lps = log(rho);
    lps[1] += normal_lpdf(x[i] | theta[1], sqrt(sigsq[1]));
    lps[2] += normal_lpdf(x[i] | theta[2], sqrt(sigsq[2]));
    target += log_sum_exp(lps);
  }
}
