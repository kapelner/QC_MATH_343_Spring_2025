pacman::p_load(ggplot2, rstan)

set.seed(1)
n = 100

#############we don't get to see the real DGP!
true_theta_0 = 1
true_theta_1 = 4
true_sigsq_0 = 2
true_sigsq_1 = 3
true_rho = 0.3

x = array(NA, n)
for (i in 1 : n){
  x[i] =  if (runif(1) < true_rho){
    rnorm(1, true_theta_0, sqrt(true_sigsq_0))
  } else {
    rnorm(1, true_theta_1, sqrt(true_sigsq_1))
  }
  
}
#############we don't get to see the real DGP!

ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x), bins = 40)

stan_model_data = list(
  n = n,
  x = x
)

#build the stan model object and run the sampler
stan_fit = stan(
  seed = 1,
  file = "lec05_stan_spec_mixture_model.stan",
  model_name = "mixture_model",
  data = stan_model_data,
  iter = 2000
)

plot(stan_fit)
traceplot(stan_fit)

#inference
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_0, true_value = true_theta_0)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_1, true_value = true_theta_1)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$sigsq_0, true_value = true_theta_0)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$sigsq_1, true_value = true_theta_1)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$rho, true_value = true_rho)