rm(list = ls())
source("visualize_function.R")
pacman::p_load(rstan, ggplot2)

n = 50
seed = 1
set.seed(seed)

#############we don't get to see the real DGP!
true_theta_1 = 1
true_theta_2 = 6
true_sigsq_1 = 1
true_sigsq_2 = 1
true_rho = 0.2

x = array(NA, n)
for (i in 1 : n){
  x[i] =  if (runif(1) < true_rho){
    rnorm(1, true_theta_1, sqrt(true_sigsq_1))
  } else {
    rnorm(1, true_theta_2, sqrt(true_sigsq_2))
  }
}
rm(i)
#############we don't get to see the real DGP!

#plot the real data
ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x))

stan_model_data = list(
  n = n,
  x = x
)

#build the stan model object and run the sampler
stan_mod_obj = stan_model(file = "lec17_stan_spec_mixture_model.stan", model_name = "mixture_model")
stan_mod_obj

stan_fit = rstan::sampling(
  stan_mod_obj,
  seed = seed,
  data = stan_model_data,
  iter = 5000
)

#straight to inference
theta_1s = extract(stan_fit)$theta[, 1]
theta_2s = extract(stan_fit)$theta[, 2]
sigsq_1s = extract(stan_fit)$sigsq[, 2]
sigsq_2s = extract(stan_fit)$sigsq[, 2]
rhos = extract(stan_fit)$rho[, 1]
visualize_chain_and_compute_estimates_and_cr(theta_1s, true_value = true_theta_1, alpha = 0.05)
visualize_chain_and_compute_estimates_and_cr(theta_2s, true_value = true_theta_2, alpha = 0.05)
visualize_chain_and_compute_estimates_and_cr(sigsq_1s, true_value = true_sigsq_1, alpha = 0.05)
visualize_chain_and_compute_estimates_and_cr(sigsq_2s, true_value = true_sigsq_2, alpha = 0.05)
visualize_chain_and_compute_estimates_and_cr(rhos, true_value = true_rho, alpha = 0.05)

