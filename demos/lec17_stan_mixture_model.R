pacman::p_load(rstan)
source("lec01_visualize_function.R")
n = 50

#############we don't get to see the real DGP!
true_theta_1 = 0.5678
true_theta_2 = 3.1415

x = array(NA, n)
for (i in 1 : n){
  x[i] =  if (runif(1) <= true_theta_1){
    0
  } else {
    rpois(1, true_theta_2) + 1
  }
}
#############we don't get to see the real DGP!

#plot the real data
ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x))

stan_model_data = list(
  n = n,
  x = x
)

#build the stan model object and run the sampler
stan_fit = stan(
  seed = 1,
  file = "lec04_stan_spec_mixture_model.stan",
  model_name = "mixture_model",
  data = stan_model_data
)

#straight to inference
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_1, true_value = true_theta_1)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_2, true_value = true_theta_2)


#Do we have to thin?
stan_ac(stan_fit, separate_chains = TRUE)

stan_fit = stan(
  seed = 1,
  file = "lec04_stan_spec_mixture_model.stan",
  model_name = "mixture_model",
  thin = 3,
  data = stan_model_data
)
stan_ac(stan_fit, separate_chains = TRUE)

visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_1, true_value = true_theta_1)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_2, true_value = true_theta_2)
