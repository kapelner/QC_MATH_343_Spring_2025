rm(list = ls())
source("visualize_function.R")
pacman::p_load(rstan, ggplot2)



n = 50
seed = 1
set.seed(seed)

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
  n_0 = sum(x == 0),
  n_not_0 = n - sum(x == 0),
  sum_x_minus_1_greater_than_0 = sum(x[x > 0] - 1)
)

#build the stan model object and run the sampler
stan_mod_obj = stan_model(file = "lec17_stan_spec_hurdle_model.stan", model_name = "hurdle_model")
stan_mod_obj

stan_fit = rstan::sampling(
  stan_mod_obj,
  seed = seed,
  data = stan_model_data,
  iter = 5000
)

#straight to inference
theta_1s = extract(stan_fit)$theta_1
theta_2s = extract(stan_fit)$theta_2
visualize_chain_and_compute_estimates_and_cr(theta_1s, true_value = true_theta_1, alpha = 0.05)
visualize_chain_and_compute_estimates_and_cr(theta_2s, true_value = true_theta_2, alpha = 0.05)


#Do we have to thin?
par(mfrow = c(1, 2))
ell_min = 0
ell_max = 100
#let's get a close look to make sure by zooming in on the y-axis
r_max = 0.2
acf(theta_1s, 
    xlim = c(ell_min, ell_max), ylim = c(0, r_max), lag.max = ell_max, main = "beta0")
acf(theta_2s, 
    xlim = c(ell_min, ell_max), ylim = c(0, r_max), lag.max = ell_max, main = "beta1")
#not really...

