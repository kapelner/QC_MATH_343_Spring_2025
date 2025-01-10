source("lec02_visualize_function.R")
pacman::p_load(rstan, ggplot2)
set.seed(1)

n = 50

#############we don't get to see the real DGP!
true_theta_0 = 1
true_theta_1 = 2
t_time = seq(0, 5, length.out = n)
x = array(NA, n)
for (i in 1 : n){
  x[i] = rpois(1, true_theta_0 + true_theta_1 * t_time[i])
}
#############we don't get to see the real DGP!

ggplot(data.frame(t = t_time, x = x)) + 
  geom_point(aes(x = t_time, y = x))

stan_model_data = list(
  n = n,
  t_time = t_time, 
  sum_time = sum(t_time),
  x = x
)

#build the stan model object and run the sampler
stan_fit = stan(
  seed = 1,
  file = "lec03_stan_spec_poisson_regression.stan",
  model_name = "poisson_regression",
  data = stan_model_data,
  iter = 5000
)

plot(stan_fit)
traceplot(stan_fit)

#inference
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_0, true_value = true_theta_0)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_1, true_value = true_theta_1)

