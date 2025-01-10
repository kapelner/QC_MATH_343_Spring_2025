source("lec02_visualize_function.R")

#Hamiltonian Monte Carlo (2011)
#https://academic.oup.com/jrsssb/article/73/2/123/7034367
#"The paper proposes Metropolis adjusted Langevin and Hamiltonian Monte 
#"Carlo sampling methods defined on the Riemann manifold to resolve the 
#"shortcomings of existing Monte Carlo algorithms when sampling from 
#"target densities that may be high dimensional and exhibit strong correlations."
#https://arxiv.org/pdf/1701.02434.pdf (a nice introduction)
#"instead of fumbling around parameter space with random, uninformed jumps 
#[like in Metropolis steps] ..."

#No U-Turn Sampler for Hamiltonian Monte Carlo (2014)
#https://jmlr.org/papers/v15/hoffman14a.html

#This was coded into a software package called "stan"
#which is accessible from most popular languages (including R
#via the package "rstan"). See
#https://en.wikipedia.org/wiki/Stan_(software)
pacman::p_load(rstan, ggplot2)
#on your computer... make sure the following line works otherwise stan is not 
#installed correctly!
# example(stan_model, package = "rstan", run.dontrun = TRUE)

#let's take a look at these models we did fit with stan
#unfortunately, stan doesn't support integer-valued parameters
#so the Poisson change-point model as we defined it cannot be
#fit in Stan (there are ways to still do fit that model but it's beyond
#the scope of the course)

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
  n_0 = sum(x == 0),
  n_not_0 = n - n_0,
  sum_x_minus_1_greater_than_0 = sum(x[x > 0] - 1)
)

#build the stan model object and run the sampler
stan_fit = stan(
  seed = 1,
  file = "lec02_stan_spec_hurdle_model.stan",
  model_name = "hurdle_model",
  data = stan_model_data
)

#straight to inference
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_1, true_value = true_theta_1)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_2, true_value = true_theta_2)


#Do we have to thin?
#
#The tradition of thinning when running MCMC stems primarily from the
#use of samplers that require a large number of iterations to achieve 
#the desired effective sample size. Because of the efficiency (effective 
#samples per second) of Hamiltonian Monte Carlo, rarely should this be 
#necessary when using Stan.

#but still looks like a good idea...
stan_ac(stan_fit, separate_chains = TRUE)

stan_fit = stan(
  seed = 1,
  file = "lec02_stan_spec_hurdle_model.stan",
  model_name = "hurdle_model",
  thin = 3,
  data = stan_model_data
)
stan_ac(stan_fit, separate_chains = TRUE)

visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_1, true_value = true_theta_1)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_2, true_value = true_theta_2)

