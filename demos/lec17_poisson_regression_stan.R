rm(list = ls())
source("visualize_function.R")
pacman::p_load(rstan, ggplot2)


#Hybrid MCMC: the work of Duane
#https://www.sciencedirect.com/science/article/abs/pii/037026938791197X
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
#which is accessible from most popular languages (including R via the package "rstan"). 
#See https://en.wikipedia.org/wiki/Stan_(software)

#make sure the following line runs without error on your machine 
#(otherwise stan is not installed correctly!)
example(stan_model, package = "rstan", run.dontrun = TRUE)

n = 50
seed = 1
set.seed(seed)

#############we don't get to see the real DGP!
true_beta_0 = 1
true_beta_1 = 1
x = seq(0, 2, length.out = n)
y = array(NA, n)
for (i in 1 : n){
  y[i] = rpois(1, exp(true_beta_0 + true_beta_1 * x[i]))
}
#############we don't get to see the real DGP!

ggplot(data.frame(x = x, y = y)) + 
  geom_point(aes(x = x, y = y))

stan_model_data = list(
  n = n,
  y_bar = mean(y),
  sum_xi_yi = sum(x * y),
  x = x
)

#recommended to run stan in two steps:

#(1) build the stan model object
stan_mod_obj = stan_model(file = "lec17_stan_spec_poisson_regression.stan", model_name = "poisson_regression")
#Note: this takes awhile as it's compiling a super-efficient program based on our spec in C++
stan_mod_obj

#(2) run the sampler
stan_fit = rstan::sampling(
  stan_mod_obj,
  seed = seed,
  data = stan_model_data,
  iter = 5000,
  warmup = 2500, #default burn is half of all iterations (it's large because it uses samples to "tune" itself) 
  thin = 1 #supposedly no need to thin after the algorithm gets tuned (see internet discussions)
)

plot(stan_fit)
traceplot(stan_fit)
#by default, stan has four chains 
#they can run on independent CPU cores using the cores argument
#but it usually doesn't run faster due to startup and shutdown costs per chain

#we can of course extract the chains:
beta_0s = extract(stan_fit)$beta_0
beta_1s = extract(stan_fit)$beta_1
head(cbind(beta_0s, beta_1s))
length(beta_0s) #why do we wind up with 10,000?

#let's use our function that we've been using since the beginning of the semester
visualize_chain_and_compute_estimates_and_cr(beta_0s, true_value = true_beta_0, alpha = 0.05)
visualize_chain_and_compute_estimates_and_cr(beta_1s, true_value = true_beta_1, alpha = 0.05)

#frequentist inference
freq_infer_res = coef(summary(glm(y ~ x, family = "poisson")))
#compare to frequentist point estimates
freq_infer_res[, 1, drop = FALSE]
#compare to frequentist CIs
cbind(
  freq_infer_res[, 1, drop = FALSE] - 1.96 * freq_infer_res[, 2, drop = FALSE],
  freq_infer_res[, 1, drop = FALSE] + 1.96 * freq_infer_res[, 2, drop = FALSE]
)
