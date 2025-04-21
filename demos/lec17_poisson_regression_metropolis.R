rm(list = ls())
pacman::p_load(ggplot2)
source("visualize_function.R")
#Random walk MCMC: the work of Metropolis and Hastings
#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Equation+of+state+calculations+by+fast+computing+machines&btnG=
#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Monte+Carlo+sampling+methods+using+Markov+chains+and+their+applications&btnG=

n = 50
set.seed(1)

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

#frequentist inference
freq_infer_res = coef(summary(glm(y ~ x, family = "poisson")))
freq_infer_res


#metropolis!

#chains
num_tot_samples = 200e3
beta0s = array(NA, num_tot_samples)
beta1s = array(NA, num_tot_samples)

#hyperparams
sigma_mh = 0.5 #MH sampling variance - 
#this is the hyperparameter in the proposal distr determining the
#degree to which you move around in the random walk through posterior space


#start vals
beta0s[1] = 1
beta1s[1] = 1
#diagnostic data (useful to collect)
accept_beta0s = array(TRUE, num_tot_samples)
accept_beta1s = array(TRUE, num_tot_samples)
#functions that are useful
ln_p_beta_0_beta_1_given_y_t_time = function(beta0, beta1){
	sum(log(dpois(y, exp(beta0 + beta1 * x))))
}

for (t in 2 : num_tot_samples){
	beta0_t = beta0s[t - 1]
	beta1_t = beta1s[t - 1]
	
	#sample beta_0 first
	beta0star = rnorm(1, beta0_t, sigma_mh)
	#calc r
	ln_r = ln_p_beta_0_beta_1_given_y_t_time(beta0star, beta1_t) - 
	  ln_p_beta_0_beta_1_given_y_t_time(beta0_t, beta1_t)
	if (is.nan(ln_r) || (runif(1) > exp(ln_r))){
		#reject - set it equal to previous value
		beta0star = beta0_t
		accept_beta0s[t] = FALSE
	} #o/t accept
	
	#sample beta1 next
	beta1star = rnorm(1, beta1_t, sigma_mh)
	#calc r
	ln_r = ln_p_beta_0_beta_1_given_y_t_time(beta0star, beta1star) - 
	  ln_p_beta_0_beta_1_given_y_t_time(beta0star, beta1_t)
	if (is.nan(ln_r) || (runif(1) > exp(ln_r))){
		#reject
		beta1star = beta1_t
		accept_beta1s[t] = FALSE
	} #o/t accept
	
	#record this iteration
	beta0s[t] = beta0star
	beta1s[t] = beta1star
}

#diagnostics - how often did the chain move?
mean(accept_beta0s)
mean(accept_beta1s)
#not so great... we may want to go back and change our proposal distributions
#because now we have to thin too much
sigma_mh = 0.05 #MH sampling variance
#much better now... no need to further optimize but you can play around at home if you want

#collect all iterations
gibbs_chain = data.frame(
  beta_0 = beta0s, 
  beta_1 = beta1s, 
  t = 1 : num_tot_samples
)

#assess convergence
max_t_for_plotting = 10000
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = beta_0)) + 
  xlim(0, max_t_for_plotting)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = beta_1)) + 
  xlim(0, max_t_for_plotting)

#looks like it converged right away, let's burn a few just to be sure
t_burn_in = 500

#burn the chains
gibbs_chain = gibbs_chain[t_burn_in : num_tot_samples, ]

##assess autocorrelation
par(mfrow = c(1, 2))
ell_min = 320
ell_max = 380
#let's get a close look to make sure by zooming in on the y-axis
r_max = 0.2
acf(gibbs_chain$beta_0, 
    xlim = c(ell_min, ell_max), ylim = c(0, r_max), lag.max = ell_max, main = "beta0")
acf(gibbs_chain$beta_1, 
    xlim = c(ell_min, ell_max), ylim = c(0, r_max), lag.max = ell_max, main = "beta1")

#let's make a judgment call
t_thin = 340

#thin the chains
gibbs_chain = gibbs_chain[seq(1, nrow(gibbs_chain), by = t_thin), ]
#how many samples left?
nrow(gibbs_chain)

#bayesian inference
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$beta_0, true_value = true_beta_0, alpha = 0.05)
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$beta_1, true_value = true_beta_1, alpha = 0.05)
#compare to frequentist point estimates
freq_infer_res[, 1, drop = FALSE]
#compare to frequentist CIs
cbind(
  freq_infer_res[, 1, drop = FALSE] - 1.96 * freq_infer_res[, 2, drop = FALSE],
  freq_infer_res[, 1, drop = FALSE] + 1.96 * freq_infer_res[, 2, drop = FALSE]
)




