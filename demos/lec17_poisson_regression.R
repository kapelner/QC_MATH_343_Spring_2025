pacman::p_load(ggplot2)
source("lec02_visualize_function.R")
set.seed(1)
#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Equation+of+state+calculations+by+fast+computing+machines&btnG=
#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Monte+Carlo+sampling+methods+using+Markov+chains+and+their+applications&btnG=

n = 50

#############we don't get to see the real DGP!
true_theta_0 = 1
true_theta_1 = 2
t_time = seq(0, 5, length.out = n)
y = array(NA, n)
for (i in 1 : n){
	y[i] = rpois(1, true_theta_0 + true_theta_1 * t_time[i])
}
#############we don't get to see the real DGP!

ggplot(data.frame(t = t_time, y = y)) + 
  geom_point(aes(x = t_time, y = y))


#chains
num_tot_samples = 1e6
theta0s = array(NA, num_tot_samples)
theta1s = array(NA, num_tot_samples)

#start vals
theta0s[1] = 1
theta1s[1] = 1
#hyperparams
sigma_mh = 0.5 #MH sampling variance
#diagnostic data (useful to collect)
accept_theta0s = array(TRUE, num_tot_samples)
accept_theta1s = array(TRUE, num_tot_samples)
#functions that are useful
ln_p_beta_0_beta_1_given_y_t_time = function(b0, b1){
	sum(log(dpois(y, b0 + b1 * t_time)))
}

for (t in 2 : num_tot_samples){
	theta0_t = theta0s[t - 1]
	theta1_t = theta1s[t - 1]
	
	#sample beta_0 first
	theta0star = rnorm(1, theta0_t, sigma_mh)
	#calc r
	ln_r = ln_p_beta_0_beta_1_given_y_t_time(theta0star, theta1_t) - 
	  ln_p_beta_0_beta_1_given_y_t_time(theta0_t, theta1_t)
	if (is.nan(ln_r) || (runif(1) > exp(ln_r))){
		#reject - set it equal to previous value
		theta0star = theta0_t
		accept_theta0s[t] = FALSE
	} #o/t accept
	
	#sample beta1 next
	theta1star = rnorm(1, theta1_t, sigma_mh)
	#calc r
	ln_r = ln_p_beta_0_beta_1_given_y_t_time(theta0star, theta1star) - 
	  ln_p_beta_0_beta_1_given_y_t_time(theta0star, theta1_t)
	if (is.nan(ln_r) || (runif(1) > exp(ln_r))){
		#reject
		theta1star = theta1_t
		accept_theta1s[t] = FALSE
	} #o/t accept
	
	#record this iteration
	theta0s[t] = theta0star
	theta1s[t] = theta1star
}

#diagnostics - how often did the chain move?
mean(accept_theta0s)
mean(accept_theta1s)
#not so great... we may want to go back and change our proposal distributions
#if we have to thin too much

#collect all iterations
gibbs_chain = data.frame(
  theta_0 = theta0s, 
  theta_1 = theta1s, 
  t = 1 : num_tot_samples
)

#assess convergence
max_t_for_plotting = 3000
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta_0)) + 
  xlim(0, max_t_for_plotting)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta_1)) + 
  xlim(0, max_t_for_plotting)

#looks like it converged right away, let's burn a few just to be sure
t_burn_in = 500

#burn the chains
theta0s = theta0s[t_burn_in : num_tot_samples]
theta1s = theta1s[t_burn_in : num_tot_samples]

##assess autocorrelation
par(mfrow = c(1, 2))
ell_max = 125
r_max = 0.3
acf(theta0s, 
    xlim = c(0, ell_max + 10), ylim = c(0, r_max), lag.max = ell_max, main = "beta0")
acf(theta1s, 
    xlim = c(0, ell_max + 10), ylim = c(0, r_max), lag.max = ell_max, main = "beta1")
#let's get a closer look to make sure by zooming in on the y-axis

#let's make a judgment call - maybe at 100
t_thin = 100

#thin the chains
gibbs_chain = gibbs_chain[seq(1, nrow(gibbs_chain), by = t_thin), ]
#how many samples left?
nrow(gibbs_chain)

#inference
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta_0, true_theta_0, bins = 100)
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta_1, true_theta_1, bins = 100)
