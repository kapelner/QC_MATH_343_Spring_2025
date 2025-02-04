pacman::p_load(ggplot2, MCMCpack)
source("visualize_function.R")

set.seed(1)
n = 100

#############we don't get to see the real DGP!
true_theta_0 = 1
true_theta_1 = 6
true_sigsq_0 = 1
true_sigsq_1 = 1
true_rho = 0.2

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
  geom_histogram(aes(x = x))


#chains
num_tot_samples = 50e3
theta0s = array(NA, num_tot_samples)
theta1s = array(NA, num_tot_samples)
sigsq0s = array(NA, num_tot_samples)
sigsq1s = array(NA, num_tot_samples)
rhos = array(NA, num_tot_samples)
Is = matrix(NA, nrow = num_tot_samples, ncol = n)
#start positions
theta0s[1] = min(x)
theta1s[1] = max(x)
sigsq0s[1] = var(x)
sigsq1s[1] = var(x)
rhos[1] = 0.5
Is[1, ] = c(rep(1L, n / 2), rep(0L, n/2)) 

for (t in 2 : num_tot_samples){
  sigsq0 = sigsq0s[t - 1]
  sigsq1 = sigsq1s[t - 1]
  rho = rhos[t - 1]
  I = Is[t - 1, ]
  
  n0 = sum(I)
  n1 = n - n0
  xbar_0 = sum(I * x) / n0
  xbar_1 = sum((1 - I) * x) / n1
  theta0s[t] = rnorm(1, ifelse(is.nan(xbar_0), 0, xbar_0), sqrt(sigsq0 / n0))
  theta1s[t] = rnorm(1, ifelse(is.nan(xbar_1), 0, xbar_1), sqrt(sigsq1 / n1))
  sigsq0s[t] = rinvgamma(1, n0 / 2, sum(I * (x - theta0s[t])^2) / 2)
  sigsq1s[t] = rinvgamma(1, n1 / 2, sum((1 - I) * (x - theta1s[t])^2) / 2)
  
  if (is.na(theta0s[t])){
    stop("sampler error")
  }
  
  for (i in 1 : n){#now draw the Is
    a = rho* dnorm(x[i], theta0s[t], sqrt(sigsq0s[t]))
    b = (1 - rho) * dnorm(x[i], theta1s[t], sqrt(sigsq1s[t]))
    Is[t, i] = rbinom(1, 1, a / (a + b))
    # log_a = log(rho) + dnorm(x[i], theta0s[t], sqrt(sigsq0s[t]), log = TRUE)
    # log_b = log(1 - rho) + dnorm(x[i], theta1s[t], sqrt(sigsq1s[t]), log = TRUE)
    # Is[t, i] = rbinom(1, 1, exp(log_a - log(exp(log_a) + exp(log_b))))
    # cat("a =", a, "b = ", b, "p =", a / (a + b), "I =", Is[i, t], "\n")
  }
  rhos[t] = rbeta(1, 1 + n0, 1 + n1)
  # cat("t =", t, "n0 = ", n0, "n1 =", n1, "rhos[t] =", rhos[t], "\n")
}

#collect all iterations
gibbs_chain = data.frame(
  theta0s = theta0s, 
  theta1s = theta1s, 
  sigsq0s = sigsq0s,
  sigsq1s = sigsq1s,
  rhos = rhos,
  t = 1 : num_tot_samples
)
gibbs_chain = cbind(gibbs_chain, Is)
head(gibbs_chain)


###assess convergence
t_max_burn = 2000
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta0s)) + 
  xlim(0, t_max_burn)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta1s)) + 
  xlim(0, t_max_burn)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = sigsq0s)) + 
  xlim(0, t_max_burn)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = sigsq1s)) + 
  xlim(0, t_max_burn)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = rhos)) + 
  xlim(0, t_max_burn)


t_burn_in = 250
gibbs_chain = gibbs_chain[t_burn_in : num_tot_samples, ]

plot(gibbs_chain$sigsq1s[1:1000], gibbs_chain$sigsq1s[2:1001])

##assess autocorrelation
par(mfrow = c(1, 1))
ell_max = 50
r_max = 0.5
acf(gibbs_chain$theta0s, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)
acf(gibbs_chain$theta1s, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)
acf(gibbs_chain$sigsq0s, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)
acf(gibbs_chain$sigsq1s, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)
acf(gibbs_chain$rhos, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)


t_thin = 15

#thin the chains
gibbs_chain = gibbs_chain[seq(1, nrow(gibbs_chain), by = t_thin), ]
#how many samples left?
nrow(gibbs_chain)

#inference
mixture_in_order = mean(gibbs_chain$theta0s) < mean(gibbs_chain$theta1s)
mixture_in_order
if (mixture_in_order){
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta0s, true_value = true_theta_0, alpha = 0.05)
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta1s, true_value = true_theta_1, alpha = 0.05)
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$sigsq0s, true_value = true_sigsq_0, alpha = 0.05)
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$sigsq1s, true_value = true_sigsq_1, alpha = 0.05)
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$rhos, true_value = true_rho, alpha = 0.05)
} else {
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta0s, true_value = true_theta_1, alpha = 0.05)
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta1s, true_value = true_theta_0, alpha = 0.05)
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$sigsq0s, true_value = true_sigsq_1, alpha = 0.05)
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$sigsq1s, true_value = true_sigsq_0, alpha = 0.05)
  visualize_chain_and_compute_estimates_and_cr(gibbs_chain$rhos, true_value = 1 - true_rho, alpha = 0.05)  
}

