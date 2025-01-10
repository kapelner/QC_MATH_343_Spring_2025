pacman::p_load(ggplot2)
set.seed(1)
source("lec02_visualize_function.R")

n = 40

#############we don't get to see the real DGP!
true_m = 10       #theta_1
true_theta_1 = 2 #theta_2
true_theta_2 = 4 #theta_3

x = rpois(true_m, true_theta_1)
x = c(x, rpois(n - true_m, true_theta_2))
#############we don't get to see the real DGP!

#plot the real data
ggplot(data.frame(x = x, t = 1 : n)) + 
  geom_point(aes(x = t, y = x))


#chains
num_tot_samples = 1e5
theta1s = array(NA, num_tot_samples)
theta2s = array(NA, num_tot_samples)
ms = array(NA, num_tot_samples)
#start positions
theta1s[1] = 1
theta2s[1] = 1
ms[1] = 1

#convenience function to calculate log probability
ln_p_m = function(m, theta1, theta2){
  if (m == 0){			
    sum(x[1 : n]) * log(theta2)
  } else if (m == n){
    (theta2 - theta1) * m + sum(x[1 : m]) * log(theta1)
  } else {
    (theta2 - theta1) * m + sum(x[1 : m]) * log(theta1) + sum(x[(m + 1) : n]) * log(theta2)
  }			
}

for (t in 2 : num_tot_samples){
  m = ms[t - 1]
  theta1 = rgamma(1, 1 + sum(x[1 : m]), m)
  theta2 = rgamma(1, 1 + sum(x[(m + 1) : n]), n - m)
  
  #now we need to calculate all the m dist

  ln_m_dist = array(NA, n - 1)
  for (m in 1 : (n - 1)){
    ln_m_dist[m] = ln_p_m(m, theta1, theta2)
  }
  ln_m_dist	
  ln_m_dist = ln_m_dist - max(ln_m_dist)
  m_dist = exp(ln_m_dist) / sum(exp(ln_m_dist))
  
  
  theta1s[t] = theta1
  theta2s[t] = theta2
  ms[t] = sample(1 : (n - 1), 1, prob = m_dist)
}

#collect all iterations
gibbs_chain = data.frame(
  theta1s = theta1s, 
  theta2s = theta2s, 
  ms = ms,
  t = 1 : num_tot_samples
)

#assess convergence
max_t = 1e4
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta1s)) +
  xlim(1, max_t)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta2s)) +
  xlim(1, max_t)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = ms)) +
  xlim(1, max_t)
                 
#looks like the gibbs sampler burns in right away... so just to be safe
t_burn_in = 100
gibbs_chain = gibbs_chain[t_burn_in : num_tot_samples, ]


##assess autocorrelation
par(mfrow = c(3, 1))
acf(theta1s, xlim = c(1, 25), ylim = c(-0.05, 0.3))
acf(theta2s, xlim = c(1, 25), ylim = c(-0.05, 0.3))
acf(ms, xlim = c(1, 25), ylim = c(-0.05, 0.3))

#looks like it thins around 6
t_thin = 100

#thin the chains
gibbs_chain = gibbs_chain[seq(1, nrow(gibbs_chain), by = t_thin), ]

#how many samples left?
nrow(gibbs_chain)

#inference
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta1s, true_theta_1)
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta2s, true_theta_2)
