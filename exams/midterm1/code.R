####Problem 1

source("../../demos/visualize_function.R")

set.seed(8)

n = 60

#############we don't get to see the real DGP!
true_theta_1 = 0.4 
true_theta_2 = 0.2 
true_theta_3 = 37 

x = rexp(true_m, true_theta_1)
x = c(x, rexp(n - true_m, true_theta_2))
#############we don't get to see the real DGP!

#plot the real data
ggplot(data.frame(num_months_between_earthquake = x, earthquake_number = 1:n)) + 
  geom_point(aes(x = earthquake_number, y = num_months_between_earthquake))


#chains
num_tot_samples = 1e4
theta1s = array(NA, num_tot_samples)
theta2s = array(NA, num_tot_samples)
theta3s = array(NA, num_tot_samples)
#start positions
theta1s[1] = 1
theta2s[1] = 1
theta3s[1] = 1

#convenience function to calculate log probability
ln_p_m = function(m, theta1, theta2){
  m * log(theta1) - theta1 * sum(x[1 : m]) - 
  m * log(theta2) - theta2 * sum(x[(m+1) : n])
}

for (t in 2 : num_tot_samples){
  m = theta3s[t - 1]
  theta1 = rgamma(1, m + 1, 1 + sum(x[1 : m]))
  theta2 = rgamma(1, n - m + 1, 1 + sum(x[(m + 1) : n]))
  
  #now we need to calculate all the m dist
  
  ln_m_dist = array(NA, n - 1)
  for (m in 1 : (n - 1)){
    ln_m_dist[m] = ln_p_m(m, theta1, theta2)
  }
  ln_m_dist = ln_m_dist - max(ln_m_dist)
  m_dist = exp(ln_m_dist) / sum(exp(ln_m_dist))
  sum(m_dist)
  theta1s[t] = theta1
  theta2s[t] = theta2
  theta3s[t] = sample(1 : (n - 1), 1, prob = m_dist)
}

#collect all iterations
gibbs_chain = data.frame(
  theta1s = theta1s, 
  theta2s = theta2s, 
  theta3s = theta3s,
  t = 1 : num_tot_samples
)

#assess convergence
max_t = 100
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta1s)) +
  xlim(1, max_t)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta2s)) +
  xlim(1, max_t)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta3s)) +
  xlim(1, max_t)

#looks like the gibbs sampler burns in right away... so just to be safe
t_burn_in = 8
gibbs_chain = gibbs_chain[t_burn_in : num_tot_samples, ]

head(gibbs_chain)

##assess autocorrelation
par(mfrow = c(3, 1))
acf(theta1s, xlim = c(1, 25), ylim = c(-0.05, 0.3))
acf(theta2s, xlim = c(1, 25), ylim = c(-0.05, 0.3))
acf(theta3s, xlim = c(1, 25), ylim = c(-0.05, 0.3))

#looks like it thins around 6
t_thin = 5

#thin the chains
gibbs_chain = gibbs_chain[seq(1, nrow(gibbs_chain), by = t_thin), ]

#how many samples left?
nrow(gibbs_chain)

#inference
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta3s)
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta1s, alpha=0.05)
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta2s)
visualize_chain_and_compute_estimates_and_cr(gibbs_chain$theta1s - gibbs_chain$theta2s)


#Problem 2
pacman::p_load(ggplot2)
set.seed(1)

n1 = 17
n2 = 37
x1 = rnorm(n1, 6, 2)
x2 = rnorm(n2, 6, 4)
x = c(x1, x2)
round(mean(x1), 2)
round(mean(x2), 2)
round(sd(x1), 2)
round(sd(x2), 2)

B = 10^5
test_stats = array(NA, B)
for (b in 1 : B){
  x1_b = sample(x, n1)
  x2_b = setdiff(x, x1_b)
  test_stats[b] = mean(x1_b) - mean(x2_b)
}

ggplot(data.frame(xbar_diff = test_stats)) + geom_histogram(aes(x = xbar_diff), bins = 1000)

round(mean(x1), 2) - round(mean(x2), 2)

test_stats = array(NA, B)
for (b in 1 : B){
  x1_b = sample(x, n1)
  x2_b = setdiff(x, x1_b)
  test_stats[b] = sd(x1_b) - sd(x2_b)
}

#ggplot(data.frame(s_ratio = test_stats)) + geom_histogram(aes(x = s_ratio), bins = 1000)
quantile(test_stats, c(0.025, 0.975))
round(sd(x1), 2) - round(sd(x2), 2)

#Problem 3
pacman::p_load(ggplot2, survival, GGally, data.table)
lung_data = na.omit(data.table(survival::lung)[, .(time, status, meal.cal)])
more_cal = as.numeric(lung_data$meal.cal > 1200) 
n_1 = sum(more_cal == 1)
n_2 = sum(more_cal == 0)
n_1
n_2
y_1 = lung_data$time[more_cal == 1]
c_1 = lung_data$status[more_cal == 1] - 1
y_2 = lung_data$time[more_cal == 0]
c_2 = lung_data$status[more_cal == 0] - 1

tail(y_2)
tail(c_2)

sum(c_1 == 1)
sum(c_2 == 1)

more_cal = factor(more_cal, labels = c("Group 2", "Group 1"))
survival_fit_obj = survfit2(Surv(lung_data$time, lung_data$status - 1) ~ more_cal)
ggsurv(survival_fit_obj, lty.est=c(1,3), surv.col = 1)

round(0.12 + 1.96 * sqrt(.12 * (1-.12)/149) * c(-1,1), 3)

#n_1: number of subjects in group 1
#n_2: number of subjects in group 2
#y_1: survival times for group 1
#y_2: survival times for group 2
#c_1: censor vector for group 1
#c_2: censor vector for group 2

B = 5000
median_diffs = array(NA, B)
for (b in 1 : B){
  idx_1_b = sample(1 : n_1, n_1, replace = TRUE) 
  idx_2_b = sample(1 : n_2, n_2, replace = TRUE) 
  y_1_b = y_1[idx_1_b]
  c_1_b = c_1[idx_1_b]
  y_2_b = y_2[idx_2_b]
  c_2_b = c_2[idx_2_b]
  km_1 = survfit2(Surv(y_1_b, c_1_b) ~ 1)
  km_2 = survfit2(Surv(y_2_b, c_2_b) ~ 1)
  res_1 = summary(km_1)$table
  res_2 = summary(km_2)$table
  median_diffs[b] = res_1["median"] - res_2["median"]
}

ggplot(data.frame(median_diffs = median_diffs)) + geom_histogram(aes(x = median_diffs), bins = 100)

lung_data$more_cal = more_cal
survival_diff_obj = survdiff(Surv(lung_data$time, lung_data$status - 1) ~ more_cal, lung_data)
survival_diff_obj





