rm(list = ls())
source("../../demos/visualize_function.R")
pacman::p_load(rstan, ggplot2, extremefit)


### Problem 1
n = 500
seed = 1
set.seed(seed)

#############we don't get to see the real DGP!
true_theta_3 = 0.4 #hurdle prob
true_theta_2 = 1.5
beta_0 = 4
beta_1 = 0
beta_2 = 0
beta_3 = 0

y = array(NA, n)
for (i in 1 : n){
  y[i] =  if (runif(1) < true_theta_3){
            0
          } else {
            #lomax(true_theta_1, true_theta_2) = ParetoI(true_theta_1, true_theta_2) - true_theta_1
            true_theta_i = exp(beta_0)
            true_theta_i / runif(1)^{1 / true_theta_2} - true_theta_i
          }
}
min(y[y > 0])
mean(y[y > 0])
max(y)
exp(beta_0) / (true_theta_2 - 1)
var(y[y > 0])
rm(i)

X = cbind(
  1,
  sample(1 : 4, n, replace = TRUE, prob = c(0.1, 0.5, 0.3, 0.1)),
  sample(0 : 1, n, replace = TRUE, prob = c(0.7, 0.3)),
  rexp(n, 1)
)
#############we don't get to see the real DGP!

#plot the real data
ggplot(data.frame(y = y)) +
  geom_histogram(aes(x = y), bins = 50) + 
  xlab("y")
ggplot(data.frame(y = y[y > 0])) +
  geom_histogram(aes(x = y), bins = 50) +
  scale_x_log10() + 
  xlab("y[y > 0]")

stan_model_data = list(
  p =               ncol(X),
  n_0 =             sum(y == 0),
  n_plus =          sum(y > 0),
  y_gr_0 =          y[y > 0],
  X_y_gr_0 =        X[y > 0, ],
  sum_xvec_y_gr_0 = colSums(X[y > 0, ])
)

#build the stan model object
stan_mod_obj = stan_model(file = "lomax_hurdle_model.stan", model_name = "hurdle_model")
stan_mod_obj

#run the sampler
stan_fit = rstan::sampling(
  stan_mod_obj,
  seed = seed,
  data = stan_model_data,
  iter = 5000
)

visualize_chain_and_compute_estimates_and_cr = function(
    samples, 
    plot_mmse = TRUE, 
    plot_mmae = TRUE, 
    true_value = NULL, 
    alpha = NULL, 
    bins = 30,
    title = NULL,
    colors = c("blue", "orange", "green", "red")){
  ggplot_obj = ggplot(data.frame(samples = samples)) +
    geom_histogram(aes(x = samples), bins = bins)
  
  mmse = mean(samples)
  mmae = median(samples)
  if (plot_mmse){
    ggplot_obj = ggplot_obj + geom_vline(xintercept = mmse, col = colors[1])
  }
  
  if (plot_mmae){
    ggplot_obj = ggplot_obj + geom_vline(xintercept = mmae, col = colors[2])
  }
  
  if (!is.null(true_value)){
    ggplot_obj = ggplot_obj + 
      geom_vline(xintercept = true_value, col = colors[3]) 
  }
  if (!is.null(alpha)){
    ggplot_obj = ggplot_obj + 
      geom_vline(xintercept = quantile(samples, .025), col = colors[4]) + 
      geom_vline(xintercept = quantile(samples, .975), col = colors[4])
  }
  if (!is.null(title)){
    ggplot_obj = ggplot_obj + 
      ggtitle(title)
  }
  plot(ggplot_obj)
  
  ret = list(
    mmse = mmse,
    mmae = mmae,
    theta = true_value
  )
  if (!is.null(alpha)){
    ret$cr_one_minus_alpha_theta = c(
      quantile(samples, alpha / 2), 
      quantile(samples, 1 - alpha / 2)
    )
  }
  ret
}

#straight to inference
theta_3s = extract(stan_fit)$theta_3
theta_2s = extract(stan_fit)$theta_2
beta_0s = extract(stan_fit)$bbeta[, 1]
beta_1s = extract(stan_fit)$bbeta[, 2]
beta_2s = extract(stan_fit)$bbeta[, 3]
beta_3s = extract(stan_fit)$bbeta[, 4]
visualize_chain_and_compute_estimates_and_cr(theta_3s, title = "Stan samples for theta_3", bins = 300)
visualize_chain_and_compute_estimates_and_cr(theta_2s, title = "Stan samples for theta_2", bins = 300)
visualize_chain_and_compute_estimates_and_cr(beta_0s, title = "Stan samples for beta_0", bins = 300)
visualize_chain_and_compute_estimates_and_cr(beta_1s, title = "Stan samples for beta_1", bins = 300)
visualize_chain_and_compute_estimates_and_cr(beta_2s, title = "Stan samples for beta_2", bins = 300)
visualize_chain_and_compute_estimates_and_cr(beta_3s, title = "Stan samples for beta_3", bins = 300)

exp(4.3 + 0.05*(2) + 0.1 *(1) + -0.1 *(5)) / (2.1-1)


### Problem 2

x1 = c(0.03, -0.74,  0.19, -1.80,  1.47,  0.15,  2.17,  0.48)
sort(x1)
