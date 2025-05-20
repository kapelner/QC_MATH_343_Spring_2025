rm(list = ls())
source("../../demos/visualize_function.R")
pacman::p_load(rstan, ggplot2, extremefit)


### Problem 1

n = 250
seed = 1
set.seed(seed)

#############we don't get to see the real DGP!
true_theta_3 = 0.7 #hurdle prob
true_theta_2 = 2
true_theta_1 = 5
beta_0 = -1
beta_1 = 1

x = array(NA, n)
for (i in 1 : n){
  x[i] =  if (runif(1) < true_theta_3){
    0
  } else {
    #lomax is ParetoI(true_theta_1, true_theta_2) - true_theta_1
    true_theta_i = true_theta_1
    true_theta_i / runif(1)^{1 / true_theta_2} - true_theta_i
  }
}
min(x)
ggplot(data.frame(x = x)) +
  geom_histogram(aes(x = x))
rm(i)
#############we don't get to see the real DGP!

#plot the real data
ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x))
