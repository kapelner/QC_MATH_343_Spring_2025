pacman::p_load(fitdistrplus, ggplot2, optimx)

set.seed(1)

n = 40

#############we don't get to see the real DGP!
true_k = 3.5
true_lambda = 1.1
true_theta = 1 / true_lambda * gamma(1 / true_k + 1)
y = rweibull(n, shape = true_k, scale = 1 / true_lambda)
#############we don't get to see the real DGP!

ggplot(data.frame(y = y)) +
  aes(x = y) +
  geom_histogram(aes(y=..density..)) +
  geom_density(col = "blue")

#find MLE's
fitdist_obj = fitdist(y, "weibull", method = "mle")
k_hat_hat_mle = as.numeric(fitdist_obj$estimate[1])
lambda_hat_hat_mle = 1 / as.numeric(fitdist_obj$estimate[2])

#use invariance of mle thm to estimate the mean
theta_hat_hat_mle = 1 / lambda_hat_hat_mle * gamma(1 / k_hat_hat_mle + 1)
theta_hat_hat_mle

#or just use ybar
mean(y)

#real value
true_theta

#I imagine the MLE would do better than ybar (just like we found with the logistic distribution)

#but what if there is censoring??

#e.g. let's censor at t_f = 1
t_f = 1
c_vec = ifelse(y > t_f, 1, 0)
y[c_vec == 1] = NA
cbind(y, c_vec)

#now we still need to estimate k, lambda and the mean theta

#let's code the log-likelihood function
#since we're going to put it into an optimizer, let thetavec = [k, lambda]
#and make thetavec the argument as it will be trying many different values to find
#the best

loglik = function(thetavec){
  n_0 = sum(c_vec == 0)
  n_1 = sum(c_vec == 1)
  sum_log_y_uncensored = sum(log(y[c_vec == 0]))
  sum_y_uncensored_to_the_k = sum(y[c_vec == 0]^thetavec[1])
  
  n_0 * thetavec[1] * log(thetavec[2]) +
    n_0 * log(thetavec[1]) +
    (thetavec[1] - 1) * sum_log_y_uncensored -
    thetavec[2]^thetavec[1] * sum_y_uncensored_to_the_k -
    n_1 * thetavec[2]^thetavec[1] * t_f^thetavec[1]
}

#reasonable starting point
thetavec_0 = c(2, 2)
#note we need an algorithm that allows for bounding the parameters
#in the Weibull model, both k,lambda > 0 so we set lower = 0
optimx_obj = optimx(thetavec_0, loglik, lower = .Machine$double.eps, 
      control = list(maximize = TRUE), method = 'L-BFGS-B')
k_hat_hat_mle = optimx_obj$p1
lambda_hat_hat_mle = optimx_obj$p2
       
#use invariance of mle thm to estimate the mean
theta_hat_hat_mle = 1 / lambda_hat_hat_mle * gamma(1 / k_hat_hat_mle + 1)
theta_hat_hat_mle

#real value
true_theta
