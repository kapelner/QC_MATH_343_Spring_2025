pacman::p_load(ggplot2, survival, ggsurvfit)

set.seed(1)

n = 40

#############we don't get to see the real DGP!
true_k = 3.5
true_lambda = 1.1
true_theta = 1 / true_lambda * gamma(1 / true_k + 1)
y = rweibull(n, shape = true_k, scale = 1 / true_lambda)
#############we don't get to see the real DGP!
 
#now we plot empirical survival curve (empirical CDF complement) since our right 
#censoring from class is defined as the maximum time
ecdf_obj = ecdf(y) #this was the function we used last semester for the K-S test
res = 1e-5
y_grid = seq(from = 0, to = max(y) + res, by = res)
ggplot(data.frame(y = y_grid, surv = 1 - ecdf_obj(y_grid))) + 
  geom_line(aes(x = y, y = surv)) +
  xlab("time") +
  ylab("survival probability estimate")

#we can also do this with a package
#we first create a Surv object which requires the y-vec and the c-vec but c-vec 
#is defined as -c-vec from our class meaning 1 if observed and 0 if censored
survival_obj = Surv(y, rep(1, n)) #this means all y are uncensored
#the Surv package takes y and c 

survfit2(survival_obj ~ 1) %>% 
  ggsurvfit() +
  ylim(0, 1) + 
  xlab("time") +
  ylab("survival probability estimate")

#let's do nonparametric inference so we need an alpha level for CI's and tests
#and its corresponding z quantile for two-sided CI's and test retention regions
alpha = 0.05
z = qnorm(1 - alpha/2)

#now let theta := survival at time = 0.8
S_hat_t = 1 - ecdf_obj(0.8)
S_hat_t
var_S_hat_t = S_hat_t * (1 - S_hat_t) / n
c(S_hat_t - z * sqrt(var_S_hat_t), S_hat_t + z * sqrt(var_S_hat_t))
#typical inferences: 1yr survival, 2yr survival etc
#this is different than mean survival or median survival

#trouble with frequentism?
#let theta := survival at time = 0.25
S_hat_t = 1 - ecdf_obj(0.25)
S_hat_t
var_S_hat_t = S_hat_t * (1 - S_hat_t) / n
c(S_hat_t - z * sqrt(var_S_hat_t), S_hat_t + z * sqrt(var_S_hat_t))
#what's illegal about this? 
#So, we should probably use another asymptotic result
#or do something Bayesian...

#e.g. let's censor at t_f = 1
t_f = 1
c_vec = ifelse(y > t_f, 0, 1)

#for the empirical survival function, 
#we just set y to be the censored value which by definition from class
#is the maximum y value
y_cens = y
y_cens[c_vec == 0] = t_f

#now we plot the empirical CDF complement
ecdf_obj = ecdf(y_cens) #this was the function we used last semester for the K-S test
y_grid = seq(from = 0, to = max(y_cens) + res, by = res)
ggplot(data.frame(y = y_grid, surv = 1 - ecdf_obj(y_grid))) + 
  geom_line(aes(x = y, y = surv))

#Here's where the empirical CDF breaks down since we cannot estimate what happens
#after t_f. Since there are no more observations y_i > t_f, it estimates zero.

#So now we use the survival package which properly handles this
survival_obj = Surv(y_cens, c_vec) #we flip our vector to be in their format
survfit2(survival_obj ~ 1) %>% 
  ggsurvfit() +
  xlab("time") +
  ylim(0, 1) +
  ylab("survival probability estimate") + 
  add_confidence_interval() +
  add_risktable()


#let's now censor at times that are less than the maximum due to differential
#recruitment. Let's randomly call 30% of them "censored => 70% dead
set.seed(1)
c_vec = rbinom(n, 1, prob = 0.7)

#the empirical survival function won't work anymore as "ignoring censoring 
#erroneously treats patients who are censored as part of the risk set for 
#the entire follow-up period (see https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html)
#we need to use the Kaplan-Meier estimator and that's exactly what the package does
#https://scholar.google.com/scholar?q=Nonparametric+estimation+from+incomplete+observations&hl=en&btnG=Search&as_sdt=1%2C39&as_sdtp=on

survival_obj = Surv(y, c_vec)
survival_fit_obj = survfit2(survival_obj ~ 1) 
survival_fit_obj %>% 
  ggsurvfit() +
  xlab("time") +
  ylim(0, 1) +
  ylab("survival probability estimate") + 
  add_confidence_interval() +
  add_risktable()
#why does the K-S survival function hit zero? There's no censored observations at
#time max(y)

#can we do inference for any time t?
summary(survival_fit_obj) 
#those are the upper/lower limits of the bands you see in the plot
#how about the line at t = 0.901 the survival is 0.4653 how about the std error?
sqrt(0.4653 * (1 - 0.4653) / n)
sqrt(0.4653 * (1 - 0.4653) / sum(c_vec == 1))
0.4653 + qnorm(.975) * sqrt(0.4653 * (1 - 0.4653) / sum(c_vec == 1)) * c(-1,1)
#they're using the t dsitribution I believe?

#how about inference for theta := Med[Y]?
phi_hat_hat = summary(survival_fit_obj)$table[7]
phi_hat_hat
summary(survival_fit_obj)$table[7 : 9]
#this is using the formulas... much better to use the bootstrap

B = 1e4
phi_b = array(NA, B)
for (b in 1 : B){
  i_b = sample(1 : n, n, replace = TRUE)
  survival_obj = Surv(y[i_b], c_vec[i_b])
  survival_fit_obj_b = survfit2(survival_obj ~ 1)
  phi_b[b] = summary(survival_fit_obj_b)$table[7]
}

#let's look at the distribution and the CI's
ci_a = quantile(phi_b, alpha / 2)
ci_b = quantile(phi_b, 1 - alpha / 2)
c(ci_a, phi_hat_hat, ci_b)

ggplot(data.frame(phi_b = phi_b)) + 
  geom_histogram(aes(x = phi_b), bins = 500) +
  geom_vline(xintercept = ci_a, col = "red") +
  geom_vline(xintercept = ci_b, col = "red") +
  geom_vline(xintercept = phi_hat_hat, col = "blue")
