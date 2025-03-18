pacman::p_load(ggplot2)

set.seed(1)

n = 100

betavec = c(1, 0.2)
sigsq = 0.2

xmin = 0.25
xmax = 2.25
X = cbind(1, seq(from = xmin, to = xmax, length.out = n))

expe_y = X %*% betavec #i.e. h*(X)
p_plus_one = ncol(X)
p_plus_one
dim(X)
Xt = t(X)
XtX = Xt %*% X
XtXinv = solve(XtX)
XtXinvXt = XtXinv %*% Xt
H = X %*% XtXinvXt
dim(H)

#now we introduce randomness - we draw one epsilon vector via the "core assumption"
eps = rnorm(n, mean = 0, sd = sqrt(sigsq))

#now we draw one y and we're in 342 land
y = expe_y + eps
b = XtXinvXt %*% y
b

ggplot(data.frame(x = X[, 2], y = y)) +
  geom_point(aes(x = x, y = y)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + #accentuate axes
  geom_abline(intercept = betavec[1], slope = betavec[2], col = "limegreen") +
  geom_abline(intercept = b[1],       slope = b[2],       col = "red")

#the green line is different than the red line due to which type of error?

#what is the variance of B? We know mathematically the answer (we will verify via the simulated results later)
varB = sigsq * XtXinv
varB
#which estimate are we "more sure of"? Intercept or Slope?
#why is Cov[B_0, B_1] negative?

#let's see how much variation there is in this estimation problem and
#verify the distribution of B and SSE
Nsim = 1e5
bs = matrix(NA, nrow = Nsim, ncol = 2)
sses = array(NA, Nsim)
yhats = matrix(NA, nrow = Nsim, ncol = n)
es = matrix(NA, nrow = Nsim, ncol = n)
for (nsim in 1 : Nsim){
  eps = rnorm(n, mean = 0, sd = sqrt(sigsq))
  y = X %*% betavec + eps
  bs[nsim, ] = XtXinvXt %*% y
  yhats[nsim, ] = X %*% bs[nsim, ]
  es[nsim, ] = y - yhats[nsim, ]
  sses[nsim] = sum(es[nsim, ]^2)
}

#now let's take a look at many of the g lines (i.e. many b's from B)
num_estimations_to_show = 100
ggplot(data.frame(x = X[, 2], y = y, b0s = bs[, 1], b1s = bs[, 2])) +
  geom_point(aes(x = x, y = y)) +
  geom_abline(
    intercept = bs[1 : num_estimations_to_show, 1], 
    slope =     bs[1 : num_estimations_to_show, 2], 
    col = "red") + 
  geom_abline(intercept = betavec[1], slope = betavec[2], col = "limegreen", linewidth = 2)

#is B_0 unbiased and normally distributed with variance sigsq (XtX)^-1_1,1?
ggplot(data.frame(b0s = bs[, 1])) +
  geom_histogram(aes(x = b0s, y = after_stat(density)), bins = 1000) + 
  geom_vline(xintercept = betavec[1], col = "limegreen", linewidth = 2) + 
  geom_vline(xintercept = mean(bs[, 1]), col = "red") + 
  stat_function(
    fun = dnorm, 
    args = list(mean = betavec[1], sd = sqrt(varB[1, 1])), 
    linewidth = 1,
    col = "orange")

#is B_1 unbiased and normally distributed with variance sigsq (XtX)^-1_2,2?
ggplot(data.frame(b1s = bs[, 2])) +
  geom_histogram(aes(x = b1s, y = after_stat(density)), bins = 1000) + 
  geom_vline(xintercept = betavec[2], col = "limegreen", linewidth = 2) + 
  geom_vline(xintercept = mean(bs[, 2]), col = "red") + 
  stat_function(
    fun = dnorm, 
    args = list(mean = betavec[2], sd = sqrt(varB[2, 2])), 
    linewidth = 1,
    col = "orange")


#extra credit: is the estimate of b0 and b1 related?
ggplot(data.frame(b0s = bs[, 1], b1s = bs[, 2])) +
  geom_point(aes(x = b0s, y = b1s))
varB

#what about the distribution of yhat_17?
ggplot(data.frame(yhat_17s = yhats[, 17])) +
  geom_histogram(aes(x = yhat_17s, y = after_stat(density)), bins = 1000) + 
  geom_vline(xintercept = expe_y[17], col = "limegreen", linewidth = 2) + 
  geom_vline(xintercept = mean(yhats[, 17]), col = "red") + 
  stat_function(
    fun = dnorm, 
    args = list(mean = expe_y[17], sd = sqrt(sigsq * H[17, 17])), 
    linewidth = 1,
    col = "orange")

#what about the distribution of e_37?
ggplot(data.frame(e_37s = es[, 37])) +
  geom_histogram(aes(x = e_37s, y = after_stat(density)), bins = 1000) + 
  geom_vline(xintercept = 0, col = "limegreen", linewidth = 2) + 
  geom_vline(xintercept = mean(es[, 37]), col = "red") + 
  stat_function(
    fun = dnorm, 
    args = list(mean = 0, sd = sqrt(sigsq * (1 - H[17, 17]))), 
    linewidth = 1,
    col = "orange")


#is the SSE's divided by sigsq chisq with n - p_plus_one df distributed?
ggplot(data.frame(sse_over_sigsq = sses / sigsq)) +
  geom_histogram(aes(x = sse_over_sigsq, y = after_stat(density)), bins = 1000) +
  stat_function(
    fun = dchisq, 
    args = list(df = n - p_plus_one), 
    linewidth = 1,
    col = "orange")

#how about the other term that Cochran's Thm predicts?
ggplot(data.frame(yhat_min_expe_y = apply(bs, 1, function(b){sum((X %*% (b - betavec))^2)})) / sigsq) +
  geom_histogram(aes(x = yhat_min_expe_y, y = after_stat(density)), bins = 1000) +
  stat_function(
    fun = dchisq, 
    args = list(df = p_plus_one), 
    linewidth = 1,
    col = "orange")


#now let's do inference for one run and check if our formulas match R's lm method
set.seed(1)
eps = rnorm(n, mean = 0, sd = sqrt(sigsq))
y = X %*% betavec + eps

b = XtXinvXt %*% y
yhat = X %*% b
e = y - yhat
SSE = sum(e^2)
df_err = n - p_plus_one
df_err
MSE = SSE / df_err
MSE #MSE is an unbiased estimate of sigsq and thus should be approx sigsq = 0.9
s_e = sqrt(MSE)
s_e #RMSE is a consistent estimate of sigma and thus should be approx sigma = 0.95
#what is se[B_0]?
s_B_0 = s_e * sqrt(XtXinv[1, 1])
s_B_0
#what is se[B_1]?
s_B_1 = s_e * sqrt(XtXinv[2, 2])
s_B_1

#for H_0: beta_0 = 0, what is the t-statistic and p-val?
t_0 = abs(b[1]) / s_B_0
t_0
pval0 = 2 * (1 - pt(t_0, df_err))
pval0

#for H_0: beta_1 = 0, what is the p-val?
t_1 = abs(b[2]) / s_B_1
t_1
pval1 = 2 * (1 - pt(t_1, df_err))
pval1

#conclusion: retain H_0, 
#not enough evidence to support the conjecture that x affects y linearly

#check with the authority
summary(lm(y ~ 0 + X))

#compute a 95% CI for B_0
alpha = 0.05
t_one_minus_alpha_over_two_df = qt(1 - alpha / 2, df_err)
b[1] + c(-1, +1) * t_one_minus_alpha_over_two_df * s_B_0 
#compute a 95% CI for B_1
b[2] + c(-1, +1) * t_one_minus_alpha_over_two_df * s_B_1 



#let's graph the data again
ggplot_obj = ggplot(data.frame(x = X[, 2], y = y)) +
  geom_point(aes(x = x, y = y)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + #accentuate axes
  geom_abline(intercept = betavec[1], slope = betavec[2], col = "limegreen") +
  geom_abline(intercept = b[1],       slope = b[2],       col = "red")
ggplot_obj

#now let's look at mean response CI's and prediction CI's
x_star = 1.5
#make it into the x_star_vec by appending the 1 and ensuring it's a row vector
x_star_vec = t(c(1, x_star))
#what is the predicted value?
yhat_star = as.numeric(x_star_vec %*% b)
yhat_star
#this is the best guess of both y_i and mu_i := h*(x_i) = expected response 
#mu_i means the average of infinite responses generated from this x_i

#let's make a 95% confidence interval for 
#mu_star = x_star beta_vec
alpha = 0.05
t_one_minus_alpha_over_two_df = qt(1 - alpha / 2, df_err)
t_one_minus_alpha_over_two_df
z_one_minus_alpha_over_two = qnorm(1 - alpha / 2)
z_one_minus_alpha_over_two
#why is the t not equal to the z?


yhat_star + c(-1, +1) *
  t_one_minus_alpha_over_two_df * s_e * 
  as.numeric(sqrt(x_star_vec %*% XtXinv %*% t(x_star_vec)))

#let's make a 95% confidence interval for 
#y_star = x_star beta_vec + epsilon_star
yhat_star + c(-1, +1) *
  t_one_minus_alpha_over_two_df * s_e * 
  as.numeric(sqrt(1 + x_star_vec %*% XtXinv %*% t(x_star_vec)))  

#why is the predictive interval much bigger?

#now let's look at how that interval varies
RES = 0.03
x_stars = seq(from = 0, to = 3, by = RES)
X_star_vecs = cbind(1, x_stars)
ci_mu_star_one_min_alphas = matrix(NA, nrow = length(x_stars), ncol = 2)
ci_y_star_one_min_alphas = matrix(NA,  nrow = length(x_stars), ncol = 2)
ci_y_star_approx_one_min_alphas = matrix(NA,  nrow = length(x_stars), ncol = 2)
for (i in 1 : length(x_stars)){
  x_star_vec = X_star_vecs[i, , drop = FALSE]
  yhat_star = as.numeric(x_star_vec %*% b)
  ci_mu_star_one_min_alphas[i, ] = yhat_star + c(-1, +1) *
    t_one_minus_alpha_over_two_df * s_e * 
    as.numeric(sqrt(x_star_vec %*% XtXinv %*% t(x_star_vec)))
  ci_y_star_one_min_alphas[i, ] = yhat_star + c(-1, +1) *
    t_one_minus_alpha_over_two_df * s_e * 
    as.numeric(sqrt(1 + x_star_vec %*% XtXinv %*% t(x_star_vec))) 
  ci_y_star_approx_one_min_alphas[i, ] = yhat_star + c(-1, +1) *
    z_one_minus_alpha_over_two * s_e
}


#let's plot the 342 formula approx prediction intervals for y_*
eps_offset = 0
for (i in 1 : length(x_stars)){
  ggplot_obj = ggplot_obj + geom_segment(
    x = X_star_vecs[i, 2] + eps_offset, 
    xend = X_star_vecs[i, 2] + eps_offset,
    y = ci_y_star_approx_one_min_alphas[i, 1],
    yend = ci_y_star_approx_one_min_alphas[i, 2],
    col = "red",
    lwd = 2, 
    inherit.aes = FALSE
  )
}
ggplot_obj

#then let's plot the exact prediction intervals for y_*

for (i in 1 : length(x_stars)){
  ggplot_obj = ggplot_obj + geom_segment(
    x = X_star_vecs[i, 2], 
    xend = X_star_vecs[i, 2],
    y = ci_y_star_one_min_alphas[i, 1],
    yend = ci_y_star_one_min_alphas[i, 2],
    col = "yellow",
    lwd = 1, 
    inherit.aes = FALSE
  )
}
ggplot_obj

#then let's plot the mean intervals for h_*(x_*)
for (i in 1 : length(x_stars)){
  ggplot_obj = ggplot_obj + geom_segment(
    x = X_star_vecs[i, 2], 
    xend = X_star_vecs[i, 2],
    y = ci_mu_star_one_min_alphas[i, 1],
    yend = ci_mu_star_one_min_alphas[i, 2],
    col = "blue",
    lwd = 3, 
    inherit.aes = FALSE
  )
}
ggplot_obj

rm(list = ls())

#How does this look in the real world? It's a one-liner:
summary(lm(medv ~ ., MASS::Boston))
#pvals are valid if you are looking at *one* test of H_0: beta_j = 0
