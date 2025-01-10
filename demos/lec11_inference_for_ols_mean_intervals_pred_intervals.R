pacman::p_load(ggplot2)

set.seed(1)

n = 100

betavec = c(1, 0.2)
sigsq = 0.9

xmin = 1
xmax = 2
X = cbind(1, seq(from = xmin, to = xmax, length.out = n))
p_plus_one = ncol(X)
Xt = t(X)
XtX = Xt %*% X
XtXinv = solve(XtX)
XtXinvXt = XtXinv %*% Xt

#now we introduce randomness into the epsilons
eps = rnorm(n, mean = 0, sd = sqrt(sigsq))
#which means y will be a draw from Y
y = X %*% betavec + eps
#and b will be a draw from B
b = XtXinvXt %*% y
#and e will be a draw from E
e = y - X %*% b
#and SSE will be a draw from ||E||^2
SSE = sum(e^2)
df_err = n - p_plus_one
s_e = sqrt(SSE / df_err) #RMSE

ggplot(data.frame(x = X[, 2], y = y)) +
  geom_point(aes(x = x, y = y)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + #accentuate axes
  geom_abline(intercept = betavec[1], slope = betavec[2], col = "limegreen") +
  geom_abline(intercept = b[1],       slope = b[2],       col = "red") + 
  xlim(0, 3)

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
for (i in 1 : length(x_stars)){
  x_star_vec = X_star_vecs[i, , drop = FALSE]
  yhat_star = as.numeric(x_star_vec %*% b)
  ci_mu_star_one_min_alphas[i, ] = yhat_star + c(-1, +1) *
    t_one_minus_alpha_over_two_df * s_e * 
    as.numeric(sqrt(x_star_vec %*% XtXinv %*% t(x_star_vec)))
  ci_y_star_one_min_alphas[i, ] = yhat_star + c(-1, +1) *
    t_one_minus_alpha_over_two_df * s_e * 
    as.numeric(sqrt(1 + x_star_vec %*% XtXinv %*% t(x_star_vec)))  
}
 
ggplot_obj = ggplot(data.frame(x = X[, 2], y = y)) +
  geom_point(aes(x = x, y = y))


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
for (i in 1 : length(x_stars)){
  ggplot_obj = ggplot_obj + geom_segment(
    x = X_star_vecs[i, 2], 
    xend = X_star_vecs[i, 2],
    y = ci_mu_star_one_min_alphas[i, 1],
    yend = ci_mu_star_one_min_alphas[i, 2],
    col = "blue",
    lwd = 1, 
    inherit.aes = FALSE
  )
}
ggplot_obj +
  geom_abline(intercept = betavec[1], slope = betavec[2], col = "limegreen", lwd = 2) +
  geom_abline(intercept = b[1],       slope = b[2],       col = "red", lwd = 2) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + #accentuate axes
  xlim(0, 3) +
  ylim(-2, 4)

