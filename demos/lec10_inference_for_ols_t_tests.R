pacman::p_load(ggplot2)

set.seed(1)

n = 100

betavec = c(1, 0.2)
sigsq = 0.9

xmin = 1
xmax = 2
X = cbind(1, seq(from = xmin, to = xmax, length.out = n))
p_plus_one = ncol(X)
p_plus_one
dim(X)
Xt = t(X)
dim(Xt)
XtX = Xt %*% X
XtX
Matrix::rankMatrix(XtX)[1]
XtXinv = solve(XtX)
XtXinv
Matrix::rankMatrix(XtXinv)[1]
XtXinvXt = XtXinv %*% Xt
H = X %*% XtXinvXt
dim(H)
matrix_trace = function(A){sum(diag(A))}
matrix_trace(H)

In = diag(n)
I_minus_H = In - H
matrix_trace(I_minus_H)

#now we introduce randomness into the epsilons
eps = rnorm(n, mean = 0, sd = sqrt(sigsq))
y = X %*% betavec + eps
b = XtXinvXt %*% y
b

ggplot(data.frame(x = X[, 2], y = y)) +
  geom_point(aes(x = x, y = y)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + #accentuate axes
  geom_abline(intercept = betavec[1], slope = betavec[2], col = "limegreen") +
  geom_abline(intercept = b[1],       slope = b[2],       col = "red")

#the green line is different than the red line due to...

#what is the variance of B?
varB = sigsq * XtXinv
varB
#which estimate are we "more sure of"
#why is Cov[B_0, B_1] negative?

#let's see how much variation there is in this estimation problem and
#verify the distribution of B and SSE
Nsim = 1e5
bs = matrix(NA, nrow = Nsim, ncol = 2)
sses = array(NA, Nsim)
for (nsim in 1 : Nsim){
  eps = rnorm(n, mean = 0, sd = sqrt(sigsq))
  y = X %*% betavec + eps
  bs[nsim, ] = XtXinvXt %*% y
  sses[nsim] = sum((y - X %*% bs[nsim, ])^2)
}
num_estimations_to_show = 200
ggplot(data.frame(x = X[, 2], y = y, b0s = bs[, 1], b1s = bs[, 2])) +
  geom_point(aes(x = x, y = y)) +
  geom_abline(
    intercept = bs[1 : num_estimations_to_show, 1], 
    slope =     bs[1 : num_estimations_to_show, 2], 
    col = "red")

ggplot(data.frame(b0s = bs[, 1])) +
  geom_histogram(aes(x = b0s, y = after_stat(density)), bins = 1000) + 
  geom_vline(xintercept = betavec[1], col = "limegreen", linewidth = 2) + 
  geom_vline(xintercept = mean(bs[, 1]), col = "red") + 
  stat_function(
    fun = dnorm, 
    args = list(mean = betavec[1], sd = sqrt(varB[1, 1])), 
    linewidth = 2,
    col = "limegreen")
ggplot(data.frame(b1s = bs[, 2])) +
  geom_histogram(aes(x = b1s, y = after_stat(density)), bins = 1000) + 
  geom_vline(xintercept = betavec[2], col = "limegreen", linewidth = 2) + 
  geom_vline(xintercept = mean(bs[, 2]), col = "red") + 
  stat_function(
    fun = dnorm, 
    args = list(mean = betavec[2], sd = sqrt(varB[2, 2])), 
    linewidth = 2,
    col = "limegreen")
ggplot(data.frame(b0s = bs[, 1], b1s = bs[, 2])) +
  geom_point(aes(x = b0s, y = b1s))

ggplot(data.frame(sse_over_sigsq = sses / sigsq)) +
  geom_histogram(aes(x = sse_over_sigsq, y = after_stat(density)), bins = 1000) + 
  geom_vline(xintercept = n - p_plus_one, col = "limegreen", linewidth = 2) + 
  geom_vline(xintercept = mean(sses / sigsq), col = "red") + 
  stat_function(
    fun = dchisq, 
    args = list(df = n - p_plus_one), 
    linewidth = 2,
    col = "limegreen")

#now let's do inference for one run and check if our formulas match R's lm method
set.seed(1)
eps = rnorm(n, mean = 0, sd = sqrt(sigsq))
y = X %*% betavec + eps
ggplot(data.frame(x = X[, 2], y = y)) +
  geom_point(aes(x = x, y = y))

b = XtXinvXt %*% y
yhat = X %*% b
e = y - yhat
SSE = sum(e^2)
df_err = n - p_plus_one
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
#what is a 95% CI for B_0
alpha = 0.05
t_one_minus_alpha_over_two_df = qt(1 - alpha / 2, df_err)
b[1] + c(-1, +1) * t_one_minus_alpha_over_two_df * s_B_0 
#what is a 95% CI for B_1
b[2] + c(-1, +1) * t_one_minus_alpha_over_two_df * s_B_1 

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
