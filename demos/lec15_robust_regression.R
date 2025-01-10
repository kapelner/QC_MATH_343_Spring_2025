## Robust Regression Demos

X = model.matrix(medv ~ ., MASS::Boston)
n = nrow(X)
p_plus_one = ncol(X)
p_plus_one
df_error = n - p_plus_one
df_error
y = MASS::Boston$medv
XtX = t(X) %*% X
XtXinv = solve(XtX)
b = XtXinv %*% t(X) %*% y
yhat = X %*% b 
e = y - yhat 
SSE = sum(e^2)
MSE = SSE / df_error
s_e = sqrt(MSE)

full_mod = lm(medv ~ ., MASS::Boston)

#let's display the results for the full model with 95% CI's
ols_results = matrix(nrow = p_plus_one, ncol = 4)
colnames(ols_results) = c("b", "ci_95%_lower","ci_95%_upper", "pval")
rownames(ols_results) = rownames(b)
ols_results[, 1] = b #the original OLS estimate
s_b = coef(summary(full_mod))[, 2] #get the standard errors for all features
ols_results[, 2] = b - qt(.975, df_error) * s_b
ols_results[, 3] = b + qt(.975, df_error) * s_b
ols_results[, 4] = round(coef(summary(full_mod))[, 4], 5)
ols_results

#calculate partial F test on indus and age
reduced_mod = lm(medv ~ . - indus - age, MASS::Boston)
anova(full_mod, reduced_mod)

## Robust regression
#Scenario #1: only independence of errors can be assumed --> bootstrap
set.seed(1)

Bsamp = 1e4

b_bs = matrix(NA, nrow = p_plus_one, ncol = Bsamp)
for (bsamp in 1 : Bsamp){
  b_idx = sample(1 : n, n, replace = TRUE)
  X_b = X[b_idx, ]
  y_b = y[b_idx]
  b_bs[, bsamp] = solve(t(X_b) %*% X_b) %*% t(X_b) %*% y_b
}

#let's calculate 95\% CI's and pvals for all variables
bootstrap_results = matrix(nrow = p_plus_one, ncol = 4)
colnames(bootstrap_results) = c("b", "ci_95%_lower","ci_95%_upper", "pval")
rownames(bootstrap_results) = rownames(b)
bootstrap_results[, 1] = b #the original OLS estimate
bootstrap_results[, 2] = apply(b_bs, 1, quantile, 0.025)
bootstrap_results[, 3] = apply(b_bs, 1, quantile, 0.975)
bootstrap_results[, 4] = apply(b_bs, 1, function(bs){
  min(c(
    sum(bs > 0) / Bsamp,
    sum(bs < 0) / Bsamp
  ))
})
bootstrap_results
ols_results

###how to do omnibus F? (not covered)
p = p_plus_one - 1
pval_F_omnibus_bs = array(NA, Bsamp)
for (bsamp in 1 : Bsamp){
  b_idx = sample(1 : n, n, replace = TRUE)
  X_b = X[b_idx, ]
  y_b = y[b_idx]
  yhat_b = X_b %*% solve(t(X_b) %*% X_b) %*% t(X_b) %*% y_b
  e_b = y_b - yhat_b
  ybar_b = mean(y_b)
  f_stat = (sum((yhat_b - ybar_b)^2) / p) / (sum(e_b^2) / df_error)
  pval_F_omnibus_bs[bsamp] = pf(f_stat, p, df_error, lower.tail=FALSE)
}
mean(pval_F_omnibus_bs > 0.05)

###how to do partial F? (not covered)
pval_F_partial_bs = array(NA, Bsamp)
for (bsamp in 1 : Bsamp){
  b_idx = sample(1 : n, n, replace = TRUE)
  X_b = X[b_idx, ]
  y_b = y[b_idx]
  yhat_b = X_b %*% solve(t(X_b) %*% X_b) %*% t(X_b) %*% y_b
  e_b = y_b - yhat_b
  #now do reduced model
  X_reduced_b = X_b[, -c(4, 8)] #indices 4 and 8 are indus and age variables
  yhat_reduced_b = X_reduced_b %*% solve(t(X_reduced_b) %*% X_reduced_b) %*% t(X_reduced_b) %*% y_b
  e_reduced_b = y_b - yhat_reduced_b
  f_stat = ((sum(e_reduced_b^2) - sum(e_b^2)) / 2) / (sum(e_b^2) / df_error)
  pval_F_partial_bs[bsamp] = pf(f_stat, 2, df_error, lower.tail=FALSE)
}
mean(pval_F_partial_bs > 0.05)


#Scenario #2: errors iid but normality can't be assumed --> asymptotic Wald tests

#assess normality (not covered)
pacman::p_load(car)
qqPlot(full_mod$residuals, distribution="norm")
#conclusion: the errors are not normally distributed => the lm inference is wrong

ols_asymp_results = matrix(nrow = p_plus_one, ncol = 4)
colnames(ols_asymp_results) = c("b", "ci_95%_lower","ci_95%_upper", "pval")
rownames(ols_asymp_results) = rownames(b)
ols_asymp_results[, 1] = b #the original OLS estimate
ols_asymp_results[, 2] = b - qnorm(.975) * s_b
ols_asymp_results[, 3] = b + qnorm(.975) * s_b
ols_asymp_results[, 4] = round(2 * (1 - pnorm(abs(b), 0, s_b)), 5)
ols_asymp_results
ols_results

#let's do the asymptotic omnibus chisq test
s_e = summary(full_mod)$sigma
S_subset = 2 : p_plus_one
XtXinv_S_cross_S_inv = solve(XtXinv[S_subset, S_subset])
b_S = b[S_subset]
chisq_test_stat = 1 / s_e^2 * as.numeric(t(b_S) %*% XtXinv_S_cross_S_inv %*% b_S)
1 - pchisq(chisq_test_stat, p)

#let's do the asymptotic partial chisq test
S_subset = c(4, 8)
XtXinv_S_cross_S_inv = solve(XtXinv[S_subset, S_subset])
b_S = b[S_subset]
chisq_test_stat = 1 / s_e^2 * as.numeric(t(b_S) %*% XtXinv_S_cross_S_inv %*% b_S)
1 - pchisq(chisq_test_stat, 2)


#Scenario #3: error independent normals but heteroskedastic --> asymptotic Wald tests
#Scenario #4: error independence but heteroskedastic and normality can't be assumed --> asymptotic Wald tests
#https://scholar.google.com/scholar?q=A+Heteroskedasticity-Consistent+Covariance+Matrix+Estimator+and+a+Direct+Test+for+Heteroskedasticity&hl=en&btnG=Search&as_sdt=1%2C39&as_sdtp=on
#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=The+behavior+of+maximum+likelihood+estimates+under+nonstandard+conditions&btnG=

#tests of heteroskedasticity (not covered in this class)
summary(lm(full_mod$residuals^2 ~ . - medv, MASS::Boston))
pacman::p_load(lmtest, sandwich)
bptest(full_mod)
ggplot(data.frame(e = e, x = X[, "rm"])) + aes(x = x, y = e) + geom_point()
#see that fanning pattern? that's heteroskedasticity
ggplot(data.frame(e = e, x = X[, "lstat"])) + aes(x = x, y = e) + geom_point()
#this is indicative of misspecification error and heteroskedasticity
ggplot(data.frame(e = e, x = X[, "ptratio"])) + aes(x = x, y = e) + geom_point()
#definite heteroskedasticity!
#conclusion: the errors are heteroskedastic => the lm inference is wrong

ols_asymp_huber_results = matrix(nrow = p_plus_one, ncol = 4)
colnames(ols_asymp_huber_results) = c("b", "ci_95%_lower","ci_95%_upper", "pval")
rownames(ols_asymp_huber_results) = rownames(b)
ols_asymp_huber_results[, 1] = b #the original OLS estimate
Sigma_huber = XtXinv %*% t(X) %*% diag(as.numeric(e)^2) %*% X %*% XtXinv
s_b_huber_white = sqrt(diag(Sigma_huber))
s_b_huber_white
#let's see if the R package agrees - calculate the standard errors of B
coeftest(full_mod, vcov = vcovHC(full_mod, type = "HC0"))[, 2]
s_b
ols_asymp_huber_results[, 2] = b - qnorm(.975) * s_b_huber_white
ols_asymp_huber_results[, 3] = b + qnorm(.975) * s_b_huber_white
ols_asymp_huber_results[, 4] = round(2 * (1 - pnorm(abs(b), 0, s_b_huber_white)), 5)
ols_asymp_huber_results
ols_results

#let's do the asymptotic omnibus chisq test
S_subset = 2 : p_plus_one
Sigma_huber_S_cross_S_inv = solve(Sigma_huber[S_subset, S_subset])
b_S = b[S_subset]
chisq_test_stat = as.numeric(t(b_S) %*% Sigma_huber_S_cross_S_inv %*% b_S)
1 - pchisq(chisq_test_stat, p)

#let's do the asymptotic partial chisq test
S_subset = c(4, 8)
Sigma_huber_S_cross_S_inv = solve(Sigma_huber[S_subset, S_subset])
b_S = b[S_subset]
chisq_test_stat = as.numeric(t(b_S) %*% Sigma_huber_S_cross_S_inv %*% b_S)
1 - pchisq(chisq_test_stat, 2)



#HW give 95% CI for Rsq