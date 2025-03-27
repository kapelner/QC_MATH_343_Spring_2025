# Lasso (and bonus: variable selection)
# https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Regression+Shrinkage+and+Selection+Via+the+Lasso&btnG=
# Definitely a smash hit!

#let's compare the shrinkage of lasso and ridge
n = 100
x = rnorm(n)
true_beta_1 = 0.3 #assume intercept is zero and known for purposes of this demo
y = true_beta_1 * x + rnorm(n)
lambda_lasso = 50
lambda_ridge = 100

RES = 500
possible_betas = seq(-2, 2, length.out = RES)
sse_terms = array(NA, RES)
lasso_abs_regular_terms = array(NA, RES)
ridge_sq_regular_terms = array(NA, RES)
lasso_objective_funs = array(NA, RES)
ridge_objective_funs = array(NA, RES)
for (k in 1 : RES){
  sse_terms[k] = sum((y - x * possible_betas[k])^2)
  lasso_abs_regular_terms[k] = lambda_lasso * abs(possible_betas[k])
  ridge_sq_regular_terms[k] =  lambda_ridge * possible_betas[k]^2
  lasso_objective_funs[k] =    sse_terms[k] + lasso_abs_regular_terms[k]
  ridge_objective_funs[k] =    sse_terms[k] + ridge_sq_regular_terms[k]
}
betahathat_lasso = possible_betas[which.min(lasso_objective_funs)]
betahathat_ridge = possible_betas[which.min(ridge_objective_funs)]

pacman::p_load(ggplot2, data.table)
ggplot(melt(data.table(
  beta = possible_betas, 
  sse = sse_terms,
  abs_regular = lasso_abs_regular_terms,
  sq_regular = ridge_sq_regular_terms,
  lasso_objective_fun = lasso_objective_funs,
  ridge_objective_funs = ridge_objective_funs
), id.vars = c("beta"))) +
  geom_line(aes(x = beta, y = value, color = variable), lwd = 2) +
  geom_vline(xintercept = 0, col = "grey") +
  geom_vline(xintercept = betahathat_lasso, col = "blue") +
  geom_vline(xintercept = betahathat_ridge, col = "purple") +
  geom_vline(xintercept = true_beta_1, col = "orange") +
  xlim(-0.5, 1)

#Let's do this same boston housing data demo using the lasso. There is no closed form solution 
#so we will use the numerical optimization found in the `glmnet` package.
rm(list = ls())
pacman::p_load(glmnet)

p_extra = 350 #1000

set.seed(1)
y = MASS::Boston$medv
X = model.matrix(medv ~ ., MASS::Boston)
X = cbind(X, matrix(rnorm(nrow(X) * p_extra), ncol = p_extra))
colnames(X) = c("(Intercept)", colnames(MASS::Boston)[1:13], paste0("junk_", 1 : p_extra))
dim(X)

#now we standardize the columns
X = apply(X, 2, function(x_dot_j){(x_dot_j - mean(x_dot_j)) / sd(x_dot_j)})
X[, 1] = 1 #reset the intercept as it was turned into NaN's since its sd = 0

#now if we try to model it, p + 1 > n so OLS will not work. Let's try lasso
#Let's see the lasso estimate for a default value of lambda:

lambda = 1
b_lasso = glmnet(X, y, lambda = lambda, alpha = 1)$beta
head(b_lasso, 30)

#those dots are zeroes (this is called sparse representation)

# But let's see how it performs relative to OLS and Ridge. To do so, we'll use the same setup but 
#not add quite as many junk features so we can compare to OLS and Ridge:

# Now we'll split into train-test so we can see which does better.

K = 5
set.seed(1)
test_indices = sample(1 : nrow(X), round(1 / K * nrow(X)))
X_test = X[test_indices, ]
y_test = y[test_indices]
train_indices = setdiff(1 : nrow(X), test_indices)
X_train = X[train_indices, ]
y_train = y[train_indices]

# Lasso is more sensitive to lambda than ridge, so let's start small

lambda = 1

# And we'll fit both models:

b_ols = solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train
b_ridge = solve(t(X_train) %*% X_train + lambda * diag(ncol(X_train))) %*% t(X_train) %*% y_train
b_lasso = glmnet(X_train, y_train, lambda = lambda, alpha = 1)$beta
num_coefficients_to_display = min(ncol(X), 30)
data.frame(
  b_ols = head(b_ols, num_coefficients_to_display),
  b_ridge = head(b_ridge, num_coefficients_to_display),
  b_lasso = head(as.numeric(b_lasso), num_coefficients_to_display)
)

#notice how the junk data slope coefficients are zero in the lasso estimate
#lasso shrinks to zero as there is a lot of sharp prior density on zero, hence
#the posterior is likely to have the mode at zero as well unless it's tugged on
#a LOT by the likelihood

# And look at oos performance:

y_hat_ols = X_test %*% b_ols
y_hat_ridge = X_test %*% b_ridge
y_hat_lasso = X_test %*% b_lasso
rmse_ols = sd(y_test - y_hat_ols)
rmse_ridge = sd(y_test - y_hat_ridge)
rmse_lasso = sd(y_test - y_hat_lasso)
rmse_ols
rmse_ridge
rmse_lasso
cat("lasso RMSE reduction over OLS:", round((rmse_ols - rmse_lasso) / rmse_ols * 100, 1), "%\n")
cat("lasso RMSE reduction over ridge:", round((rmse_ols - rmse_ridge) / rmse_ridge * 100, 1), "%\n")

# Why did it do better than OLS???? Because penalized regression is a good idea if 
# you know many of your features are junk.

# Why did it do better than Ridge???? This is an unfair question as lambda = 1 means 
# different things to ridge than it does to lasso
# to truly compare we would have to find the best lambda_ridge and then the best lambda_lasso
# and we haven't done model selection yet in 342W


# Lasso for variable selection

# Consider the following modeling procedure:
# (1) use lasso as a prestep to screen / select variables
# (2) use OLS on only those variables that lasso selected

#Let's compare that to OLS and pure lasso for prediction

# Let's pick out the variables that are nonzero slope coefficients

lasso_variables_selected = which(as.numeric(b_lasso) != 0)
colnames(X)[lasso_variables_selected]

X_train_lasso_selected = X_train[, lasso_variables_selected]
b_lasso_then_ols = solve(t(X_train_lasso_selected) %*% X_train_lasso_selected) %*% t(X_train_lasso_selected) %*% y_train
y_hat_lasso_then_ols = X_test[, lasso_variables_selected] %*% b_lasso_then_ols
rmse_lasso_then_ols = sd(y_test - y_hat_lasso_then_ols)
rmse_ols = sd(y_test - y_hat_ols)
rmse_lasso = sd(y_test - y_hat_lasso)
rmse_lasso_then_ols
rmse_ols
rmse_lasso
cat("lasso_then_ols RMSE reduction over OLS:", round((rmse_ols - rmse_lasso_then_ols) / rmse_ols * 100, 1), "%\n")
cat("lasso_then_ols RMSE reduction over lasso:", round((rmse_lasso - rmse_lasso_then_ols) / rmse_lasso_then_ols * 100, 1), "%\n")

#interesting idea, huh?







