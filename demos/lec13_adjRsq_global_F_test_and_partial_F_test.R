pacman::p_load(ggplot2)

X = model.matrix(medv ~ ., MASS::Boston)
n = nrow(X)
p_plus_one = ncol(X)
p_plus_one
df_error = n - p_plus_one
df_error
y = MASS::Boston$medv
b = solve(t(X) %*% X) %*% t(X) %*% y
yhat = X %*% b 
e = y - yhat 
SSE = sum(e^2)
MSE = SSE / df_error
s_e = sqrt(MSE)

full_mod = lm(medv ~ ., MASS::Boston)
summary(full_mod)

#calculate Rsq_adj manually
Rsq_adj = 1 - MSE / var(y)
Rsq_adj

#calculate F test manually
SSR = sum((yhat - mean(y))^2)
p = p_plus_one - 1
MSR = SSR / p
F_omni = MSR / MSE
F_omni
#pval
1 - pf(F_omni, p, df_error)

#calculate partial F test manually
summary_table = coef(summary(lm(medv ~ ., MASS::Boston)))
summary_table[order(summary_table[,4]), ]
#it looks like indus and age don't contribute as they have 
#p-vals of 0.74 and 0.96 respectively
#let's test them both together, i.e. 
#H_0: beta_indus = 0 and beta_age = 0 
#so that the number of features in the set S is:
k = 2

reduced_mod = lm(medv ~ . - indus - age, MASS::Boston)
e_A = reduced_mod$residuals

Fhathat = (sum(e_A^2) - sum(e^2)) / k / MSE
Fhathat
#and the pval
1 - pf(Fhathat, k, df_error)
#fail to reject H_0

#check the authority - use the "anova" function
#to test "nested" models
anova(full_mod, reduced_mod)


#why is the partial F test so important?
rm(list = ls())

#let's create a design matrix where p+1 = 4
n = 20
set.seed(1)

X = cbind(rnorm(n), rnorm(n))
#where the 3rd feature really is the 2nd feature plus tiny disturbances
X = cbind(X, X[, 2] + rnorm(n, 0, 0.01))

ggplot(data.frame(x2 = X[,2], x3 = X[,3])) +
  geom_point(aes(x = x2, y = x3))

#but this matrix is still full rank
#as x2 is not equal to x3, just very, very close
as.numeric(Matrix::rankMatrix(X))

#now imagine h*(x) where all features matter
beta_vec = c(0, 1, 1, 1)

eps = rnorm(n)
y = cbind(1, X) %*% beta_vec + eps

#now let's run a linear model
full_mod = lm(y ~ X)
summary(full_mod)
#conclusion from omnibus F: at least some features matter
#conclusion from t tests: x1 matters and 
#it appears that x2 and x3 both don't matter
#but this conclusion is from the t-test which tests
#individual contribution!

#how do we test "do both x2 and x3 matter together as a single unit"? 
#Yes, we can run a partial F test
red_mod = lm(y ~ X[, 1])
anova(full_mod, red_mod)
#yes they both matter together!



