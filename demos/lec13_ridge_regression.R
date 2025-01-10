# Ridge Regression

#https://scholar.google.com/scholar?q=Ridge+Regression%3A+Biased+Estimation+for+Nonorthogonal+Problems&hl=en&btnG=Search&as_sdt=1%2C39&as_sdtp=on
#not as many cites on the original paper as I thought - but people use it all the time

#Let's take a look at the boston housing data and add many useless features.
#We are not limited to n - (p + 1) anymore

p_extra = 1 #1000

set.seed(1)
y = MASS::Boston$medv
X = model.matrix(medv ~ ., MASS::Boston)
X = cbind(X, matrix(rnorm(nrow(X) * p_extra), ncol = p_extra))
colnames(X) = c("(Intercept)", colnames(MASS::Boston)[1:13], paste0("junk_", 1 : p_extra))
dim(X)

#now we standardize the columns
X = apply(X, 2, function(x_dot_j){(x_dot_j - mean(x_dot_j)) / sd(x_dot_j)})
X[, 1] = 1 #reset the intercept
sum(colMeans(X)^2)

#now if we try to model it, p + 1 > n so OLS will not work. Let's try ridge
#Let's see the ridge estimate for a default value of lambda:

lambda = 1
b_ridge = solve(t(X) %*% X + lambda * diag(ncol(X))) %*% t(X) %*% y
head(b_ridge, 30)
# Clearly this works as an algorithm where OLS wouldn't. 

# But let's see how it performs relative to OLS. To do so, we'll use the same setup but 
#not add quite as many junk features so we can compare to OLS:

# Now we'll split into train-test so we can see which does better.

K = 5
set.seed(1)
test_indices = sample(1 : nrow(X), round(1 / K * nrow(X)))
X_test = X[test_indices, ]
y_test = y[test_indices]
train_indices = setdiff(1 : nrow(X), test_indices)
X_train = X[train_indices, ]
y_train = y[train_indices]

# Let's use a big lambda since we have intuition that most of the features are junk:
  
lambda = 1

# And we'll fit both models:

b_ols = solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train
b_ridge = solve(t(X_train) %*% X_train + lambda * diag(ncol(X_train))) %*% t(X_train) %*% y_train
num_coefficients_to_display = min(ncol(X), 30)
data.frame(
  b_ols = head(b_ols, num_coefficients_to_display),
  b_ridge = head(b_ridge, num_coefficients_to_display)
)

#notice how the junk data slope coefficients are closer to zero in the ridge estimate

# And look at oos performance:
 
y_hat_ols = X_test %*% b_ols
y_hat_ridge = X_test %*% b_ridge
rmse_ols = sd(y_test - y_hat_ols)
rmse_ridge = sd(y_test - y_hat_ridge)
rmse_ols
rmse_ridge
cat("ridge RMSE reduction over OLS:", round((rmse_ols - rmse_ridge) / rmse_ols * 100, 1), "%")

# Why did it do better than OLS???? Because penalized regression is a good idea if 
# you know many of your features are junk.

#What if we shrink harder?
lambda = 100
lambda = 1000

#But only if you know many of your features are junk. 
#Let's repeat this with only 10 junk features...


# Lasso (and bonus: variable selection)
# https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Regression+Shrinkage+and+Selection+Via+the+Lasso&btnG=
# Definitely a smash hit!

                                                                                                      
                                                                                                                        ```{r}
                                                                                                                        lasso_mod_optimal_lambda = cv.glmnet(X_train, y_train, alpha = 1, lambda = 10^seq(-3, 3, by = 0.1))
                                                                                                                        y_hat_optimal_lasso = predict(lasso_mod_optimal_lambda, X_test)
                                                                                                                        rmse_optimal_lasso = sd(y_test - y_hat_optimal_lasso)
                                                                                                                        rmse_optimal_lasso
                                                                                                                        cat("optimal lambda:", lasso_mod_optimal_lambda$lambda.min, "\n")
                                                                                                                        cat("optimal lasso advantage over OLS:", round((rmse_ols - rmse_optimal_lasso) / rmse_ols * 100, 1), "%\n")
                                                                                                                        ```
                                                                                                                        
                                                                                                                        Wow - did better than ridge in predictive accuracy. Lambda values are completely not comparable since L1 and L2 penalties are categorically different. 
                                                                                                                        
                                                                                                                        What do the estimates look like?
                                                                                                                          
                                                                                                                          ```{r}
                                                                                                                        head(coef(lasso_mod_optimal_lambda), 30)
                                                                                                                        ```
                                                                                                                        
                                                                                                                        That "." means 0 in a sparse matrix. We never studied these. But they are very memory efficient. Which ones are non-zero?
                                                                                                                          
                                                                                                                          ```{r}
                                                                                                                        b_lasso = coef(lasso_mod_optimal_lambda)[, 1]
                                                                                                                        b_lasso[b_lasso != 0]
                                                                                                                        ```
                                                                                                                        
                                                                                                                        Wow - it deleted all 364 variables except for 4: intercept, rm, ptratio and lstat!!!
                                                                                                                          
                                                                                                                          If you remember in the regression tree, these were the most important (highest level splits).  That is killer variable selection!
                                                                                                                          
                                                                                                                          The coefficient values are also approximately the OLS estimates:
                                                                                                                          
                                                                                                                          ```{r}
                                                                                                                        lm(medv ~ rm + ptratio + lstat, MASS::Boston) #i.e. a regression on the original data with no junk
                                                                                                                        ```
                                                                                                                        
                                                                                                                        It's amazing that it really deleted ALL the junk and left the most predictive variables of the original 13 features and the estimates of those four is pretty on target.



                                                                                                                          