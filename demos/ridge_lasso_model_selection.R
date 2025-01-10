
Of course by using CV, we can optimize the lambda value to give ridge even better performance. The package `glmnet` does that for us automatically:
  
  ```{r}
pacman::p_load(glmnet)
ridge_mod_optimal_lambda = cv.glmnet(X_train, y_train, alpha = 0, lambda = 10^seq(-3, 3, by = 0.1))
y_hat_optimal_ridge = predict(ridge_mod_optimal_lambda, X_test)
rmse_optimal_ridge = sd(y_test - y_hat_optimal_ridge)
rmse_optimal_ridge
cat("optimal lambda:", ridge_mod_optimal_lambda$lambda.min, "\n")
cat("optimal ridge advantage over OLS:", round((rmse_ols - rmse_optimal_ridge) / rmse_ols * 100, 1), "%\n")
```

Of course you can use `mlr` as well but `glmnet` is probably more optimized.


Lasso -- While we're at it, we might as well use CV to find the best lambda.



# Elastic Net

We can use `mlr` to CV over alpha, but here we can't. So let's let $\alpha = 0.5$ meaning "half lasso and half ridge" penalty:

```{r}
elastic_net_mod_optimal_lambda = cv.glmnet(X_train, y_train, alpha = 0.2, lambda = 10^seq(-3, 3, by = 0.1))
y_hat_optimal_elastic_net = predict(elastic_net_mod_optimal_lambda, X_test)
rmse_optimal_elastic_net = sd(y_test - y_hat_optimal_elastic_net)
rmse_optimal_elastic_net
cat("optimal elastic_net advantage over OLS:", round((rmse_ols - rmse_optimal_elastic_net) / rmse_ols * 100, 1), "%\n")
cat("optimal lambda:", elastic_net_mod_optimal_lambda$lambda.min, "\n")
```

Slightly better than lasso. I imagine if we optimized $\alpha$ we can do even better. Elastic nets can also give easy variable selection:

```{r}
head(coef(elastic_net_mod_optimal_lambda), 30)

```

Here we "found" one more variable. That makes sense - as alpha decreases, the ridge penalty becomes more pronounced and it's harder to shrink exactly to zero. Unsure about the $\alpha$ value that stops the hard shrinkage to zero. Good project to think about!
  
  