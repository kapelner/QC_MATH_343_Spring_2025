###problem 2

y = MASS::Boston$medv
round(mean(y), 2)
round(sd(y), 2)
f = medv ~ zn + rm + nox + dis + lstat
mod = lm(f, MASS::Boston)
round(coef(mod), 2)
summary(mod)

round(-15.2 + 1.96 * 5.289 *sqrt(0.42) * c(-1,1), 2)
(4.44-3) / (5.289 * sqrt(0.0069))

5.289^2 * 500
505 * 9.20^2
505/500 * 13986.76 / 42743.2
5 / (5.289 * sqrt(1 - 0.0130))
mod$fitted.values[1]
round(29.15 + 1.96 * 5.289 * sqrt(1 + 0.0053) * c(-1,1), 2)
503 * 5.54^2
(15437.87 - 13986.76) / 3 / (13986.7 / 500)


X = model.matrix(f, MASS::Boston)
XtXinv = solve(t(X) %*% X)
signif(XtXinv, 2)
H = X %*% XtXinv %*% t(X)
signif(H[1:6,1:6], 2)

f_red = medv ~ rm + lstat
mod = lm(f_red, MASS::Boston)
round(coef(mod), 2)
summary(mod)


X = model.matrix(f_red, MASS::Boston)
XtXinv = solve(t(X) %*% X)
round(XtXinv %*% t(X) %*% diag(mod$residuals^2) %*% X %*% XtXinv, 2)


round(5.09 + 1.96 * sqrt(0.59) * c(-1,1), 2)

round(XtXinv, 2)

pacman::p_load(rms)
model = ols(f_red, x=TRUE, data = MASS::Boston)    
robcov(model)

        
###problem 3
pacman::p_load(carData)
D = carData::GSSvocab
?carData::GSSvocab

mod = MASS::glm.nb(vocab ~ . -educGroup - year -ageGroup, D)
summary(mod)

round((exp(-0.0267524) - 1) * 100, 2)
round(0.0582323 + 1.96 * 0.0008373 * c(-1,1), 4)
exp(0.7761238) * exp(0.0021438 * 25) * exp(0.0582323 *17)

mod = MASS::glm.nb(vocab ~ . -educGroup - year -ageGroup -gender - nativeBorn, D)
summary(mod)

-115304.3 - -115635.4


D$yprime = log(((D$vocab + 1) / 12) / (1 - (D$vocab + 1) / 12))
summary(D$yprime)
mod = lm(yprime ~ . -educGroup - year -ageGroup, D)
summary(mod)

