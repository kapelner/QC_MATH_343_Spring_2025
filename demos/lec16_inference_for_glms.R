## Bernoulli regression

#let's look at the Pima diabetes data again
pima = na.omit(MASS::Pima.tr2)
pima$type = ifelse(pima$type == "Yes", 1, 0)
p = ncol(pima) - 1 #-1 for the response
p

#since we're doing bernoulli regression, we need a link function
family_function = binomial(link = "logit")

#and let's fit the generalized linear model on all features
full_mod = glm(type ~ ., pima, family = family_function)
summary(full_mod)
#it displays the Wald tests that are computed via the sqrt of the diagonal 
#entries from the Fisher information matrix and computing the matrix is not super difficult;
#see here: https://web.stanford.edu/class/archive/stats/stats200/stats200.1172/Lecture26.pdf

#notice how we have the log likelihood easily accessible from R
as.numeric(logLik(full_mod))

#let's make sure this is the same as the manual calculation
b = coef(full_mod)
b
y = pima$type
X = model.matrix(type ~ ., pima)
log_odds_hat = as.numeric(X %*% b)
head(log_odds_hat)
sum(-y * log(1 + exp(-log_odds_hat)) - (1 - y) * log(1 + exp(log_odds_hat)))
#yes - this is exactly what the glm function is doing for us

#we can now compute the omnibus test
#here's the null model: just the intercept
null_mod = glm(type ~ 1, pima, family = family_function)
summary(null_mod)

lik_ratio_test_stat = 2 * (as.numeric(logLik(full_mod)) - as.numeric(logLik(null_mod)))
#pval
1 - pchisq(lik_ratio_test_stat, p)
#big time reject of H_0 which makes sense

#let's verify with R's built-in function anova
anova(full_mod, null_mod) #maybe some numeric error?

#now let's do the subset test. There are some variables that don't seem significant at all
summary(full_mod)
S_subset = c("npreg", "bp", "skin")
red_mod_formula = as.formula(paste("type ~ ", paste(c(".", S_subset), collapse = "-")))
red_mod_formula
red_mod = glm(red_mod_formula, pima, family = family_function)
summary(red_mod)

lik_ratio_test_stat = 2 * (as.numeric(logLik(full_mod)) - as.numeric(logLik(red_mod)))
#pval
1 - pchisq(lik_ratio_test_stat, length(S_subset))
#fail to reject H_0: they don't matter

#let's verify with R's built-in function anova
anova(full_mod, red_mod) 


#But if I yank the ones that seem only marginally significant...
summary(full_mod)
S_subset = c(S_subset, "bmi", "age")
red_mod_formula = as.formula(paste("type ~ ", paste(c(".", S_subset), collapse = "-")))
red_mod = glm(red_mod_formula, pima, family = family_function)
summary(red_mod)

lik_ratio_test_stat = 2 * (as.numeric(logLik(full_mod)) - as.numeric(logLik(red_mod)))
#pval
1 - pchisq(lik_ratio_test_stat, length(S_subset))
#reject H_0: these features when considered together matter a whole lot

#let's verify with R's built-in function anova
anova(full_mod, red_mod) 


## Poisson regression
pacman::p_load(ggplot2, skimr)

#example dataset about housing in the Philippines from the textbook repo
#https://github.com/proback/BeyondMLR
philippines_housing = read.csv("philippines_housing.csv")
skim(philippines_housing)
#the response here is total (household size)
ggplot(philippines_housing) + aes(x = total) + geom_bar()
#looks reasonably poisson-distributed

full_mod = glm(total ~ ., philippines_housing, family = "poisson")
summary(full_mod)

null_mod = glm(total ~ 1, philippines_housing, family = "poisson")
summary(null_mod) 
#make sure the null model is correct - it should be ybar
exp(coef(null_mod)) #make sure to exponentiate the beta estimates due to link function
mean(philippines_housing$total)

#let's run the omnibus test
anova(full_mod, null_mod)
#yes, the features matter

#let's run the partial test removing everything but the most significant feature, age
red_mod = glm(total ~ age, philippines_housing, family = "poisson")

anova(full_mod, red_mod)
#the features removed seem to matter


## Weibull regression
pacman::p_load(survival, skimr, ggplot2)

#load the lung data set
lung = na.omit(survival::lung)
lung$status = lung$status - 1 #needs to be 0=alive, 1=dead
skim(lung)
#here the response is the "time" variable and the
#variable "status" = cvec - 1,
#so p = 8, all continous variables

#we first create the Surv object which allows for the censoring to play
#a role in the estimation:
surv_obj = Surv(lung$time, lung$status)
surv_obj
#remember the + values are those that are censored

#to fit a glm with censoring, we can't use glm anymore, we use 
#the survival package. For some reason it doesn't interpret the "." notation
#to delete the y and the c, so we remove them manually:
full_mod = survreg(surv_obj ~ . - time - status, lung)
summary(full_mod)
full_mod$scale
#notice the last entry in the summary table. That's log(khathatmle). It's
#estimated value is -0.376 => khathatmle = exp(-0.376) = 0.6866. If you 
#remember from MATH 340, if k < 1, this indicates the longer you survive,
#the higher the chances of survival. Strange!

#let's do the omnibus test
null_mod = survreg(surv_obj ~ 1, lung)
summary(null_mod) 
#the null model fits only the intercept and k
#the prediction will be:
exp(coef(null_mod)) * gamma(1 + 1 / null_mod$scale)
#why is the average survival so high?
mean(subset(lung, status == 0)$time)
mean(subset(lung, status == 1)$time)

anova(full_mod, null_mod)
#the features matter!

#let's do a partial effects test - remove all those variables that don't seem to matter
red_mod = survreg(surv_obj ~ inst + sex + ph.ecog + ph.karno + wt.loss, lung)
summary(red_mod)

anova(full_mod, red_mod)
#the left out features don't seem to affect survival predictions

