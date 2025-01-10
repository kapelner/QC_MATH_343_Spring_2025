
#https://scholar.google.com/scholar?q=Bootstrap+methods%3A+another+look+at+the+jackknife

n = 50
#let phi1 := Med[X], i.e. median survival
#let phi2 := Q[X,.9], i.e. 90\%ile survival

#############we don't get to see the real DGP!
x = rweibull(n, 3, 1)
phi1_real = qweibull(0.5, 3, 1)
phi2_real = qweibull(0.9, 3, 1)
#############we don't get to see the real DGP!

ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x), bins = 50)

#point estimators
phi1_hat_hat = median(x)
phi2_hat_hat = quantile(x, 0.9)
phi1_hat_hat
phi2_hat_hat

#bootstrap tells you how much we can trust these estimates
#i.e. what their CI's? We first need to define alpha
alpha = 0.05

#let's compute the asymptotically valid bootstrap CI on phi1 first
#set number of bootstrap samples... median is quick so let's do many
B = 1e6
phi_b = array(NA, B)
for (b in 1 : B){
  phi_b[b] = median(sample(x, n, replace = TRUE))
}

#let's look at the distribution and the CI's
ci_a = quantile(phi_b, alpha / 2)
ci_b = quantile(phi_b, 1 - alpha / 2)
c(ci_a, phi1_hat_hat, ci_b)

ggplot(data.frame(phi_b = phi_b)) + 
  geom_histogram(aes(x = phi_b), bins = 1000) +
  geom_vline(xintercept = ci_a, col = "red") +
  geom_vline(xintercept = ci_b, col = "red") +
  geom_vline(xintercept = phi1_hat_hat, col = "blue") +
  geom_vline(xintercept = phi1_real, col = "green")
#it is discrete because there's few possible medians on resamplings

#let's compute the asymptotically valid bootstrap CI on phi2
#set number of bootstrap samples... quantile is slow so let's do fewer
B = 1e5
phi_b = array(NA, B)
for (b in 1 : B){
  phi_b[b] = quantile(sample(x, n, replace = TRUE), 0.9)
}

#let's look at the distribution and the CI's
ci_a = quantile(phi_b, alpha / 2)
ci_b = quantile(phi_b, 1 - alpha / 2)
c(ci_a, phi2_hat_hat, ci_b)

ggplot(data.frame(phi_b = phi_b)) + 
  geom_histogram(aes(x = phi_b), bins = 500) +
  geom_vline(xintercept = ci_a, col = "red") +
  geom_vline(xintercept = ci_b, col = "red") +
  geom_vline(xintercept = phi2_hat_hat, col = "blue") +
  geom_vline(xintercept = phi2_real, col = "green")
#it is discrete because there's even few possible 90%iles on resamplings
