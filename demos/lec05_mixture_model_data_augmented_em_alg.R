#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=The+calculation+of+posterior+distributions+by+data+augmentation&btnG=
#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Maximum+Likelihood+from+Incomplete+Data+via+the+EM+Algorithm&btnG=

pacman::p_load(ggplot2)

set.seed(1)
n = 100

#############we don't get to see the real DGP!
true_theta_0 = 1
true_theta_1 = 4
true_sigsq_0 = 2
true_sigsq_1 = 3
true_rho = 0.3

x = array(NA, n)
for (i in 1 : n){
  x[i] =  if (runif(1) < true_rho){
    rnorm(1, true_theta_0, sqrt(true_sigsq_0))
  } else {
    rnorm(1, true_theta_1, sqrt(true_sigsq_1))
  }
  
}
#############we don't get to see the real DGP!

ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x), bins = 40)


#chains
EPS = 1e-6
num_maximum_samples = 1e4
theta0s = array(NA, num_maximum_samples)
theta1s = array(NA, num_maximum_samples)
sigsq0s = array(NA, num_maximum_samples)
sigsq1s = array(NA, num_maximum_samples)
rhos = array(NA, num_maximum_samples)
expeIs = matrix(NA, nrow = num_maximum_samples, ncol = n)
#start positions
theta0s[1] = min(x)
theta1s[1] = max(x)
sigsq0s[1] = var(x)
sigsq1s[1] = var(x)
rhos[1] = 0.5
t_break = NA
#no need to set starting points for expeIs since it's expectation first

for (t in 2 : num_maximum_samples){
  theta0 = theta0s[t - 1]
  theta1 = theta1s[t - 1]
  sigsq0 = sigsq0s[t - 1]
  sigsq1 = sigsq1s[t - 1]
  rho = rhos[t - 1]
  
  ###E-step
  for (i in 1 : n){
    a = rho / sqrt(sigsq0) * exp(-1 / (2 * sigsq0) * (x[i] - theta0)^2)
    b = (1 - rho) / sqrt(sigsq1) * exp(-1 / (2 * sigsq1) * (x[i] - theta1)^2)
    expeIs[t, i] = a / (a + b)
  }
  
  ###M-step
  It = expeIs[t, ]
  n0 = sum(It)
  n1 = n - n0
  theta0s[t] = sum(x * It) / n0
  theta1s[t] = sum(x * (1 - It)) / n1
  theta0 = theta0s[t]
  theta1 = theta1s[t]
  sigsq0s[t] = sum((x - theta0)^2 * It) / n0
  sigsq1s[t] = sum((x - theta1)^2 * (1 - It)) / n1
  rhos[t] = n0 / (n0 + n1)
  
  ###check convergence
  if (sqrt(
    (theta0s[t - 1] - theta0s[t])^2 +
    (theta1s[t - 1] - theta1s[t])^2 +
    (sigsq0s[t - 1] - sigsq0s[t])^2 +
    (sigsq1s[t - 1] - sigsq1s[t])^2 +
    (rhos[t - 1] -    rhos[t])^2
  ) < EPS){
    t_break = t
    break
  }
}
rm(a, b, i, t, n0, n1, theta0, theta1, sigsq0, sigsq1, rho, It)

#collect all iterations
em_chain = data.frame(
  theta0s = theta0s, 
  theta1s = theta1s, 
  sigsq0s = sigsq0s,
  sigsq1s = sigsq1s,
  rhos = rhos,
  t = 1 : num_maximum_samples
)
em_chain = cbind(em_chain, expeIs)
if (!is.na(t_break)){
  em_chain = em_chain[1 : t_break, ]
}

#point estimates are found on the last line (the convergent iteration)
em_chain[nrow(em_chain), 1 : 6]
