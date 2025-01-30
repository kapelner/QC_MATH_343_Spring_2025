pacman::p_load(ggplot2, data.table)
set.seed(1)

n1 = 86
n2 = 114
#thus n = 200

#############we don't get to see the real DGP!
x1 = rlogis(n1)
x2 = rnorm(n2)
#############we don't get to see the real DGP!

#look at data
ggplot(data.frame(x = c(x1, x2), samp = c(rep("x1", n1), rep("x2", n2)))) + 
  geom_histogram(aes(x = x, fill = samp, col = samp), alpha = 0.5, bins = 50)

#let's set the test statistic to be the ratio of sample variances

B = 1e5
x = c(x1, x2)
x_ind = 1 : length(x)

thetahathat_bs = array(NA, B)
for (b in 1 : B){
  x_ind_b = sample(x_ind) #shuffle the indices
  #cut them up into two pieces
  x_ind_b1 = x_ind_b[1 : n1]
  x_ind_b2 = x_ind_b[(n1 + 1) : n2]
  x_b1 = x[x_ind_b1]
  x_b2 = x[x_ind_b2]
  #calculate the test statistic
  thetahathat_bs[b] = var(x_b1) / var(x_b2)
}
rm(x, x_ind, b, x_ind_b, x_ind_b1, x_ind_b2, x_b1, x_b2)

#now calculate the actual test statistic
thetahathat = var(x1) / var(x2)

alpha = 0.05
ret_a = quantile(thetahathat_bs, alpha / 2)
ret_b = quantile(thetahathat_bs, 1 - alpha / 2)
ggplot(data.frame(thetahathat_bs = thetahathat_bs)) + 
  geom_histogram(aes(x = thetahathat_bs), bins = B / 100) + 
  geom_vline(xintercept = ret_a, col = "red") + 
  geom_vline(xintercept = ret_b, col = "red") +
  geom_vline(xintercept = thetahathat, col = "blue")

#hence, we reject the null hypothesis
#calc p-value
2 * min(sum(thetahathat_bs > thetahathat), sum(thetahathat_bs < thetahathat)) / B

rm(list = ls())

#when will the parametric test really pay the rent? Another example to show you.

set.seed(1)

n1 = 10
n2 = 10
#thus n = 200

#############we don't get to see the real DGP!
x1 = rweibull(n1, 3, 1)
x2 = rweibull(n1, 3, 1.5)
#############we don't get to see the real DGP!


#look at data
ggplot(data.frame(x = c(x1, x2), samp = c(rep("x1", n1), rep("x2", n2)))) + 
  geom_boxplot(aes(x = x, col = samp))
round(rbind(
  sort(x1),
  sort(x2)
), 2)

#we want to test if the DGPs of these two populations are unequal
#let's compare permutation test (exact) to two-sample KS test 
#(very approximate given low n)

#here it makes sense to use a "better-designed" test statistic
#such as the 2-sample t-stat for different variances!

#can we use the 
#here, all permutations can be done as the following is ~184,000
choose(n1 + n2, n1)

#the test statistic here to 

pacman::p_load(combinat)
x = c(x1, x2)
x_ind = 1 : length(x)
x_ind_permutations = combn(x_ind, n1)
dim(x_ind_permutations)
B = ncol(x_ind_permutations)

thetahathat_bs = array(NA, B)
for (b in 1 : B){
  x_ind_b = sample(x_ind) #shuffle the indices
  #cut them up into two pieces
  x_ind_b1 = x_ind_permutations[, b]
  x_ind_b2 = setdiff(x_ind, x_ind_b1)
  x_b1 = x[x_ind_b1]
  x_b2 = x[x_ind_b2]
  #calculate the test statistic
  thetahathat_bs[b] = (mean(x_b1) - mean(x_b2)) / sqrt(var(x_b1) / n1 + var(x_b2) / n2)
}
rm(x, b, x_ind_b, x_ind_b1, x_ind_b2, x_b1, x_b2)

#now calculate the actual test statistic
thetahathat = (mean(x1) - mean(x2)) / sqrt(var(x1) / n1 + var(x2) / n2)

alpha = 0.05
ret_a = quantile(thetahathat_bs, alpha / 2)
ret_b = quantile(thetahathat_bs, 1 - alpha / 2)
ggplot(data.frame(thetahathat_bs = thetahathat_bs)) + 
  geom_histogram(aes(x = thetahathat_bs), bins = round(B / 100)) + 
  geom_vline(xintercept = ret_a, col = "red") + 
  geom_vline(xintercept = ret_b, col = "red") +
  geom_vline(xintercept = thetahathat, col = "blue")

#hence we reject H_0
#calc p-value
2 * min(sum(thetahathat_bs > thetahathat), sum(thetahathat_bs < thetahathat)) / B

ks.test(x1, x2)
#retain H_0

#it seems like the permutation test has higher power... let's see in a simulation

Nsim = 100
alpha = 0.05
rejections_perm = array(NA, Nsim)
rejections_ks = array(NA, Nsim)

for (nsim in 1 : Nsim){
  #pull from DGP each time
  x1 = rweibull(n1, 3, 1)
  x2 = rweibull(n1, 3, 1.5)
  x = c(x1, x2)
  
  thetahathat_bs = array(NA, B)
  for (b in 1 : B){
    x_ind_b = sample(x_ind) #shuffle the indices
    #cut them up into two pieces
    x_ind_b1 = x_ind_permutations[, b]
    x_ind_b2 = setdiff(x_ind, x_ind_b1)
    x_b1 = x[x_ind_b1]
    x_b2 = x[x_ind_b2]
    #calculate the test statistic
    thetahathat_bs[b] = (mean(x_b1) - mean(x_b2)) / sqrt(var(x_b1) / n1 + var(x_b2) / n2)
  }
  
  ret_a = quantile(thetahathat_bs, alpha / 2)
  ret_b = quantile(thetahathat_bs, 1 - alpha / 2)
  
  thetahathat = (mean(x1) - mean(x2)) / sqrt(var(x1) / n1 + var(x2) / n2)
  rejections_perm[nsim] = thetahathat < ret_a | thetahathat > ret_b
  rejections_ks[nsim] = ks.test(x1, x2)$p.value < alpha
}
rm(x, x_ind, x_ind_permutations, b, x_ind_b, x_ind_b1, x_ind_b2, x_b1, x_b2, thetahathat, nsim)

mean(rejections_perm, na.rm = TRUE)
mean(rejections_ks, na.rm = TRUE)
