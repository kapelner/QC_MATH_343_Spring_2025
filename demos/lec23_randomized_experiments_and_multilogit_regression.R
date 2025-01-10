pacman::p_load(tidyverse)

#we will look at a real RCT with the data anonymized
D = read_csv("clinical_data_cleaned.csv")
D = D %>%
  mutate(x1_fac = as.factor(x1_fac)) %>%
  mutate(x2_fac = as.factor(x2_fac)) %>%
  mutate(x3_fac = as.factor(x3_fac)) %>%
  mutate(x13_fac = as.factor(x13_fac))

skimr::skim(D)
#is this an equal allocation experiment?

n = nrow(D)
w = D$w
y = D$y
X = D %>% select(-w, -y)
Xmm = model.matrix(~ 0 + ., X)
XT = Xmm[w == 1, ]
XC = Xmm[w == 0, ]
nT = nrow(XT)
nC = nrow(XC)


#let's do the basic analysis
mod = lm(y ~ w)
summary(mod)
#conclusion? what error type could this be?

#let's do the randomization test
set.seed(1)
alpha = .05
Nsim = 1e5
bTs = array(NA, Nsim)
for (nsim in 1 : Nsim){
  wsim = sample(w) #draw a new allocation from space of all allocations
  bTs[nsim] = mean(y[wsim == 1]) -mean(y[wsim == 0])
}
#get the RET region
ret_region = quantile(bTs, c(alpha / 2, 1 - alpha / 2))
ret_region

#look at null distribution and the real estimate:
ggplot(data.frame(bTs = bTs)) + 
  aes(x = bTs) + 
  geom_histogram(bins = 1000) +
  geom_vline(xintercept = coef(mod)[2], col = "red") + 
  geom_vline(xintercept = ret_region[1], col = "green") + 
  geom_vline(xintercept = ret_region[2], col = "green")
#conclusion? what error type could this be?

#now let's consider the baseline covariates, p = 13
#let's first see if there is any difference


p = ncol(XT)
balance_stats = matrix(NA, nrow = p, ncol = 6)
colnames(balance_stats) = c("covariate", "avg_T", "avg_C", "std_diff", "t_test_pval", "significance")
for (j in 1 : p){
  xbar_j_T = mean(XT[, j])
  xbar_j_C = mean(XC[, j])
  pval = t.test(XT[, j], XC[, j])$p.val
  balance_stats[j, ] = c(
    colnames(XT)[j],
    xbar_j_T,
    xbar_j_T,
    (xbar_j_T - xbar_j_C) / sqrt(var(XT[, j]) / nT + var(XC[, j]) / nC),
    pval,
    ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "")))
  )
}
data.frame(balance_stats)
#looks like they did a good job balancing the baseline covariates

##### Blocking Design

#although the experiment has already run, for pedagogical value, let's do a block design
#let's block on sex
table(X$sex)
#we only have 36 females and 180 males so let's create just sex and w:
n_Female = table(X$sex)[1]
n_Female_T = n_Female / 2 #equal allocation
n_Female_C = n_Female / 2
n_Male = table(X$sex)[2]
n_Male_T = n_Male / 2 #equal allocation
n_Male_C = n_Male / 2
Xblocked = rbind(
  cbind(rep(0, n_Female), sample(c(rep(1, n_Female_T), rep(0, n_Female_C)))),
  cbind(rep(1, n_Male),   sample(c(rep(1, n_Male_T),   rep(0, n_Male_C))))
)
colnames(Xblocked) = c("sex", "w")
Xblocked


##### Rerandomization Design

#define the objective to be: 
#sum of abs standardized differences (sum of abs t-stats)
#define the threshold for best to be the best 5%
set.seed(1)
Nsim = 1e4
ws = matrix(NA, n, Nsim)
sum_of_abs_t_stats_for_all_covariates_rerand = array(NA, Nsim)
for (nsim in 1 : Nsim){
  ws[, nsim] = sample(w) #draw a new allocation from space of all allocations
  XT = model.matrix(~ 0 + ., X[ws[, nsim] == 1, ])
  XC = model.matrix(~ 0 + ., X[ws[, nsim] == 0, ])
  nT = nrow(XT)
  nC = nrow(XC)
  
  p = ncol(XT)
  sum_of_abs_t_stats_for_all_covariates_rerand[nsim] = 0
  for (j in 1 : p){
    xTj = XT[, j]
    xCj = XC[, j]
    sum_of_abs_t_stats_for_all_covariates_rerand[nsim] = sum_of_abs_t_stats_for_all_covariates_rerand[nsim] +
      abs(t.test(xTj, xCj)$statistic)
      #(mean(xTj) - mean(xCj)) / sqrt(var(xTj) / nT + var(xCj) / nC) #SLOW
  }
}
#here's what the objective function looks like:
ggplot(data.frame(sum_of_abs_t_stats_for_all_covariates_rerand = sum_of_abs_t_stats_for_all_covariates_rerand)) + 
  aes(x = sum_of_abs_t_stats_for_all_covariates_rerand) + 
  geom_histogram(bins = 1000) + 
  geom_vline(xintercept = quantile(sum_of_abs_t_stats_for_all_covariates_rerand, 0.05), color = "blue")
#now we locate the legal w's
ws_rerandom = ws[, which(sum_of_abs_t_stats_for_all_covariates_rerand < quantile(sum_of_abs_t_stats_for_all_covariates_rerand, 0.05))]
dim(ws_rerandom)
#these are the best 500

##### Pairwise Matching design

#let's first normalize the columns
Xmmnorm = apply(Xmm, 2, function(xj){(xj - mean(xj)) / sd(xj)})
#let's use a C++ optimized distance matrix

pacman::p_load(Rcpp)
cppFunction("
NumericMatrix compute_distance_matrix_cpp(NumericMatrix X) {
	int n = X.nrow();
	int p = X.ncol();
	NumericMatrix D(n, n);
	std::fill(D.begin(), D.end(), std::numeric_limits<double>::max());
	for (int i_1 = 0; i_1 < (n - 1); i_1++){
    for (int i_2 = i_1 + 1; i_2 < n; i_2++){
      double sqd_diff = 0;
      for (int j = 0; j < p; j++){
        sqd_diff += pow(X(i_1, j) - X(i_2, j), 2); //by default the cmath library in std is loaded
      }
      D(i_1, i_2) = sqd_diff;
      D(i_2, i_1) = D(i_1, i_2);
    }
  }
  return D;
}")

dist_mat = compute_distance_matrix_cpp(Xmmnorm)
dim(dist_mat)
pacman::p_load(nbpMatching)
indicies_pairs = as.matrix(nonbimatch(distancematrix(dist_mat))$matches[, c("Group1.Row", "Group2.Row")])
for (i in 1 : n){
  indicies_pairs[i, ] = sort(indicies_pairs[i, ])
}	
indicies_pairs = unique(indicies_pairs)

#now let's create a w
w = array(NA, n)
coin_flips = runif(n / 2) < 0.5
for (m in 1 : (n/2)){
  if (coin_flips[m]){
    w[indicies_pairs[m, 1]] = 1
    w[indicies_pairs[m, 2]] = 0
  } else {
    w[indicies_pairs[m, 1]] = 0
    w[indicies_pairs[m, 2]] = 1   
  }
}
w

#how good is the imbalance?
XT = model.matrix(~ 0 + ., X[w == 1, ])
XC = model.matrix(~ 0 + ., X[w == 0, ])
p = ncol(XT)
sum_of_abs_t_stats_for_all_covariates_pm = 0
for (j in 1 : p){
  xTj = XT[, j]
  xCj = XC[, j]
  sum_of_abs_t_stats_for_all_covariates_pm = sum_of_abs_t_stats_for_all_covariates_pm +
    abs(t.test(xTj, xCj)$statistic)
}
sum_of_abs_t_stats_for_all_covariates_pm
#that's a lot lower than the rerandomization allocations. How much lower?
1 - ecdf(sum_of_abs_t_stats_for_all_covariates_rerand)(sum_of_abs_t_stats_for_all_covariates_pm)
#just one random w from PM is better than ~99.99% rerandomization w's



### Multinomial logistic regression

rm(list = ls())
pacman::p_load(nnet)
D = read_csv("http://peopleanalytics-regression-book.org/data/health_insurance.csv")
skimr::skim(D)
#convert characters to factors
D = D %>% mutate(gender = as.factor(gender), product = as.factor(product))
table(D$product)
#the response is the product which is A, B, C

#now run the model
multi_model = multinom(product ~ ., D)
summary(multi_model)
#unfortunately it doesn't display pvals for us
# calculate z-statistics of coefficients
z_stats = summary(multi_model)$coefficients / summary(multi_model)$standard.errors
# convert to p-values
p_values = (1 - pnorm(abs(z_stats)))*2
res = cbind(
  t(summary(multi_model)$coefficients), 
  t(p_values)
)[, c(1, 3, 2, 4)]
colnames(res) = c("coef_B", "pval", "coef_C", "pval")
res

head(predict(multi_model, D, type = "probs"), 10)
head(predict(multi_model, D), 10)
