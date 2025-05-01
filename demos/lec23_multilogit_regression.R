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
