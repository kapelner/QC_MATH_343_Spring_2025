pacman::p_load(tidyverse, gmodels)

#Picture E
#let's take a look at "Berkson's Paradox" using y is binary
rm(list = ls())
set.seed(1)
n = 2000
disease1 = rbinom(n, 1, 0.15)
disease2 = rbinom(n, 1, 0.17)
#note: disease1 and disease2 are completely independent
hospitalization = rbinom(n, 1, 1 / (1 + exp(-(-3.1 + 6.2 * disease1 * disease2))))
#hospitalization is the "collider" --- it's a function of both disease1 and disease2

disease_example_data = data.table(
  disease1 = disease1,
  disease2 = disease2,
  hospitalization = hospitalization
)
disease_example_data

#we are dealing with binary data, tables are the a good way to visualize
#the relationships among the variables

#we can see here that disease 1 and 2 are independent
with(disease_example_data[disease1 == 0], CrossTable(disease1, disease2))
with(disease_example_data[disease1 == 1], CrossTable(disease1, disease2))
with(disease_example_data[disease2 == 0], CrossTable(disease1, disease2))
with(disease_example_data[disease2 == 1], CrossTable(disease1, disease2))

#the regression says the same thing
summary(glm(disease2 ~ disease1, disease_example_data, family = "binomial"))
#b_1 is near zero and not significantly different from zero
#and real beta_1 in the structural model is zero => no bias in its estimation

#now what if we condition on hospitalization? That is, only do the study on hospital patients!
with(disease_example_data[disease1 == 0 & hospitalization == 1], CrossTable(disease1, disease2))
with(disease_example_data[disease1 == 1 & hospitalization == 1], CrossTable(disease1, disease2))
with(disease_example_data[disease2 == 0 & hospitalization == 1], CrossTable(disease1, disease2))
with(disease_example_data[disease2 == 1 & hospitalization == 1], CrossTable(disease1, disease2))

summary(glm(disease2 ~ disease1, disease_example_data[hospitalization == 1], family = "binomial"))
#b_1 > 0 and significantly different from zero
#and real beta_1 in the structural model is zero => *big* bias in its estimation

#now don't have any selection bias - look at the total population (hospitalized and unhospitalized)
summary(glm(disease2 ~ disease1, disease_example_data, family = "binomial"))
#conclusion: selection bias matters --- make sure your dataset is sampled
#using a simple random sample from the population (lecture 1 in 341!!!)

##now let's take a look at "blocking" the path with regressions 
#which yields bias

#picture F
rm(list = ls())
n = 100
set.seed(1)
x1 = runif(n)
x2 = 4 + 3 * x1 + rnorm(n)
y = -3 + 2 * x2 + rnorm(n)

summary(lm(y ~ x1))
#b1 is appromxiately 2 * 3 = 6 which is unbiased (correct)

summary(lm(y ~ x1 + x2))
#b1 is approximately 0 => biased (incorrect) 
#as the effect of x1 on y is fully blocked

#picture G
rm(list = ls())
n = 100
set.seed(1)
x1 = runif(n)
x2 = 4 + 3 * x1 + rnorm(n)
y = -3 + 3.5 * x1 + 2.5 * x2 + rnorm(n)

summary(lm(y ~ x1))
#b1 is appromxiately 3.5 + 3 * 2.5 = 11 which is unbiased (correct)

summary(lm(y ~ x1 + x2))
#b1 is approximately 3.5 => biased (incorrect) 
#as the effect of x1 on y through x2 is blocked
