
# Spurious Correlation

#Take a look at the following real data:

pacman::p_load(tidyverse, magrittr, data.table)

spurious = data.frame(
  yearly_divorce_rate_maine_per_1000 = c(5,4.7,4.6,4.4,4.3,4.1,4.2,4.2,4.2,4.1),
  yearly_US_consumption_margarine_per_capita = c(8.2,7,6.5,5.3,5.2,4,4.6,4.5,4.2,3.7)
)

with(spurious, 
     cor(yearly_divorce_rate_maine_per_1000, yearly_US_consumption_margarine_per_capita))


#And visually,

ggplot(spurious, aes(x = yearly_divorce_rate_maine_per_1000, y = yearly_US_consumption_margarine_per_capita)) +
  geom_point() + geom_smooth()

#How did this happen? I looked at many, many different datasets until I found something impressive! 
#Well, we can imagine doing the same thing. Let's look at a million datasets and find the dataset most correlated with the yearly consumption of margarine per capita:

y = spurious$yearly_US_consumption_margarine_per_capita
n = length(y)

n_sim = 1e6
best_abs_corr = 0
best_random_xs = NULL
for (i in 1 : n_sim){
  x = rnorm(n)
  
  random_abs_corr = abs(cor(x, y))
  if (random_abs_corr > best_abs_corr){
    best_abs_corr = random_abs_corr
    best_random_xs = x
  }
}
spurious$best_random_xs = best_random_xs

best_abs_corr

#And visually,

ggplot(spurious, aes(x = best_random_xs, y = yearly_US_consumption_margarine_per_capita)) +
  geom_point() + geom_smooth() + ggtitle(paste("Spurious Correlation has |r| = ", round(best_abs_corr, 3)))

#So what's the narrative here? If you look through a gajillion random features that have no causal 
#connection with the phenomenon $y$, you will eventually find something that "clicks". It's the same 
#as the "chance capitalization" from 342W taken to an extreme. Here are a whole bunch of them:
#https://www.tylervigen.com/spurious-correlations

#However, these will all vanish if you keep collecting data. 
#Anything that is built upon falsehood will crumble!

