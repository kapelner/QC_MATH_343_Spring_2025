pacman::p_load(tidyverse, data.table, gmodels)

# Here's an Example of REAL Correlation without Causation
# Remember, correlation (non-spurious) will persist as the sample size increases.
# So, no amount of data can turn a "correlational estimate into a causal estimate"

#Consider the phenomenon y = "num car accidents" with observed feature x = "num umbrellas sold" but common cause z = "rain amount". 
#It is clear the umbrella sales has *no causal* relationship with car accidents. But they *are correlated* because they are linked by a common cause. Here's the data example that makes this clear.

#The data generating process as specified by the causal diagram looks as follows:
  
rm(list = ls())
set.seed(1)
n = 600
sigma = 0.3

#structural causal model for the DAG "B"
z = runif(n, 0, 6)
x = 0.3 + 0.5 * z + rnorm(n, sigma)
y = 10 + 1.5 * z + rnorm(n, 0, sigma) #y is a variable driven by z with noise

umbrella_example_data = data.frame(
  rainfall = z,
  umbrella_sales = x,
  num_car_accidents = y
)

#Consider only $x$ and $y$. Here's what it appears as:
ggplot(umbrella_example_data) +
  aes(x = umbrella_sales, y = num_car_accidents) +
  geom_point() + 
  geom_smooth(method = "lm")

#and the estimate for the effect of x on y looks like:
summary(lm(num_car_accidents ~ umbrella_sales, umbrella_example_data))
#b_1 is positive and significantly different from zero 
#but real beta_1 in the structural causal model is zero 
#=> *big* bias in its estimation
# So what's the interpretation of b_1?

#What you can't say is that $x$ is a causal contributor to $y$! You may want to say it, but you can't!

#Now let's build a model of $y$ linear in both $x$ and $z$. What happens?

summary(lm(num_car_accidents ~ umbrella_sales + rainfall, umbrella_example_data))
#b_1 is now near zero and not significantly different from zero

#Why is this? Well, you can look at how x affects y in local areas of z for instance.

quantile_z_min = 0.46
quantile_z_max = 0.54
small_window_of_rainfall = umbrella_example_data$rainfall < 
  quantile(umbrella_example_data$rainfall, quantile_z_max) &
  umbrella_example_data$rainfall >
  quantile(umbrella_example_data$rainfall, quantile_z_min)

ggplot(umbrella_example_data[small_window_of_rainfall, ]) +
  aes(x = umbrella_sales, y = num_car_accidents) +
  geom_point()
#there doesn't appear to be a relationship anymore
summary(lm(num_car_accidents ~ umbrella_sales, umbrella_example_data[small_window_of_rainfall, ]))
#b_1 is near zero and not significantly different from zero
#and real beta_1 in the structural model is zero => no bias in its estimation

#If you force the common cause (lurking variable) to be an approximate constant, then you won't see any affect of x on y.
#Over the entire population, you average over epsilon and x the slope of y on x given a value of z then you average over z. 
#That's what multivariate regression is

#let's take a look at "Simpson's Paradox"

rm(list = ls())
set.seed(1)
n = 300
sigma = 4
max_age = 80

#structural causal model for the DAG "D"
x2 = runif(n, 0, max_age)
x1 = 5 + 0.2 * x2 + rnorm(n, 0, sigma)
y = 75 - 1.4 * x1 + 0.9 * x2 + rnorm(n, 0, sigma)


cholesterol_example_data = data.table(
  age = x2,
  exercise = x1,
  cholesterol = y
)

#Consider only $x$ and $y$. Here's what it appears as:
ggplot(cholesterol_example_data) +
  aes(x = exercise, y = cholesterol) +
  geom_point() + 
  geom_smooth(method = "lm")

#and the estimate for the effect of x1 on y looks like:
summary(lm(cholesterol ~ exercise, cholesterol_example_data))
#b_1 is positive and significantly different from zero
#but real beta_1 = -1.4 in the structural causal model
#=> *big* bias in estimation!
#Interpretation of b_1?

#how did this happen? Let's color by age
cholesterol_example_data[, age_binned := cut(age, seq(0, max_age, by = 10))]

ggplot(melt(cholesterol_example_data, id.vars = c("exercise", "cholesterol"), measure.vars = "age_binned", value.name = "age_bracket")) +
  aes(x = exercise, y = cholesterol, color = age_bracket) +
  geom_point() + 
  geom_smooth(method = "lm")  


#and the estimate for the effect of x1 on y conditioning on age looks like:
summary(lm(cholesterol ~ exercise + age, cholesterol_example_data))
#b_1 is negative and significantly different from zero
#and real beta_1 = -1.4 in the structural causal model which is inside the 95% CI
#=> no bias in estimation!
#Interpretation of b_1? Interpretation of b_1 given the DAG?
