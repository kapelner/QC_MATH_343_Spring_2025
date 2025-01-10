pacman::p_load(ggplot2, survival, ggsurvfit)

set.seed(1)

n = 40

#############we don't get to see the real DGP!
true_k = 3.5
true_lambda = 1.1
true_theta = 1 / true_lambda * gamma(1 / true_k + 1)
y = rweibull(n, shape = true_k, scale = 1 / true_lambda)
#############we don't get to see the real DGP!


#let's now censor at times that are less than the maximum due to differential
#recruitment. Let's randomly call 30% of them "censored
set.seed(1)
c_vec = rbinom(n, 1, prob = 0.3)

#the empirical survival function won't work anymore as "ignoring censoring 
#erroneously treats patients who are censored as part of the risk set for 
#the entire follow-up period (see https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html)
#we need to use the Kaplan-Meier estimator and that's exactly what the package does
#https://scholar.google.com/scholar?q=Nonparametric+estimation+from+incomplete+observations&hl=en&btnG=Search&as_sdt=1%2C39&as_sdtp=on

survival_obj = Surv(y, 1 - c_vec) #we flip our vector to be in their format
survival_fit_obj = survfit2(survival_obj ~ 1) 
survival_fit_obj %>% 
  ggsurvfit() +
  xlab("time") +
  ylim(0, 1) +
  ylab("survival probability estimate") + 
  add_confidence_interval() +
  add_risktable()
#why does the K-S survival function hit zero? There's no censored observations at
#time max(y)

#can we do inference for any time t?
summary(survival_fit_obj) 
#those are the upper/lower limits of the bands you see in the plot

#how about inference for theta := Med[Y]?
summary(survival_fit_obj)$table[7 : 9]



########### let's build a K-S survival curve from scratch

set.seed(1)

n = 6

true_k = 3.5
true_lambda = 1.1
true_theta = 1 / true_lambda * gamma(1 / true_k + 1)
y = sort(rweibull(n, shape = true_k, scale = 1 / true_lambda))
 
#let's censor the 2nd and fourth
c_vec = c(0, 1, 0, 1, 1, 0)

#create the table
d_i = c(1, 0, 1, 0, 0, 1)
q_i = c_vec
survival_table = data.frame(t_i = y, d_i = d_i, q_i = q_i)
survival_table

#n_1
n_i = array(NA, n)
n_i[1] = n
n_i[2] = n_i[1] #repeat
n_i[3] = n_i[2] - d_i[1] - q_i[2]
n_i[4] = n_i[3] #repeat
n_i[5] = n_i[4] #repeat
n_i[6] = n_i[5] - d_i[3] - q_i[4] - q_i[5]

survival_table$n_i = n_i
#now we ignore all censorings as they are accounted for in 
survival_table = survival_table[c_vec == 0, ]
survival_table$q_i = NULL
survival_table$p_i = 1 - survival_table$d_i / survival_table$n_i
survival_table$p_surv_i = cumprod(survival_table$p_i)
survival_table

survival_obj = Surv(y, 1 - c_vec) #we flip our vector to be in their format
# survival_fit_obj = survfit(survival_obj ~ 1) 
# plot(survival_fit_obj)
# summary(survival_fit_obj)

survival_fit_obj = survfit2(survival_obj ~ 1) 
survival_fit_obj %>% 
  ggsurvfit() +
  xlab("time") +
  ylim(0, 1) +
  ylab("survival probability estimate") + 
  add_confidence_interval() +
  add_risktable()


#Log Rank Test
#https://scholar.google.com/scholar?q=Evaluation+of+survival+data+and+two+new+rank+order+statistics+arising+in+its+consideration&hl=en&btnG=Search&as_sdt=1%2C39&as_sdtp=on
#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Asymptotically+Efficient+Rank+Invariant+Test+Procedures&btnG=

#let's use the lung dataset (same as your lab) and consider the two populations: 
#male survival vs female survival
rm(list = ls())
is_male = 1 - (survival::lung$sex - 1) #zero is female, one is male
n_1 = sum(is_male == 1)
n_2 = sum(is_male == 0)
y_1 = survival::lung$time[is_male == 1]
c_1 = 1 - (survival::lung$status[is_male == 1] - 1)
y_2 = survival::lung$time[is_male == 0]
c_2 = 1 - (survival::lung$status[is_male == 0] - 1)
#what are the number of observed events?
sum(c_1 == 0)
sum(c_2 == 0)

#we'll learn about data.table soon enough
pacman::p_load(data.table)
num_observation_to_print = 50

#create a data frame from the original data
tab_1 = data.table(t_i = y_1, d_1i = ifelse(c_1 == 0, 1, 0), q_1i = ifelse(c_1 == 0, 0, 1))
setorder(tab_1, "t_i")
tab_1[1:num_observation_to_print]

#now we sum all d_i's and q_i's into each specific time period
tab_1 = tab_1[, .(d_1i = sum(d_1i), q_1i = sum(q_1i)), by = t_i]
tab_1[1:num_observation_to_print]


#now we duplicate for the other dataset
tab_2 = data.table(t_i = y_2, d_2i = ifelse(c_2 == 0, 1, 0), q_2i = ifelse(c_2 == 0, 0, 1))
setorder(tab_2, "t_i")
tab_2 = tab_2[, .(d_2i = sum(d_2i), q_2i = sum(q_2i)), by = t_i]
tab_2[1:num_observation_to_print]

#now we put the datasets together
tab =   merge(tab_1, tab_2, by = "t_i", all = TRUE)
tab[1:num_observation_to_print]
#replace d_i = NA's with zeroes as this means there was no event in this group
#replace q_i = NA's with zeroes as this means there was no censoring in this group
tab[is.na(d_1i), d_1i := 0]
tab[is.na(d_2i), d_2i := 0]
tab[is.na(q_1i), q_1i := 0]
tab[is.na(q_2i), q_2i := 0]
tab[1:num_observation_to_print]

#for censorings, we have to move their timestamp to the previous which has a d_i
tab[d_1i + d_2i > 0, t_i_with_pos_d_i := t_i]
pacman::p_load(zoo)
tab[, t_i_with_pos_d_i := na.locf(t_i_with_pos_d_i)]
tab[1:num_observation_to_print]

#now we compute the cumulative number of events and censors
tab[, cum_d1 := cumsum(d_1i)]
tab[, cum_q1 := cumsum(q_1i)]
tab[, cum_d2 := cumsum(d_2i)]
tab[, cum_q2 := cumsum(q_2i)]
tab[1:num_observation_to_print]


#now the n_i is the original sample size minus those events and censorings in the previous time period:
tab[, n_1i := n_1 - shift(cum_d1, 1) - shift(cum_q1, 1)]
tab[, n_2i := n_2 - shift(cum_d2, 1) - shift(cum_q2, 1)]
#and the first entry is the total sample size
tab$n_1i[1] = n_1
tab$n_2i[1] = n_2
#we can also drop useless columns
tab[, c("cum_d1", "cum_q1", "cum_d2", "cum_q2", "q_1i", "q_2i", "t_i") := NULL]
tab[1:num_observation_to_print]

#now we can compute expected values
tab[, e1 := n_1i / (n_1i + n_2i) * (d_1i + d_2i)]
tab[, e2 := n_2i / (n_1i + n_2i) * (d_1i + d_2i)]
tab[1:num_observation_to_print]
e1_tot = sum(tab$e1)
e2_tot = sum(tab$e2)
o1_tot = sum(tab$d_1i)
o2_tot = sum(tab$d_2i)
e1_tot
e2_tot
o1_tot
o2_tot

thetahathat = (o1_tot - e1_tot)^2 / e1_tot + (o2_tot - e2_tot)^2 / e2_tot
thetahathat
thetahathat > qchisq(.95, 1)
#=>Reject
#pval
1 - pchisq(thetahathat, 1)


#now let's do the log rank test using the R package
rm(list = ls())
y = survival::lung$time
c_vec = 1 - (survival::lung$status - 1)

survival_obj = Surv(y, 1 - c_vec)
survival_obj
survival_diff_obj = survdiff(survival_obj ~ is_male)
survival_diff_obj
