# Cox Proportional Hazard (COXPH) 1Model Inference
#https://scholar.google.com/scholar?hl=en&as_sdt=7%2C39&q=Regression+Models+and+Life-Tables&btnG=

#this fitting is also in the survival package 
#(that package gives you Kaplan-Meier, Weibull, coxph and much more!)
pacman::p_load(survival, skimr, ggplot2)

#load the lung data set (same dataset we used for Kaplan-Meier and Weibull demos)
?survival::lung
lung = na.omit(survival::lung)
lung$status = lung$status - 1 #needs to be 0=alive, 1=dead (see Note in documentation!)
skim(lung)

#we first create the Surv object which allows for the censoring to play
#a role in the estimation:
surv_obj = Surv(lung$time, lung$status)
surv_obj

full_mod = coxph(surv_obj ~ . - time - status, lung)
summary(full_mod)
exp(coef(full_mod))
#interpretation? Remember the unit of e^b_j > 0 is a multiplier on hazards of 
#dying at any time t. These coefficients are not related to expected survival
#like in Weibull regression. So hazards > 1 means more likely to die at any time t
#and hazards < 1 means less likely to die at any time t. So an increase
#in ph.ecog of one unit => a multiplier of 2.478 which is a 2.5 chance of dying
#at any time when compared to a subject without that increase of ph.ecog
#which makes sense (see what ph.ecog measures in documentation)


#fit weibull for contrast
full_mod_weibull = survreg(surv_obj ~ . - time - status, lung)
summary(full_mod_weibull)
exp(coef(full_mod_weibull))
#interpretation?

#now let's do omnibus test
null_mod = coxph(surv_obj ~ 1, lung)
summary(null_mod) 
anova(full_mod, null_mod)
#yes, the at least one variable matter (that's obvious)

#let's do a partial effects test - remove all those variables that don't seem to matter
#note that these removed variables are the same as those found for Weibull regression
red_mod = coxph(surv_obj ~ inst + sex + ph.ecog + ph.karno + wt.loss, lung)
summary(red_mod)
anova(full_mod, red_mod)
#the left out features don't seem to affect hazard rate of survival
#(same conclusion for the Weibull model)

#which to use coxph or weibull? 
#people generally use coxph as it requires less assumptions about the process
#thus it is more believable in general situations
#but it is a hot area of debate e.g.




