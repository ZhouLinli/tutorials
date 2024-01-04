library(tidyverse)
library(broom) # for nice regression tables
library(rstanarm) # for Bayesian regression
library(broom.mixed) # for nice regression tables from rstan
library(shinystan)

#' **Scenario**
#' A senior administrator asks for GPAs by Greek organization.
#' 
#' Read in the data
grades <- read_csv("/Users/linlizhou/Documents/Rprojects/DEubank/regression/StudOrgs.csv")
#' Y1GPA = first year cumulative GPA
#' Y12GPA = cumulative GPA for first and second years combined, or
#' blank if the student didn't complete the second year.
#' Org = Greek organization or "None"
#' 
#' Students pledge an organization in the spring of their first year, and
#' you think the idea is to see if there's an effect on GPAs for the second
#' year. 
#' 
#' *Averages*
avg <- grades %>% 
  group_by(term = str_c("Org",Org)) %>% 
  summarize(N = n(),Average = mean(Y12GPA, na.rm = TRUE), 
            SE = sd(Y12GPA, na.rm = TRUE) / sqrt(sum(!is.na(Y12GPA)))) # average after removing blanks

avg

#' It sure looks like XXX is lower than the rest... You send this off in an email, and
#' sometime later hear that actions are being taken to sanction XXX. Is this justified?
#' 
#' *Linear regression using least squares*
ls_avg <- lm(Y12GPA ~ 0 + Org, data = grades) 

#' show the coefficients
ls_coefs <- ls_avg %>% 
  broom::tidy() %>% 
  select(term, LS = estimate)

ls_coefs

#' cf https://stats.stackexchange.com/questions/234727/standard-errors-of-regression-coefficients-in-a-dummy-variable-regression-model
#' 
#' *Linear regression using Bayes*
#' Uses Maximum Likelihood instead of least squares
#' cf https://en.wikipedia.org/wiki/Maximum_likelihood_estimation
b_avg <- stan_glm(Y12GPA ~ 0 + Org, data = grades, family = gaussian() )

b_coefs <- b_avg %>% 
  broom.mixed::tidy() %>% 
  select(term, Bayes = estimate)

#' interactive exploration
shinystan::launch_shinystan(b_avg)

#' Comparison of means
avg %>% 
  select(term, Average) %>% 
  left_join(ls_coefs) %>% 
  left_join(b_coefs)
