library(tidyverse)
library(rstan)
library(broom.mixed) # for cleaning up regression tables from rstan
library(bayesplot) # for plotting Bayes results

#' Stan options
options(mc.cores = parallel::detectCores())

#' Read in the data
grades <- read_csv("/Users/linlizhou/Documents/Rprojects/DEubank/regression/StudOrgs.csv") %>% 
  mutate(Y2GPA = pmin(2*Y12GPA - Y1GPA, 4),
         # binary indicator of second year retention
         Retain = as.integer(!is.na(Y2GPA)),
         # binary indicator of XXX
         OrgXXX = if_else(Org == "XXX", 1, 0),
         # create GPAs centered at zero
         Y1GPAc = scale(Y1GPA, center = TRUE, scale = FALSE),
         Y2GPAc = scale(Y2GPA, center = TRUE, scale = FALSE))

#' Set up the data
dataList <- list(GPA1 = grades$Y1GPAc, 
                 GPA2 = grades$Y2GPA2,
                 Retain = grades$Retain,
                 OrgXXX = grades$OrgXXX,
                 N = nrow(grades))


#' Compile the model
gpa_model <- stan_model("/Users/linlizhou/Documents/Rprojects/DEubank/regression/GPA.stan")

# apply model to the data
stanFit <- sampling(object =gpa_model, 
                    data = dataList, 
                    chains = 3, 
                    iter = 1000, 
                    warmup = 200, 
                    thin = 1)

# take a look 
launch_shinystan(stanFit)