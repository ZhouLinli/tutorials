library(tidyverse)
library(odbc) # install.packages("odbc")
library(glmnet)

source("code/ipeds_utilities.R")
source("code/network.R")
source("code/get_data.R")

model_data <- get_ret_data()

# take a look at the shape of the data
plot(cor_network(model_data, .7), 
      vertex.label = NA,
      vertex.size  = 2)

# eliminate constant columns and those with a high percentage of NAs
to_omit <- which(map_dbl(model_data, sd, na.rm = TRUE) == 0)
if(length(to_omit) > 0) model_data <- model_data %>% select(-!!to_omit)

na_rate <- map_dbl(model_data, ~sum(is.na(.x))/length(.x))
to_omit <- which(na_rate > .4)
if(length(to_omit) > 0) model_data <- model_data %>% select(-!!to_omit)

#' set up for regression
lasso_data <- model_data %>% 
         select_if(is.numeric) %>% 
         na.omit() %>% 
         map_df(scale) # convert to z-scores (mean = 0, SD = 1)

#' Look at the correlations with Retain
cor_scans <- scan_by_correlation(lasso_data, "Retain", take_abs = FALSE)

y <- lasso_data$Retain
x <- lasso_data %>% select(-Retain,-UNITID) %>% as.matrix()
  
#' *LASSO* 
#' For theory and explanations, see:
#' https://hastie.su.domains/StatLearnSparsity_files/SLS.pdf
#' For use in R, see:
#' https://glmnet.stanford.edu/articles/glmnet.html
#' 
lasso_cv <- cv.glmnet(x, y)
plot(lasso_cv)

lambda_min <- lasso_cv$lambda.min
log_lamba_min <- log(lambda_min)

betas <- coef(lasso_cv, s = .05) # try .05 instead
#' Q: what happens when we increase the s (lambda) penalty factor?

# add in the column names
beta_df <- data.frame(ColName = c("Constant",colnames(x)[betas@i]),
                      Beta    = betas@x, 
                      i       = betas@i, # index
                      stringsAsFactors = FALSE)

#' Restrict data to these inputs
x_selected       <- x[,betas@i]
x_selected_names <- attributes(x)$dimnames[[2]][betas@i]

#' Plot mean squared error for these 
#' 
lasso_fit <- glmnet(x_selected, y)
plot(lasso_fit, xvar = "lambda", label = TRUE)

#' try Ordinary Least Squares with these vars to check the R^2
ols_model <- lm(y ~ x_selected)
ols_model %>% 
  broom::glance()

#' What if we remove inputs with coefficient < .1 in abs value?
x_sub_selected_i <- beta_df %>% 
                           filter(abs(Beta) > .08) %>% 
                           select(i) %>% 
                           pull()

x_sub_selected <- x[,x_sub_selected_i] # keep ith columns

ols_sub_model <- lm(y ~ x_sub_selected)

ols_sub_model %>% 
  broom::glance() 

#' so we give up a little R^2 but reduce explanatory vars greatly
#' Which columns are these?
beta_df %>% 
  filter(i %in% x_sub_selected_i)

#' *Dimensionality of Selected Variables*
#' Plot the selected vars by correlation, R^2 ~ .5
plot(cor_network(as.data.frame(x_selected), .7), 
     vertex.label = NA,
     vertex.size  = 2)

#' Formal dimension analysis using Principal Component Analysis, 
#' which in math is called Singular Value Decomposition (SVD) 
x_selected_svd <- svd(cov(x_selected))

#' The relative sizes of the dims are in the $d vector, which are
#' the eigenvalues, i.e. diagonals of the matrix decomposition
plot(x_selected_svd$d / sum(x_selected_svd$d)) # fraction of variance

#' As is common, the first dimension captures a disproportionately large
#' amount of the variance
x_selected_1 <- data.frame(
  Column = x_selected_names,
  Loading = round(x_selected_svd$u[,1],2))


