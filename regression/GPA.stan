// This is unfinished

// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data 
data {
  int<lower=0> N; // number of samples
  vector<lower=0, upper=4>[N] GPA1;
  vector<lower=0, upper=4>[N] GPA2;
  vector<lower=0, upper=1>[N] Retain;
  vector<lower=0, upper=1>[N] OrgXXX;
}

// The parameters accepted by the model. 
parameters {
  // inputs
  real GPA1_mu;
  real<lower=0> GPA1_sigma;
  
  real GPA2_mu;
  real<lower=0> GPA2_sigma;
  
  // effect of GPA1 on OrgXXX selection
  real XXX_1_mu; // constant term
  real<lower=0> XXX_1_sigma; 
  
  real XXX_GPA1_mu; // grade effect
  real<lower=0> XXX_GPA1_sigma;
  
  // Retention predictor
  real Retain_1_mu; // constant term
  real<lower=0> Retain_1_sigma; 

  real Retain_GPA1_mu; // grade effect
  real<lower=0> Retain_GPA1_sigma;
  
  real Retain_XXX_mu; // OrgXXX effect
  real<lower=0> Retain_XXX_sigma;
  
  // GPA2 predictor
  real GPA2_1_mu; // constant term
  real<lower=0> GPA2_1_sigma; 

  real GPA2_GPA1_mu; // grade effect
  real<lower=0> GPA2_GPA1_sigma;
  
  real GPA2_XXX_mu; // OrgXXX effect
  real<lower=0> GPA2_XXX_sigma;
}

functions{
  // log likelihood of choosing XXX
  vector lp_XXX(XXX_1_mu,XXX_GPA1_mu, GPA1){
     return XXX_1_mu + XXX_GPA1_mu * GPA1;
  }
  
  // log likelihood of retaining
  vector lp_Retain(Retain_1,Retain_GPA1, GPA1, Retain_XXX, OrgXXX){
     return Retain_1 + Retain_GPA1 * GPA1 + Retain_XXX * OrgXXX;
  }
  
}

// The model to be estimated. We model the output
model {
  GPA1 ~ normal(GPA1_mu, GPA1_sigma);
  GPA2 ~ normal(GPA1_mu, GPA1_sigma);
  
  GPA2_1 ~ normal(mu, sigma);
  GPA2 ~ normal(mu, sigma);
  GPA2 ~ normal(mu, sigma);
}

