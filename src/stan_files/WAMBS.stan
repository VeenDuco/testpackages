data {
  int<lower=0> n; // number of case
  vector[n] diff; // vector with n observations on variable diff
  vector[n] age;  // vector with n observations on variable age
  vector[n] age2; // vector with n observations on variable age2
  // hyperparameters of the prior distributions
  real mu0;       // mean intercept
  real sigma20;   // variance intercept
  real mu1;       // mean age
  real sigma21;   // variance age
  real mu2;       // mean age2
  real sigma22;   // variance age2
  real kappa0;    // shape inverse gamma on epsilon
  real theta0;    // rate inverse gamma on epsilon
  int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
}

parameters{
  real beta_intercept;    // regression parameter intercept
  real beta_age;          // regression parameter age
  real beta_age2;         // regression parameter age2
  real<lower=0> epsilon2; // residual variance parameter
}

model{
  // priors
  beta_intercept ~ normal(mu0, sqrt(sigma20));
  beta_age       ~ normal(mu1, sqrt(sigma21));
  beta_age2      ~ normal(mu2, sqrt(sigma22));
  epsilon2       ~ inv_gamma(kappa0, theta0);
  // model
  // note sqrt(epsilon2) as normal() is defined using sd and not variance
  if(run_estimation == 1){
  diff ~ normal(beta_intercept + beta_age * age + beta_age2 * age2, sqrt(epsilon2));
  }
}

generated quantities{
  // predictives
  vector[n] diff_rep;
  for(i in 1:n){
  diff_rep[i] = normal_rng(beta_intercept + beta_age * age[i] + beta_age2 * age2[i], sqrt(epsilon2));
  }
}
