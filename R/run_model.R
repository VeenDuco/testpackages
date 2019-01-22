#' Get prior or posterior predictive samples for the model
#'
#' Enter a more detailed description if you like.
#'
#' @param n number of observations
#' @param diff outcome variable
#' @param age linear predictor
#' @param age2 quadratic predictor
#' @param intercept.mu hyperparameter mean intercept prior
#' @param intercept.variance hyperparameter variance intercept prior, note not sd
#' @param age.mu hyperparameter mean age prior
#' @param age.variance hyperparameter variance age prior, note not sd
#' @param age2.mu hyperparameter mean age2 prior
#' @param age2.variance hyperparameter variance age2 prior, note not sd
#' @param kappa0 shape inverse gamma prior on residual variance, note not sd
#' @param theta0 rate inverse gamma prior on residual variance, note not sd
#' @param run_estimation to evaluate likelihood or not, thus 0 to get prior predictive 1 to get posterior predictive
#'
#' @return returns prior of posterior predictive samples.
#'
#' @examples
#' \dontrun{
#' run_model(10, rnorm(10), rnorm(10), rnorm(10), 0, 1, 0, 1, 0, 1, .5, .5, 0)
#' }
#'
#' @export
run_model <- function(n, diff, age, age2, intercept.mu, intercept.variance,
                      age.mu, age.variance, age2.mu, age2.variance, kappa0,
                      theta0, run_estimation) {

  # temp <- '
  # data {
  # int<lower=0> n; // number of case
  # vector[n] diff; // vector with n observations on variable diff
  # vector[n] age;  // vector with n observations on variable age
  # vector[n] age2; // vector with n observations on variable age2
  # // hyperparameters of the prior distributions
  # real mu0;       // mean intercept
  # real sigma20;   // variance intercept
  # real mu1;       // mean age
  # real sigma21;   // variance age
  # real mu2;       // mean age2
  # real sigma22;   // variance age2
  # real kappa0;    // shape inverse gamma on epsilon
  # real theta0;    // rate inverse gamma on epsilon
  # int<lower = 0, upper = 1> run_estimation; // a switch to evaluate the likelihood
  # }
  #
  # parameters{
  # real beta_intercept;    // regression parameter intercept
  # real beta_age;          // regression parameter age
  # real beta_age2;         // regression parameter age2
  # real<lower=0> epsilon2; // residual variance parameter
  # }
  #
  # model{
  # // priors
  # beta_intercept ~ normal(mu0, sqrt(sigma20));
  # beta_age       ~ normal(mu1, sqrt(sigma21));
  # beta_age2      ~ normal(mu2, sqrt(sigma22));
  # epsilon2       ~ inv_gamma(kappa0, theta0);
  # // model
  # // note sqrt(epsilon2) as normal() is defined using sd and not variance
  # if(run_estimation == 1){
  # diff ~ normal(beta_intercept + beta_age * age + beta_age2 * age2, sqrt(epsilon2));
  # }
  # }
  #
  # generated quantities{
  # // predictives
  # vector[n] diff_rep;
  # for(i in 1:n){
  # diff_rep[i] = normal_rng(beta_intercept + beta_age * age[i] + beta_age2 * age2[i], sqrt(epsilon2));
  # }
  # }
  #
  # '
  # model <- rstan::stan_model(model_code = temp)

  samples <- rstan::sampling(object = stanmodels$WAMBS,
                             chains = 2, iter = 1500,
                             data=list(n       = n,
                                       diff    = diff,
                                       age     = age,
                                       age2    = age2,
                                       mu0     = intercept.mu,
                                       sigma20 = if(intercept.variance == 0) .00001 else intercept.variance,
                                       mu1     = age.mu,
                                       sigma21 = if(age.variance == 0) .00001 else age.variance,
                                       mu2     = age2.mu,
                                       sigma22 = if(age2.variance == 0) .00001 else age2.variance,
                                       kappa0  = kappa0,
                                       theta0  = theta0,
                                       run_estimation = run_estimation
                             ))

  # if(input$addvariance == TRUE){
  return(rstan::extract(samples, pars = "diff_rep"))

}
