// generated with brms 2.14.4
functions {
  /* cumulative-logit log-PDF for a single response
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: ordinal thresholds
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_logit_lpmf(int y, real mu, real disc, vector thres) {
     int nthres = num_elements(thres);
     real p;
     if (y == 1) {
       p = inv_logit(disc * (thres[1] - mu));
     } else if (y == nthres + 1) {
       p = 1 - inv_logit(disc * (thres[nthres] - mu));
     } else {
       p = inv_logit(disc * (thres[y] - mu)) -
           inv_logit(disc * (thres[y - 1] - mu));
     }
     return log(p);
   }
  /* cumulative-logit log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   disc: discrimination parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real cumulative_logit_merged_lpmf(int y, real mu, real disc, vector thres, int[] j) {
     return cumulative_logit_lpmf(y | mu, disc, thres[j[1]:j[2]]);
   }
  /* ordered-logistic log-PDF for a single response and merged thresholds
   * Args:
   *   y: response category
   *   mu: latent mean parameter
   *   thres: vector of merged ordinal thresholds
   *   j: start and end index for the applid threshold within 'thres'
   * Returns:
   *   a scalar to be added to the log posterior
   */
   real ordered_logistic_merged_lpmf(int y, real mu, vector thres, int[] j) {
     return ordered_logistic_lpmf(y | mu, thres[j[1]:j[2]]);
   }
}
data {
  int<lower=1> N;  // total number of observations
  int Y[N];  // response variable
  int<lower=2> nthres;  // number of thresholds
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix
  int prior_only;  // should the likelihood be ignored?
}
transformed data {
  int Kc = K;
  matrix[N, Kc] Xc;  // centered version of X
  vector[Kc] means_X;  // column means of X before centering
  for (i in 1:K) {
    means_X[i] = mean(X[, i]);
    Xc[, i] = X[, i] - means_X[i];
  }
}
parameters {
  vector[Kc] b;  // population-level effects
  ordered[nthres] Intercept;  // temporary thresholds for centered predictors
}
transformed parameters {
  real<lower=0> disc = 1;  // discrimination parameters
}
model {
<<<<<<< HEAD
=======
  // initialize linear predictor term
  vector[N] mu = Xc * b;
  // priors including all constants
  target += normal_lpdf(b | 0, 10);
  target += normal_lpdf(Intercept | 0, 10);
>>>>>>> f1e56def2f86a4253b6ca9672f509f476dfe2453
  // likelihood including all constants
  if (!prior_only) {
    // initialize linear predictor term
    vector[N] mu = Xc * b;
    for (n in 1:N) {
      target += ordered_logistic_lpmf(Y[n] | mu[n], Intercept);
    }
  }
  // priors including all constants
  target += normal_lpdf(b | 0, 10);
  target += normal_lpdf(Intercept | 0, 10);
}
generated quantities {
  // compute actual thresholds
  vector[nthres] b_Intercept = Intercept + dot_product(means_X, b);
}
