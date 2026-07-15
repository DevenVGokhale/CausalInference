data {
    int<lower=1> n; // Total number of of observations 
    vector[n] x; // Predictor variable 
    vector[n] y; // Response variable
    int prior_only; // Should the likelihood be evaluated?
}
parameters {
    real beta0; // Intercept  
    real beta1; //Slope
    real<lower=0> sigma; // Dispersion parameter
}
transformed parameters {
   real lprior = 0; // Prior contribution to the log-posterior 
   lprior += normal_lpdf(beta0 | 1, 2);
   lprior += normal_lpdf(beta1 | 0, 2);
   lprior += exponential_lpdf(sigma | 0.5);
}
model {
    //Likelihood
    if (!prior_only) {
        target += normal_lpdf(y | beta0 * exp(beta1 * x), sigma); 
    }
    //Priors 
    target += lprior; 
}
