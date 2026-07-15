data {
    int<lower=1> n; // Total number of of observations 
    vector[n] x; // Predictor variable 
    vector[n] y; // Response variable
}
parameters {
    real beta0; // Intercept  
    real beta1; //Slope
    real<lower=0> sigma; // Dispersion parameter
}
model{
    y ~ normal(beta0 * exp(beta1 * x), sigma);
    beta0 ~ normal(1, 2);
    beta1 ~ normal(0, 2);
    sigma ~ exponential(0.1);
}
