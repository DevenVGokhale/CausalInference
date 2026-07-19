/** 
* sim_hmm.cpp
This dcript contains function to simulate the hidden markov process.  
and some helper functions  
*/

#include<Rcpp.h>
using namespace Rcpp;

// helper function for simulating the hmm 
// this function calculates transition probability for a given Cavg 
double cal_p12(double Cavg, double beta0 = -5.1, double beta1 = 1.0) {

    if (Cavg < 0) stop("Cavg is concentration and must be non-negative, got Cavg=%d", Cavg);
    double log_odds = beta0 + Cavg * beta1;
    double p12 = 1.0 / (1.0 + std::exp(-log_odds));
    return p12;
}

// this function calculates the transition matrix for a given transistion probability 
NumericMatrix cal_trans_mat(double p12) {
    if (p12 < 0) stop("p12 is a probability and must be non-negative, got p12=%d", p12);
    NumericMatrix tm(2, 2); 
    tm(0, 0) = 1 - p12;
    tm(0, 1) = p12;
    //hard coding these for now... 
    // may need to think if I need change how these get calculated
    tm(1, 0) = 0.05;
    tm(1, 1) = 0.95;
    return tm;
}

// this function simulates the hmm model 
//[[Rcpp::export]]
DataFrame sim_hmm_once(List params) {

    //unpack the parameters 
    double T = params["T"];
    double K = params["K"];
    int X0 = params["X0"];

    NumericVector times = params["times"];
    Nullable<NumericVector> Cavg_in = params["Cavg"];
    NumericVector Cavg;
    // if no Cavg is given then defaults to time invariant model.
    if (Cavg_in.isNull()) {
        Cavg = NumericVector(T, 0.0);
    } else {
        Cavg = as<NumericVector>(Cavg_in);
    }
    NumericVector lambda = params["lambda"]; 

    // create empty placeholders
    IntegerVector current_state(T);
    NumericVector emissions(T);

    // initialise first state
    current_state[0] = X0;

    // simulate the hmm
    for (int t = 1; t < T; t++) {
        // calculate transition probability from Cavg at previous time step
        double p12 = cal_p12(Cavg[t - 1]);

        // get transition matrix
        NumericMatrix trans = cal_trans_mat(p12);

        // extract row corresponding to current state (0-indexed)
        NumericVector prob_row = trans.row(current_state[t - 1] - 1);

        // sample next latent state
        IntegerVector draw = sample(K, 1, true, prob_row);
        current_state[t] = draw[0];

        // generate emission from Poisson
        emissions[t] = R::rpois(lambda[current_state[t] - 1]);
    }

    return DataFrame::create(
        Named("time")     = times, 
        Named("Cavg")     = Cavg,   
        Named("latent_state")    = current_state,
        Named("adverse_events") = emissions
    );
}  
