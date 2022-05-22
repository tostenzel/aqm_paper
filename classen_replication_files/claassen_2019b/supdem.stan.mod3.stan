// bayesian latent trait model 3 for "Estimating Smooth Panels"
// item intercepts / biases and slopes / factor loadings 
// item-country intercepts
// covariation between intercepts and slopes
// single autoregressive error
// responses are modelled as binomial count

data{
  int<lower=1> N;               		// number of national survey opinions
  int<lower=1> J;               		// number of countries
  int<lower=1> K;  	        			// number of items
  int<lower=1> P;  	        			// number of items-country combinations  
  int<lower=1> T;  	        			// number of years
  int<lower=1,upper=J> jj[N];   		// country j for opinion n
  int<lower=1,upper=K> kk[N];   		// item k for opinion n
  int<lower=1,upper=P> pp[N];   		// item-project p for opinion n
  int<lower=1,upper=T> tt[N];   		// year t for opinion n
  int<lower=1> x[N];   			    	// vector of survey responses, count
  int<lower=1> samp[N];					// vector of sample sizes
}

parameters{
  real<lower=0> sigma_theta;	        // opinion evolution error variance
  real<lower=0> sigma_delta;	        // item-country intercept error variance
  vector[K] lambda;           			// item intercepts
  vector<lower=0>[K] gamm;        		// item slopes
  matrix[T,J] theta_raw; 	        	// raw matrix of T by J latent traits
  vector[P] delta_raw;					// raw item-country intercepts
  row_vector[J] theta_init;				// initial latent traits for first year
  corr_matrix[2] Omega;            // correlation matrix for Gamm pars
  vector<lower=0>[2] tau;          // cor -> cov conversion Gamm
}

transformed parameters{
  matrix[T,J] theta; 	                // matrix of T by J latent traits	
  vector[N] theta_tt_jj;				// N-vector for expanded theta vales
  vector<lower=0,upper=1>[N] pi;      	// fitted values, on logit scale
  vector[P] delta;						// item-country intercepts
  matrix[2,2] Sigma;				// variance-covariance matrix for item ints and slopes
  vector[2] Mu;					// expectation of item ints and slopes
  Mu[1] = 0.5;
  Mu[2] = 1;
  theta[1] = theta_init; 
  delta = sigma_delta * delta_raw;	
  for (t in 2:T)                        // parameter expansion for theta
	theta[t] = theta[t-1] + sigma_theta * theta_raw[t-1];
  Sigma = quad_form_diag(Omega, tau);  
  for (i in 1:N) 						// fitted values model
    theta_tt_jj[i] = theta[tt[i], jj[i]];  // expand theta to N-vector	
  pi = inv_logit(lambda[kk] + gamm[kk] .* theta_tt_jj + delta[pp]);
}

model{
  x ~ binomial(samp, pi); 				// response model
  sigma_theta ~ cauchy(0, 2); 
  sigma_delta ~ cauchy(0, 2); 
  theta_init ~ normal(0, 1);
  tau ~ cauchy(0, 2);
  Omega ~ lkj_corr(2);
  delta_raw ~ normal(0, 1);
  for(t in 1:T) 
	theta_raw[t] ~ normal(0, 1);
  for (k in 1:K-1)						// bivariate normal for item ints and slopes
    append_col(lambda, gamm)[k] ~ multi_normal(Mu, Sigma);   
}

generated quantities {
  vector[N] x_pred;						// fitted data to check model
  vector[N] log_lik; 					// log lik for WAIC calc
  for (i in 1:N) {
	x_pred[i] = binomial_rng(samp[i], pi[i]);
    log_lik[i] = binomial_lpmf(x[i] | samp[i], pi[i]); 
  }
}
