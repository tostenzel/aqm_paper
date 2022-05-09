// bayesian latent trait model 6 for "Estimating Smooth Panels"
// item intercepts / biases and slopes / factor loadings 
// item-country intercepts
// covariation between intercepts and slopes
// single autoregressive error
// responses are modelled as beta-binomial count
// validation test

data{
  int<lower=1> N;               		// number of national survey opinions
  int<lower=1> J;               		// number of countries
  int<lower=1> K;  	        			// number of items
  int<lower=1> P;  	        			// number of items-country combinations  
  int<lower=1> T;  	        			// number of years
  int<lower=1,upper=J> jj[N];   		// country j for opinion n
  int<lower=1,upper=K> kk[N];   		// item k for opinion n
  int<lower=1,upper=P> pp[N];   		// item-country p for opinion n
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
  vector[P] delta_raw;					// item-country intercepts
  row_vector[J] theta_init;				// initial latent traits for first year
  real<lower=0> phi;					// dispersion parameter
  corr_matrix[2] Omega;            // correlation matrix for item pars
  vector<lower=0>[2] tau;          // cor -> cov conversion
}

transformed parameters{
  matrix[T,J] theta; 	                // matrix of T by J latent traits	
  vector[N] theta_tt_jj;				// N-vector for expanded theta vales
  vector<lower=0,upper=1>[N] eta;      	// fitted values, on logit scale
  vector[P] delta;						// item-country intercepts
  matrix[2,2] Sigma;				// variance-covariance matrix for item ints and slopes
  vector[2] Mu;					// expectation of item ints and slopes
  vector<lower=0>[N] alpha;					// beta alpha parameter
  vector<lower=0>[N] beta;					// beta beta parameter  
  theta[1] = theta_init; 
  delta = sigma_delta * delta_raw;      // parameter expansion for delta
  for (t in 2:T)                        // parameter expansion for theta
	theta[t] = theta[t-1] + sigma_theta * theta_raw[t-1];
  Sigma = quad_form_diag(Omega, tau);  
  Mu[1] = 0.5;
  Mu[2] = 1;
  for (i in 1:N) 					
  	theta_tt_jj[i] = theta[tt[i], jj[i]];  // expand theta to N-vector	
  eta = inv_logit(lambda[kk] + gamm[kk] .* theta_tt_jj + delta[pp]);  // fitted values model
  alpha = phi * eta; 						// reparamaterise beta-binom alpha par
  beta = phi * (1 - eta); 					// reparamaterise beta-binom beta par
}

model{
  x ~ beta_binomial(samp, alpha, beta); 		// response model
  phi ~ gamma(4, 0.1);
  sigma_theta ~ cauchy(0, 2); 
  sigma_delta ~ cauchy(0, 2); 
  tau ~ cauchy(0, 2);
  Omega ~ lkj_corr(2);
  theta_init ~ normal(0, 1);
  delta_raw ~ normal(0, 1);
  for(t in 1:T) 
	theta_raw[t] ~ normal(0, 1);
  for (k in 1:K-1)						// bivariate normal for item ints and slopes
    append_col(lambda, gamm)[k] ~ multi_normal(Mu, Sigma);  
}
