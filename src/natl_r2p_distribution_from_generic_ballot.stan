// Data
data {
  int<lower=0> N;
  vector[N] avg;
  vector[N] natl_r2p;
}

// Parameters
parameters {
  real<lower=0> sigma;
}

// Model
model {
  sigma ~ exponential(0.04);
  natl_r2p ~ normal(avg, sigma);
}

