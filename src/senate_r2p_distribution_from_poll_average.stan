// Data
data {
  int<lower=0> N;
  array[N] int state;
  vector[N] avg;
  vector[N] sen_r2p;
}

// Parameters
parameters {
  vector[50] state_alpha;
  real b;
  real<lower=0> sigma;
}

// Model
model {
  vector[N] mu;
  b ~ normal(1, 0.05);
  state_alpha ~ normal(0, 0.02);
  for (i in 1:N) {
    mu[i] = b * avg[i] + state_alpha[state[i]];
  }
  sigma ~ exponential(0.04);
  sen_r2p ~ normal(mu, sigma);
}

