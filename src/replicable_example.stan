// Data
data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
  array[N] int group;
}

// Parameters
parameters {
  vector[3] group_loglam;
  real a;
  real b;
}

// Model
model {
  vector[N] mu;
  vector[N] sigma;
  vector[N] lambda;
  group_loglam ~ normal(0, 1);
  a ~ normal(0, 1);
  b ~ normal(0, 1);
  lambda ~ exponential(0.5);
  for (i in 1:N) {
    mu[i] = a + b * x[i];
    lambda[i] = exp(group_loglam[group[i]]);
  }
  
  sigma ~ exponential(lambda);
  y ~ normal(mu, sigma);
}

