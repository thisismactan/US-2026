// Data
data {
  int<lower=0> N;
  int<lower=0> N_pollsters;
  int<lower=0> N_partisan;
  vector[N] avg;
  vector[N] r2p;
  vector[N] partisan_rep;
  vector[N] partisan_dem;
  vector[N] lv;
  array[N] int pollster_code;
}

// Parameters
parameters {
  vector[N_pollsters] house_effect;
  real rep_effect;
  real dem_effect;
  real lv_effect;
  real<lower=0> sigma;
}

// Model
model {
  vector[N] mu;
  house_effect ~ normal(0, 0.02);
  rep_effect ~ normal(0, 0.02);
  dem_effect ~ normal(0, 0.02);
  lv_effect ~ normal(0, 0.02);
  sigma ~ exponential(0.03);
  for (i in 1:N) {
    mu[i] = house_effect[pollster_code[i]] + rep_effect * partisan_rep[i] + dem_effect * partisan_dem[i] + lv_effect * lv[i] + r2p[i];
  }
  avg ~ normal(mu, sigma);
}

