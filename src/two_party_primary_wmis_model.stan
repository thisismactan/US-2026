// Data
data {
  // Dev
  int<lower=0> N_t2p_obs;
  int<lower=0> N_t2p_mis;
  array[N_t2p_obs] int<lower=1, upper=N_t2p_obs+N_t2p_mis> ii_t2p_obs; // array indices for observed top-two primary
  array[N_t2p_mis] int<lower=1, upper=N_t2p_obs+N_t2p_mis> ii_t2p_mis; // array indices for missing
  vector[N_t2p_obs + N_t2p_mis] r2p;
  vector[N_t2p_obs] r_primary_obs;
  array[N_t2p_obs + N_t2p_mis] int midterm;
  // OOT
  int<lower=0> N_t2p_obs_oot;
  int<lower=0> N_t2p_mis_oot;
  array[N_t2p_obs_oot] int<lower=1, upper=N_t2p_obs_oot+N_t2p_mis_oot> ii_t2p_obs_oot; // array indices for observed top-two primary
  array[N_t2p_mis_oot] int<lower=1, upper=N_t2p_obs_oot+N_t2p_mis_oot> ii_t2p_mis_oot; // array indices for missing
  vector[N_t2p_obs_oot + N_t2p_mis_oot] r2p_oot;
  vector[N_t2p_obs_oot] r_primary_obs_oot;
  array[N_t2p_obs_oot + N_t2p_mis_oot] int midterm_oot;
}

transformed data {
  int<lower=0> N_e = N_t2p_obs + N_t2p_mis;
  int<lower=0> N_oot = N_t2p_obs_oot + N_t2p_mis_oot;
}

// Parameters
parameters {
  vector[N_t2p_mis] r_primary_mis;
  vector[N_t2p_mis_oot] r_primary_mis_oot;
  real<lower=0> sigma_t2p;
  real a_t2p;
  real b1_t2p;
  real b2_t2p;
  real b3_t2p;
}

transformed parameters {
  vector[N_e] r_primary;
  vector[N_oot] r_primary_oot;
  r_primary[ii_t2p_obs] = r_primary_obs;
  r_primary[ii_t2p_mis] = r_primary_mis;
  r_primary_oot[ii_t2p_obs_oot] = r_primary_obs_oot;
  r_primary_oot[ii_t2p_mis_oot] = r_primary_mis_oot;
}

// Model
model {
  vector[N_e] mu_t2p;
  r_primary_mis ~ normal(0.5, 0.25);
  r_primary_mis_oot ~ normal(0.5, 0.25);
  a_t2p ~ normal(0, 0.1);
  b1_t2p ~ normal(0, 0.1);
  b2_t2p ~ normal(0.9, 0.2);
  b3_t2p ~ normal(0.05, 0.1);
  
  for (i in 1:N_e) {
    mu_t2p[i] = a_t2p + b1_t2p * midterm[i] + b2_t2p * r_primary[i] + b3_t2p * r_primary[i] * midterm[i];
  }
  
  sigma_t2p ~ exponential(0.1);
  r2p ~ normal(mu_t2p, sigma_t2p);
}

// Generated quantities
generated quantities {
  vector[N_e] mu_t2p_pred;
  vector[N_e] r2p_t2p_pred;
  vector[N_oot] mu_t2p_pred_oot;
  vector[N_oot] r2p_t2p_pred_oot;
  // In-time
  for (i in 1:N_e) {
    mu_t2p_pred[i] = a_t2p + b1_t2p * midterm[i] + b2_t2p * r_primary[i] + b3_t2p * r_primary[i] * midterm[i];
    r2p_t2p_pred[i] = normal_rng(mu_t2p_pred[i], sigma_t2p);
  }
  // OOT
  for (i in 1:N_oot) {
    mu_t2p_pred_oot[i] = a_t2p + b1_t2p * midterm_oot[i] + b2_t2p * r_primary_oot[i] + b3_t2p * r_primary_oot[i] * midterm_oot[i];
    r2p_t2p_pred_oot[i] = normal_rng(mu_t2p_pred_oot[i], sigma_t2p);
  }
}