// Input data
data {
  // National swing component
  //// Dev data
  int<lower=0> N_e;
  vector[N_e] r2p;
  vector[N_e] last_r2p;
  vector[N_e] midterm;
  vector[N_e] redistricted;
  vector[N_e] natl_r2p_change;
  vector[N_e] last_pres_r2p;
  vector[N_e] contested_last;
  array[N_e] int incumbent_running;
  //// OOT data
  int<lower=0> N_oot;
  vector[N_oot] last_r2p_oot;
  vector[N_oot] midterm_oot;
  vector[N_oot] redistricted_oot;
  vector[N_oot] natl_r2p_change_oot;
  vector[N_oot] last_pres_r2p_oot;
  vector[N_oot] contested_last_oot;
  array[N_oot] int incumbent_running_oot;
  
  // Top-two primary component
  //// Dev data
  vector[N_e] has_t2p;
  int<lower=0> N_t2p_obs;
  int<lower=0> N_t2p_mis;
  array[N_t2p_obs] int<lower=1, upper=N_t2p_obs+N_t2p_mis> ii_t2p_obs; // array indices for observed top-two primary
  array[N_t2p_mis] int<lower=1, upper=N_t2p_obs+N_t2p_mis> ii_t2p_mis; // array indices for missing
  vector[N_t2p_obs] r_primary_obs;
  // OOT
  vector[N_oot] has_t2p_oot;
  int<lower=0> N_t2p_obs_oot;
  int<lower=0> N_t2p_mis_oot;
  array[N_t2p_obs_oot] int<lower=1, upper=N_t2p_obs_oot+N_t2p_mis_oot> ii_t2p_obs_oot; // array indices for observed top-two primary
  array[N_t2p_mis_oot] int<lower=1, upper=N_t2p_obs_oot+N_t2p_mis_oot> ii_t2p_mis_oot; // array indices for missing
  vector[N_t2p_obs_oot] r_primary_obs_oot;
}

// Parameters
parameters {
  real<lower=0> sigma;
  // National swing component
  vector[4] a_inc;
  vector[4] b1_inc;
  vector[4] b2_inc;
  vector[4] b3_inc;
  real a_midterm;
  real b1_midterm;
  real b1_redist;
  real b2_midterm;
  real b2_redist;
  real b3_midterm;
  real b3_redist;
  real b3_contested_last;
  vector<lower=0>[4] tau;
  corr_matrix[4] Rho;
  real<lower=0> sigma_ns;
  
  // Top-two primary component
  vector[N_t2p_mis] r_primary_mis;
  vector[N_t2p_mis_oot] r_primary_mis_oot;
  real<lower=0> sigma_t2p;
  real a_t2p;
  real b1_t2p;
  real b2_t2p;
  real b3_t2p;
  
  // Combination step
  real<lower=0, upper=1> alpha_0;
  real<lower=0, upper=1> alpha_1;
}

transformed parameters {
  // Top-two primary component
  vector[N_e] r_primary;
  vector[N_oot] r_primary_oot;
  r_primary[ii_t2p_obs] = r_primary_obs;
  r_primary[ii_t2p_mis] = r_primary_mis;
  r_primary_oot[ii_t2p_obs_oot] = r_primary_obs_oot;
  r_primary_oot[ii_t2p_mis_oot] = r_primary_mis_oot;
}

// Model
model {
  vector[N_e] mu;
  vector[N_e] alpha_t2p;
  // National swing component
  a_midterm ~ normal(0, 0.1);
  b1_midterm ~ normal(0, 0.5);
  b1_redist ~ normal(-1, 0.5);
  b2_midterm ~ normal(0, 0.5);
  b2_redist ~ normal(-1, 0.5);
  b3_midterm ~ normal(0, 0.5);
  b3_redist ~ normal(1, 0.5);
  b3_contested_last ~ normal(1, 0.5);
  Rho ~ lkj_corr(1);
  tau ~ exponential(0.5);
  sigma_ns ~ exponential(0.5);
  array[4] vector[4] inc_coefs;
  for (j in 1:4) {
    inc_coefs[j] = [a_inc[j], b1_inc[j], b2_inc[j], b3_inc[j]]';
  }
  inc_coefs ~ multi_normal(rep_vector(0, 4), quad_form_diag(Rho, tau));
  r_primary_mis ~ normal(0.5, 0.25);
  r_primary_mis_oot ~ normal(0.5, 0.25);
  a_t2p ~ normal(0, 0.1);
  b1_t2p ~ normal(0, 0.1);
  b2_t2p ~ normal(0.9, 0.2);
  b3_t2p ~ normal(0.05, 0.1);
  alpha_1 ~ normal(0.5, 0.25); // this is additional weight if there is a top-two primary
  sigma ~ exponential(0.5);
  
  for (i in 1:N_e) {
    alpha_t2p[i] = alpha_1 * has_t2p[i];
    mu[i] = (1 - alpha_t2p[i]) * (a_inc[incumbent_running[i]] + a_midterm * midterm[i] +
      (b1_inc[incumbent_running[i]] + b1_midterm * midterm[i] + b1_redist * redistricted[i]) * last_r2p[i] * contested_last[i] +
      (b2_inc[incumbent_running[i]] + b2_midterm * midterm[i] + b2_redist * redistricted[i]) * natl_r2p_change[i] +
      (b3_inc[incumbent_running[i]] + b3_midterm * midterm[i] + b3_redist * redistricted[i] + b3_contested_last * (1 - contested_last[i])) * last_pres_r2p[i]) +
      alpha_t2p[i] * (a_t2p + b1_t2p * midterm[i] + b2_t2p * r_primary[i] + b3_t2p * r_primary[i] * midterm[i]);
  }
  
  // Combine the two in the appropriate proportions
  r2p ~ normal(mu, sigma);
}

// Generated quantities
generated quantities {
  vector[N_e] mu_pred;
  vector[N_e] r2p_pred;
  vector[N_oot] mu_pred_oot;
  vector[N_oot] r2p_pred_oot;
  vector[N_e] alpha_t2p_pred;
  vector[N_oot] alpha_t2p_pred_oot;
  // Dev (2010 - 2024 data)
  for (i in 1:N_e) {
    alpha_t2p_pred[i] = alpha_1 * has_t2p[i];
    mu_pred[i] = (1 - alpha_t2p_pred[i]) * (a_inc[incumbent_running[i]] + a_midterm * midterm[i] +
      (b1_inc[incumbent_running[i]] + b1_midterm * midterm[i] + b1_redist * redistricted[i]) * last_r2p[i] * contested_last[i] +
      (b2_inc[incumbent_running[i]] + b2_midterm * midterm[i] + b2_redist * redistricted[i]) * natl_r2p_change[i] +
      (b3_inc[incumbent_running[i]] + b3_midterm * midterm[i] + b3_redist * redistricted[i] + b3_contested_last * (1 - contested_last[i])) * last_pres_r2p[i]) +
      alpha_t2p_pred[i] * (a_t2p + b1_t2p * midterm[i] + b2_t2p * r_primary[i] + b3_t2p * r_primary[i] * midterm[i]);
    r2p_pred[i] = normal_rng(mu_pred[i], sigma);
  }
  
  // OOT (2026 data)
  for (i in 1:N_oot) {
    alpha_t2p_pred_oot[i] = alpha_1 * has_t2p_oot[i];
    mu_pred_oot[i] = (1 - alpha_t2p_pred_oot[i]) * (a_inc[incumbent_running_oot[i]] + a_midterm * midterm_oot[i] +
      (b1_inc[incumbent_running_oot[i]] + b1_midterm * midterm_oot[i] + b1_redist * redistricted_oot[i]) * last_r2p_oot[i] * contested_last_oot[i] +
      (b2_inc[incumbent_running_oot[i]] + b2_midterm * midterm_oot[i] + b2_redist * redistricted_oot[i]) * natl_r2p_change_oot[i] +
      (b3_inc[incumbent_running_oot[i]] + b3_midterm * midterm_oot[i] + b3_redist * redistricted_oot[i] + b3_contested_last * (1 - contested_last_oot[i])) * last_pres_r2p_oot[i]) +
      alpha_t2p_pred_oot[i] * (a_t2p + b1_t2p * midterm_oot[i] + b2_t2p * r_primary_oot[i] + b3_t2p * r_primary_oot[i] * midterm_oot[i]);
    r2p_pred_oot[i] = normal_rng(mu_pred_oot[i], sigma);
  }
}
