// Input data
data {
  // Dev data
  int<lower=0> N_e;
  vector[N_e] r2p;
  vector[N_e] last_r2p;
  vector[N_e] midterm;
  vector[N_e] redistricted;
  vector[N_e] natl_r2p_change;
  vector[N_e] last_pres_r2p;
  vector[N_e] contested_last;
  array[N_e] int incumbent_running;
  array[N_e] int region;
  // OOT data
  int<lower=0> N_oot;
  vector[N_oot] last_r2p_oot;
  vector[N_oot] midterm_oot;
  vector[N_oot] redistricted_oot;
  real natl_r2p_change_oot_mean;
  real natl_r2p_change_oot_sd;
  vector[N_oot] last_pres_r2p_oot;
  vector[N_oot] contested_last_oot;
  array[N_oot] int incumbent_running_oot;
  array[N_oot] int region_oot;
  // Conditional data
  int<lower=0> N_cond;
  vector[N_cond] last_r2p_cond;
  vector[N_cond] midterm_cond;;
  vector[N_cond] redistricted_cond;;
  vector[N_cond] natl_r2p_change_cond;;
  vector[N_cond] last_pres_r2p_cond;;
  vector[N_cond] contested_last_cond;;
  array[N_cond] int incumbent_running_cond;;
  array[N_cond] int region_cond;
}

// Parameters
parameters {
  vector[4] a_inc;
  vector[10] a_region;
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
  real<lower=0> sigma;
  real natl_r2p_change_oot;
}

// Model
model {
  vector[N_e] mu;
  a_midterm ~ normal(0, 0.1);
  a_region ~ normal(0, 0.05);
  b1_midterm ~ normal(0, 0.5);
  b1_redist ~ normal(-1, 0.5);
  b2_midterm ~ normal(0, 0.5);
  b2_redist ~ normal(-1, 0.5);
  b3_midterm ~ normal(0, 0.5);
  b3_redist ~ normal(1, 0.5);
  b3_contested_last ~ normal(1, 0.5);
  Rho ~ lkj_corr(1);
  tau ~ exponential(0.5);
  sigma ~ exponential(0.1);
  array[4] vector[4] inc_coefs;
  for (j in 1:4) {
    inc_coefs[j] = [a_inc[j], b1_inc[j], b2_inc[j], b3_inc[j]]';
  }
  inc_coefs ~ multi_normal(rep_vector(0, 4), quad_form_diag(Rho, tau));
  natl_r2p_change_oot ~ normal(natl_r2p_change_oot_mean, natl_r2p_change_oot_sd);
  
  for (i in 1:N_e) {
    mu[i] = a_inc[incumbent_running[i]] + a_region[region[i]] + a_midterm * midterm[i] +
      (b1_inc[incumbent_running[i]] + b1_midterm * midterm[i] + b1_redist * redistricted[i]) * last_r2p[i] * contested_last[i] +
      (b2_inc[incumbent_running[i]] + b2_midterm * midterm[i] + b2_redist * redistricted[i]) * natl_r2p_change[i] +
      (b3_inc[incumbent_running[i]] + b3_midterm * midterm[i] + b3_redist * redistricted[i] + b3_contested_last * (1 - contested_last[i])) * last_pres_r2p[i];
  }
  
  r2p ~ normal(mu, sigma);
}

// Generated quantities
generated quantities {
  vector[N_e] mu_pred;
  vector[N_e] r2p_pred;
  vector[N_oot] mu_oot;
  vector[N_oot] r2p_pred_oot;
  vector[N_cond] mu_cond;
  vector[N_cond] r2p_pred_cond;
  // In-sample
  for (i in 1:N_e) {
    mu_pred[i] = a_inc[incumbent_running[i]] + a_region[region[i]] + a_midterm * midterm[i] +
      (b1_inc[incumbent_running[i]] + b1_midterm * midterm[i] + b1_redist * redistricted[i]) * last_r2p[i] * contested_last[i] +
      (b2_inc[incumbent_running[i]] + b2_midterm * midterm[i] + b2_redist * redistricted[i]) * natl_r2p_change[i] +
      (b3_inc[incumbent_running[i]] + b3_midterm * midterm[i] + b3_redist * redistricted[i] + b3_contested_last * (1 - contested_last[i])) * last_pres_r2p[i];
    r2p_pred[i] = normal_rng(mu_pred[i], sigma);
  }
  // OOT
  for (i in 1:N_oot) {
    mu_oot[i] = a_inc[incumbent_running_oot[i]] + a_region[region_oot[i]] + a_midterm * midterm_oot[i] +
      (b1_inc[incumbent_running_oot[i]] + b1_midterm * midterm_oot[i] + b1_redist * redistricted_oot[i]) * last_r2p_oot[i] * contested_last_oot[i] +
      (b2_inc[incumbent_running_oot[i]] + b2_midterm * midterm_oot[i] + b2_redist * redistricted_oot[i]) * natl_r2p_change_oot +
      (b3_inc[incumbent_running_oot[i]] + b3_midterm * midterm_oot[i] + b3_redist * redistricted_oot[i] + b3_contested_last * (1 - contested_last_oot[i])) * last_pres_r2p_oot[i];
    r2p_pred_oot[i] = normal_rng(mu_oot[i], sigma);
  }
  // Conditional distributions
  for (i in 1:N_cond) {
    mu_cond[i] = a_inc[incumbent_running_cond[i]] + a_region[region_cond[i]] + a_midterm * midterm_cond[i] +
      (b1_inc[incumbent_running_cond[i]] + b1_midterm * midterm_cond[i] + b1_redist * redistricted_cond[i]) * last_r2p_cond[i] * contested_last_cond[i] +
      (b2_inc[incumbent_running_cond[i]] + b2_midterm * midterm_cond[i] + b2_redist * redistricted_cond[i]) * natl_r2p_change_cond[i] +
      (b3_inc[incumbent_running_cond[i]] + b3_midterm * midterm_cond[i] + b3_redist * redistricted_cond[i] + b3_contested_last * (1 - contested_last_cond[i])) * last_pres_r2p_cond[i];
    r2p_pred_cond[i] = normal_rng(mu_cond[i], sigma);
  }
}
