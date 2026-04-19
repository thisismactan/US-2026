// Data
data {
  int<lower=0> N;
  int<lower=0> N_e;
  vector[N] avg;
  vector[N] days_gt_30;
  vector[N] days_gt_60;
  vector[N] days_gt_120;
  vector[N] final_election_avg;
  vector[N_e] final_election_avg_red;
  vector[N_e] r2p;
}

// Parameters
parameters {
  real b0_drift;
  real<lower=0> b1_drift;
  real<lower=0> b2_drift;
  real<lower=0> b3_drift;
  real<lower=0> sigma;
}

// Model
model {
  vector[N] log_drift_sigma;
  b0_drift ~ normal(0, 0.01);
  b1_drift ~ exponential(0.01);
  b2_drift ~ exponential(0.01);
  b3_drift ~ exponential(0.01);
  
  for (i in 1:N) {
    log_drift_sigma[i] = b0_drift + b1_drift * days_gt_30[i] + b2_drift * days_gt_60[i] + b3_drift * days_gt_120[i];
  }
  
  sigma ~ exponential(0.05);
  final_election_avg ~ normal(avg, exp(log_drift_sigma));
  r2p ~ normal(final_election_avg_red, sigma);
}

// Generated quantities
generated quantities {
  vector[N] drift_sigma_pred;
  vector[N] final_election_avg_pred;
  
  for(i in 1:N) {
    drift_sigma_pred[i] = exp(b0_drift + b1_drift * days_gt_30[i] + b2_drift * days_gt_60[i] + b3_drift * days_gt_120[i]);
    final_election_avg_pred[i] = normal_rng(avg[i], drift_sigma_pred[i]);
  }
}

