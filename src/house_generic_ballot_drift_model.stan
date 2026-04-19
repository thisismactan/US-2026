// Data
data {
  int<lower=0> N;
  vector[N] days_to_election;
  vector[N] days_gt_15;
  vector[N] days_gt_30;
  vector[N] days_gt_240;
  vector[N] drift;
}

// Parameters
parameters {
  real b0_drift;
  real<lower=0> b1_drift;
  real<lower=0> b2_drift;
  real<lower=0> b3_drift;
}

// Model
model {
  vector[N] log_drift_sigma;
  b0_drift ~ normal(0, 0.01);
  b1_drift ~ exponential(0.01);
  b2_drift ~ exponential(0.01);
  b3_drift ~ exponential(0.01);
  
  for (i in 1:N) {
    log_drift_sigma[i] = b0_drift + b1_drift * days_gt_15[i] + b2_drift * days_gt_30[i] + b3_drift * days_gt_240[i];
  }
  
  drift ~ normal(0, exp(log_drift_sigma));
}

// Generated quantities
generated quantities {
  vector[N] drift_sigma_pred;
  vector[N] drift_pred;
  
  for(i in 1:N) {
    drift_sigma_pred[i] = exp(b0_drift + b1_drift * days_gt_15[i] + b2_drift * days_gt_30[i] + b3_drift * days_gt_240[i]);
    drift_pred[i] = normal_rng(0, drift_sigma_pred[i]);
  }
}

