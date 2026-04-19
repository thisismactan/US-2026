// Data
data {
  // Dev
  int<lower=0> N;
  array[N] int period;
  vector[N] delta;
  vector[N] delta_t1;
  vector[N] delta_t2;
  vector[N] delta_t3;
  vector[N] delta_t4;
  vector[N] delta_t5;
}

// Parameters
parameters {
  real b0;
  real b1;
  real b2;
  real b3;
  real b4;
  real b5;
  corr_matrix[6] Rho;
  vector<lower=0>[6] tau;
  vector<lower=0>[4] sigma_period;
}

// Model
model {
  Rho ~ lkj_corr(1);
  tau ~ exponential(0.5);
  vector[6] b;
  b = [b0, b1, b2, b3, b4, b5]';
  b ~ multi_normal(rep_vector(0, 6), quad_form_diag(Rho, tau));
  sigma_period ~ exponential(0.1);
  vector[N] mu;
  vector[N] sigma;
  for (i in 1:N) {
    mu[i] = b0 + b1 * delta_t1[i] + b2 * delta_t2[i] + b3 * delta_t3[i] + b4 * delta_t4[i] + b5 * delta_t5[i];
    sigma[i] = sigma_period[period[i]];
  }
  delta ~ normal(mu, sigma);
}

// Generated quantities
generated quantities {
  vector[N] mu_pred;
  vector[N] sigma_pred;
  vector[N] delta_pred;
  for (i in 1:N) {
    mu_pred[i] = b0 + b1 * delta_t1[i] + b2 * delta_t2[i] + b3 * delta_t3[i] + b4 * delta_t4[i] + b5 * delta_t5[i];
    sigma_pred[i] = sigma_period[period[i]];
    delta_pred[i] = normal_rng(mu_pred[i], sigma_pred[i]);
  }
}
