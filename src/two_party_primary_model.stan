// Data
data {
  int<lower=0> N;
  vector[N] r_primary;
  vector[N] r_general;
  array[N] int midterm;
  int<lower=0> N_oot;
  vector[N_oot] r_primary_oot;
  array[N_oot] int midterm_oot;
  real pre_election_sigma;
}

// Parameters
parameters {
  real<lower=0> sigma;
  real a;
  real b1;
  real b2;
  real b3;
}

// Model
model {
  vector[N] mu;
  a ~ normal(0, 0.05);
  b1 ~ normal(0, 0.05);
  b2 ~ normal(0.95, 0.05);
  b3 ~ normal(0.05, 0.05);
  
  for (i in 1:N) {
    mu[i] = a + b1 * midterm[i] + b2 * r_primary[i] + b3 * r_primary[i] * midterm[i];
  }
  
  sigma ~ exponential(0.05);
  r_general ~ normal(mu, sigma);
}

// In-sample predictions
generated quantities {
  vector[N] mu_rep;
  vector[N] r_general_pred;
  vector[N_oot] mu_oot;
  vector[N_oot] r_general_oot;
  vector[N_oot] r_primary_oot_sim;
  
  for (i in 1:N) {
    mu_rep[i] = a + b1 * midterm[i] + b2 * r_primary[i] + b3 * r_primary[i] * midterm[i];
    r_general_pred[i] = normal_rng(mu_rep[i], sigma);
  }
  
  for (i in 1:N_oot) {
    r_primary_oot_sim[i] = normal_rng(r_primary_oot[i], pre_election_sigma);
    mu_oot[i] = a + b1 * midterm_oot[i] + b2 * r_primary_oot_sim[i] + b3 * r_primary_oot_sim[i] * midterm_oot[i];
    r_general_oot[i] = normal_rng(mu_oot[i], sigma);
  }
}

