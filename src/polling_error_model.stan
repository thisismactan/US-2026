data {
  vector[4818] poll_r2p;
  array[4818] int pollster;
  array[4818] int population;
  array[4818] int cycle;
}

parameters {
  vector[4] cycle_intercept;
  real poll_level_error;
  vector[126] house_effect;
  real<lower=0> error_poll_sigma;
  vector[4] p_cycle;
  vector[4] p_pop;
}

model {
  vector[4818] natl_r2p;
  vector[4818] lambda;
  p_pop ~ normal(0, 1);
  p_cycle ~ normal(0, 1);
  for (i in 1:4818) {
    lambda[i] = p_cycle[cycle[i]] + p_pop[population[i]];
    lambda[i] = exp(lambda[i]);
  }
  error_poll_sigma ~ exponential(lambda);
  house_effect ~ normal(0, 1);
  poll_level_error ~ normal(house_effect[pollster], error_poll_sigma);
  cycle_intercept ~ normal(0, 1);
  for (i in 1:4818) {
    natl_r2p[i] = poll_r2p[i] + cycle_intercept[cycle[i]] + poll_level_error;
  }
}


