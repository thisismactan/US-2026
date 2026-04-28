source("src/house_sim.R")

# Simulated Senate polling averages accounting for generic ballot drift
## Grab the simulated national House popular votes
natl_r2p_sims <- tibble(
  sim_id = 1:n_sims,
  natl_r2p = (house_natl_r2p %>% tail(1) %>% pull(natl_r2p)) + house_posterior$natl_r2p_change_oot
)

# Polling averages model
## Historical data
senate_r2p_var_data <- list(
  N = nrow(senate_polling_averages %>% filter(avg < 1, actual_r2p < 1, days_to_election == 0)),
  state = senate_polling_averages %>% filter(avg < 1, actual_r2p < 1, days_to_election == 0) %>% pull(state) %>% state_int_encoder(),
  avg = senate_polling_averages %>% filter(avg < 1, actual_r2p < 1, days_to_election == 0) %>% pull(avg),
  sen_r2p = senate_polling_averages %>% filter(avg < 1, actual_r2p < 1, days_to_election == 0) %>% pull(actual_r2p)
)

senate_r2p_var_model <- cmdstan_model("src/senate_r2p_distribution_from_poll_average.stan")
senate_r2p_var_model_fit <- senate_r2p_var_model$sample(
  data = senate_r2p_var_data, seed = 2026, chains = 4, iter_warmup = 500, iter_sampling = 2500, 
  parallel_chains = 4, refresh = 500
)

print(senate_r2p_var_model_fit, max_rows = 54)

senate_r2p_var_posterior <- as_tibble(senate_r2p_var_model_fit$draws(format = "df"))
state_alpha_sims <- senate_r2p_var_posterior %>%
  select(starts_with("state_alpha")) %>%
  rename_all(function(x) str_remove_all(x, "[state_alpha]|[[:punct:]]")) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "state_code", value.name = "alpha") %>%
  mutate(state_code = as.character(state_code) %>% as.numeric()) %>%
  left_join(
    tibble(state = senate_election_data %>% pull(state) %>% unique()) %>%
      mutate(state_code = state_int_encoder(state)),
    by = "state_code"
  ) %>%
  as_tibble()

senate_param_sims <- senate_r2p_var_posterior %>%
  select(b, sigma) %>%
  mutate(sim_id = 1:n())

senate_poll_sims <- expand.grid(
  sim_id = 1:n_sims,
  state = senate_average_leans_2026$state
) %>%
  left_join(senate_average_leans_2026 %>% select(state, seat_name, avg_lean, se) %>% na.omit(), by = "state") %>%
  left_join(natl_r2p_sims, by = "sim_id") %>%
  as_tibble() %>%
  mutate(lean = rnorm(n(), avg_lean, se),
         poll_r2p = natl_r2p + lean)

senate_poll_posterior <- expand.grid(
  sim_id = 1:n_sims,
  state = senate_2026_data$state
) %>%
  left_join(senate_2026_data %>% select(state, seat_name), by = "state") %>%
  as_tibble() %>%
  left_join(state_alpha_sims %>% select(sim_id, state, alpha), by = c("sim_id", "state")) %>%
  left_join(senate_param_sims, by = "sim_id") %>%
  left_join(senate_poll_sims, by = c("sim_id", "state", "seat_name")) %>%
  mutate(sim_r2p = poll_r2p + rnorm(n(), 0, sigma)) %>%
  group_by(state, seat_name) %>%
  mutate(poll_weight = 1 / var(sim_r2p))

senate_poll_sim_ids <- expand.grid(
  sim_id = 1:n_sims,
  state = senate_2026_data$state
) %>%
  left_join(senate_2026_data %>% select(state, seat_name), by = "state") %>%
  as_tibble()

## Do the conditionals
r2p_margin_diffs <- seq(-0.1, 0.1, by = 0.01)
senate_poll_conditional_posterior <- senate_poll_sim_ids[rep(seq_len(nrow(senate_poll_sim_ids)), 
                                                             each = length(r2p_margin_diffs)), ] %>%
  mutate(margin_diff = rep(rep(r2p_margin_diffs, n_sims), length(senate_2026_data$state))) %>%
  left_join(state_alpha_sims %>% select(sim_id, state, alpha), by = c("sim_id", "state")) %>%
  left_join(senate_param_sims, by = "sim_id") %>%
  left_join(senate_average_leans_2026, by = c("state", "seat_name")) %>%
  mutate(sim_r2p = generic_ballot_mean + avg_lean + margin_diff / 2 + rnorm(n(), 0, sigma)) %>%
  group_by(state, seat_name) %>%
  mutate(poll_weight = 1 / var(sim_r2p))

senate_2026_conditional_data <- senate_2026_data[rep(seq_len(nrow(senate_2026_data)), each = length(r2p_margin_diffs)), ] %>%
  mutate(natl_r2p_change = rep(r2p_margin_diffs, length(senate_2026_data$state)) / 2 + generic_ballot_mean - (house_natl_r2p %>% tail(1) %>% pull(natl_r2p)))

# National swing model
senate_swing_model_data <- list(
  # Historical data
  r2p = senate_election_data %>% pull(r2p),
  last_r2p = senate_election_data %>% pull(last_r2p),
  midterm = as.numeric(senate_election_data %>% pull(midterm)),
  incumbent_running = senate_election_data %>% pull(incumbent_running),
  region = case_when(senate_election_data$region == "Deep South" ~ 1,
                     senate_election_data$region == "Great Lakes" ~ 2,
                     senate_election_data$region == "Great Plains" ~ 3,
                     senate_election_data$region == "Mid-Atlantic" ~ 4,
                     senate_election_data$region == "Mountain West" ~ 5,
                     senate_election_data$region == "New England" ~ 6,
                     senate_election_data$region == "Pacific Coast" ~ 7,
                     senate_election_data$region == "South Atlantic" ~ 8,
                     senate_election_data$region == "Southwest" ~ 9,
                     senate_election_data$region == "Upper South" ~ 10),
  natl_r2p_change = senate_election_data %>% pull(natl_r2p_change),
  last_pres_r2p = senate_election_data %>% pull(last_pres_r2p),
  contested_last = as.numeric(senate_election_data %>% pull(contested_last)),
  N_e = nrow(senate_election_data),
  # 2026 data
  last_r2p_oot = senate_2026_data %>% pull(last_r2p),
  midterm_oot = as.numeric(senate_2026_data %>% pull(midterm)),
  natl_r2p_oot_mean = house_natl_r2p %>% tail(1) %>% pull(natl_r2p) + house_model_data$natl_r2p_change_oot_mean,
  natl_r2p_oot_sd = sqrt(natl_r2p_change_oot_var),
  natl_r2p_last_oot = senate_2026_data %>% pull(last_natl_r2p),
  incumbent_running_oot = senate_2026_data %>% pull(incumbent_running),
  region_oot = case_when(senate_2026_data$region == "Deep South" ~ 1,
                         senate_2026_data$region == "Great Lakes" ~ 2,
                         senate_2026_data$region == "Great Plains" ~ 3,
                         senate_2026_data$region == "Mid-Atlantic" ~ 4,
                         senate_2026_data$region == "Mountain West" ~ 5,
                         senate_2026_data$region == "New England" ~ 6,
                         senate_2026_data$region == "Pacific Coast" ~ 7,
                         senate_2026_data$region == "South Atlantic" ~ 8,
                         senate_2026_data$region == "Southwest" ~ 9,
                         senate_2026_data$region == "Upper South" ~ 10),
  last_pres_r2p_oot = senate_2026_data %>% pull(last_pres_r2p),
  contested_last_oot = as.numeric(senate_2026_data %>% pull(contested_last)),
  N_oot = nrow(senate_2026_data),
  # Conditional data
  last_r2p_cond = senate_2026_conditional_data %>% pull(last_r2p),
  midterm_cond = as.numeric(senate_2026_conditional_data %>% pull(midterm)),
  natl_r2p_change_cond = senate_2026_conditional_data$natl_r2p_change,
  natl_r2p_last_cond = senate_2026_conditional_data %>% pull(last_natl_r2p),
  incumbent_running_cond = senate_2026_conditional_data %>% pull(incumbent_running),
  region_cond = case_when(senate_2026_conditional_data$region == "Deep South" ~ 1,
                          senate_2026_conditional_data$region == "Great Lakes" ~ 2,
                          senate_2026_conditional_data$region == "Great Plains" ~ 3,
                          senate_2026_conditional_data$region == "Mid-Atlantic" ~ 4,
                          senate_2026_conditional_data$region == "Mountain West" ~ 5,
                          senate_2026_conditional_data$region == "New England" ~ 6,
                          senate_2026_conditional_data$region == "Pacific Coast" ~ 7,
                          senate_2026_conditional_data$region == "South Atlantic" ~ 8,
                          senate_2026_conditional_data$region == "Southwest" ~ 9,
                          senate_2026_conditional_data$region == "Upper South" ~ 10),
  last_pres_r2p_cond = senate_2026_conditional_data %>% pull(last_pres_r2p),
  contested_last_cond = as.numeric(senate_2026_conditional_data %>% pull(contested_last)),
  N_cond = nrow(senate_2026_conditional_data)
)

senate_swing_model <- cmdstan_model("src/senate_model_sim.stan")
senate_swing_model_fit <- senate_swing_model$sample(
  data = senate_swing_model_data, seed = 2026, chains = 4, iter_warmup = 500, iter_sampling = 2500, 
  parallel_chains = 4, refresh = 500
)
print(senate_swing_model_fit, max_rows = 50)
senate_swing_posterior <- as_tibble(senate_swing_model_fit$draws(format = "df"))
n_sims <- nrow(senate_swing_posterior)

senate_state_swing_posterior <- senate_swing_posterior %>%
  select(starts_with("r2p_pred_oot")) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(senate_2026_data) %>%
  melt(measure.vars = paste0("V", as.character(1:n_sims)), variable.name = "sim_id", value.name = "r2p_pred") %>%
  as_tibble() %>%
  mutate(sim_id = as.integer(sim_id)) %>%
  left_join(tibble(sim_id = 1:n_sims, natl_r2p = senate_swing_posterior$natl_r2p_oot), by = "sim_id") %>%
  select(sim_id, natl_r2p, everything()) %>%
  group_by(state, seat_name) %>%
  mutate(swing_weight = 1 / var(r2p_pred)) %>%
  ungroup()

senate_state_posterior <- senate_state_swing_posterior %>%
  select(state, seat_name, sim_id, swing_r2p = r2p_pred, swing_weight) %>%
  left_join(senate_poll_posterior %>% select(sim_id, state, seat_name, poll_r2p = sim_r2p, poll_weight), 
            by = c("state", "seat_name", "sim_id")) %>%
  mutate(poll_r2p = ifelse(is.na(poll_r2p), 0, poll_r2p),
         poll_weight = ifelse(is.na(poll_weight), 0, poll_weight)) %>%
  mutate(r2p = (swing_r2p * swing_weight + poll_r2p * poll_weight) / (swing_weight + poll_weight))

## Do the conditionals
senate_conditional_posteriors <- senate_swing_posterior %>%
  select(starts_with("r2p_pred_cond")) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(senate_2026_conditional_data) %>%
  melt(measure.vars = paste0("V", as.character(1:n_sims)), variable.name = "sim_id", value.name = "r2p_swing_pred") %>%
  as_tibble() %>%
  mutate(sim_id = as.integer(sim_id),
         margin_diff = rep(rep(r2p_margin_diffs, length(senate_2026_data$state)), n_sims)) %>%
  select(state, seat_name, sim_id, margin_diff, r2p_swing_pred) %>%
  group_by(state, seat_name, margin_diff) %>%
  mutate(r2p_swing_weight = 1 / var(r2p_swing_pred)) %>%
  ungroup() %>%
  left_join(senate_poll_conditional_posterior, by = c("state", "seat_name", "sim_id", "margin_diff")) %>%
  select(state, seat_name, sim_id, margin_diff, r2p_swing_pred, r2p_swing_weight, r2p_poll_pred = sim_r2p, 
         r2p_poll_weight = poll_weight) %>%
  mutate(r2p_poll_pred = ifelse(is.na(r2p_poll_pred), 0, r2p_poll_pred),
         r2p_poll_weight = ifelse(is.na(r2p_poll_weight), 0, r2p_poll_weight),
         r2p_pred = (r2p_swing_weight * r2p_swing_pred + r2p_poll_weight * r2p_poll_pred) /
           (r2p_swing_weight + r2p_poll_weight)) %>%
  mutate(r2p_pred = case_when(state == "New Mexico" ~ 0,
                              TRUE ~ r2p_pred)) %>%
  group_by(sim_id, margin_diff) %>%
  summarise(rep = sum(r2p_pred > 0.5) + 31,
            dem = sum((r2p_pred <= 0.5) & state != "Nebraska") + 34,
            ind = sum((r2p_pred <= 0.5) & state == "Nebraska")) %>%
  ungroup() %>%
  melt(id.vars = c("sim_id", "margin_diff"), variable.name = "party", value.name = "seats") %>%
  group_by(party, margin_diff, seats) %>%
  summarise(prob = n() / n_sims) %>%
  ungroup() %>%
  arrange(party, margin_diff)

senate_conditional_posteriors %>%
  write_csv("output/senate_seats_conditional_posteriors.csv")

senate_state_posterior_summary_stats <- senate_state_posterior %>%
  group_by(state, seat_name) %>%
  summarise(r_prob = mean(r2p > 0.5),
            pct_05 = quantile(r2p, 0.05),
            avg = mean(r2p),
            pct_95 = quantile(r2p, 0.95))

senate_seat_sims <- senate_state_posterior %>%
  group_by(sim_id) %>%
  summarise(rep = sum(r2p > 0.5) + 31,
            dem = sum((r2p <= 0.5) & state != "Nebraska") + 34,
            ind = sum((r2p <= 0.5) & state == "Nebraska"))

senate_state_posterior %>%
  arrange(state, seat_name, sim_id) %>%
  select(state, seat_name, sim_id, r2p_pred = r2p) %>%
  write_csv("output/senate_state_posterior.csv")
