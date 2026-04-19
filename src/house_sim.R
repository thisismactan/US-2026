#source("src/process_data.R")
source("src/process_polls.R")

set.seed(2026)

# Simulate polling average variation from now to Election Day
## Fit the model if need be
if(!exists("polling_ar_posterior_coefs")) {
  historical_r2p_averages_smoothed_delta <- historical_generic_ballot_averages_smoothed %>%
    filter(year != 2018) %>%
    group_by(year) %>%
    mutate(delta = avg - lag(avg),
           delta_t1 = lag(delta),
           delta_t2 = lag(delta, 2),
           delta_t3 = lag(delta, 3),
           delta_t4 = lag(delta, 4),
           delta_t5 = lag(delta, 5)) %>%
    ungroup() %>%
    na.omit()
  
  polling_ar_data <- list(
    N = nrow(historical_r2p_averages_smoothed_delta),
    period = case_when(historical_r2p_averages_smoothed_delta$days_to_election >= 225 ~ 1, 
                       historical_r2p_averages_smoothed_delta$days_to_election >= 100 ~ 2,
                       historical_r2p_averages_smoothed_delta$days_to_election >= 60 ~ 3,
                       historical_r2p_averages_smoothed_delta$days_to_election >= 0 ~ 4),
    delta = historical_r2p_averages_smoothed_delta$delta,
    delta_t1 = historical_r2p_averages_smoothed_delta$delta_t1,
    delta_t2 = historical_r2p_averages_smoothed_delta$delta_t2,
    delta_t3 = historical_r2p_averages_smoothed_delta$delta_t3,
    delta_t4 = historical_r2p_averages_smoothed_delta$delta_t4,
    delta_t5 = historical_r2p_averages_smoothed_delta$delta_t5
  )
  
  polling_ar_model <- cmdstan_model("src/poll_average_ar_model.stan")
  polling_ar_model_fit <- polling_ar_model$sample(
    data = polling_ar_data, seed = 2026, chains = 4, iter_warmup = 500, iter_sampling = 2500, 
    parallel_chains = 4, refresh = 500
  )
  polling_ar_model_posterior <- as_tibble(polling_ar_model_fit$draws(format = "df"))
  polling_ar_posterior_coefs <- polling_ar_model_posterior[, c("b0", "b1", "b2", "b3", "b4", "b5")] %>% as.matrix()
  polling_ar_posterior_sigmas <- list(
    period1 = polling_ar_model_posterior$`sigma_period[1]`,
    period2 = polling_ar_model_posterior$`sigma_period[2]`,
    period3 = polling_ar_model_posterior$`sigma_period[3]`,
    period4 = polling_ar_model_posterior$`sigma_period[4]`
  )
}

## Now get the posterior distribution on total generic ballot drift
r2p_average_2026_changes_today <- generic_ballot_averages_2026_smoothed %>%
  mutate(delta = avg - lag(avg),
         delta_t1 = lag(delta),
         delta_t2 = lag(delta, 2),
         delta_t3 = lag(delta, 3),
         delta_t4 = lag(delta, 4),
         delta_t5 = lag(delta, 5)) %>%
  tail(1)

n_sims <- nrow(polling_ar_posterior_coefs)
today_matrix <- matrix(r2p_average_2026_changes_today$delta, nrow = n_sims, ncol = 1)
lag_matrix <- matrix(c(1, r2p_average_2026_changes_today$delta_t1, r2p_average_2026_changes_today$delta_t2,
                       r2p_average_2026_changes_today$delta_t3, r2p_average_2026_changes_today$delta_t4,
                       r2p_average_2026_changes_today$delta_t5), 
                     nrow = n_sims, ncol = 6, byrow = TRUE)
colnames(today_matrix) <- "delta"
colnames(lag_matrix) <- c("int", "delta_t1", "delta_t2", "delta_t3", "delta_t4", "delta_t5")
n_days_to_election <- r2p_average_2026_changes_today$days_to_election
change_matrix <- matrix(0, n_sims, n_days_to_election)

for(i in 1:n_days_to_election) {
  election_period <- case_when(n_days_to_election - i >= 225 ~ 1,
                               n_days_to_election - i >= 100 ~ 2,
                               n_days_to_election - i >= 60 ~ 3,
                               n_days_to_election - i >= 0 ~ 4)
  change_matrix[, i] <- today_matrix
  tomorrow_draws <- rowSums(polling_ar_posterior_coefs * lag_matrix) + 
    rnorm(n_sims, 0, polling_ar_posterior_sigmas[[election_period]])
  lag_matrix <- cbind(1, today_matrix, lag_matrix[, 2:5])
  today_matrix <- matrix(tomorrow_draws, nrow = n_sims, ncol = 1)
}

generic_ballot_drift_sims <- rowSums(change_matrix)

## Approximate Election Day generic ballot distribution as normal distribution
generic_ballot_drift_mean <- mean(generic_ballot_drift_sims)
generic_ballot_mean <- (generic_ballot_averages_2026_smoothed %>% tail(1) %>% pull(avg)) + 
  generic_ballot_drift_mean
generic_ballot_var <- (generic_ballot_averages_2026_smoothed %>% tail(1) %>% pull(se))^2 +
  var(generic_ballot_drift_sims)

natl_r2p_var_data <- list(
  N = nrow(historical_generic_ballot_averages_smoothed %>% filter(days_to_election == 0)),
  avg = historical_generic_ballot_averages_smoothed %>% filter(days_to_election == 0) %>% pull(avg),
  natl_r2p = historical_generic_ballot_averages_smoothed %>% filter(days_to_election == 0) %>% pull(natl_r2p)
)

natl_r2p_var_model <- cmdstan_model("src/natl_r2p_distribution_from_generic_ballot.stan")
natl_r2p_var_model_fit <- natl_r2p_var_model$sample(
  data = natl_r2p_var_data, seed = 2026, chains = 4, iter_warmup = 500, iter_sampling = 2500, 
  parallel_chains = 4, refresh = 500
)

natl_r2p_change_oot_var <- (natl_r2p_var_model_fit$draws(format = "df")$sigma %>% mean())^2 + generic_ballot_var
natl_r2p_change_oot_mean <- generic_ballot_mean - (house_natl_r2p %>% filter(year == 2024) %>% pull(natl_r2p))

# National swing model
house_model_data <- list(
  # Historical data
  r2p = house_election_data %>% pull(r2p),
  last_r2p = house_election_data %>% pull(last_r2p),
  midterm = as.numeric((house_election_data %>% pull(year) %% 4) == 2),
  redistricted = as.numeric(house_election_data %>% pull(redistricted)),
  incumbent_running = case_when(house_election_data$incumbent_running == "Both (redistricting)" ~ 1,
                                house_election_data$incumbent_running == "DEM" ~ 2,
                                house_election_data$incumbent_running == "None" ~ 3,
                                house_election_data$incumbent_running == "REP" ~ 4),
  natl_r2p_change = house_election_data %>% pull(natl_r2p_change),
  last_pres_r2p = house_election_data %>% pull(last_pres_r2p),
  contested_last = as.numeric(house_election_data %>% pull(contested_last)),
  N_e = nrow(house_election_data),
  # 2026 data
  last_r2p_oot = house_2026_data %>% pull(last_r2p),
  midterm_oot = as.numeric((house_2026_data %>% pull(year) %% 4) == 2),
  redistricted_oot = as.numeric(house_2026_data %>% pull(redistricted)),
  natl_r2p_change_oot_mean = natl_r2p_change_oot_mean,
  natl_r2p_change_oot_sd = sqrt(natl_r2p_change_oot_var),
  incumbent_running_oot = case_when(house_2026_data$incumbent_running == "Both (redistricting)" ~ 1,
                                    house_2026_data$incumbent_running == "DEM" ~ 2,
                                    house_2026_data$incumbent_running == "None" ~ 3,
                                    house_2026_data$incumbent_running == "REP" ~ 4),
  last_pres_r2p_oot = house_2026_data %>% pull(last_pres_r2p),
  contested_last_oot = as.numeric(house_2026_data %>% pull(contested_last)),
  N_oot = nrow(house_2026_data)
)

house_model <- cmdstan_model("src/house_model_sim.stan")
house_model_fit <- house_model$sample(
  data = house_model_data, seed = 2026, chains = 4, iter_warmup = 500, iter_sampling = 2500, 
  parallel_chains = 4, refresh = 500
)
print(house_model_fit, max_rows = 45)
house_posterior <- as_tibble(house_model_fit$draws(format = "df"))
n_sims <- nrow(house_posterior)

# Incorporating top-two primaries where applicable
top2_primary_results_2026 <- read_csv("data/top_two_primary_results_2026.csv") %>%
  mutate(r_primary = rep_primary / (rep_primary + dem_primary))

top2_primary_data <- list(
  # Dev
  N = nrow(historical_top_two_primary),
  r_primary = historical_top_two_primary$primary_pct,
  r_general = historical_top_two_primary$general_pct,
  midterm = historical_top_two_primary$midterm,
  # OOT
  N_oot = nrow(top2_primary_results_2026),
  r_primary_oot = top2_primary_results_2026$r_primary,
  midterm_oot = rep(TRUE, nrow(top2_primary_results_2026)),
  # Extra variance prior to actual results coming in
  pre_election_sigma = 0.1
)

top2_primary_model <- cmdstan_model("src/two_party_primary_model.stan")
top2_primary_model_fit <- top2_primary_model$sample(
  data = top2_primary_data, seed = 2026, chains = 4, iter_warmup = 500, iter_sampling = 2500, 
  parallel_chains = 4, refresh = 500
)
print(top2_primary_model_fit, max_rows = 6)
top2_primary_posterior <- top2_primary_model_fit$draws(format = "df") %>%
  select(starts_with("r_general_oot")) %>%
  as.matrix() %>%
  t() %>%
  as.data.frame() %>%
  mutate(state = top2_primary_results_2026$state,
         seat_number = top2_primary_results_2026$seat_number) %>%
  melt(id.vars = c("state", "seat_number"), variable.name = "sim_id", value.name = "r2p_top2_pred") %>%
  mutate(sim_id = as.numeric(sim_id)) %>%
  as_tibble() %>%
  group_by(state, seat_number) %>%
  mutate(r2p_top2_weight = 1 / var(r2p_top2_pred)) %>%
  ungroup()
  
# Grab the posterior draws
## National Republican 2-party vote share (needed to work out where the district-level polls say things should be)
district_poll_error_sd <- 0.05

house_district_level_poll_posterior <- tibble(
  sim_id = 1:n_sims,
  natl_r2p = house_posterior$natl_r2p_change_oot + (house_natl_r2p %>% tail(1) %>% pull(natl_r2p))
) %>%
  left_join(house_district_average_leans_2026[rep(seq_len(nrow(house_district_average_leans_2026)), each = n_sims),] %>%
              mutate(sim_id = rep(1:n_sims, n() / n_sims)),
            by = "sim_id") %>%
  mutate(r2p_district_poll_pred = natl_r2p + rnorm(n(), avg_lean, sqrt(se^2 + district_poll_error_sd^2))) %>%
  group_by(state, seat_number) %>%
  mutate(r2p_district_poll_weight = 1 / var(r2p_district_poll_pred))

## District-level predictions from swing model
house_district_posterior <- house_posterior %>%
  select(starts_with("r2p_pred_oot")) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(house_2026_data) %>%
  melt(measure.vars = paste0("V", as.character(1:n_sims)), variable.name = "sim_id", value.name = "r2p_swing_pred") %>%
  as_tibble() %>%
  mutate(sim_id = as.integer(sim_id)) %>%
  left_join(tibble(sim_id = 1:n_sims, natl_r2p_change_oot = house_posterior$natl_r2p_change_oot), by = "sim_id") %>%
  mutate(natl_r2p = last_natl_r2p + natl_r2p_change_oot) %>%
  select(sim_id, natl_r2p, natl_r2p_change_oot, everything()) %>%
  group_by(state, seat_number) %>%
  mutate(r2p_swing_weight = 1 / var(r2p_swing_pred)) %>%
  ungroup() %>%
  left_join(house_district_level_poll_posterior, by = c("state", "seat_number", "sim_id", "natl_r2p")) %>%
  left_join(top2_primary_posterior, by = c("state", "seat_number", "sim_id")) %>%
  mutate(r2p_district_poll_pred = ifelse(is.na(r2p_district_poll_pred), 0, r2p_district_poll_pred),
         r2p_district_poll_weight = ifelse(is.na(r2p_district_poll_weight), 0, r2p_district_poll_weight),
         r2p_top2_pred = ifelse(is.na(r2p_top2_pred), 0, r2p_top2_pred),
         r2p_top2_weight = 0,
         r2p_pred = (r2p_swing_weight * r2p_swing_pred + r2p_district_poll_weight * r2p_district_poll_pred + r2p_top2_weight * r2p_top2_pred) /
           (r2p_swing_weight + r2p_district_poll_weight + r2p_top2_weight)) %>%
  # One last thing: for uncontested districts, set Republican share to appropriate value
  mutate(r2p_pred = case_when(!contested_rep ~ 0,
                              !contested_dem ~ 1,
                              TRUE ~ r2p_pred))

house_district_posterior_summary_stats <- house_district_posterior %>%
  group_by(year, state, seat_number) %>%
  summarise(r_prob = mean(r2p_pred >= 0.5),
            pct_05 = quantile(r2p_pred, 0.05),
            avg = mean(r2p_pred),
            pct_95 = quantile(r2p_pred, 0.95))

house_district_posterior_summary_stats

# If all districts went to their most likely winner
house_district_posterior_summary_stats %>% 
  left_join(house_district_r2p %>% filter(year == 2024) %>% select(state, seat_number, r2p), 
            by = c("state", "seat_number")) %>% 
  mutate(result = case_when((r2p > 0.5) & (r_prob > 0.5) ~ "R hold", 
                            (r2p < 0.5) & (r_prob < 0.5) ~ "D hold", 
                            (r2p > 0.5) & (r_prob < 0.5) ~ "D from R",
                            (r2p < 0.5) & (r_prob > 0.5) ~ "R from D")) %>%
  group_by(result) %>% 
  summarise(n = n())

# Overall forecast
house_district_posterior %>%
  group_by(sim_id) %>%
  summarise(r_seats = sum(r2p_pred >= 0.5)) %>%
  summarise(r_prob = mean(r_seats >= 218),
            pct_05 = quantile(r_seats, 0.05),
            avg = mean(r_seats),
            pct_95 = quantile(r_seats, 0.95))

house_seat_sims <- house_district_posterior %>%
  group_by(sim_id) %>%
  summarise(rep = sum(r2p_pred > 0.5),
            dem = sum(r2p_pred <= 0.5))
