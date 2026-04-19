source("src/process_data.R")

# House model
house_model_data <- list(
  # Dev data
  r2p = house_election_data$r2p,
  last_r2p = house_election_data$last_r2p,
  midterm = as.numeric((house_election_data$year %% 4) == 2),
  redistricted = as.numeric(house_election_data$redistricted),
  incumbent_running = as.factor(house_election_data$incumbent_running) %>% as.integer(),
  natl_r2p_change = house_election_data$natl_r2p_change,
  last_pres_r2p = house_election_data$last_pres_r2p,
  contested_last = as.numeric(house_election_data$contested_last),
  N_e = nrow(house_election_data)
)

house_model <- cmdstan_model("src/house_model.stan")
house_model_fit <- house_model$sample(data = house_model_data, seed = 2026, chains = 4, iter_warmup = 500,
                                      iter_sampling = 2500, parallel_chains = 4, refresh = 500)
print(house_model_fit, max_rows = 45)
house_posterior <- as_tibble(house_model_fit$draws(format = "df"))
n_sims <- nrow(house_posterior)



mcmc_trace(house_posterior, pars = c("b1_inc[1]"))

# Do a dev/val split: everything up through 2020 as the training set, 2024 as the test set
house_model_data_split <- list(
  # Dev data
  r2p = house_election_data %>% filter(year < 2024) %>% pull(r2p),
  last_r2p = house_election_data %>% filter(year < 2024) %>% pull(last_r2p),
  midterm = as.numeric((house_election_data %>% filter(year < 2024) %>% pull(year) %% 4) == 2),
  redistricted = as.numeric(house_election_data %>% filter(year < 2024) %>% pull(redistricted)),
  incumbent_running = case_when(house_election_data$incumbent_running[house_election_data$year < 2024] == "Both (redistricting)" ~ 1,
                                house_election_data$incumbent_running[house_election_data$year < 2024] == "DEM" ~ 2,
                                house_election_data$incumbent_running[house_election_data$year < 2024] == "None" ~ 3,
                                house_election_data$incumbent_running[house_election_data$year < 2024] == "REP" ~ 4),
  natl_r2p_change = house_election_data %>% filter(year < 2024) %>% pull(natl_r2p_change),
  last_pres_r2p = house_election_data %>% filter(year < 2024) %>% pull(last_pres_r2p),
  contested_last = as.numeric(house_election_data %>% filter(year < 2024) %>% pull(contested_last)),
  N_e = nrow(house_election_data %>% filter(year < 2024)),
  # OOT data
  r2p_oot = house_election_data %>% filter(year == 2024) %>% pull(r2p),
  last_r2p_oot = house_election_data %>% filter(year == 2024) %>% pull(last_r2p),
  midterm_oot = as.numeric((house_election_data %>% filter(year == 2024) %>% pull(year) %% 4) == 2),
  redistricted_oot = as.numeric(house_election_data %>% filter(year == 2024) %>% pull(redistricted)),
  incumbent_running_oot = case_when(house_election_data$incumbent_running[house_election_data$year == 2024] == "Both (redistricting)" ~ 1,
                                    house_election_data$incumbent_running[house_election_data$year == 2024] == "DEM" ~ 2,
                                    house_election_data$incumbent_running[house_election_data$year == 2024] == "None" ~ 3,
                                    house_election_data$incumbent_running[house_election_data$year == 2024] == "REP" ~ 4),
  natl_r2p_change_oot = house_election_data %>% filter(year == 2024) %>% pull(natl_r2p_change),
  last_pres_r2p_oot = house_election_data %>% filter(year == 2024) %>% pull(last_pres_r2p),
  contested_last_oot = as.numeric(house_election_data %>% filter(year == 2024) %>% pull(contested_last)),
  N_oot = nrow(house_election_data %>% filter(year == 2024))
)

house_model_dev_val <- cmdstan_model("src/house_model_dev_val.stan")
house_model_dev_val_fit <- house_model_dev_val$sample(data = house_model_data_split, seed = 2026, chains = 4, iter_warmup = 500,
                                                      iter_sampling = 2500, parallel_chains = 4, refresh = 500)
print(house_model_dev_val_fit, max_rows = 45)
house_dev_val_posterior <- as_tibble(house_model_dev_val_fit$draws(format = "df"))
n_sims <- nrow(house_dev_val_posterior)

house_district_val_posterior <- house_dev_val_posterior %>%
  select(starts_with("r2p_pred_oot")) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(house_election_data %>% filter(year == 2024)) %>%
  melt(measure.vars = paste0("V", as.character(1:n_sims)), variable.name = "sim_id", value.name = "r2p_pred") %>%
  as_tibble()

house_district_val_posterior_summary_stats <- house_district_val_posterior %>%
  group_by(year, state, seat_number) %>%
  summarise(r2p = mean(r2p),
            r_prob = mean(r2p_pred >= 0.5),
            pct_05 = quantile(r2p_pred, 0.05),
            avg = mean(r2p_pred),
            pct_95 = quantile(r2p_pred, 0.95))

house_district_val_posterior_summary_stats %>%
  ggplot(aes(x = avg, y = r2p)) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, col = "blue") +
  geom_smooth(method = "loess") +
  lims(x = c(0.1, 0.9), y = c(0.1, 0.9)) +
  labs(x = "Posterior mean Republican 2-party vote share", y = "Actual Republican 2-party vote share",
       title = "Mean predicted Republican 2-party vote share vs. actual",
       subtitle = "Contested House elections in 2024",
       caption = "Dev sample: 2,709 contested House elections 2010-2022")

# Do a logit version of the above
logit_house_model_data_split <- list(
  # Dev data
  logit_r2p = house_election_data %>% filter(year < 2024) %>% pull(r2p) %>% logit(),
  logit_last_r2p = house_election_data$last_r2p[house_election_data$year < 2024] %>%
    replace_when(house_election_data$last_r2p[house_election_data$year < 2024] == 0 ~ 0.0001,
                 house_election_data$last_r2p[house_election_data$year < 2024] == 1 ~ 0.9999) %>%
    logit(),
  midterm = as.numeric((house_election_data %>% filter(year < 2024) %>% pull(year) %% 4) == 2),
  redistricted = as.numeric(house_election_data %>% filter(year < 2024) %>% pull(redistricted)),
  incumbent_running = case_when(house_election_data$incumbent_running[house_election_data$year < 2024] == "Both (redistricting)" ~ 1,
                                house_election_data$incumbent_running[house_election_data$year < 2024] == "DEM" ~ 2,
                                house_election_data$incumbent_running[house_election_data$year < 2024] == "None" ~ 3,
                                house_election_data$incumbent_running[house_election_data$year < 2024] == "REP" ~ 4),
  natl_r2p_change = house_election_data %>% filter(year < 2024) %>% pull(natl_r2p_change),
  last_pres_r2p = house_election_data %>% filter(year < 2024) %>% pull(last_pres_r2p),
  contested_last = as.numeric(house_election_data %>% filter(year < 2024) %>% pull(contested_last)),
  N_e = nrow(house_election_data %>% filter(year < 2024)),
  # OOT data
  logit_r2p_oot = house_election_data %>% filter(year == 2024) %>% pull(r2p) %>% logit(),
  logit_last_r2p_oot = house_election_data$last_r2p[house_election_data$year == 2024] %>%
    replace_when(house_election_data$last_r2p[house_election_data$year == 2024] == 0 ~ 0.0001,
                 house_election_data$last_r2p[house_election_data$year == 2024] == 1 ~ 0.9999) %>%
    logit(),
  midterm_oot = as.numeric((house_election_data %>% filter(year == 2024) %>% pull(year) %% 4) == 2),
  redistricted_oot = as.numeric(house_election_data %>% filter(year == 2024) %>% pull(redistricted)),
  incumbent_running_oot = case_when(house_election_data$incumbent_running[house_election_data$year == 2024] == "Both (redistricting)" ~ 1,
                                    house_election_data$incumbent_running[house_election_data$year == 2024] == "DEM" ~ 2,
                                    house_election_data$incumbent_running[house_election_data$year == 2024] == "None" ~ 3,
                                    house_election_data$incumbent_running[house_election_data$year == 2024] == "REP" ~ 4),
  natl_r2p_change_oot = house_election_data %>% filter(year == 2024) %>% pull(natl_r2p_change),
  last_pres_r2p_oot = house_election_data %>% filter(year == 2024) %>% pull(last_pres_r2p),
  contested_last_oot = as.numeric(house_election_data %>% filter(year == 2024) %>% pull(contested_last)),
  N_oot = nrow(house_election_data %>% filter(year == 2024))
)

logit_house_model_dev_val <- cmdstan_model("src/logit_house_model_dev_val.stan")
logit_house_model_dev_val_fit <- logit_house_model_dev_val$sample(data = logit_house_model_data_split, seed = 2026, chains = 4, iter_warmup = 500,
                                                                  iter_sampling = 2500, parallel_chains = 4, refresh = 500)
print(logit_house_model_dev_val_fit, max_rows = 45)
logit_house_dev_val_posterior <- as_tibble(logit_house_model_dev_val_fit$draws(format = "df"))
n_sims <- nrow(logit_house_dev_val_posterior)

logit_house_district_val_posterior <- logit_house_dev_val_posterior %>%
  select(starts_with("logit_r2p_pred_oot")) %>%
  mutate_all(invlogit) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(house_election_data %>% filter(year == 2024)) %>%
  melt(measure.vars = paste0("V", as.character(1:n_sims)), variable.name = "sim_id", value.name = "r2p_pred") %>%
  as_tibble()

logit_house_district_val_posterior_summary_stats <- logit_house_district_val_posterior %>%
  group_by(year, state, seat_number) %>%
  summarise(r2p = mean(r2p),
            r_prob = mean(r2p_pred >= 0.5),
            pct_05 = quantile(r2p_pred, 0.05),
            avg = mean(r2p_pred),
            pct_95 = quantile(r2p_pred, 0.95))

house_district_val_posterior_summary_stats %>%
  arrange(desc(r2p - avg))

logit_house_district_val_posterior_summary_stats %>%
  ggplot(aes(x = avg, y = r2p)) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  geom_point(alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, col = "blue") +
  geom_smooth(method = "loess") +
  lims(x = c(0.1, 0.9), y = c(0.1, 0.9)) +
  labs(x = "Posterior mean Republican 2-party vote share", y = "Actual Republican 2-party vote share",
       title = "Mean predicted Republican 2-party vote share vs. actual (logit model)",
       subtitle = "Contested House elections in 2024",
       caption = "Dev sample: 2,709 contested House elections 2010-2022")

house_2026_sim_data <- sim_natl_r2p %>% 
  cross_join(house_2026_data) %>%
  mutate(natl_r2p_change = natl_r2p - last_natl_r2p,
         midterm = as.numeric(midterm),
         redistricted = as.numeric(redistricted),
         incumbent_running = case_when(incumbent_running == "Both (redistricting)" ~ 1,
                                       incumbent_running == "DEM" ~ 2,
                                       incumbent_running == "None" ~ 3,
                                       incumbent_running == "REP" ~ 4),
         contested_last = as.numeric(contested_last))

sim_mu <- house_2026_sims <- vector("list", n_sims)

incumbent_running_idx <- list(
  which(house_2026_data$incumbent_running == "Both (redistricting)"),
  which(house_2026_data$incumbent_running == "DEM"),
  which(house_2026_data$incumbent_running == "None"),
  which(house_2026_data$incumbent_running == "REP")
)

for(i in 1:n_sims) {
  # Pull parameters from posterior draw
  a_inc <- house_posterior[i, c("a_inc[1]", "a_inc[2]", "a_inc[3]", "a_inc[4]")] %>% as.matrix() %>% as.vector()
  b1_inc <- house_posterior[i, c("b1_inc[1]", "b1_inc[2]", "b1_inc[3]", "b1_inc[4]")] %>% as.matrix() %>% as.vector()
  b2_inc <- house_posterior[i, c("b2_inc[1]", "b2_inc[2]", "b2_inc[3]", "b2_inc[4]")] %>% as.matrix() %>% as.vector()
  b3_inc <- house_posterior[i, c("b3_inc[1]", "b3_inc[2]", "b3_inc[3]", "b3_inc[4]")] %>% as.matrix() %>% as.vector()
  b1_midterm <- house_posterior[i, "b1_midterm"] %>% as.numeric()
  b1_redist <- house_posterior[i, "b1_redist"] %>% as.numeric()
  b2_midterm <- house_posterior[i, "b2_midterm"] %>% as.numeric()
  b2_redist <- house_posterior[i, "b2_redist"] %>% as.numeric()
  b3_midterm <- house_posterior[i, "b3_midterm"] %>% as.numeric()
  b3_redist <- house_posterior[i, "b3_redist"] %>% as.numeric()
  b3_contested_last <- house_posterior[i, "b3_contested_last"] %>% as.numeric()
  sigma <- house_posterior[i, "sigma"] %>% as.numeric()
  
  # Set up things for looping
  this_house_sim <- house_2026_sim_data %>%
    filter(sim_id == i)
  a_inc_sim <- b1_inc_sim <- b2_inc_sim <- b3_inc_sim <- rep(NA, nrow(this_house_sim))
  
  # Get mu for this posterior draw
  for(j in 1:4) {
    a_inc_sim[incumbent_running_idx[[j]]] <- a_inc[j]
    b1_inc_sim[incumbent_running_idx[[j]]] <- b1_inc[j]
    b2_inc_sim[incumbent_running_idx[[j]]] <- b2_inc[j]
    b3_inc_sim[incumbent_running_idx[[j]]] <- b3_inc[j]
  }
  
  sim_mu[[i]] <- this_house_sim %>%
    mutate(a_inc = a_inc_sim,
           b1_inc = b1_inc_sim,
           b2_inc = b2_inc_sim,
           b3_inc = b3_inc_sim,
           b1_midterm = b1_midterm,
           b1_redist = b1_redist,
           b2_midterm = b2_midterm,
           b2_redist = b2_redist,
           b3_midterm = b3_midterm,
           b3_redist = b3_redist,
           b3_contested_last = b3_contested_last,
           sigma = sigma) %>%
    # Execute the actual model
    mutate(mu = a_inc + (b1_inc + b1_midterm * midterm + b2_redist * redistricted) * last_r2p * contested_last +
             (b2_inc + b2_midterm * midterm + b2_redist * redistricted) * natl_r2p_change +
             (b3_inc + b3_midterm * midterm + b3_redist * redistricted + b3_contested_last * (1 - contested_last)) * last_pres_r2p) %>%
    mutate(r2p = rnorm(n(), mu, sigma))
}

house_sims <- bind_rows(sim_mu)
house_sims %>%
  group_by(state, seat_number) %>%
  summarise(r_prob = mean(r2p >= 0.5),
            pct_05 = quantile(r2p, 0.05),
            avg = mean(r2p),
            pct_95 = quantile(r2p, 0.95)) %>%
  print(n = Inf)

house_sims %>%
  group_by(sim_id) %>%
  summarise(r_seats = sum(r2p >= 0.5)) %>%
  summarise(r_prob = mean(r_seats >= 218),
            pct_05 = quantile(r_seats, 0.05),
            avg = mean(r_seats),
            pct_95 = quantile(r_seats, 0.95))

# Top-two primary results
historical_top_two_primary <- read_csv("data/rep_two_party_primary_results_historical.csv") %>%
  filter(general_pct != 0, general_pct != 1) %>%
  mutate(midterm = year %% 4 == 2)

historical_top_two_primary %>%
  ggplot(aes(x = primary_pct, y = general_pct, col = midterm)) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme(legend.position = "bottom") +
  labs(title = "General election performance vs. top-two primary result", x = "Republican 2-party vote share in primary",
       y = "Republican vote share in general election")

## Do a little model
two_party_primary_data <- list(
  r_primary = historical_top_two_primary$primary_pct,
  r_general = historical_top_two_primary$general_pct,
  midterm = as.numeric(historical_top_two_primary$midterm),
  N = nrow(historical_top_two_primary)
)

two_party_primary_model <- cmdstan_model("src/two_party_primary_model.stan")
two_party_primary_model_fit <- two_party_primary_model$sample(data = two_party_primary_data, seed = 2026,  chains = 4, 
                                                              iter_warmup = 500, iter_sampling = 2500, parallel_chains = 4, refresh = 500)

## Look at the model
print(two_party_primary_model_fit)
two_party_primary_posterior <- as_tibble(two_party_primary_model_fit$draws(format = "df"))

two_party_primary_district_posterior <- two_party_primary_posterior %>%
  select(starts_with("r_general_pred")) %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(historical_top_two_primary) %>%
  melt(measure.vars = paste0("V", as.character(1:n_sims)), variable.name = "sim_id", value.name = "r2p_pred") %>%
  as_tibble() %>%
  mutate(sim_id = as.integer(sim_id))

two_party_primary_district_posterior_summary_stats <- two_party_primary_district_posterior %>%
  group_by(year, state, seat_number) %>%
  summarise(r2p = mean(general_pct),
            r_prob = mean(r2p_pred >= 0.5),
            pct_05 = quantile(r2p_pred, 0.05),
            avg = mean(r2p_pred),
            pct_95 = quantile(r2p_pred, 0.95))

two_party_primary_district_posterior_summary_stats

# Same model but with missing data (an attempt being made)


house_formula <- alist(
  # Model Republican 2-party share from election covariates
  r2p ~ dnorm(mu, sigma),
  mu <- a + a_inc[incumbent_running] + 
    (b1 + b1_inc[incumbent_running] + b1_midterm * midterm + b1_redist * redistricted) * last_r2p + 
    (b2 + b2_inc[incumbent_running] + b2_midterm * midterm + b2_redist * redistricted) * natl_r2p_change + 
    (b3 + b3_inc[incumbent_running] + b3_midterm * midterm + b3_redist * redistricted) * last_pres_r2p,
  c(a_inc, b1_inc, b2_inc, b3_inc)[incumbent_running] ~ dmvnorm(0, Rho, tau),
  c(a, b1, b1_midterm, b2, b2_midterm, b3, b3_midterm) ~ dnorm(0, 1),
  c(b1_redist, b2_redist) ~ dnorm(-1, 1),
  b3_redist ~ dnorm(1, 1),
  sigma ~ dexp(1),
  tau ~ dexp(1),
  Rho ~ dlkjcorr(1),
  
  # Model national popular vote change from polls (measurement error model)
  natl_r2p_change <- natl_r2p - last_natl_r2p,
  natl_r2p <- poll_r2p + error_poll,
  error_poll <- cycle_intercept[cycle] + poll_level_error,
  cycle_intercept[cycle] ~ dnorm(0, 1),
  poll_level_error ~ dnorm(0, error_poll_sigma),
  
  ## Polling error model
  error_poll_sigma ~ dexp(exp(p + p_cycle[cycle] + p_pollster[pollster] + p_pop[population])),
  p ~ dnorm(0, 1),
  p_cycle[cycle] ~ dnorm(0, 1),
  p_pollster[pollster] ~ dnorm(0, 1),
  p_pop[population] ~ dnorm(0, 1)
)

house_model_data <- list(
  r2p = house_election_data$r2p,
  incumbent_running = house_election_data$incumbent_running,
  midterm = house_election_data$midterm,
  redistricted = house_election_data$redistricted,
  last_r2p = house_election_data$r2p,
  natl_r2p_change = house_election_data$natl_r2p_change,
  last_pres_r2p = house_election_data$last_pres_r2p,
  natl_r2p = house_election_data$natl_r2p,
  last_natl_r2p = house_election_data$last_natl_r2p,
  poll_r2p = historical_generic_ballot_polls$poll_r2p,
  cycle = as.integer(factor(historical_generic_ballot_polls$cycle)),
  pollster = as.integer(factor(historical_generic_ballot_polls$pollster)),
  population = as.integer(factor(historical_generic_ballot_polls$population))
)

house_ulam <- ulam(house_formula, data = house_model_data, chains = 4, cores = 4)

house_ppd <- sim(house_ulam, data = house_model_data %>% 
                   mutate(pollster = as.integer(factor(pollster)),
                          population = as.integer(factor(population)),
                          cycle = as.integer(factor(cycle))) %>%
                   select(r2p, last_r2p, natl_r2p, last_natl_r2p, last_pres_r2p, incumbent_running, midterm, redistricted,
                          poll_r2p, cycle, pollster, sample_size, population, days_to_election) %>%
                   filter(cycle == 2022))

basic_house_ppd <- sim(basic_house_ulam, data = house_model_data %>% filter(year == 2022)) %>%
  inv_logit() %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(house_model_data) %>%
  select(year, state, seat_number, r2p, last_r2p, r2p_change, V1:V1000) %>%
  melt(id.vars = c("year", "state", "seat_number", "r2p", "last_r2p", "r2p_change"),
       variable.name = "id", value.name = "pred_r2p") %>%
  as_tibble() %>%
  mutate(id = as.character(id) %>% str_replace("V", "") %>% as.numeric())

house_2010_2022_ulam <- ulam(basic_house_formula, 
                             data = house_model_data %>%
                               filter(year <= 2022) %>% 
                               select(logit_r2p, last_r2p, natl_r2p_change, last_pres_r2p,
                                      incumbent_running, midterm, redistricted),
                             chains = 4, cores = 4, iter = 10500, warmup = 500)

house_2010_2020_ulam <- ulam(basic_house_formula, 
                             data = house_model_data %>%
                               filter(year <= 2020) %>% 
                               select(logit_r2p, last_r2p, natl_r2p_change, last_pres_r2p,
                                      incumbent_running, midterm, redistricted),
                             chains = 4, cores = 4)

# 

house_polling_error_formula <- alist(
  # Model national popular vote change from polls (measurement error model)
  natl_r2p <- poll_r2p + cycle_intercept[cycle] + poll_level_error,
  cycle_intercept[cycle] ~ dnorm(0, 1),
  poll_level_error ~ dnorm(avg_error, error_poll_sigma),
  
  ## Polling error model
  avg_error <- house_effect[pollster] + pop_effect[pop],
  house_effect[pollster] ~ dnorm(0, 1),
  pop_effect[pop] ~ dnorm(0, 1),
  error_poll_sigma ~ dexp(0.5),
)

house_polling_error_data <- list(
  natl_r2p = house_election_data$natl_r2p,
  poll_r2p = historical_generic_ballot_polls$poll_r2p,
  cycle = as.integer(factor(historical_generic_ballot_polls$cycle)),
  pollster = as.integer(factor(historical_generic_ballot_polls$pollster)),
  population = as.integer(factor(historical_generic_ballot_polls$population))
)

house_polling_error_model <- stan("src/polling_error_model.stan", data = house_polling_error_data,
                                  chains = 4, cores = 4)

# Bayesian diagnostics
precis(basic_house_ulam)
precis(house_2010_2022_ulam, depth = 4)
precis(house_2010_2020_ulam)

# Plot prior vs. posterior
house_model_prior <- extract.prior(house_2010_2022_ulam)
house_model_posterior <- extract.samples(house_2010_2022_ulam)

bind_rows(
  tibble(distribution = "prior",
         sigma = house_model_prior$sigma[,1]),
  tibble(distribution = "posterior",
         sigma = house_model_posterior$sigma[,1])
) %>%
  ggplot(aes(x = sigma)) +
  geom_density(aes(col = distribution))

# District-level average pp error on 2024
house_2010_2022_preds <- predicted_draws(house_2010_2022_ulam, newdata = house_model_data %>% filter(year == 2024), seed = 1) %>%
  ungroup() %>%
  mutate(pred_r2p = inv_logit(.prediction))

house_2010_2022_preds %>%
  select(year, state, seat_number, r2p, pred_r2p)

house_2010_2022_preds <- sim(house_2010_2022_ulam, data = house_model_data %>% filter(year == 2024), n = 10000) %>%
  inv_logit() %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(house_model_data %>% filter(year == 2024)) %>%
  select(year, state, seat_number, r2p, last_r2p, r2p_change, V1:V10000) %>%
  melt(id.vars = c("year", "state", "seat_number", "r2p", "last_r2p", "r2p_change"),
       variable.name = "id", value.name = "pred_r2p") %>%
  as_tibble() %>%
  mutate(id = as.character(id) %>% str_replace("V", "") %>% as.numeric())

house_2010_2020_preds <- sim(house_2010_2020_ulam, data = house_model_data %>% filter(year == 2024), n = 10000) %>%
  inv_logit() %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(house_model_data %>% filter(year == 2024)) %>%
  select(year, state, seat_number, r2p, last_r2p, r2p_change, V1:V1000) %>%
  melt(id.vars = c("year", "state", "seat_number", "r2p", "last_r2p", "r2p_change"),
       variable.name = "id", value.name = "pred_r2p") %>%
  as_tibble() %>%
  mutate(id = as.character(id) %>% str_replace("V", "") %>% as.numeric())

house_2010_2020_mae <- house_2010_2020_preds %>%
  group_by(state, seat_number) %>%
  summarise(mae = mean(abs(pred_r2p - r2p)))

house_2010_2022_mae <- house_2010_2022_preds %>%
  group_by(state, seat_number) %>%
  summarise(mae = mean(abs(pred_r2p - r2p)))

# MAE comparison
house_2010_2020_mae %>%
  rename(mae_2020 = mae) %>%
  left_join(house_2010_2022_mae %>% rename(mae_2022 = mae)) %>% 
  print(n = Inf)

basic_house_ppd %>%
  arrange(state, seat_number, year, id) %>%
  group_by(year, state, seat_number, r2p) %>%
  summarise(mae = mean(abs(pred_r2p - r2p)))

# Calibration
house_2010_2020_probs <- house_2010_2020_preds %>%
  group_by(state, seat_number, r2p) %>%
  summarise(prob_r = mean(pred_r2p > 0.5)) %>%
  mutate(r_win = r2p > 0.5,
         prob_bin = cut(prob_r, c(-Inf, 0.02, 0.1, 0.2, 0.4, 0.6, 0.8, 0.98, 0.95, Inf))) %>%
  ungroup()

house_2010_2022_probs <- house_2010_2022_preds %>%
  group_by(state, seat_number, r2p) %>%
  summarise(prob_r = mean(pred_r2p > 0.5)) %>%
  mutate(r_win = r2p > 0.5,
         prob_bin = cut(prob_r, c(-Inf, 0.02, 0.1, 0.2, 0.4, 0.6, 0.8, 0.9, 0.98, Inf))) %>%
  ungroup()

bind_rows(
  house_2010_2020_probs %>%
    group_by(prob_bin) %>%
    summarise(n = n(),
              mean_prob = mean(prob_r),
              actual_prob = mean(r_win)) %>%
    mutate(model = "2010_2020"),
  house_2010_2022_probs %>%
    group_by(prob_bin) %>%
    summarise(n = n(),
              mean_prob = mean(prob_r),
              actual_prob = mean(r_win)) %>%
    mutate(model = "2010_2022")
) 

# In-sample calibration of the model on all the data
full_model_calibration <- sim(basic_house_ulam, data = house_model_data) %>%
  inv_logit() %>%
  t() %>%
  as.data.frame() %>%
  as_tibble() %>%
  bind_cols(house_model_data) %>%
  select(year, state, seat_number, r2p, last_r2p, r2p_change, V1:V1000) %>%
  melt(id.vars = c("year", "state", "seat_number", "r2p", "last_r2p", "r2p_change"),
       variable.name = "id", value.name = "pred_r2p") %>%
  as_tibble() %>%
  mutate(id = as.character(id) %>% str_replace("V", "") %>% as.numeric()) %>%
  group_by(year, state, seat_number, r2p) %>%
  summarise(prob_r = mean(pred_r2p > 0.5)) %>%
  group_by(prob_bin = cut(prob_r, c(-Inf, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.98, Inf))) %>%
  summarise(n = n(),
            actual_prob = mean(r2p > 0.5),
            mean_prob = mean(prob_r))