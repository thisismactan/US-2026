library(tidyverse)
library(tidybayes)
library(tidybayes.rethinking)
library(rethinking)
library(reshape2)
library(mvnfast)

# House historical data
## Lot of official names for state Democratic parties
dem_parties <- c("DEMOCRAT", "DEMOCRATIC", "DEMOCRATIC-FARM-LABOR", "DEMOCRATIC-FARMER-LABOR",
                 "DEMOCRATIC-NONPARTISAN LEAGUE", "DEMOCRATIC-NPL")

## Read the historical House data
house_76_24 <- read_csv("data/1976-2024-house.csv") %>%
  mutate(candidatevotes = as.numeric(candidatevotes),
         state = str_to_title(state),
         seat_number = ifelse(district == 0, 1, district))

## Calculate percentage of vote in each election
house_district_pct <- house_76_24 %>%
  filter(stage == "GEN", !special) %>%
  select(year, state, seat_number, candidate, party, candidatevotes, totalvotes, fusion_ticket) %>%
  group_by(year, state, seat_number, candidate, totalvotes) %>%
  summarise(candidatevotes = sum(candidatevotes, na.rm = TRUE),
            party = case_when(any(party %>% str_starts("DEMOCRAT")) ~ "DEMOCRAT",
                              any(party == "REPUBLICAN") ~ "REPUBLICAN",
                              TRUE ~ first(party))) %>%
  mutate(party = ifelse(party %in% dem_parties, "DEMOCRAT", party),
         pct = candidatevotes / totalvotes) 

## Convert to two-party Republican vote share
house_district_r2p <- house_district_pct %>%
  group_by(year, state, seat_number, party) %>%
  summarise(partyvotes = sum(candidatevotes, na.rm = TRUE)) %>%
  ungroup() %>%
  spread(party, partyvotes, fill = 0) %>%
  mutate(r2p = REPUBLICAN / (REPUBLICAN + DEMOCRAT)) %>%
  select(year, state, seat_number, REPUBLICAN, DEMOCRAT, r2p)

## Identify winners
house_district_winners <- house_district_pct %>%
  group_by(year, state, seat_number) %>%
  filter(candidatevotes == max(candidatevotes, na.rm = TRUE)) %>%
  arrange(state, seat_number, year) %>%
  rename(winner = candidate)

## Read data on House historical incumbents
house_incumbents_running <- read_csv("data/historical_house_incumbents.csv")

## Identify contested races (logic: Republican vote share is in (0,1))
house_contested <- house_district_r2p %>%
  group_by(state, seat_number) %>%
  arrange(state, seat_number, year) %>%
  mutate(contested = (r2p > 0) & (r2p < 1),
         contested_last = lag(contested)) %>%
  select(year, state, seat_number, contested, contested_last)

## Calculate national-level House vote and 2-party Republican vote share
house_natl_pct <- house_76_24 %>%
  mutate(party = ifelse(party %in% dem_parties, "DEMOCRAT", party)) %>%
  group_by(year, party) %>%
  summarise(natl_votes = sum(candidatevotes, na.rm = TRUE)) %>%
  mutate(totalvotes = sum(natl_votes, na.rm = TRUE),
         natl_pct = natl_votes / totalvotes)

house_natl_r2p <- house_natl_pct %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  select(year, party, natl_votes) %>%
  spread(party, natl_votes) %>%
  mutate(natl_r2p = REPUBLICAN / (REPUBLICAN + DEMOCRAT))

house_natl_r2p

## Also get most recent available presidential election results (2 years ago for midterms; 4 years ago for pres years)
if (!("cd_pres_results.csv" %in% list.files("data/cd-pres-results"))) {
  source("src/shape_cd_pres_results.R")
}

cd_pres_results <- read_csv("data/cd-pres-results/cd_pres_results.csv")

# Lump everything together into the modeling dataset
house_model_data <- house_district_r2p %>%
  select(year, state, seat_number, r2p) %>%
  left_join(house_natl_r2p %>% select(year, natl_r2p), by = "year") %>%
  arrange(state, seat_number, year) %>%
  group_by(state, seat_number) %>%
  mutate(last_r2p = lag(r2p),
         r2p_change = r2p - lag(r2p),
         last_natl_r2p = lag(natl_r2p),
         natl_r2p_change = natl_r2p - lag(natl_r2p)) %>%
  left_join(house_contested, by = c("year", "state", "seat_number")) %>%
  left_join(house_incumbents_running %>% select(year, state, seat_number, incumbent_running, redistricted), 
            by = c("year", "state", "seat_number")) %>%
  left_join(cd_pres_results, by = c("year", "state", "seat_number")) %>%
  mutate(incumbent_running = incumbent_running %>% as.factor() %>% as.integer(),
         midterm = as.numeric(year %% 4 == 2),
         redistricted = as.numeric(redistricted)) %>%
  filter(contested, contested_last) %>%
  filter(!is.na(natl_r2p_change), !is.na(r2p_change)) %>%
  filter(year >= 2010) %>%
  ungroup() %>%
  mutate(logit_r2p = log(r2p / (1 - r2p)))

house_model_data %>%
  ggplot(aes(x = natl_r2p_change, y = r2p_change)) +
  geom_point()

## Bayesian regression on the logit of r2p
basic_house_formula <- alist(
  logit_r2p ~ dnorm(mu, sigma),
  mu <- a + a_inc[incumbent_running] + 
    (b1 + b1_inc[incumbent_running] + b1_midterm * midterm + b1_redist * redistricted) * last_r2p + 
    (b2 + b2_inc[incumbent_running] + b2_midterm * midterm + b2_redist * redistricted) * natl_r2p_change + 
    (b3 + b3_inc[incumbent_running] + b3_midterm * midterm + b3_redist * redistricted) * last_pres_r2p,
  c(a_inc, b1_inc, b2_inc, b3_inc)[incumbent_running] ~ dmvnorm(0, Rho, tau),
  a ~ dnorm(0, 0.2),
  b1 ~ dnorm(1, 0.5),
  b1_midterm ~ dnorm(0, 0.5),
  b1_redist ~ dnorm(-1, 0.5),
  b2 ~ dnorm(1, 0.5),
  b2_midterm ~ dnorm(0, 0.5),
  b2_redist ~ dnorm(-1, 0.5),
  b3 ~ dnorm(1, 0.5),
  b3_midterm ~ dnorm(0, 0.5),
  b3_redist ~ dnorm(1, 0.5),
  sigma ~ dexp(1),
  tau ~ dexp(1),
  Rho ~ dlkjcorr(1)
)

basic_house_ulam <- ulam(basic_house_formula, 
                         data = house_election_data %>% 
                           select(logit_r2p, last_r2p, natl_r2p_change, last_pres_r2p,
                                  incumbent_running, midterm, redistricted),
                         chains = 4, cores = 4)
precis(basic_house_ulam)
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

full_model_calibration

state_i <- "Alabama"
district_j <- 3
year_t <- 2022

filtered_ppd <- basic_house_ppd %>%
  filter(state == state_i, year == year_t)

filtered_ppd %>%
  ggplot(aes(x = pred_r2p, y = after_stat(density))) +
  facet_wrap(~seat_number, scales = "free_x") +
  geom_histogram(binwidth = 0.01) +
  geom_vline(data = filtered_ppd %>% group_by(seat_number) %>% summarise(r2p = mean(r2p)),
             aes(xintercept = r2p)) +
  lims(x = c(0, 1))
