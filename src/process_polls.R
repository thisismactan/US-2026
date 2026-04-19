source("src/lib.R")

# House polls
## 2026 polls
download.file("https://www.nytimes.com/newsgraphics/polls/house.csv", "data/polls/house_polls_2026.csv")
house_polls_2026_raw <- read_csv("data/polls/house_polls_2026.csv")

generic_ballot_polls_2026 <- house_polls_2026_raw %>%
  filter(state == "US", stage == "general") %>%
  select(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
         start_date, end_date, question_id, sample_size, population, tracking, internal, partisan, cycle, 
         election_date, party, pct) %>%
  group_by(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
           start_date, end_date, population, tracking, internal, partisan, cycle, election_date, party) %>%
  summarise(n = mean(sample_size),
            pct = mean(pct)) %>%
  mutate_at(c("election_date", "start_date", "end_date"), as.Date, format = "%m/%d/%y") %>%
  mutate(year = year(election_date),
         spread = as.numeric(round((end_date - start_date) / 2)),
         median_date = start_date + spread,
         days_to_election = as.numeric(election_date - median_date),
         party = case_when(party == "DEM" ~ "dem",
                           party == "REP" ~ "rep"),
         partisan_lean = case_when(partisan == "DEM" ~ -0.02,
                                   partisan == "REP" ~ 0.02,
                                   TRUE ~ 0.0),
         method_weight = case_when(methodology %>% str_detect("Opt-In") ~ 0.1,
                                   methodology == "Nonprobability Panel" ~ 0.25,
                                   methodology == "Text-to-Web" ~ 0.33,
                                   is.na(methodology) ~ 0.5,
                                   methodology == "Probability Panel" ~ 0.75,
                                   TRUE ~ 1.0)) %>%
  filter(!is.na(n), !is.na(population), population != "a", !is.na(party)) %>%
  mutate(state = state_abbr_decoder(state)) %>%
  group_by(poll_id) %>%
  mutate(keep_lv = ifelse(any(population == "lv"), population == "lv", population %in% c("rv", "v"))) %>%
  ungroup() %>%
  filter(keep_lv) %>%
  group_by(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
           start_date, end_date, population, tracking, internal, partisan, partisan_lean, cycle, election_date) %>%
  mutate(r2p = pct / sum(pct) - partisan_lean) %>%
  filter(party == "rep") %>%
  ungroup()

if(!exists("most_recent_n_generic_ballot_polls")) {
  most_recent_n_generic_ballot_polls <- nrow(generic_ballot_polls_2026)
}

if(nrow(generic_ballot_polls_2026) > most_recent_n_generic_ballot_polls) {
  print("New generic ballot polls detected!")
} else {
  print("No new generic ballot polls detected.")
}
most_recent_n_generic_ballot_polls <- nrow(generic_ballot_polls_2026)

### Polling averages over time
election_date_2026 <- as.Date("2026-11-03")
poll_average_dates <- seq(as.Date("2024-11-05"), min(today(), election_date_2026))
poll_average_df_list <- vector("list", length(poll_average_dates))

start_time <- Sys.time()
for(i in seq_along(poll_average_dates)) {
  filtered_polls <- generic_ballot_polls_2026 %>%
    filter(end_date <= poll_average_dates[i],
           poll_average_dates[i] - median_date <= 90) %>%
    mutate(poll_age = as.numeric(poll_average_dates[i] - median_date),
           weight = method_weight * ifelse(is.na(partisan), 5, 1) * ifelse(population == "lv", 3, 1) * n^(0.25) /
             (exp((poll_age + 7)^0.5) * ifelse(spread == 0, 3, 1)),
           partisan_lean = 0.0,
           r2p = r2p - partisan_lean)
  poll_average_df_list[[i]] <- filtered_polls %>%
    filter(weight > 0) %>%
    select(weight, r2p) %>%
    summarise(avg = wtd.mean(r2p, weight),
              eff_n = sum(weight)^2 / sum(weight^2),
              sd = sqrt(wtd.var(r2p, weight, normwt = TRUE) * n() / (n() - 1.5)),
              .groups = "drop") %>%
    mutate(se = sd / sqrt(eff_n),
           avg_date = poll_average_dates[i],
           days_to_election = as.numeric(election_date_2026 - avg_date))
}
generic_ballot_averages_2026 <- bind_rows(poll_average_df_list) %>%
  mutate(year = 2026)
print(Sys.time() - start_time)

generic_ballot_averages_2026_smoothed <- generic_ballot_averages_2026 %>%
  arrange(avg_date) %>%
  mutate(avg = rollmeanr(avg, k = 5, na.pad = TRUE),
         eff_n = rollmeanr(eff_n, k = 5, na.pad = TRUE),
         sd = rollmeanr(sd, k = 5, na.pad = TRUE),
         se = sd / sqrt(eff_n)) 

### House effect estimation
pollster_codes <- generic_ballot_polls_2026 %>%
  group_by(pollster) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(pollster_code = 1:n())

house_effect_tbl <- generic_ballot_polls_2026 %>%
  select(poll_id, pollster, methodology, partisan, population, median_date, party, r2p) %>%
  left_join(pollster_codes %>% select(pollster, pollster_code),
            by = "pollster") %>%
  left_join(generic_ballot_averages_2026_smoothed %>% select(avg_date, avg),
            by = c("median_date" = "avg_date")) %>%
  mutate(partisan = ifelse(is.na(partisan), "None", partisan)) %>%
  filter(!is.na(avg), !is.na(r2p))

house_effect_data <- list(
  N = nrow(house_effect_tbl),
  N_pollsters = n_distinct(house_effect_tbl$pollster_code),
  N_partisan = n_distinct(house_effect_tbl$partisan),
  avg = house_effect_tbl$avg,
  r2p = house_effect_tbl$r2p,
  partisan_rep = house_effect_tbl$partisan == "REP",
  partisan_dem = house_effect_tbl$partisan == "DEM",
  lv = house_effect_tbl$population == "lv",
  pollster_code = house_effect_tbl$pollster_code
)

house_effect_model <- cmdstan_model("src/house_effect_model.stan")
house_effect_model_fit <- house_effect_model$sample(
  data = house_effect_data, seed = 2026, chains = 4, iter_warmup = 500, iter_sampling = 2500, 
  parallel_chains = 4, refresh = 500
)

print(house_effect_model_fit, max_rows = 100)

house_effects <- house_effect_model_fit$draws(format = "df") %>%
  select(starts_with("house_effect")) %>%
  rename_all(function(x) str_remove_all(x, "house_effect\\[|\\]")) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "pollster_code", value.name = "house_effect") %>%
  mutate(pollster_code = as.character(pollster_code) %>% as.numeric()) %>%
  group_by(pollster_code) %>%
  summarise(house_effect = mean(house_effect)) %>%
  left_join(pollster_codes, by = "pollster_code")

partisan_effects <- house_effect_model_fit$draws(format = "df") %>%
  select(rep_effect, dem_effect) %>%
  mutate(sim_id = 1:n()) %>%
  melt(id.vars = "sim_id", variable.name = "partisan", value.name = "partisan_effect") %>%
  group_by(partisan) %>%
  summarise(partisan_effect = mean(partisan_effect))

house_effects %>%arrange(house_effect) %>% print(n = Inf)
partisan_effects 

## Historical generic ballot polls
historical_generic_ballot_polls <- read_csv("data/polls/generic_ballot_polls_historical.csv") %>%
  select(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
         start_date, end_date, question_id, sample_size, population, tracking, internal, partisan, cycle, 
         election_date, dem, rep, ind) %>%
  melt(measure.vars = c("dem", "rep", "ind"), variable.name = "party", value.name = "pct") %>%
  group_by(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
           start_date, end_date, population, tracking, internal, partisan, cycle, election_date, party) %>%
  summarise(n = mean(sample_size),
            pct = mean(pct)) %>%
  mutate_at(c("election_date", "start_date", "end_date"), as.Date, format = "%m/%d/%y") %>%
  mutate(year = year(election_date),
         spread = as.numeric(round((end_date - start_date) / 2)),
         median_date = start_date + spread,
         days_to_election = as.numeric(election_date - median_date),
         method_weight = case_when(methodology %>% str_detect("Opt-In") ~ 0.1,
                                   methodology == "Nonprobability Panel" ~ 0.25,
                                   methodology == "Text-to-Web" ~ 0.33,
                                   is.na(methodology) ~ 0.5,
                                   methodology == "Probability Panel" ~ 0.75,
                                   TRUE ~ 1.0)) %>%
  filter(!is.na(n), !is.na(population), population != "a", party %in% c("dem", "rep")) %>%
  group_by(poll_id) %>%
  mutate(keep_lv = ifelse(any(population == "lv"), population == "lv", population %in% c("rv", "v"))) %>%
  ungroup() %>%
  filter(keep_lv) %>%
  group_by(poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
           start_date, end_date, population, tracking, internal, partisan, cycle, election_date) %>%
  mutate(r2p = pct / sum(pct)) %>%
  filter(party == "rep") %>%
  ungroup()

### Historical polling averages
poll_average_dates <- seq(as.Date("2016-11-08"), as.Date("2024-11-05"))
poll_average_df_list <- vector("list", length(poll_average_dates))
house_election_dates <- as.Date(c("2018-11-06", "2020-11-03", "2022-11-08", "2024-11-05"))

start_time <- Sys.time()
for(i in seq_along(poll_average_dates)) {
  next_election_date <- min(house_election_dates[house_election_dates >= poll_average_dates[i]])
  filtered_polls <- historical_generic_ballot_polls %>%
    filter(end_date <= poll_average_dates[i],
           poll_average_dates[i] - median_date <= 90) %>%
    mutate(poll_age = as.numeric(poll_average_dates[i] - median_date),
           weight = method_weight * ifelse(is.na(partisan), 5, 1) * ifelse(population == "lv", 3, 1) * n^(0.25) /
             (exp((poll_age + 7)^0.5) * ifelse(spread == 0, 3, 1)),
           partisan_lean = 0.0,
           r2p = r2p - partisan_lean)
  poll_average_df_list[[i]] <- filtered_polls %>%
    filter(weight > 0) %>%
    select(election_date, weight, r2p) %>%
    group_by(election_date) %>%
    summarise(avg = wtd.mean(r2p, weight),
              eff_n = sum(weight)^2 / sum(weight^2),
              sd = sqrt(wtd.var(r2p, weight, normwt = TRUE) * n() / (n() - 1.5)),
              .groups = "drop") %>%
    mutate(se = sd / sqrt(eff_n),
           avg_date = poll_average_dates[i],
           days_to_election = as.numeric(next_election_date - avg_date))
}
historical_generic_ballot_averages <- bind_rows(poll_average_df_list) %>%
  mutate(year = year(election_date))
print(Sys.time() - start_time)

historical_generic_ballot_averages_smoothed <- historical_generic_ballot_averages %>%
  arrange(avg_date) %>%
  mutate(avg = rollmeanr(avg, k = 5, na.pad = TRUE),
         eff_n = rollmeanr(eff_n, k = 5, na.pad = TRUE),
         sd = rollmeanr(sd, k = 5, na.pad = TRUE),
         se = sd / sqrt(eff_n)) %>%
  left_join(house_natl_r2p %>% ungroup() %>% select(year, natl_r2p), by = "year") 

historical_generic_ballot_averages_smoothed %>%
  ggplot(aes(x = avg_date, y = avg)) +
  geom_ribbon(aes(ymin = avg - 1.645 * se, ymax = avg + 1.645 * se), fill = "red", alpha = 0.2, linewidth = 0) +
  geom_line(col = "red", linewidth = 1) +
  scale_y_continuous(breaks = (0:5) / 5, labels = percent_format(), limits = c(0.3, 0.6))

# Senate polls
### Some independents are effectively the Democratic nominee and have similar ups and downs in polling, probably
ind_dems <- c("Bernie Sanders", "Angus S. King Jr.", "Dan Osborn", "Evan McMullin")

## 2026
download.file("https://www.nytimes.com/newsgraphics/polls/senate.csv", "data/polls/senate_polls_2026.csv")

senate_polls_2026 <- read_csv("data/polls/senate_polls_2026.csv") %>%
  filter(stage == "general") %>%
  group_by(question_id) %>%
  mutate(n_dems = sum(party == "DEM"),
         n_reps = sum(party == "REP")) %>%
  ungroup() %>%
  filter(n_dems <= 1, n_reps <= 1)

if(!exists("most_recent_n_senate_polls")) {
  most_recent_n_senate_polls <- nrow(senate_polls_2026)
}

if(nrow(senate_polls_2026) > most_recent_n_senate_polls) {
  print("New Senate polls detected!")
} else {
  print("No new Senate polls detected.")
}
most_recent_n_senate_polls <- nrow(senate_polls_2026)

senate_candidates_2026 <- read_csv("data/senate_candidates_2026.csv") %>%
  filter(candidate_fullname != "None") %>%
  arrange(state, seat_name, candidate_fullname) %>%
  group_by(state, seat_name) %>%
  summarise(cand_list = paste(candidate_fullname, collapse = "|"))

senate_poll_cand_lists_2026 <- read_csv("data/polls/senate_polls_2026.csv") %>%
  filter(party %in% c("DEM", "REP") | candidate_name %in% ind_dems) %>%
  arrange(question_id, candidate_name) %>%
  group_by(poll_id, question_id) %>%
  summarise(cand_list = paste(candidate_name, collapse = "|"))

senate_poll_leans_2026 <- senate_polls_2026 %>%
  select(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
         start_date, end_date, question_id, sample_size, population, tracking, internal, partisan, cycle, 
         election_date, ranked_choice_reallocated, party, candidate_name, pct) %>%
  filter(party %in% c("DEM", "REP") | candidate_name %in% ind_dems) %>%
  left_join(senate_poll_cand_lists_2026, by = c("poll_id", "question_id")) %>%
  # Handle pesky polls that ask multiple questions
  group_by(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
           start_date, end_date, population, tracking, internal, partisan, cycle, election_date, 
           ranked_choice_reallocated, cand_list, party, candidate_name) %>%
  summarise(n = mean(sample_size),
            pct = mean(pct)) %>%
  mutate_at(c("election_date", "start_date", "end_date"), as.Date, format = "%m/%d/%y") %>%
  mutate(year = year(election_date),
         spread = as.numeric(round((end_date - start_date) / 2)),
         median_date = start_date + spread,
         days_to_election = as.numeric(election_date - median_date),
         method_weight = case_when(methodology %>% str_detect("Opt-In") ~ 0.1,
                                   methodology == "Nonprobability Panel" ~ 0.25,
                                   methodology == "Text-to-Web" ~ 0.33,
                                   is.na(methodology) ~ 0.5,
                                   methodology == "Probability Panel" ~ 0.75,
                                   TRUE ~ 1.0),
         party = case_when(party == "DEM" ~ "dem",
                           party == "REP" ~ "rep",
                           candidate_name %in% ind_dems ~ "dem")) %>%
  filter(!is.na(n), !is.na(population), population != "a") %>%
  mutate(state = state_abbr_decoder(state),
         seat_name = ifelse(state %in% c("Florida", "Ohio"), "Class III", "Class II")) %>%
  inner_join(senate_candidates_2026, by = c("state", "seat_name", "cand_list")) %>%
  group_by(poll_id) %>%
  mutate(keep_lv = ifelse(any(population == "lv"), population == "lv", population %in% c("rv", "v"))) %>%
  ungroup() %>%
  filter(keep_lv) %>%
  group_by(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
           start_date, end_date, population, tracking, internal, partisan, cycle, election_date, 
           ranked_choice_reallocated, cand_list) %>%
  mutate(r2p = pct / sum(pct)) %>%
  filter(party == "rep") %>%
  inner_join(generic_ballot_averages_2026_smoothed %>%
               ungroup() %>%
               select(avg_date, generic_ballot_avg = avg, generic_ballot_eff_n = eff_n, 
                      generic_ballot_sd = sd, generic_ballot_se = se),
             by = c("median_date" = "avg_date")) %>%
  mutate(poll_age = as.numeric(today() - median_date),
         partisan_lean = case_when(partisan == "DEM" ~ -0.02,
                                   partisan == "REP" ~ 0.02,
                                   partisan == "IND" ~ -0.02,
                                   is.na(partisan) ~ 0.0,
                                   TRUE ~ 0.0),
         r2p_lean = r2p - partisan_lean - generic_ballot_avg,
         weight = method_weight * ifelse(is.na(partisan), 5, 1) * ifelse(population == "lv", 3, 1) * n^(0.25) / 
           (exp((poll_age + 7)^0.1) * ifelse(spread == 0, 3, 1)))

senate_average_leans_2026 <- senate_poll_leans_2026 %>%
  filter(weight > 0) %>%
  select(state, seat_name, election_date, weight, r2p_lean) %>%
  group_by(state, seat_name, election_date) %>%
  summarise(avg_lean = wtd.mean(r2p_lean, weight),
            eff_n = sum(weight)^2 / sum(weight^2),
            sd = sqrt(wtd.var(r2p_lean, weight, normwt = TRUE) * n() / (n() - 1.5)),
            .groups = "drop") %>%
  mutate(se = sd / sqrt(eff_n),
         avg_date = poll_average_dates[i],
         days_to_election = as.numeric(election_date - avg_date))
  
## Historical
historical_senate_candidates <- read_csv("data/historical_senate_candidates.csv") %>%
  arrange(year, state, seat_name, candidate_fullname) %>%
  group_by(year, state, seat_name) %>%
  summarise(cand_list = paste(candidate_fullname, collapse = "|")) %>%
  ungroup()

historical_senate_poll_cand_lists <- read_csv("data/polls/senate_polls_historical.csv") %>%
  filter(party %in% c("DEM", "REP") | candidate_name %in% ind_dems) %>%
  arrange(question_id, candidate_name) %>%
  group_by(poll_id, question_id) %>%
  summarise(cand_list = paste(candidate_name, collapse = "|"))

historical_senate_polls_r2p <- read_csv("data/polls/senate_polls_historical.csv") %>%
  select(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
         start_date, end_date, question_id, sample_size, population, tracking, internal, partisan, cycle, 
         election_date, ranked_choice_reallocated, party, candidate_name, pct) %>%
  filter(party %in% c("DEM", "REP") | candidate_name %in% ind_dems) %>%
  left_join(historical_senate_poll_cand_lists, by = c("poll_id", "question_id")) %>%
  # Handle pesky polls that ask multiple questions
  group_by(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
           start_date, end_date, population, tracking, internal, partisan, cycle, election_date, 
           ranked_choice_reallocated, cand_list, party, candidate_name) %>%
  summarise(sample_size = mean(sample_size),
            pct = mean(pct)) %>%
  mutate_at(c("election_date", "start_date", "end_date"), as.Date, format = "%m/%d/%y") %>%
  mutate(year = year(election_date),
         spread = as.numeric(round((end_date - start_date) / 2)),
         median_date = start_date + spread,
         days_to_election = as.numeric(election_date - median_date),
         method_weight = case_when(methodology %>% str_detect("Opt-In") ~ 0.1,
                                   methodology == "Nonprobability Panel" ~ 0.25,
                                   methodology == "Text-to-Web" ~ 0.33,
                                   is.na(methodology) ~ 0.5,
                                   methodology == "Probability Panel" ~ 0.75,
                                   TRUE ~ 1.0),
         party = case_when(party == "DEM" ~ "dem",
                           party == "REP" ~ "rep",
                           candidate_name %in% ind_dems ~ "dem"),
         partisan_lean = case_when(partisan == "DEM" ~ -0.02,
                                   partisan == "REP" ~ 0.02,
                                   partisan == "IND" ~ -0.02,
                                   is.na(partisan) ~ 0.0,
                                   TRUE ~ 0.0)) %>%
  filter(!is.na(sample_size), !is.na(population), population != "a") %>%
  inner_join(historical_senate_candidates, by = c("year", "state", "seat_name", "cand_list")) %>%
  # If there's an LV result and other results, drop the others
  group_by(poll_id) %>%
  mutate(keep_lv = ifelse(any(population == "lv"), population == "lv", population %in% c("rv", "v"))) %>%
  ungroup() %>%
  filter(keep_lv) %>%
  group_by(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, method_weight,
           start_date, median_date, end_date, spread, sample_size, population, keep_lv, tracking, internal, partisan, 
           partisan_lean, cycle, election_date, ranked_choice_reallocated, cand_list, party) %>%
  summarise(pct = sum(pct)) %>%
  group_by(state, seat_name, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, method_weight,
           start_date, median_date, end_date, spread, sample_size, population, keep_lv, tracking, internal, partisan, 
           partisan_lean, cycle, election_date, ranked_choice_reallocated, cand_list) %>%
  mutate(r2p = pct / sum(pct) - partisan_lean) %>%
  ungroup() %>%
  left_join(historical_generic_ballot_averages %>% 
              select(election_date, avg_date, generic_ballot_avg = avg, generic_ballot_eff_n = eff_n,
                     generic_ballot_sd = sd, generic_ballot_se = se), 
            by = c("election_date", "median_date" = "avg_date")) %>%
  select(state, seat_name, election_date, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, start_date, 
         median_date, end_date, spread, pop = population, n = sample_size, population, tracking, internal, partisan, partisan_lean,
         cand_list, party, r2p, pct, generic_ballot_avg, generic_ballot_eff_n, generic_ballot_sd, generic_ballot_se) %>%
  filter(party == "rep") %>%
  mutate(r2p_lean = r2p - generic_ballot_avg)

## Senate averages over time (at the lean level)
poll_average_dates <- seq(as.Date("2016-11-08"), as.Date("2024-11-05"))
poll_average_df_list <- vector("list", length(poll_average_dates))
senate_election_dates <- as.Date(c("2018-11-06", "2020-11-03", "2022-11-08", "2024-11-05"))

start_time <- Sys.time()
for(i in seq_along(poll_average_dates)) {
  next_election_date <- min(senate_election_dates[senate_election_dates >= poll_average_dates[i]])
  filtered_polls <- historical_senate_polls_r2p %>%
    filter(end_date <= poll_average_dates[i],
           election_date == next_election_date) %>%
    mutate(poll_age = as.numeric(poll_average_dates[i] - median_date),
           r2p_lean = r2p_lean - partisan_lean,
           weight = ifelse(is.na(partisan), 5, 1) * ifelse(pop == "lv", 3, 1) * n^(0.25) / 
             (exp(poll_age^0.1 + 1) * ifelse(spread == 0, 3, 1)))
  poll_average_df_list[[i]] <- filtered_polls %>%
    filter(weight > 0) %>%
    select(state, seat_name, election_date, weight, party, r2p_lean) %>%
    group_by(state, seat_name, election_date, party = as.character(party)) %>%
    summarise(avg_lean = wtd.mean(r2p_lean, weight),
              eff_n = sum(weight)^2 / sum(weight^2),
              sd = sqrt(wtd.var(r2p_lean, weight, normwt = TRUE) * n() / (n() - 1.5)),
              .groups = "drop") %>%
    mutate(se = sd / sqrt(eff_n),
           avg_date = poll_average_dates[i],
           days_to_election = as.numeric(election_date - avg_date))
}
senate_average_leans <- bind_rows(poll_average_df_list) %>%
  mutate(year = year(election_date))
print(Sys.time() - start_time)

senate_polling_averages <- senate_average_leans %>%
  left_join(historical_generic_ballot_averages_smoothed %>%
              select(election_date, avg_date, generic_ballot_avg = avg, generic_ballot_se = se), 
            by = c("election_date", "avg_date")) %>%
  mutate(avg = avg_lean + generic_ballot_avg) %>%
  left_join(historical_senate_results %>% 
              mutate(seat_name = case_when(class == 1 ~ "Class I",
                                           class == 2 ~ "Class II",
                                           class == 3 ~ "Class III")) %>%
              select(year, state, seat_name, actual_r2p = rep),
            by = c("year", "state", "seat_name"))

# House district polls
house_candlists <- house_2026_data %>%
  select(state, seat_number, cand1_name, cand2_name, cand3_name, cand4_name) %>%
  melt(id.vars = c("state", "seat_number"), variable.name = "candidate_num", value.name = "candidate") %>%
  filter(!is.na(candidate)) %>%
  arrange(state, seat_number, candidate) %>% 
  group_by(state, seat_number) %>%
  summarise(cand_list = paste(candidate, collapse = "|")) %>%
  ungroup()

house_district_leans_2026 <- house_polls_2026_raw %>%
  filter(stage == "general", state != "US", !str_detect(candidate_name, "Don't know|Neither|Would not vote")) %>%
  group_by(question_id) %>%
  filter(!(any(candidate_name %>% str_detect("Generic"))), party %in% c("REP", "DEM")) %>%
  ungroup() %>%
  mutate(candidate_name = case_when(candidate_name == "JoAnna Mendonza" ~ "JoAnna Mendoza",
                                    TRUE ~ candidate_name)) %>%
  arrange(state, seat_number, candidate_name) %>%
  group_by(question_id, state, seat_number) %>%
  mutate(cand_list = paste(candidate_name, collapse = "|"),
         state = state_abbr_decoder(state)) %>%
  ungroup() %>%
  inner_join(house_candlists, by = c("state", "seat_number", "cand_list")) %>% 
  group_by(state, seat_number, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, 
           start_date, end_date, population, tracking, internal, partisan, cycle, election_date, 
           ranked_choice_reallocated, cand_list, party, candidate_name) %>%
  summarise(sample_size = mean(sample_size),
            pct = mean(pct)) %>%
  mutate_at(c("election_date", "start_date", "end_date"), as.Date, format = "%m/%d/%y") %>%
  mutate(year = year(election_date),
         spread = as.numeric(round((end_date - start_date) / 2)),
         median_date = start_date + spread,
         days_to_election = as.numeric(election_date - median_date),
         method_weight = case_when(methodology %>% str_detect("Opt-In") ~ 0.1,
                                   methodology == "Nonprobability Panel" ~ 0.25,
                                   methodology == "Text-to-Web" ~ 0.33,
                                   is.na(methodology) ~ 0.5,
                                   methodology == "Probability Panel" ~ 0.75,
                                   TRUE ~ 1.0),
         party = case_when(party == "DEM" ~ "dem",
                           party == "REP" ~ "rep",
                           candidate_name %in% ind_dems ~ "dem"),
         partisan_lean = case_when(partisan == "DEM" ~ -0.02,
                                   partisan == "REP" ~ 0.02,
                                   partisan == "IND" ~ -0.02,
                                   is.na(partisan) ~ 0.0,
                                   TRUE ~ 0.0)) %>%
  filter(!is.na(sample_size), !is.na(population), population != "a") %>%
  # If there's an LV result and other results, drop the others
  group_by(poll_id) %>%
  mutate(keep_lv = ifelse(any(population == "lv"), population == "lv", population %in% c("rv", "v"))) %>%
  ungroup() %>%
  filter(keep_lv) %>%
  group_by(state, seat_number, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, method_weight,
           start_date, median_date, end_date, spread, sample_size, population, keep_lv, tracking, internal, partisan, 
           partisan_lean, cycle, election_date, ranked_choice_reallocated, cand_list, party) %>%
  summarise(pct = sum(pct)) %>%
  group_by(state, seat_number, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, method_weight,
           start_date, median_date, end_date, spread, sample_size, population, keep_lv, tracking, internal, partisan, 
           partisan_lean, cycle, election_date, ranked_choice_reallocated, cand_list) %>%
  mutate(r2p = pct / sum(pct) - partisan_lean) %>%
  ungroup() %>%
  left_join(generic_ballot_averages_2026_smoothed %>% 
              select(avg_date, generic_ballot_avg = avg, generic_ballot_eff_n = eff_n,
                     generic_ballot_sd = sd, generic_ballot_se = se), 
            by = c("median_date" = "avg_date")) %>%
  mutate(poll_age = as.numeric(today() - median_date),
         r2p_lean = r2p - partisan_lean - generic_ballot_avg,
         weight = method_weight * ifelse(is.na(partisan), 5, 1) * ifelse(population == "lv", 3, 1) * sample_size^(0.25) / 
           (exp((poll_age + 7)^0.1) * ifelse(spread == 0, 3, 1))) %>%
  select(state, seat_number, election_date, poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, start_date, 
         median_date, end_date, spread, pop = population, n = sample_size, population, tracking, internal, partisan, partisan_lean,
         cand_list, party, weight, r2p_lean, generic_ballot_avg, generic_ballot_eff_n, generic_ballot_sd, generic_ballot_se) %>%
  filter(party == "rep")

if(!exists("most_recent_n_house_district_polls")) {
  most_recent_n_house_district_polls <- nrow(house_district_leans_2026)
}

if(nrow(house_district_leans_2026) > most_recent_n_house_district_polls) {
  print("New House district-level polls detected!")
} else {
  print("No new House district-level polls detected.")
} 
most_recent_n_house_district_polls <- nrow(house_district_leans_2026)

### Get average leans
house_district_average_leans_2026 <- house_district_leans_2026 %>%
  group_by(state, seat_number) %>%
  summarise(avg_lean = wtd.mean(r2p_lean, weight),
            total_n = sum(n),
            sd = sqrt(wtd.var(r2p_lean, weight, normwt = TRUE)),
            eff_n = sum(weight)^2 / sum(weight^2)) %>%
  mutate(sd = ifelse(is.na(sd), sqrt(0.25 / total_n), sd),
         se = sd / sqrt(eff_n)) %>%
  ungroup()

