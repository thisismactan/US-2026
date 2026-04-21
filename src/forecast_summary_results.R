source("src/senate_sim.R")

# Forecast timelines ####
## Create them if they don't exist yet
if(!("house_forecast_timeline.csv" %in% list.files("output"))) {
  house_forecast_timeline <- house_seat_sims %>% 
    melt(id.vars = "sim_id", variable.name = "party", value.name = "seats") %>% 
    left_join(natl_r2p_sims %>%
                select(sim_id, rep = natl_r2p) %>%
                mutate(dem = 1 - rep) %>%
                melt(id.vars = "sim_id", variable.name = "party", value.name = "vote"),
              by = c("sim_id", "party")) %>%
    group_by(party = as.character(party)) %>% 
    summarise(prob_majority = mean(seats > 435/2), 
              seats_pct_05 = quantile(seats, 0.05), 
              seats_avg = mean(seats), 
              seats_pct_95 = quantile(seats, 0.95),
              vote_pct_05 = quantile(vote, 0.05),
              vote_avg = mean(vote),
              vote_pct_95 = quantile(vote, 0.95)) %>%
    ungroup() %>%
    mutate(forecast_date = today()) %>% 
    select(forecast_date, everything())
  
  write_csv(house_forecast_timeline, "output/house_forecast_timeline.csv")
}

if(!("house_district_forecast_timeline.csv" %in% list.files("output"))) {
  house_district_forecast_timeline <- house_district_posterior_summary_stats %>%
    mutate(forecast_date = today()) %>%
    ungroup() %>%
    rename(r_pct_05 = pct_05, r_avg = avg, r_pct_95 = pct_95) %>%
    select(forecast_date, everything())
  
  write_csv(house_district_forecast_timeline, "output/house_district_forecast_timeline.csv")
}

if(!("senate_forecast_timeline.csv" %in% list.files("output"))) {
  senate_forecast_timeline <- senate_seat_sims %>%
    mutate(forecast_date = today()) %>%
    melt(id.vars = c("forecast_date", "sim_id"), variable.name = "party", value.name = "seats") %>%
    group_by(party = as.character(party)) %>%
    summarise(prob_majority_rep = mean(seats >= 50),
              prob_majority_dem = mean(seats > 50),
              seats_pct_05 = quantile(seats, 0.05),
              seats_avg = mean(seats),
              seats_pct_95 = quantile(seats, 0.95)) %>%
    mutate(prob_majority = ifelse(party == "rep", prob_majority_rep, prob_majority_dem),
           forecast_date = today()) %>%
    select(forecast_date, party, prob_majority, seats_pct_05, seats_avg, seats_pct_95)
  
  write_csv(senate_forecast_timeline, "output/senate_forecast_timeline.csv")
}

if(!("senate_state_forecast_timeline.csv" %in% list.files("output"))) {
  senate_state_forecast_timeline <- senate_state_posterior_summary_stats %>%
    mutate(forecast_date = today()) %>%
    ungroup() %>%
    rename(r_pct_05 = pct_05, r_avg = avg, r_pct_95 = pct_95) %>%
    select(forecast_date, everything())
  
  write_csv(senate_state_forecast_timeline, "output/senate_state_forecast_timeline.csv")
}

## Now once they're definitely there, add today's
today_forecast_list <- list(
  house_forecast = house_seat_sims %>% 
    melt(id.vars = "sim_id", variable.name = "party", value.name = "seats") %>% 
    left_join(natl_r2p_sims %>%
                select(sim_id, rep = natl_r2p) %>%
                mutate(dem = 1 - rep) %>%
                melt(id.vars = "sim_id", variable.name = "party", value.name = "vote"),
              by = c("sim_id", "party")) %>%
    group_by(party = as.character(party)) %>% 
    summarise(prob_majority = mean(seats > 435/2), 
              seats_pct_05 = quantile(seats, 0.05), 
              seats_avg = mean(seats), 
              seats_pct_95 = quantile(seats, 0.95),
              vote_pct_05 = quantile(vote, 0.05),
              vote_avg = mean(vote),
              vote_pct_95 = quantile(vote, 0.95)) %>%
    ungroup() %>%
    mutate(forecast_date = today()) %>% 
    select(forecast_date, everything()),
  house_district_forecast = house_district_posterior_summary_stats %>%
    mutate(forecast_date = today()) %>%
    ungroup() %>%
    rename(r_pct_05 = pct_05, r_avg = avg, r_pct_95 = pct_95) %>%
    select(forecast_date, everything()),
  senate_forecast = senate_seat_sims %>%
    mutate(forecast_date = today()) %>%
    melt(id.vars = c("forecast_date", "sim_id"), variable.name = "party", value.name = "seats") %>%
    group_by(party = as.character(party)) %>%
    summarise(prob_majority_rep = mean(seats >= 50),
              prob_majority_dem = mean(seats > 50),
              seats_pct_05 = quantile(seats, 0.05),
              seats_avg = mean(seats),
              seats_pct_95 = quantile(seats, 0.95)) %>%
    mutate(prob_majority = ifelse(party == "rep", prob_majority_rep, prob_majority_dem),
           forecast_date = today()) %>%
    select(forecast_date, party, prob_majority, seats_pct_05, seats_avg, seats_pct_95),
  senate_state_forecast = senate_state_posterior_summary_stats %>%
    mutate(forecast_date = today()) %>%
    ungroup() %>%
    rename(r_pct_05 = pct_05, r_avg = avg, r_pct_95 = pct_95) %>%
    select(forecast_date, everything())
)

house_forecast_timeline <- read_csv("output/house_forecast_timeline.csv") %>%
  filter(forecast_date < today()) %>%
  bind_rows(today_forecast_list$house_forecast)

house_district_forecast_timeline <- read_csv("output/house_district_forecast_timeline.csv") %>%
  filter(forecast_date < today()) %>%
  bind_rows(today_forecast_list$house_district_forecast)

senate_forecast_timeline <- read_csv("output/senate_forecast_timeline.csv") %>%
  filter(forecast_date < today()) %>%
  bind_rows(today_forecast_list$senate_forecast)

senate_state_forecast_timeline <- read_csv("output/senate_state_forecast_timeline.csv") %>%
  filter(forecast_date < today()) %>%
  bind_rows(today_forecast_list$senate_state_forecast)

write_csv(house_forecast_timeline, "output/house_forecast_timeline.csv")
write_csv(house_district_forecast_timeline, "output/house_district_forecast_timeline.csv")
write_csv(senate_forecast_timeline, "output/senate_forecast_timeline.csv")
write_csv(senate_state_forecast_timeline, "output/senate_state_forecast_timeline.csv")

# Actual viewing of summary stuff ####
## House district-level forecasts for presentation
house_district_posterior_summary_stats %>%
  mutate(r_prob = percent(r_prob, accuracy = 1),
         pct_05 = percent(pct_05, accuracy = 0.1),
         avg = percent(avg, accuracy = 0.1),
         pct_95 = percent(pct_95, accuracy = 0.1)) %>%
  ungroup() %>%
  select(State = state, District = seat_number, `Prob(R)` = r_prob, `5th percentile` = pct_05, Average = avg, 
         `95th percentile` = pct_95) %>%
  print(n = Inf)

## Latest House forecast
house_forecast_timeline %>%
  tail(2)

## Senate state-level forecasts for presentation
senate_state_posterior_summary_stats %>%
  mutate(r_prob = percent(r_prob, accuracy = 1),
         pct_05 = percent(pct_05, accuracy = 0.1),
         avg = percent(avg, accuracy = 0.1),
         pct_95 = percent(pct_95, accuracy = 0.1)) %>%
  ungroup() %>%
  select(State = state, Class = seat_name, `Prob(R)` = r_prob, `5th percentile` = pct_05, Average = avg, 
         `95th percentile` = pct_95) %>%
  print(n = Inf)

## Latest Senate forecast
senate_forecast_timeline %>%
  tail(3)

## Joint probability distribution of House and Senate majorities
senate_seat_sims %>% 
  mutate(senate_maj = ifelse(rep >= 50, "Republican", "Democratic")) %>%
  select(sim_id, senate_maj) %>%
  left_join(house_seat_sims %>%
              mutate(house_maj = ifelse(rep > 435/2, "Republican", "Democratic")) %>%
              select(sim_id, house_maj),
            by = "sim_id") %>%
  group_by(senate_maj, house_maj) %>%
  summarise(prob = n() / n_sims) %>% 
  spread(senate_maj, prob)

senate_state_posterior_summary_stats %>%
  print(n = Inf)
