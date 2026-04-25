source("src/lib.R")

# House data
## Lot of official names for state Democratic parties
dem_parties <- c("DEMOCRAT", "DEMOCRATIC", "DEMOCRATIC-FARM-LABOR", "DEMOCRATIC-FARMER-LABOR",
                 "DEMOCRATIC-NONPARTISAN LEAGUE", "DEMOCRATIC-NPL")

## Read the historical House data
house_76_24 <- read_csv("data/1976-2024-house-fixed.csv") %>%
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
         pct = candidatevotes / totalvotes) %>%
  filter(state != "District Of Columbia")

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

## Calculate state-level House vote and 2-party Republican vote share
## Calculate national-level House vote and 2-party Republican vote share
house_state_pct <- house_76_24 %>%
  mutate(party = ifelse(party %in% dem_parties, "DEMOCRAT", party)) %>%
  group_by(year, state, party) %>%
  summarise(state_votes = sum(candidatevotes, na.rm = TRUE)) %>%
  mutate(totalvotes = sum(state_votes, na.rm = TRUE),
         state_pct = state_votes / totalvotes)

house_state_r2p <- house_state_pct %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  select(year, state, party, state_votes) %>%
  spread(party, state_votes) %>%
  mutate(state_r2p = REPUBLICAN / (REPUBLICAN + DEMOCRAT))

house_state_r2p

## Also get most recent available presidential election results (2 years ago for midterms; 4 years ago for pres years)
if (!("cd_pres_results.csv" %in% list.files("data/cd-pres-results"))) {
  source("src/shape_cd_pres_results.R")
}

cd_pres_results <- read_csv("data/cd-pres-results/cd_pres_results.csv")

## Also grab the top-two primary data from districts that do it
historical_top_two_primary <- read_csv("data/rep_two_party_primary_results_historical.csv") %>%
  filter(general_pct != 0, general_pct != 1) %>%
  mutate(midterm = year %% 4 == 2) 

## Also grab hand-crafted regional divisions
state_divisions <- read_csv("data/state_divisions.csv")

## Lump everything together into an election results dataset
house_election_data <- house_district_r2p %>%
  select(year, state, seat_number, r2p) %>%
  left_join(house_state_r2p %>% select(year, state, state_r2p), by = c("year", "state")) %>%
  left_join(house_natl_r2p %>% select(year, natl_r2p), by = "year") %>%
  arrange(state, seat_number, year) %>%
  group_by(state, seat_number) %>%
  mutate(last_r2p = lag(r2p),
         r2p_change = r2p - lag(r2p),
         last_state_r2p = lag(state_r2p),
         state_r2p_change = r2p - lag(state_r2p),
         last_natl_r2p = lag(natl_r2p),
         natl_r2p_change = natl_r2p - lag(natl_r2p)) %>%
  left_join(house_contested, by = c("year", "state", "seat_number")) %>%
  left_join(house_incumbents_running %>% select(year, state, seat_number, incumbent_running, redistricted), 
            by = c("year", "state", "seat_number")) %>%
  left_join(cd_pres_results, by = c("year", "state", "seat_number")) %>%
  filter(contested, !is.na(natl_r2p_change), !is.na(r2p_change), year >= 2010, r2p < 0.95) %>%
  ungroup() %>%
  mutate(logit_r2p = log(r2p / (1 - r2p))) %>%
  left_join(historical_top_two_primary %>% select(-midterm), by = c("year", "state", "seat_number")) %>%
  left_join(state_divisions %>% select(state, region), by = "state")

# Generic congressional ballot polling
historical_generic_ballot_polls <- read_csv("data/polls/generic_ballot_polls_historical.csv") %>%
  select(poll_id, pollster_id, pollster, sponsor_ids, sponsor_candidate_party, methodology, start_date, end_date,
         question_id, sample_size, population, tracking, internal, partisan, cycle, election_date, dem, rep, ind) %>%
  mutate_at(c("election_date", "start_date", "end_date"), as.Date, format = "%m/%d/%y") %>%
  mutate(spread = as.numeric(round((end_date - start_date) / 2)),
         median_date = start_date + spread,
         days_to_election = as.numeric(election_date - median_date),
         poll_r2p = rep / (dem + rep)) %>%
  filter(!is.na(sample_size), !is.na(population)) %>%
  left_join(house_natl_r2p %>% select(cycle = year, natl_r2p), by = "cycle")

# House data for the election to come
## Need last_r2p, natl_r2p_change, last_pres_r2p, incumbent_running, midterm, redistricted
cd_pres_results_2026 <- read_csv("data/cd-pres-results/cd_2026_pres_2024.csv")

house_2026_data <- read_csv("data/district_data_2026.csv") %>% 
  left_join(house_district_r2p %>%
              left_join(house_natl_r2p %>% select(year, natl_r2p), by = "year") %>%
              filter(year == 2024) %>%
              mutate(year = 2026) %>%
              select(year, state, seat_number, last_r2p = r2p, last_natl_r2p = natl_r2p),
            by = c("year", "state", "seat_number")) %>%
  left_join(cd_pres_results_2026, by = c("year", "state", "seat_number")) %>%
  mutate(contested_last = (last_r2p > 0) & (last_r2p < 1)) %>%
  left_join(state_divisions %>% select(state, region), by = "state")

# Senate data
# Presidential leans by state and year
historical_presidential_results <- read_csv("data/presidential_election_results_by_state.csv")

historical_national_presidential_results <- historical_presidential_results %>%
  group_by(year, party) %>%
  summarise(votes = sum(votes)) %>%
  group_by(year) %>%
  mutate(votes = votes / sum(votes)) %>%
  spread(party, votes) %>%
  mutate(natl_margin = Democratic - Republican) %>%
  dplyr::select(year, natl_margin)

presidential_leans <- historical_presidential_results %>%
  group_by(year, state, party) %>%
  summarise(votes = sum(votes)) %>%
  group_by(year, state) %>%
  mutate(votes = votes / sum(votes)) %>%
  spread(party, votes) %>%
  mutate(state_margin = Republican - Democratic) %>%
  dplyr::select(year, state, state_margin) %>%
  left_join(historical_national_presidential_results, by = "year") %>%
  mutate(pres_lean = state_margin - natl_margin) %>%
  ungroup()

historical_senate_incumbents <- read_csv("data/historical_senate_incumbents.csv")

historical_senate_results <- read_csv("data/1976-2024-senate.csv", lazy = FALSE) %>%
  filter(year >= 2010, stage == "gen", grepl("democrat|republican", party_simplified, ignore.case = TRUE)) %>%
  mutate(state = str_to_title(state)) %>%
  dplyr::select(year, state, special, party = party_simplified, candidatevotes) %>%
  left_join(historical_senate_incumbents, by = c("state", "year", "special")) %>%
  mutate(party = case_when(grepl("republican", party, ignore.case = TRUE) ~ "rep",
                           grepl("democrat", party, ignore.case = TRUE) ~ "dem")) %>%
  group_by(year, state, class, special, democrat_running, republican_running, incumbent_running, incumbent_first_elected, party,
           dem_statewide_elected, rep_statewide_elected) %>%
  summarise(party_votes = sum(candidatevotes)) %>%
  group_by(year, state, class) %>%
  mutate(party_votes = party_votes / sum(party_votes)) %>%
  spread(party, party_votes, fill = 0) %>%
  ungroup() %>%
  arrange(state, class, year) %>%
  mutate(margin = rep - dem) %>%
  group_by(state, class) %>%
  mutate(last_margin = lag(margin),
         last_incumbent = lag(incumbent_running),
         open_seat = incumbent_running == "None",
         contested_last = lag(democrat_running) & lag(republican_running)) %>%
  left_join(house_natl_r2p %>% select(year, natl_r2p), by = "year") %>%
  group_by(state, class) %>%
  arrange(state, class, year) %>%
  mutate(natl_r2p_change = natl_r2p - lag(natl_r2p)) %>%
  ungroup()

last_senate_results <- historical_senate_results %>%
  group_by(state, class) %>%
  dplyr::slice(n()) %>%
  ungroup()

historical_senate_results_filtered <- historical_senate_results %>%

  # Remove elections where either this election or the last one went uncontested or where the incumbent or the last incumbent was an independent
  filter(democrat_running, republican_running, incumbent_running != "IND", last_incumbent != "IND") 

senate_election_data <- historical_senate_results_filtered %>%
  rename(r2p = margin,
         last_r2p = last_margin) %>%
  mutate(r2p = 0.5 + r2p / 2,
         last_r2p = 0.5 + last_r2p / 2,
         midterm = year %% 4 == 2,
         incumbent_running = case_when(incumbent_running == "DEM" ~ 1,
                                       incumbent_running == "None" ~ 2,
                                       incumbent_running == "REP" ~ 3),
         last_pres_year = floor(year / 4) * 4) %>%
  left_join(presidential_leans %>% select(year, state, last_pres_r2p = state_margin), by = c("last_pres_year" = "year", "state")) %>%
  mutate(last_pres_r2p = 0.5 + last_pres_r2p / 2) %>%
  left_join(state_divisions %>% select(state, region), by = "state")

senate_2026_data <- read_csv("data/senate_candidates_2026.csv") %>%
  group_by(state, seat_name) %>%
  mutate(contested = !any(candidate_fullname == "None")) %>%
  ungroup() %>%
  distinct(state, seat_name, incumbent_running, contested) %>%
  mutate(class = case_when(seat_name == "Class I" ~ 1,
                           seat_name == "Class II" ~ 2,
                           seat_name == "Class III" ~ 3),
         midterm = TRUE) %>%
  left_join(last_senate_results %>%
              mutate(contested_last = democrat_running & republican_running) %>%
              select(last_election_year = year, state, class, last_r2p = rep, contested_last), 
            by = c("state", "class")) %>%
  left_join(presidential_leans %>% filter(year == 2024) %>% select(state, last_pres_r2p = state_margin),
            by = "state") %>%
  left_join(house_natl_r2p %>% select(year, last_natl_r2p = natl_r2p),
            by = c("last_election_year" = "year")) %>%
  mutate(last_pres_r2p = 0.5 + last_pres_r2p / 2,
         incumbent_running = case_when(incumbent_running == "DEM" ~ 1,
                                       incumbent_running == "None" ~ 2,
                                       incumbent_running == "REP" ~ 3)) %>%
  left_join(state_divisions %>% select(state, region), by = "state")

