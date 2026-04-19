library(tidyverse)
library(reshape2)
library(readxl)
library(stringr)

cd_pres_results_list <- vector("list", length(files_list))

# 2010 boundaries, 2008 results
cd_2010_pres_2008 <- read_excel("data/cd-pres-results/cd_2010_pres_2008.xlsx", sheet = "Results",
                                skip = 1) %>%
  mutate(last_r2p = McCain / (Obama + McCain),
         state_abbr = sapply(str_split(CD, "-"), function(x) x[1]),
         seat_number = sapply(str_split(CD, "-"), function(x) x[2]),
         seat_number = ifelse(seat_number == "AL", 1, as.numeric(seat_number)),
         year = 2010,
         pres_year = 2008) %>%
  select(year, state_abbr, seat_number, pres_year, last_r2p) %>%
  na.omit()

# 2012 boundaries, 2008 results
cd_2012_pres_2008 <- read_excel("data/cd-pres-results/cd_2012_pres_2008.xlsx", sheet = "Results",
                                skip = 1) %>%
  mutate(obama = as.numeric(`Obama...4`),
         mccain = as.numeric(`McCain...5`)) %>%
  mutate(last_r2p = mccain / (obama + mccain),
         state_abbr = sapply(str_split(`District...1`, "-"), function(x) x[1]),
         seat_number = sapply(str_split(`District...1`, "-"), function(x) x[2]),
         seat_number = ifelse(seat_number == "AL", 1, as.numeric(seat_number)),
         year = 2012,
         pres_year = 2008) %>%
  select(year, state_abbr, seat_number, pres_year, last_r2p) %>% 
  na.omit()

# 2014 boundaries, 2012 results
cd_2014_pres_2012 <- read_excel("data/cd-pres-results/cd_2014_pres_2012.xlsx", sheet = "Vote totals",
                                skip = 1) %>%
  mutate(last_r2p = Romney / (Obama...5 + Romney),
         state_abbr = sapply(str_split(CD, "-"), function(x) x[1]),
         seat_number = sapply(str_split(CD, "-"), function(x) x[2]),
         seat_number = ifelse(seat_number == "AL", 1, as.numeric(seat_number)),
         year = 2014,
         pres_year = 2012) %>%
  select(year, state_abbr, seat_number, pres_year, last_r2p) %>% 
  na.omit()

# 2016 boundaries, 2012 results
cd_2016_pres_2012 <- read_excel("data/cd-pres-results/cd_2016_pres_2012.xlsx", sheet = "Vote totals",
                                skip = 1) %>%
  mutate(last_r2p = Trump / (Clinton + Trump),
         state_abbr = sapply(str_split(CD, "-"), function(x) x[1]),
         seat_number = sapply(str_split(CD, "-"), function(x) x[2]),
         seat_number = ifelse(seat_number == "AL", 1, as.numeric(seat_number)),
         year = 2016,
         pres_year = 2012) %>%
  select(year, state_abbr, seat_number, pres_year, last_r2p) %>% 
  na.omit()

# 2018 boundaries, 2016 results
cd_2018_pres_2016 <- read_excel("data/cd-pres-results/cd_2018_pres_2016.xlsx", sheet = "Vote totals",
                                skip = 1) %>%
  mutate(last_r2p = Trump...12 / (Clinton + Trump...12),
         state_abbr = sapply(str_split(District, "-"), function(x) x[1]),
         seat_number = sapply(str_split(District, "-"), function(x) x[2]),
         seat_number = ifelse(seat_number == "AL", 1, as.numeric(seat_number)),
         year = 2018,
         pres_year = 2016) %>%
  select(year, state_abbr, seat_number, pres_year, last_r2p) %>% 
  na.omit()

# 2020 boundaries, 2016 results
cd_2020_pres_2016 <- read_excel("data/cd-pres-results/cd_2020_pres_2016.xlsx", sheet = "Vote totals",
                                skip = 1) %>%
  mutate(last_r2p = Trump...12 / (Clinton + Trump...12),
         state_abbr = sapply(str_split(District, "-"), function(x) x[1]),
         seat_number = sapply(str_split(District, "-"), function(x) x[2]),
         seat_number = ifelse(seat_number == "AL", 1, as.numeric(seat_number)),
         year = 2020,
         pres_year = 2016) %>%
  select(year, state_abbr, seat_number, pres_year, last_r2p) %>% 
  na.omit()

# 2022 boundaries, 2020 results
cd_2022_pres_2020 <- read_excel("data/cd-pres-results/cd_2022_pres_2020.xlsx", sheet = "Vote totals") %>%
  mutate(last_r2p = Trump / (Biden + Trump),
         state_abbr = sapply(str_split(District, "-"), function(x) x[1]),
         seat_number = sapply(str_split(District, "-"), function(x) x[2]),
         seat_number = ifelse(seat_number == "AL", 1, as.numeric(seat_number)),
         year = 2022,
         pres_year = 2020) %>%
  select(year, state_abbr, seat_number, pres_year, last_r2p) %>% 
  na.omit()

# 2024 boundaries, 2020 results
cd_2024_pres_2020 <- read_excel("data/cd-pres-results/cd_2024_pres_2020.xlsx", sheet = "Vote totals",
                                skip = 2) %>%
  mutate(last_r2p = Trump...12 / (Biden + Trump...12),
         state_abbr = sapply(str_split(...1, "-"), function(x) x[1]),
         seat_number = sapply(str_split(...1, "-"), function(x) x[2]),
         seat_number = ifelse(seat_number == "AL", 1, as.numeric(seat_number)),
         year = 2024,
         pres_year = 2020) %>%
  select(year, state_abbr, seat_number, pres_year, last_r2p) %>% 
  na.omit()

# Stick them all together
cd_pres_results <- bind_rows(cd_2010_pres_2008, cd_2012_pres_2008, cd_2014_pres_2012,
                             cd_2016_pres_2012, cd_2018_pres_2016, cd_2020_pres_2016,
                             cd_2022_pres_2020, cd_2024_pres_2020) %>%
  left_join(read_csv("data/state_divisions.csv") %>% select(state, state_abbr = abbrev),
            by = "state_abbr") %>%
  select(year, state, seat_number, pres_year, last_pres_r2p = last_r2p)

write_csv(cd_pres_results, "data/cd-pres-results/cd_pres_results.csv")
