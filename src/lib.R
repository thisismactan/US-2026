# Data handling
library(tidyverse)
library(tidybayes)
library(reshape2)
library(mvnfast)
library(scales)
library(Hmisc)
library(zoo)

# Bayesian stuff
library(cmdstanr)
library(rstan)

# Other nonsense
library(geomViolinDiscrete)
default_palette <- "Okabe-Ito" # let's do our little part to not be ableist here
showtext::showtext_auto()

party_colors <- c("rep" = "red",
                  "dem" = "blue")

party_labels <- c("rep" = "Republican",
                  "dem" = "Democratic")

logit <- function(p) {
  return(log(p / (1 - p)))
}

invlogit <- function(x) {
  return(exp(x) / (1 + exp(x)))
}

district_abbr_encoder <- function(state, seat_number) {
  state_abbr <- case_when(state == "Alabama" ~ "AL",
                          state == "Alaska" ~ "AK",
                          state == "Arkansas" ~ "AR",
                          state == "Arizona" ~ "AZ",
                          state == "California" ~ "CA",
                          state == "Colorado" ~ "CO",
                          state == "Connecticut" ~ "CT",
                          state == "Delaware" ~ "DE",
                          state == "Florida" ~ "FL",
                          state == "Georgia" ~ "GA",
                          state == "Hawaii" ~ "HI",
                          state == "Idaho" ~ "ID",
                          state == "Illinois" ~ "IL",
                          state == "Indiana" ~ "IN",
                          state == "Iowa" ~ "IA",
                          state == "Kansas" ~ "KS",
                          state == "Kentucky" ~ "KY",
                          state == "Louisiana" ~ "LA",
                          state == "Maine" ~ "ME",
                          state == "Maryland" ~ "MD",
                          state == "Massachusetts" ~ "MA",
                          state == "Michigan" ~ "MI",
                          state == "Minnesota" ~ "MN",
                          state == "Mississippi" ~ "MS",
                          state == "Missouri" ~ "MO",
                          state == "Montana" ~ "MT",
                          state == "Nebraska" ~ "NE",
                          state == "Nevada" ~ "NV",
                          state == "New Hampshire" ~ "NH",
                          state == "New Jersey" ~ "NJ",
                          state == "New Mexico" ~ "NM",
                          state == "New York" ~ "NY",
                          state == "North Carolina" ~ "NC",
                          state == "North Dakota" ~ "ND",
                          state == "Ohio" ~ "OH",
                          state == "Oklahoma" ~ "OK",
                          state == "Oregon" ~ "OR",
                          state == "Pennsylvania" ~ "PA",
                          state == "Rhode Island" ~ "RI",
                          state == "South Carolina" ~ "SC",
                          state == "South Dakota" ~ "SD",
                          state == "Tennessee" ~ "TN",
                          state == "Texas" ~ "TX",
                          state == "Utah" ~ "UT",
                          state == "Vermont" ~ "VT",
                          state == "Virginia" ~ "VA",
                          state == "Washington" ~ "WA",
                          state == "West Virginia" ~ "WV",
                          state == "Wisconsin" ~ "WI",
                          state == "Wyoming" ~ "WY")
  seat_number_full <- ifelse(seat_number < 10, paste0("0", as.character(seat_number)), as.character(seat_number))
  
  return(paste(state_abbr, seat_number_full, sep = "-"))
}

state_abbr_decoder <- function(state_abbr) {
  state <- case_when(state_abbr == "AL" ~ "Alabama",
                     state_abbr == "AK" ~ "Alaska",
                     state_abbr == "AR" ~ "Arkansas",
                     state_abbr == "AZ" ~ "Arizona",
                     state_abbr == "CA" ~ "California",
                     state_abbr == "CO" ~ "Colorado",
                     state_abbr == "CT" ~ "Connecticut",
                     state_abbr == "DE" ~ "Delaware",
                     state_abbr == "FL" ~ "Florida",
                     state_abbr == "GA" ~ "Georgia",
                     state_abbr == "HI" ~ "Hawaii",
                     state_abbr == "ID" ~ "Idaho",
                     state_abbr == "IL" ~ "Illinois",
                     state_abbr == "IN" ~ "Indiana",
                     state_abbr == "IA" ~ "Iowa",
                     state_abbr == "KS" ~ "Kansas",
                     state_abbr == "KY" ~ "Kentucky",
                     state_abbr == "LA" ~ "Louisiana",
                     state_abbr == "ME" ~ "Maine",
                     state_abbr == "MD" ~ "Maryland",
                     state_abbr == "MA" ~ "Massachusetts",
                     state_abbr == "MI" ~ "Michigan",
                     state_abbr == "MN" ~ "Minnesota",
                     state_abbr == "MS" ~ "Mississippi",
                     state_abbr == "MO" ~ "Missouri",
                     state_abbr == "MT" ~ "Montana",
                     state_abbr == "NE" ~ "Nebraska",
                     state_abbr == "NV" ~ "Nevada",
                     state_abbr == "NH" ~ "New Hampshire",
                     state_abbr == "NJ" ~ "New Jersey",
                     state_abbr == "NM" ~ "New Mexico",
                     state_abbr == "NY" ~ "New York",
                     state_abbr == "NC" ~ "North Carolina",
                     state_abbr == "ND" ~ "North Dakota",
                     state_abbr == "OH" ~ "Ohio",
                     state_abbr == "OK" ~ "Oklahoma",
                     state_abbr == "OR" ~ "Oregon",
                     state_abbr == "PA" ~ "Pennsylvania",
                     state_abbr == "RI" ~ "Rhode Island",
                     state_abbr == "SC" ~ "South Carolina",
                     state_abbr == "SD" ~ "South Dakota",
                     state_abbr == "TN" ~ "Tennessee",
                     state_abbr == "TX" ~ "Texas",
                     state_abbr == "UT" ~ "Utah",
                     state_abbr == "VT" ~ "Vermont",
                     state_abbr == "VA" ~ "Virginia",
                     state_abbr == "WA" ~ "Washington",
                     state_abbr == "WV" ~ "West Virginia",
                     state_abbr == "WI" ~ "Wisconsin",
                     state_abbr == "WY" ~ "Wyoming")
  
  return(state)
}

state_int_encoder <- function(state, seat_number) {
  state_code <- case_when(state == "Alabama" ~ 1,
                          state == "Alaska" ~ 2,
                          state == "Arkansas" ~ 3,
                          state == "Arizona" ~ 4,
                          state == "California" ~ 5,
                          state == "Colorado" ~ 6,
                          state == "Connecticut" ~ 7,
                          state == "Delaware" ~ 8,
                          state == "Florida" ~ 9,
                          state == "Georgia" ~ 10,
                          state == "Hawaii" ~ 11,
                          state == "Idaho" ~ 12,
                          state == "Illinois" ~ 13,
                          state == "Indiana" ~ 14,
                          state == "Iowa" ~ 15,
                          state == "Kansas" ~ 16,
                          state == "Kentucky" ~ 17,
                          state == "Louisiana" ~ 18,
                          state == "Maine" ~ 19,
                          state == "Maryland" ~ 20,
                          state == "Massachusetts" ~ 21,
                          state == "Michigan" ~ 22,
                          state == "Minnesota" ~ 23,
                          state == "Mississippi" ~ 24,
                          state == "Missouri" ~ 25,
                          state == "Montana" ~ 26,
                          state == "Nebraska" ~ 27,
                          state == "Nevada" ~ 28,
                          state == "New Hampshire" ~ 29,
                          state == "New Jersey" ~ 30,
                          state == "New Mexico" ~ 31,
                          state == "New York" ~ 32,
                          state == "North Carolina" ~ 33,
                          state == "North Dakota" ~ 34,
                          state == "Ohio" ~ 35,
                          state == "Oklahoma" ~ 36,
                          state == "Oregon" ~ 37,
                          state == "Pennsylvania" ~ 38,
                          state == "Rhode Island" ~ 39,
                          state == "South Carolina" ~ 40,
                          state == "South Dakota" ~ 41,
                          state == "Tennessee" ~ 42,
                          state == "Texas" ~ 43,
                          state == "Utah" ~ 44,
                          state == "Vermont" ~ 45,
                          state == "Virginia" ~ 46,
                          state == "Washington" ~ 47,
                          state == "West Virginia" ~ 48,
                          state == "Wisconsin" ~ 49,
                          state == "Wyoming" ~ 50)
  
  return(state_code)
}


