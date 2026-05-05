source("src/house_sim.R")

# Generic ballot polling drift
change_matrix %>%
  as.data.frame() %>%
  as_tibble() %>%
  mutate(id = 1:n_sims) %>%
  melt(id.vars = "id", variable.name = "days", value.name = "change") %>%
  as_tibble() %>%
  mutate(days = as.character(days) %>% str_remove("V") %>% as.numeric()) %>%
  group_by(id) %>%
  arrange(id, days) %>%
  mutate(total_change = cumsum(change)) %>%
  filter(abs(max(total_change)) < 0.1) %>%
  filter(id %in% 1:100) %>%
  ggplot(aes(x = days, y = 100 * total_change, group = id)) +
  geom_line(aes(col = factor(id)), alpha = 0.5, show.legend = FALSE) +
  scale_colour_manual(values = rainbow(100)) +
  labs(title = "Sample generic ballot average drift trajectories",
       x = "Days", y = "Total change in Republican 2-party vote share (pp)", caption = "100 samples")

ggsave("output/visualizations/generic_ballot_sample_trajectories.png", width = 10, height = 5, dpi = 100)

change_matrix %>%
  rowSums() %>%
  as.data.frame() %>%
  filter(abs(`.`) < 0.1) %>%
  ggplot(aes(x = 100 * `.`, y = after_stat(density))) +
  geom_histogram(binwidth = 1, col = "black") +
  scale_x_continuous() +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Generic ballot drift posterior distribution given today's polling",
       x = "Total change in Republican 2-party share", y = "Probability")

ggsave("output/visualizations/generic_ballot_drift_posterior_distribution.png", width = 10, height = 8, dpi = 100)


# Generic ballot polling
generic_ballot_averages_2026_smoothed %>%
  mutate(margin = 2 * avg - 1,
         margin_upper = margin + 1.645 * 2 * se,
         margin_lower = margin - 1.645 * 2 * se) %>%
  na.omit() %>%
  ggplot(aes(x = avg_date, y = 100 * margin)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = election_date_2026, linetype = 2) +
  geom_line(col = "black", linewidth = 1) +
  geom_ribbon(aes(ymin = 100 * margin_lower, ymax = 100 * margin_upper),
              fill = "black", alpha = 0.2) +
  geom_point(data = generic_ballot_polls_2026 %>%
               mutate(margin = 2 * r2p - 1) %>%
               filter(party == "rep"),
             aes(x = end_date, y = 100 * margin), size = 1, col = "black", alpha = 0.5) +
  scale_x_date(labels = date_format("%b %Y"), limits = as.Date(c("2025-01-01", "2026-12-01")),
               breaks = "2 months") +
  scale_y_continuous(labels = label_number(style_positive = "plus", style_negative = "minus"),
                     limits = c(-20, 20)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Generic ballot polls", x = "Date", y = "Republican 2-party margin (pp)",
       caption = "Shaded region = 90% CI")
  
ggsave("output/visualizations/generic_ballot_average.png", width = 10, height = 5, dpi = 100)

# National House popular vote
natl_r2p_sims %>%
  ggplot(aes(x = natl_r2p, y = after_stat(count) / nrow(natl_r2p_sims))) +
  geom_histogram(binwidth = 0.01, color = "red", fill = "red", alpha = 0.5) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(title = "2026 national environment forecast", x = "Republican share of two-party national House vote", y = "Probability",
       subtitle = today() %>% format("%B %d, %Y"))

# House simulations
## Histogram - number of seats
house_district_posterior %>%
  group_by(sim_id) %>%
  summarise(rep = sum(r2p_pred > 0.5),
            dem = sum(r2p_pred <= 0.5)) %>%
  melt(id.vars = "sim_id", variable.name = "party", value.name = "seats") %>%
  as_tibble() %>%
  mutate(party = as.character(party)) %>%
  ggplot(aes(x = seats, y = after_stat(count) / n_sims)) +
  facet_wrap(~party, labeller = labeller(party = party_labels), scales = "free_x", nrow = 2) +
  geom_vline(xintercept = 217.5) +
  geom_vline(data = house_district_posterior %>%
               group_by(sim_id) %>%
               summarise(rep = sum(r2p_pred > 0.5),
                         dem = sum(r2p_pred <= 0.5)) %>%
               melt(id.vars = "sim_id", variable.name = "party", value.name = "seats") %>%
               group_by(party = as.character(party)) %>% 
               summarise(avg = mean(seats)),
             aes(xintercept = avg, col = party), show.legend = FALSE) +
  geom_histogram(aes(fill = party, col = party), binwidth = 1, alpha = 0.5, show.legend = FALSE, linewidth = 0.5) +
  scale_x_continuous(breaks = seq(100, 300, by = 50), limits = c(125, 300)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_colour_manual(name = "Party", values = party_colors, labels = party_labels) +
  scale_fill_manual(name = "Party", values = party_colors, labels = party_labels) +
  labs(title = "2026 House forecast", x = "House seats won", y = "Probability",
       subtitle = today() %>% format("%B %d, %Y"), caption = "218 seats needed for majority")

ggsave("output/visualizations/house_forecast.png", width = 10, height = 5, dpi = 100)

## House forecast timeline
house_forecast_timeline %>%
  ggplot(aes(x = forecast_date, y = prob_majority, col = party)) +
  geom_vline(xintercept = election_date_2026, linetype = 2) +
  geom_line(linewidth = 1) +
  scale_x_date(labels = date_format("%b %Y"), limits = as.Date(c("2026-04-01", "2026-12-01")),
               breaks = "1 month") +
  scale_y_continuous(labels = percent_format(), breaks = (0:5) / 5, limits = c(0, 1)) +
  scale_colour_manual(name = "Party", labels = party_labels, values = party_colors) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  labs(title = "House forecast over time - majority probability", x = "Date",
       y = "Probability of House majority")

ggsave("output/visualizations/house_forecast_timeline.png", width = 10, height = 5, dpi = 100)
  

## Scatterplot - Republican vote share vs. Republican seats
house_district_posterior %>%
  group_by(sim_id, natl_r2p) %>%
  summarise(r_seats = sum(r2p_pred >= 0.5)) %>%
  ggplot(aes(x = natl_r2p, y = r_seats)) +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 217.5) +
  geom_abline(intercept = 0, slope = 435, linetype = 2) +
  geom_point(col = "red", alpha = 0.025) +
  geom_smooth() +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Seats vs. national House popular vote", x = "Republican 2-party national vote share", y = "Republican seats won",
       subtitle = today() %>% format("%B %d, %Y"))

# Senate simulations
## Distribution of seats
senate_seat_sims %>%
  mutate(majority = case_when(rep >= 50 ~ "Republican",
                              dem > 50 ~ "Democratic",
                              TRUE ~ "Democratic")) %>%
  melt(id.vars = c("sim_id", "majority", "ind"), variable.name = "party", value.name = "seats") %>%
  group_by(party = as.character(party), seats) %>%
  summarise(prob = n() / n_sims) %>% 
  ggplot(aes(x = seats, y = prob)) +
  facet_wrap(~party, labeller = labeller(party = party_labels), nrow = 2, scales = "free_x") +
  geom_vline(data = senate_seat_sims %>%
               mutate(majority = case_when(rep >= 50 ~ "Republican",
                                           dem > 50 ~ "Democratic",
                                           TRUE ~ "Democratic")) %>%
               melt(id.vars = c("sim_id", "majority", "ind"), variable.name = "party", value.name = "seats") %>%
               group_by(party = as.character(party)) %>%
               summarise(avg = mean(seats)),
             aes(xintercept = avg, col = party), linewidth = 1, show.legend = FALSE) +
  geom_col(aes(fill = party, col = party), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(30, 70, by = 2), limits = c(34, 66)) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  scale_colour_manual(name = "Party", values = party_colors, labels = party_labels) +
  scale_fill_manual(name = "Party", values = party_colors, labels = party_labels) +
  labs(title = "2026 Senate forecast", x = "Senate seats held post-election", y = "Probability",
       subtitle = today() %>% format("%B %d, %Y"),
       caption = "51 seats or 50 seats + VP needed for majority")

ggsave("output/visualizations/senate_forecast.png", width = 10, height = 5, dpi = 100)

## Senate forecast timeline
senate_forecast_timeline %>%
  ggplot(aes(x = forecast_date, y = prob_majority, col = party)) +
  geom_vline(xintercept = election_date_2026, linetype = 2) +
  geom_line(linewidth = 1) +
  scale_x_date(labels = date_format("%b %Y"), limits = as.Date(c("2026-04-01", "2026-12-01")),
               breaks = "1 month") +
  scale_y_continuous(labels = percent_format(), breaks = (0:5) / 5, limits = c(0, 1)) +
  scale_colour_manual(name = "Party", labels = party_labels, values = party_colors) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  labs(title = "Senate forecast over time - majority probability", x = "Date",
       y = "Probability of Senate majority")

ggsave("output/visualizations/senate_forecast_timeline.png", width = 10, height = 5, dpi = 100)

## Senate antfarm
antfarm_states <- c("Nebraska", "Florida", "Texas", "Iowa", "Alaska", "Ohio", "Michigan", "North Carolina",
                    "Maine", "Georgia", "Minnesota", "New Hampshire", "New Mexico", "Virginia", "Illinois")

senate_state_forecast_timeline %>%
  select(forecast_date, state, rep = r_prob) %>%
  mutate(dem = ifelse(state != "Nebraska", 1 - rep, 0),
         ind = ifelse(state == "Nebraska", 1 - rep, 0)) %>%
  melt(measure.vars = c("dem", "rep", "ind"), variable.name = "party", value.name = "prob") %>%
  filter(state %in% antfarm_states) %>%
  as_tibble() %>%
  ggplot(aes(x = forecast_date, y = prob, col = party)) +
  facet_wrap(~state, nrow = 5) +
  geom_vline(xintercept = election_date_2026, linetype = 2) +
  geom_line(data = senate_forecast_timeline, aes(y = prob_majority), alpha = 0.2, linewidth = 0.8) +
  geom_line(linewidth = 0.8) +
  scale_x_date(labels = date_format("%b"), limits = as.Date(c("2026-04-01", "2026-12-01")),
               breaks = "2 months") +
  scale_y_continuous(labels = percent_format(), breaks = (0:2) / 2, limits = c(0, 1)) +
  scale_colour_manual(name = "Party", labels = party_labels, values = party_colors) +
  theme(axis.text.x = element_text(angle = 90, size = 6), axis.text.y = element_text(size = 6),
        legend.position = "bottom") +
  labs(title = "Senate antfarm", x = "Date", y = "Win probability", subtitle = "15 most competitive states",
       caption = "Senate majority probability shown in lighter colors")

ggsave("output/visualizations/senate_antfarm.png", width = 10, height = 8, dpi = 100)

## Scatterplot - Republican vote share vs. Republican seats
senate_seat_sims %>%
  left_join(natl_r2p_sims, by = "sim_id") %>%
  group_by(natl_r2p_band = cut(natl_r2p, breaks = (0:25) / 25)) %>%
  mutate(prob = n() / n_sims,
         lower = round(min(natl_r2p), 2),
         upper = round(max(natl_r2p), 2),
         band_label = paste(percent(lower), percent(upper), sep = "-")) %>%
  filter(prob > 0.005) %>%
  ggplot(aes(x = band_label, y = rep)) +
  geom_hline(yintercept = 50) +
  geom_violin_discrete(scale = "count") +
  labs(title = "Republican Senate seat conditional forecasts", subtitle = "For varying shares of national House popular vote",
       x = "Republican share of two-party national House popular vote", y = "Republican forecasted Senate seats",
       caption = today() %>% format("%B %d, %Y"))

# Conditional distributions
house_conditional_posteriors %>%
  mutate(margin_diff = 100 * margin_diff) %>%
  ggplot(aes(x = r_seats, y = prob)) +
  facet_wrap(~margin_diff, nrow = 7, dir = "v") +
  geom_vline(xintercept = 435 / 2) +
  geom_col(col = "black", fill = "red", alpha = 0.5) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "House seat posterior conditional on House generic ballot error",
       x = "Republican House seats", y = "Probability")

senate_conditional_posteriors %>%
  mutate(margin_diff = 100 * margin_diff) %>%
  filter(party != "ind") %>%
  ggplot(aes(x = seats, y = prob)) +
  facet_wrap(~margin_diff, nrow = 7, dir = "v") +
  geom_vline(xintercept = 50) +
  geom_col(aes(fill = party), col = "black", alpha = 0.5, position = "identity") +
  scale_fill_manual(name = "Party", values = party_colors, labels = party_labels) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Senate seat posterior conditional on House generic ballot error",
       x = "Republican Senate seats", y = "Probability")
