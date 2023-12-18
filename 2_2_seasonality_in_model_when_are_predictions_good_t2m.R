library("tidyverse")
library("crch")
library("latex2exp")
library("lubridate")
library("bamlss")
library("gamlss")

df <- read_csv("0_data/t2m_merged_forecast_with_obs_medium.csv")

df <- df %>%
  drop_na() %>%
  mutate(
    lead_time = as.numeric(difftime(prediction_time, issue_time, units = "hours")),
    doy = as.numeric(difftime(prediction_time, make_date(year(prediction_time), 1, 1), units = "days")),
    doy_sin = sin(2 * pi * doy / 366),
    doy_cos = cos(2 * pi * doy / 366),
    tod = as.factor(doy - floor(doy)),
    log_ensemble_sd = log(ensemble_sd)
  ) %>%
  dplyr::select(
    location, issue_time, lead_time, forecast_period, prediction_time, doy, observation, ensemble_mean, ensemble_sd, doy_sin, doy_cos, tod, log_ensemble_sd
  ) %>%
  distinct()

# Training data
train <- df %>%
  filter(year(issue_time) < 2021)

# Testing data
test <- df %>%
  filter(year(issue_time) >= 2021)

# Remove full data
rm(df)


# 1. Get predictions lead time separated model trained for different lead times --------------------------------

predictions_lead_time_separated <- list()
predictions_other_lead_times <- list()

start <- Sys.time()
for (location_i in unique(train$location)) {
  predictions_one_location <- list()

  print(paste0("------ Location ", location_i, " ------"))

  test_df_loc <- test %>% filter(location == location_i)

  for (lead_time_i in unique(train$lead_time)) {
    # Get train and test df location, lead time
    train_df <- train %>% filter(lead_time == lead_time_i, location == location_i)
    test_df <- test %>% filter(lead_time == lead_time_i, location == location_i)

    # Fit model
    fit <- crch(
      observation ~ ensemble_mean + doy_sin + doy_cos | log(ensemble_sd) + doy_sin + doy_cos,
      data = train_df,
      dist = "gaussian",
      type = "crps",
      link.scale = "log"
    )

    # Predict on full test dataset for location
    test_df_loc <- test_df_loc %>% mutate(!!as.symbol(paste0("crps", lead_time_i)) := predict(fit, newdata = test_df_loc, at = test_df_loc$observation, type = "crps"))

    # Predict at lead time separated test df
    test_df$crps <- predict(fit, newdata = test_df, at = test_df$observation, type = "crps")

    predictions_one_location[[as.character(lead_time_i)]] <- test_df
  }

  predictions_lead_time_separated[[as.character(location_i)]] <- bind_rows(predictions_one_location)
  predictions_other_lead_times[[as.character(location_i)]] <- test_df_loc
}

end <- Sys.time()

print(paste0(difftime(end, start, units = "secs"), "s elapsed for model training and prediction"))
predictions_lead_time_separated <- bind_rows(predictions_lead_time_separated)
predictions_other_lead_times <- bind_rows(predictions_other_lead_times)


# 2. Evaluate predictions and plot ----------------------------------------

# Get CRPS scores of models trained for t1 evaluated at t2
crps_trained_for_different_lead_time <- predictions_other_lead_times %>%
  pivot_longer(contains("crps"), names_to = "type", values_to = "crps") %>%
  group_by(lead_time, type) %>%
  summarise(crps = mean(crps), .groups = "drop")

# Get baseline (model trained for lead time t1, evaluated at t1)
crps_baseline <- predictions_lead_time_separated %>%
  group_by(lead_time) %>%
  summarise(crps = mean(crps), .groups = "drop") %>%
  dplyr::rename(baseline_crps = crps)

# Merge and write to file
crps <- crps_trained_for_different_lead_time %>% left_join(crps_baseline, by = "lead_time")
crps %>% write_csv("1_generated_data/2_seasonality_in_model_predictions_different_lead_times_t2m.csv")

# Read from file
crps <- read_csv("1_generated_data/2_seasonality_in_model_predictions_different_lead_times_t2m.csv")

crps <- crps %>%
  mutate(type = as.numeric(gsub("crps", "", type))) %>%
  dplyr::rename(trained_for = type)

# Merge to and group by tod
tod_and_lead_time <- train %>% distinct(lead_time, tod)
levels(tod_and_lead_time$tod) <- c("00 UTC", "06 UTC", "12 UTC", "18 UTC")

crps <- crps %>%
  left_join(tod_and_lead_time, by = "lead_time") %>%
  left_join(tod_and_lead_time %>% dplyr::rename(tod_trained_for = tod), by = c("trained_for" = "lead_time"))

# Plot
library("viridis")
crps %>%
  filter(tod == tod_trained_for) %>%
  mutate(skill_score = 1 - crps / baseline_crps) %>%
  group_by(tod, lead_time) %>%
  arrange(trained_for, .by_group = TRUE) %>%
  mutate(elem_in_group = row_number() - 1) %>%
  ungroup() %>%
  filter(elem_in_group < 8) %>%
  ggplot(aes(lead_time, skill_score, col = factor(elem_in_group))) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~tod_trained_for) +
  scale_color_viridis(name = "Model number", discrete = TRUE, option = "D") +
  theme_classic() +
  ylab("CRPSS") +
  xlab("Lead time (hours)") +
  scale_x_continuous(breaks = 24 * c(0:8)) +
  theme(axis.title = element_text(size = 16), plot.title = element_text(size = 14, hjust = 0.5), legend.title = element_text(size = 16), legend.text = element_text(size = 12), strip.text = element_text(size=14))
ggsave("2_generated_plots/2_seasonality_in_model/fig9_t2m_crps_score_nth_model_by_tod.png", width = 9, height = 6.5)
