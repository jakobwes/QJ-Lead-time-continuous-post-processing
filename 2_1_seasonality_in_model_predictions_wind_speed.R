library("tidyverse")
library("crch")
library("latex2exp")
library("lubridate")
library("gamlss")

df <- read_csv("0_data/wind_speed_merged_forecast_with_obs_medium.csv")

# Zero observation values are most likely due to rounding and are replaced by the smallest positive one. This affects 17 values.
df$observation[df$observation == 0] <- min(df$observation[df$observation > 0])

df <- df %>%
  drop_na() %>%
  mutate(
    lead_time = as.numeric(difftime(prediction_time, issue_time, units = "hours")),
    doy = as.numeric(difftime(prediction_time, make_date(year(prediction_time), 1, 1), units = "days")),
    doy_sin = sin(2 * pi * doy / 366),
    doy_cos = cos(2 * pi * doy / 366),
    tod = as.factor(doy - floor(doy))
  ) %>%
  dplyr::select(
    location, issue_time, lead_time, forecast_period, prediction_time, doy, observation, ensemble_mean, ensemble_sd, doy_sin, doy_cos, tod
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

# Initialize truncated distribution
library("gamlss.tr")
gen.trun(par = c(0), family = "NO", name = "tr", type = "left")


# 1. Predictions lead time separated model --------------------------------

predictions_lead_time_separated <- list()

start <- Sys.time()
for (location_i in unique(train$location)) {
  predictions_one_location <- list()

  print(paste0("------ Location ", location_i, " ------"))

  for (lead_time_i in unique(train$lead_time)) {
    # Get train and test df location, lead time
    train_df <- train %>% filter(lead_time == lead_time_i, location == location_i)
    test_df <- test %>% filter(lead_time == lead_time_i, location == location_i)

    # Fit model
    fit <- crch(
      observation ~ ensemble_mean + doy_sin + doy_cos | log(ensemble_sd) + doy_sin + doy_cos,
      data = train_df,
      link.scale = "log",
      dist = "gaussian",
      type = "crps",
      truncated = TRUE,
      left = 0
    )

    # Predict on test dataset
    predicted_mu <- predict(fit, newdata = test_df, type = "location")
    predicted_sigma <- predict(fit, newdata = test_df, type = "scale")

    test_df$predicted_mu_lead_time_separated <- predicted_mu
    test_df$predicted_sigma_lead_time_separated <- predicted_sigma

    predictions_one_location[[as.character(lead_time_i)]] <- test_df
  }

  predictions_lead_time_separated[[as.character(location_i)]] <- bind_rows(predictions_one_location)
}

end <- Sys.time()

print(paste0(difftime(end, start, units = "secs"), "s elapsed for model training and prediction"))
predictions_lead_time_separated <- bind_rows(predictions_lead_time_separated)


# 2. Predictions lead time continuous models ------------------------------------------

predictions_lead_time_continuous <- list()

start <- Sys.time()
for (location_i in unique(train$location)) {
  print(paste0("------ Location ", location_i, " ------"))

  train_df <- train %>% filter(location == location_i)
  test_df <- test %>% filter(location == location_i)

  # Train model 1: base model
  lead_time_continuous_model_1 <- crch(
    observation ~ ensemble_mean + doy_sin * factor(tod) + doy_cos * factor(tod) + lead_time | log(ensemble_sd) + doy_sin * factor(tod) + doy_cos * factor(tod) + lead_time,
    data = train_df,
    link.scale = "log",
    dist = "gaussian",
    type = "crps",
    truncated = TRUE,
    left = 0
  )

  # Train model 2: without interaction tod - seasonality
  lead_time_continuous_model_2 <- crch(
    observation ~ ensemble_mean + doy_sin + doy_cos + factor(tod) + lead_time | log(ensemble_sd) + doy_sin + doy_cos + factor(tod) + lead_time,
    data = train_df,
    link.scale = "log",
    dist = "gaussian",
    type = "crps",
    truncated = TRUE,
    left = 0
  )

  # Train model 3: without main lead time effect
  lead_time_continuous_model_3 <- crch(
    observation ~ ensemble_mean + doy_sin * factor(tod) + doy_cos * factor(tod) | log(ensemble_sd) + doy_sin * factor(tod) + doy_cos * factor(tod),
    data = train_df,
    link.scale = "log",
    dist = "gaussian",
    type = "crps",
    truncated = TRUE,
    left = 0
  )

  # Train model 4: lead time - ensemble mean/sd interaction
  lead_time_continuous_model_4 <- crch(
    observation ~ ensemble_mean * lead_time + doy_sin * factor(tod) + doy_cos * factor(tod) | log(ensemble_sd) * lead_time + doy_sin * factor(tod) + doy_cos * factor(tod),
    data = train_df,
    link.scale = "log",
    dist = "gaussian",
    type = "crps",
    truncated = TRUE,
    left = 0
  )

  # Train model 5: tod - ensemble mean/sd interaction
  lead_time_continuous_model_5 <- crch(
    observation ~ ensemble_mean * factor(tod) + doy_sin * factor(tod) + doy_cos * factor(tod) + lead_time | log(ensemble_sd) * factor(tod) + doy_sin * factor(tod) + doy_cos * factor(tod) + lead_time,
    data = train_df,
    link.scale = "log",
    dist = "gaussian",
    type = "crps",
    truncated = TRUE,
    left = 0
  )

  # Predict on test dataset
  predicted_mu_1 <- predict(lead_time_continuous_model_1, newdata = test_df, type = "location")
  predicted_sigma_1 <- predict(lead_time_continuous_model_1, newdata = test_df, type = "scale")

  predicted_mu_2 <- predict(lead_time_continuous_model_2, newdata = test_df, type = "location")
  predicted_sigma_2 <- predict(lead_time_continuous_model_2, newdata = test_df, type = "scale")

  predicted_mu_3 <- predict(lead_time_continuous_model_3, newdata = test_df, type = "location")
  predicted_sigma_3 <- predict(lead_time_continuous_model_3, newdata = test_df, type = "scale")

  predicted_mu_4 <- predict(lead_time_continuous_model_4, newdata = test_df, type = "location")
  predicted_sigma_4 <- predict(lead_time_continuous_model_4, newdata = test_df, type = "scale")

  predicted_mu_5 <- predict(lead_time_continuous_model_5, newdata = test_df, type = "location")
  predicted_sigma_5 <- predict(lead_time_continuous_model_5, newdata = test_df, type = "scale")

  # Save predictions
  test_df$predicted_mu_lead_time_continuous_model_1 <- predicted_mu_1
  test_df$predicted_sigma_lead_time_continuous_model_1 <- predicted_sigma_1

  test_df$predicted_mu_lead_time_continuous_model_2 <- predicted_mu_2
  test_df$predicted_sigma_lead_time_continuous_model_2 <- predicted_sigma_2

  test_df$predicted_mu_lead_time_continuous_model_3 <- predicted_mu_3
  test_df$predicted_sigma_lead_time_continuous_model_3 <- predicted_sigma_3

  test_df$predicted_mu_lead_time_continuous_model_4 <- predicted_mu_4
  test_df$predicted_sigma_lead_time_continuous_model_4 <- predicted_sigma_4

  test_df$predicted_mu_lead_time_continuous_model_5 <- predicted_mu_5
  test_df$predicted_sigma_lead_time_continuous_model_5 <- predicted_sigma_5

  predictions_lead_time_continuous[[as.character(location_i)]] <- test_df
}
end <- Sys.time()

print(paste0(difftime(end, start, units = "secs"), "s elapsed for model training and prediction"))
predictions_lead_time_continuous <- bind_rows(predictions_lead_time_continuous)


# 3. Merge dataframes and calculate scores --------------------------------

# Merge dataframes
predictions <- left_join(
  predictions_lead_time_continuous,
  predictions_lead_time_separated %>% dplyr::select(
    location, issue_time, lead_time, forecast_period, prediction_time, doy, predicted_mu_lead_time_separated, predicted_sigma_lead_time_separated
  ),
  by = c("location", "issue_time", "lead_time", "forecast_period", "prediction_time", "doy")
) %>%
  drop_na()

if (nrow(predictions) != nrow(test)) stop("Error in predictions.")

# Reformat predictions
predictions <- predictions %>%
  pivot_longer(starts_with("predicted_"), names_to = c("type", "model"), values_to = "value", names_sep = "_lead_time_") %>%
  pivot_wider(names_from = "type", values_from = "value") %>%
  mutate(
    model = case_when(
      model == "separated" ~ "Lead time separated EMOS",
      model == "continuous_model_1" ~ "Lead time continuous model 1",
      model == "continuous_model_2" ~ "Lead time continuous model 2",
      model == "continuous_model_3" ~ "Lead time continuous model 3",
      model == "continuous_model_4" ~ "Lead time continuous model 4",
      model == "continuous_model_5" ~ "Lead time continuous model 5",
      model == "continuous_model_6" ~ "Lead time continuous model 6",
      TRUE ~ NA_character_
    )
  )

# Add scores
library("scoringRules")

predictions <- predictions %>%
  mutate(
    CRPS = crps(y = observation, family = "tnorm", location = predicted_mu, scale = predicted_sigma, lower = 0, upper = Inf),
    PIT = pNOtr(observation, mu = predicted_mu, sigma = predicted_sigma),
    MSE = (observation - ensemble_mean)^2
  )

# Write to file
write_csv(predictions, "1_generated_data/2_seasonality_in_model_predictions_wind_speed.csv")

predictions <- read_csv("1_generated_data/2_seasonality_in_model_predictions_wind_speed.csv")

# Plot CRPS score of predictions
p1 <- predictions %>%
  group_by(lead_time, model) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  filter(model %in% c("Lead time separated EMOS", "Lead time continuous model 1")) %>%
  mutate(model = case_when(
    model == "Lead time continuous model 1" ~ "Lead time continuous \n(C-SWM)",
    TRUE ~ "Lead time separated \n(S-SWM)"
  )) %>%
  ggplot(aes(lead_time, CRPS, colour = model)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(name = "EMOS model", values = c("Lead time separated \n(S-SWM)" = "#000000", "Lead time continuous \n(C-SWM)" = "#E69F00")) +
  theme_classic() +
  ylab("CRPS") +
  xlab("Lead time (hours)") +
  scale_x_continuous(breaks = 24 * c(0:8)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5))
p1
ggsave("2_generated_plots/2_seasonality_in_model/wind_speed_crps_score_lead_time_continuous_and_separated_models.png", width = 7, height = 5)

# Plot PIT histogram
# Note: This only works if an equal amount of PIT values are in either of the two categories, which is the case here.
p2 <- predictions %>%
  filter(model %in% c("Lead time separated EMOS", "Lead time continuous model 1")) %>%
  mutate(model = if_else(model == "Lead time continuous model 1", "Lead time continuous (C-SWM)", "Lead time separated (S-SWM)")) %>%
  filter(lead_time == 48) %>%
  ggplot(aes(x = PIT)) +
  geom_histogram(bins = 20, boundary = 0, aes(y = 2 * 20 * after_stat(..count.. / sum(..count..)))) +
  ylab("") +
  scale_x_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1.0) +
  facet_wrap(~model) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank())
p2
ggsave("2_generated_plots/2_seasonality_in_model/wind_speed_PIT_lead_time_48.png", width = 10, height = 5)

# Plot reliability index
get_reliability_index <- function(x) {
  histogram <- hist(x, breaks = 20, plot = FALSE)
  return(
    sum(abs(histogram$counts / length(x) - 1 / 20))
  )
}

predictions %>%
  filter(model %in% c("Lead time separated EMOS", "Lead time continuous model 1")) %>%
  mutate(model = if_else(model == "Lead time continuous model 1", "Lead time continuous EMOS", model)) %>%
  group_by(model, location, lead_time) %>%
  summarise(RI = get_reliability_index(PIT), .groups = "drop") %>%
  pivot_wider(names_from = "model", values_from = "RI") %>%
  mutate(RI_diff = `Lead time continuous EMOS` - `Lead time separated EMOS`) %>%
  group_by(lead_time) %>%
  summarise(RI_diff_mean = mean(RI_diff), RI_diff_sd = sd(RI_diff), ymin = RI_diff_mean - RI_diff_sd, ymax = RI_diff_mean + RI_diff_sd, .groups = "drop") %>%
  ggplot(aes(lead_time, RI_diff_mean, ymin = ymin, ymax = ymax)) +
  geom_line() +
  geom_point() +
  geom_errorbar() +
  theme_classic() +
  scale_x_continuous(breaks = 24 * c(0:8)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  ylab("RI (lead time continuous) - RI(lead time separated)") +
  xlab("Lead time (hours)")
ggsave("2_generated_plots/2_seasonality_in_model/wind_speed_RI_diff_continuous_and_separated_models.png", width = 10, height = 5)


# 4. Explore computation time ---------------------------------------------

train_one_loc <- train %>% filter(location == 20)
test_one_loc <- test %>% filter(location == 20)

# Computation time lead time continuous
times_continuous <- replicate(20, {
  start <- Sys.time()

  model <- crch(
    observation ~ ensemble_mean + doy_sin * factor(tod) + doy_cos * factor(tod) + lead_time | log(ensemble_sd) + doy_sin * factor(tod) + doy_cos * factor(tod) + lead_time,
    data = train_one_loc,
    link.scale = "log",
    dist = "gaussian",
    type = "crps",
    truncated = TRUE,
    left = 0
  )

  end <- Sys.time()

  as.numeric(difftime(end, start, units = "s"))
})
print(paste0("This took an average of ", mean(times_continuous), " with sd of ", sd(times_continuous)))

times_separated <- replicate(20, {
  start <- Sys.time()

  for (lead_time_i in unique(train_one_loc$lead_time)) {
    train_df <- train_one_loc %>% filter(lead_time == lead_time_i)
    test_df <- test_one_loc %>% filter(lead_time == lead_time_i)

    model <- crch(
      observation ~ ensemble_mean + doy_sin + doy_cos | log(ensemble_sd) + doy_sin + doy_cos,
      data = train_one_loc,
      link.scale = "log",
      dist = "gaussian",
      type = "crps",
      truncated = TRUE,
      left = 0
    )
  }

  end <- Sys.time()

  as.numeric(difftime(end, start, units = "s"))
})
print(paste0("This took an average of ", mean(times_separated), " with sd of ", sd(times_separated)))
