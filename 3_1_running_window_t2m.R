library("tidyverse")
library("crch")
library("latex2exp")
library("lubridate")
library("bamlss")

df <- read_csv("0_data/t2m_merged_forecast_with_obs_medium.csv")

df <- df %>%
  drop_na() %>%
  mutate(
    lead_time = as.numeric(difftime(prediction_time, issue_time, units = "hours")),
    doy = as.numeric(difftime(prediction_time, make_date(year(prediction_time), 1, 1), units = "days")),
    doy_sin = sin(2 * pi * doy / 366),
    doy_cos = cos(2 * pi * doy / 366),
    tod = as.factor(doy - floor(doy)),
    log_ensemble_sd = log(ensemble_sd),
    early_lead_time = if_else(lead_time < 100, TRUE, FALSE)
  ) %>%
  dplyr::select(
    location, issue_time, lead_time, forecast_period, prediction_time, doy, observation, ensemble_mean, ensemble_sd, doy_sin, doy_cos, tod, log_ensemble_sd, early_lead_time
  ) %>%
  rename(valid_time = prediction_time) %>%
  distinct()

df <- df %>%
  arrange(issue_time, lead_time)


source("3_helpers_t2m.R")


# 2. Get computation time -------------------------------------------------

issue_time_prediction <- make_date(2020, 12, 31)

df_one_loc <- df %>% filter(location == 20)

times <- replicate(
  20,
  {
    start <- Sys.time()

    # Lead time continuous model 4
    predictions <- .predict_lead_time_continuous_one_issue_date_crps(
      issue_time_prediction,
      df = df_one_loc,
      formula = observation ~ ensemble_mean + factor(tod) + lead_time | log_ensemble_sd + factor(tod) + lead_time
    )

    end <- Sys.time()
    end - start
  }
)
print(paste0("Computation took an average of ", mean(times), "s with standard deviation of ", sd(times), "s."))

times <- replicate(
  20,
  {
    start <- Sys.time()

    # Lead time separated
    predictions <- .predict_lead_time_separated_one_issue_date(
      issue_time_prediction,
      df = df_one_loc
    )

    end <- Sys.time()
    end - start
  }
)
print(paste0("Computation took an average of ", mean(times), "s with standard deviation of ", sd(times), "s."))


# 3. Apply over whole dataset ---------------------------------------------

library("foreach")

n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()


# Lead time separated model
start <- Sys.time()
results_separated <- foreach(
  location_i = unique(df$location),
  .packages = c("tidyverse", "crch"),
  .errorhandling = "remove"
) %dopar% {
  df_one_loc <- df %>% filter(location == location_i)

  predictions <- lapply(unique(df_one_loc$issue_time), FUN = function(issue_time_prediction) {
    .predict_lead_time_separated_one_issue_date(
      issue_time_prediction,
      df = df_one_loc
    )
  })

  bind_rows(predictions)
}
end <- Sys.time()
print(paste0("Lead time separated model took ", as.numeric(difftime(end, start, units = "mins")), "mins to calculate."))


# Lead time continuous model 0
start <- Sys.time()
results_continuous_0 <- foreach(
  location_i = unique(df$location),
  .packages = c("tidyverse", "crch"),
  .errorhandling = "remove"
) %dopar% {
  df_one_loc <- df %>% filter(location == location_i)

  predictions <- lapply(unique(df_one_loc$issue_time), FUN = function(issue_time_prediction) {
    .predict_lead_time_continuous_one_issue_date_crps(
      issue_time_prediction,
      df = df_one_loc,
      formula = observation ~ ensemble_mean | log_ensemble_sd
    )
  })

  bind_rows(predictions)
}
end <- Sys.time()
print(paste0("Model 0 took ", as.numeric(difftime(end, start, units = "mins")), "mins to calcaulate."))


# Lead time continuous model 1
start <- Sys.time()
results_continuous_1 <- foreach(
  location_i = unique(df$location),
  .packages = c("tidyverse", "crch"),
  .errorhandling = "remove"
) %dopar% {
  df_one_loc <- df %>% filter(location == location_i)

  predictions <- lapply(unique(df_one_loc$issue_time), FUN = function(issue_time_prediction) {
    .predict_lead_time_continuous_one_issue_date_crps(
      issue_time_prediction,
      df = df_one_loc,
      formula = observation ~ ensemble_mean + lead_time + factor(tod) | log_ensemble_sd + lead_time + factor(tod)
    )
  })

  bind_rows(predictions)
}
end <- Sys.time()
print(paste0("Model 1 took ", as.numeric(difftime(end, start, units = "mins")), "mins to calculate."))


# Lead time continuous model 2
start <- Sys.time()
results_continuous_2 <- foreach(
  location_i = unique(df$location),
  .packages = c("tidyverse", "crch"),
  .errorhandling = "remove"
) %dopar% {
  df_one_loc <- df %>% filter(location == location_i)

  predictions <- lapply(unique(df_one_loc$issue_time), FUN = function(issue_time_prediction) {
    .predict_lead_time_continuous_one_issue_date_crps(
      issue_time_prediction,
      df = df_one_loc,
      formula = observation ~ ensemble_mean + factor(tod) | log_ensemble_sd + factor(tod)
    )
  })

  bind_rows(predictions)
}
end <- Sys.time()
print(paste0("Model 2 took ", as.numeric(difftime(end, start, units = "mins")), "mins to calculate."))


# Lead time continuous model 3
start <- Sys.time()
results_continuous_3 <- foreach(
  location_i = unique(df$location),
  .packages = c("tidyverse", "crch"),
  .errorhandling = "remove"
) %dopar% {
  df_one_loc <- df %>% filter(location == location_i)
  browser()
  predictions <- lapply(unique(df_one_loc$issue_time), FUN = function(issue_time_prediction) {
    .predict_lead_time_continuous_one_issue_date_crps(
      issue_time_prediction,
      df = df_one_loc,
      formula = observation ~ ensemble_mean + factor(tod) + early_lead_time | log_ensemble_sd + factor(tod) + early_lead_time
    )
  })

  bind_rows(predictions)
}
end <- Sys.time()
print(paste0("Model 3 took ", as.numeric(difftime(end, start, units = "mins")), "mins to calculate."))


# Lead time continuous model 4
start <- Sys.time()
results_continuous_4 <- foreach(
  location_i = unique(df$location),
  .packages = c("tidyverse", "crch"),
  .errorhandling = "remove"
) %dopar% {
  df_one_loc <- df %>% filter(location == location_i)

  predictions <- lapply(unique(df_one_loc$issue_time), FUN = function(issue_time_prediction) {
    .predict_lead_time_continuous_one_issue_date_crps(
      issue_time_prediction,
      df = df_one_loc,
      formula = observation ~ ensemble_mean + factor(tod) + early_lead_time | log_ensemble_sd + factor(tod) + lead_time
    )
  })

  bind_rows(predictions)
}
end <- Sys.time()
print(paste0("Model 4 took ", as.numeric(difftime(end, start, units = "mins")), "mins to calculate."))


# Lead time continuous model 5
start <- Sys.time()
results_continuous_5 <- foreach(
  location_i = unique(df$location),
  .packages = c("tidyverse", "bamlss"),
  .errorhandling = "remove"
) %dopar% {
  df_one_loc <- df %>% filter(location == location_i)

  predictions <- lapply(unique(df_one_loc$issue_time), FUN = function(issue_time_prediction) {
    .predict_lead_time_continuous_one_issue_date(
      issue_time_prediction,
      df = df_one_loc,
      formula = list(observation ~ ensemble_mean + s(lead_time) + tod, sigma ~ log_ensemble_sd + s(lead_time) + tod)
    )
  })

  bind_rows(predictions)
}
end <- Sys.time()
print(paste0("Model 5 took ", as.numeric(difftime(end, start, units = "mins")), "mins to calculate."))


results_separated <- bind_rows(results_separated)
results_continuous_0 <- bind_rows(results_continuous_0)
results_continuous_1 <- bind_rows(results_continuous_1)
results_continuous_2 <- bind_rows(results_continuous_2)
results_continuous_3 <- bind_rows(results_continuous_3)
results_continuous_4 <- bind_rows(results_continuous_4)
results_continuous_5 <- bind_rows(results_continuous_5)


results_separated <- results_separated %>% rename(predicted_mu_lead_time_separated = predicted_mu, predicted_sigma_lead_time_separated = predicted_sigma)
results_continuous_0 <- results_continuous_0 %>% rename(predicted_mu_lead_time_continuous_model_0 = predicted_mu, predicted_sigma_lead_time_continuous_model_0 = predicted_sigma)
results_continuous_1 <- results_continuous_1 %>% rename(predicted_mu_lead_time_continuous_model_1 = predicted_mu, predicted_sigma_lead_time_continuous_model_1 = predicted_sigma)
results_continuous_2 <- results_continuous_2 %>% rename(predicted_mu_lead_time_continuous_model_2 = predicted_mu, predicted_sigma_lead_time_continuous_model_2 = predicted_sigma)
results_continuous_3 <- results_continuous_3 %>% rename(predicted_mu_lead_time_continuous_model_3 = predicted_mu, predicted_sigma_lead_time_continuous_model_3 = predicted_sigma)
results_continuous_4 <- results_continuous_4 %>% rename(predicted_mu_lead_time_continuous_model_4 = predicted_mu, predicted_sigma_lead_time_continuous_model_4 = predicted_sigma)
results_continuous_5 <- results_continuous_5 %>% rename(predicted_mu_lead_time_continuous_model_5 = predicted_mu, predicted_sigma_lead_time_continuous_model_5 = predicted_sigma)

results <- results_separated %>%
  left_join(
    results_continuous_0 %>% dplyr::select(c(location, issue_time, lead_time, forecast_period, valid_time, starts_with("predicted"))),
    by = c("location", "issue_time", "lead_time", "forecast_period", "valid_time")
  ) %>%
  left_join(
    results_continuous_1 %>% dplyr::select(c(location, issue_time, lead_time, forecast_period, valid_time, starts_with("predicted"))),
    by = c("location", "issue_time", "lead_time", "forecast_period", "valid_time")
  ) %>%
  left_join(
    results_continuous_2 %>% dplyr::select(c(location, issue_time, lead_time, forecast_period, valid_time, starts_with("predicted"))),
    by = c("location", "issue_time", "lead_time", "forecast_period", "valid_time")
  ) %>%
  left_join(
    results_continuous_3 %>% dplyr::select(c(location, issue_time, lead_time, forecast_period, valid_time, starts_with("predicted"))),
    by = c("location", "issue_time", "lead_time", "forecast_period", "valid_time")
  ) %>%
  left_join(
    results_continuous_4 %>% dplyr::select(c(location, issue_time, lead_time, forecast_period, valid_time, starts_with("predicted"))),
    by = c("location", "issue_time", "lead_time", "forecast_period", "valid_time")
  ) %>%
  left_join(
    results_continuous_5 %>% dplyr::select(c(location, issue_time, lead_time, forecast_period, valid_time, starts_with("predicted"))),
    by = c("location", "issue_time", "lead_time", "forecast_period", "valid_time")
  )

results %>% write_csv("1_generated_data/3_running_window_predictions_t2m.csv")
results_separated %>% write_csv("1_generated_data/3_running_window/3_running_window_predictions_separated_t2m.csv")
results_continuous_0 %>% write_csv("1_generated_data/3_running_window/3_running_window_predictions_continuous_0_t2m.csv")
results_continuous_1 %>% write_csv("1_generated_data/3_running_window/3_running_window_predictions_continuous_1_t2m.csv")
results_continuous_2 %>% write_csv("1_generated_data/3_running_window/3_running_window_predictions_continuous_2_t2m.csv")
results_continuous_3 %>% write_csv("1_generated_data/3_running_window/3_running_window_predictions_continuous_3_t2m.csv")
results_continuous_4 %>% write_csv("1_generated_data/3_running_window/3_running_window_predictions_continuous_4_t2m.csv")
results_continuous_5 %>% write_csv("1_generated_data/3_running_window/3_running_window_predictions_continuous_5_t2m.csv")

# 4. Analyse results ------------------------------------------------------

results <- read_csv("1_generated_data/3_running_window_predictions_t2m.csv")

results <- results %>%
  pivot_longer(starts_with("predicted_"), names_to = c("type", "model"), values_to = "value", names_sep = "_lead_time_") %>%
  pivot_wider(names_from = "type", values_from = "value") %>%
  mutate(
    model = case_when(
      model == "separated" ~ "Lead time separated EMOS - log link",
      model == "continuous_model_0" ~ "Lead time continuous model 0",
      model == "continuous_model_1" ~ "Lead time continuous model 1",
      model == "continuous_model_2" ~ "Lead time continuous model 2",
      model == "continuous_model_3" ~ "Lead time continuous model 3",
      model == "continuous_model_4" ~ "Lead time continuous model 4",
      model == "continuous_model_5" ~ "Lead time continuous model 5",
      model == "continuous_model_6" ~ "Lead time continuous model 6",
      TRUE ~ NA_character_
    )
  )

library("scoringRules")

results <- results %>%
  drop_na() %>%
  mutate(
    CRPS = crps(y = observation, family = "normal", mean = predicted_mu, sd = predicted_sigma),
    PIT = pnorm(observation, mean = predicted_mu, sd = predicted_sigma),
    MSE = (observation - ensemble_mean)^2
  )

# Plot CRPS score of predictions
p1 <- results %>%
  group_by(lead_time, model) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  filter(model %in% c("Lead time separated EMOS - log link", "Lead time continuous model 1")) %>%
  mutate(model = if_else(model == "Lead time continuous model 1", "Lead time continuous \n(C-RWIN)", "Lead time separated \n(S-RWIN)")) %>%
  ggplot(aes(lead_time, CRPS, colour = model)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(name = "EMOS model", values = c("Lead time separated \n(S-RWIN)" = "#000000", "Lead time continuous \n(C-RWIN)" = "#E69F00")) +
  theme_classic() +
  ylab("CRPS") +
  xlab("Lead time (hours)") +
  ggtitle(TeX("CRPS as a function of the lead time")) +
  scale_x_continuous(breaks = 24 * c(0:8)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5))
p1
ggsave("2_generated_plots/3_running_window/crps_rolling_window_t2m.png", width = 7, height = 5)

# Plot PIT histogram
# Note: This only works if an equal amount of PIT values are in either of the two categories, which is the case here.
results %>%
  filter(model %in% c("Lead time separated EMOS - log link", "Lead time continuous model 1")) %>%
  mutate(model = if_else(model == "Lead time continuous model 1", "Lead time continuous (C-RWIN)", "Lead time separated (S-RWIN)")) %>%
  filter(lead_time == 48) %>%
  ggplot(aes(x = PIT)) +
  geom_histogram(bins = 20, boundary = 0, aes(y = 2 * 20 * after_stat(..count.. / sum(..count..)))) +
  ylab("") +
  scale_x_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1.0) +
  facet_wrap(~model) +
  theme_classic() +
  ggtitle(TeX("PIT histogram at a lead time 48 hours")) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggsave("2_generated_plots/3_running_window/t2m_rolling_window_PIT_lead_time_48_lead_time_continuous_and_separated_models.png", width = 10, height = 5)

# RI diff
get_reliability_index <- function(x) {
  histogram <- hist(x, breaks = 20, plot = FALSE)
  return(
    sum(abs(histogram$counts / length(x) - 1 / 20))
  )
}

# Plot RI diff
results %>%
  filter(model %in% c("Lead time separated EMOS - log link", "Lead time continuous model 1")) %>%
  mutate(model = if_else(model == "Lead time continuous model 1", "Lead time continuous EMOS", "Lead time separated EMOS")) %>%
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
  xlab("Lead time (hours)") +
  ggtitle("Difference in reliability index")
ggsave("2_generated_plots/3_running_window/t2m_rolling_window_RI_diff_continuous_and_separated_models.png", width = 10, height = 5)
