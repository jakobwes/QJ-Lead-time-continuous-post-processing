rm(list = ls())
library("tidyverse")
library("latex2exp")
library("lubridate")
library("scoringRules")


# 1. t2m ------------------------------------------------------------------

seasonality_in_model_predictions <- read_csv("1_generated_data/2_seasonality_in_model_predictions_t2m.csv") %>%
  dplyr::rename(valid_time = prediction_time)

running_window_predictions <- read_csv("1_generated_data/3_running_window_predictions_t2m.csv") %>%
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
  ) %>%
  drop_na() %>%
  mutate(
    CRPS = crps(y = observation, family = "normal", mean = predicted_mu, sd = predicted_sigma),
    PIT = pnorm(observation, mean = predicted_mu, sd = predicted_sigma),
    MSE = (observation - ensemble_mean)^2
  )

# Extract best models
seasonality_in_model_predictions <- seasonality_in_model_predictions %>%
  filter(model %in% c("Lead time continuous model 1", "Lead time separated EMOS")) %>%
  mutate(
    model = case_when(
      model == "Lead time separated EMOS" ~ "Separated EMOS - Seasonality in model",
      model == "Lead time continuous model 1" ~ "Continuous EMOS - Seasonality in model",
      TRUE ~ NA_character_
    )
  )

running_window_predictions <- running_window_predictions %>%
  filter(model %in% c("Lead time continuous model 1", "Lead time separated EMOS - log link")) %>%
  mutate(
    model = case_when(
      model == "Lead time separated EMOS - log link" ~ "Separated EMOS - Running window",
      model == "Lead time continuous model 1" ~ "Continuous EMOS - Running window",
      TRUE ~ NA_character_
    )
  )

# Restrict running window onto range of seasonality in model predictions
running_window_predictions <- running_window_predictions %>%
  left_join(
    seasonality_in_model_predictions %>%
      distinct(location, issue_time, lead_time, forecast_period, valid_time) %>%
      mutate(in_seasonality_in_model_predictions = TRUE),
    by = c("location", "issue_time", "lead_time", "forecast_period", "valid_time")
  ) %>%
  filter(in_seasonality_in_model_predictions == TRUE)

# Merge dataframes
results <- rbind(
  seasonality_in_model_predictions %>% dplyr::select(location, issue_time, lead_time, forecast_period, valid_time, doy, observation, ensemble_mean, ensemble_sd, model, predicted_mu, predicted_sigma, CRPS, PIT, MSE),
  running_window_predictions %>% dplyr::select(location, issue_time, lead_time, forecast_period, valid_time, doy, observation, ensemble_mean, ensemble_sd, model, predicted_mu, predicted_sigma, CRPS, PIT, MSE)
)

# Plot results
results %>%
  mutate(
    Seasonality = if_else(grepl("Seasonality", model), "Seasonality in Model (-SWM)", "Running window (-RWIN)"),
    EMOS_type = if_else(grepl("Continuous", model), "Continuous (C-)", "Separated (S-)")
  ) %>%
  group_by(lead_time, Seasonality, EMOS_type) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  ggplot(aes(lead_time, CRPS, colour = EMOS_type, linetype = Seasonality)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "EMOS type", values = c("Separated (S-)" = "#000000", "Continuous (C-)" = "#E69F00")) +
  scale_linetype_manual(values = c("Seasonality in Model (-SWM)" = "solid", "Running window (-RWIN)" = "dotdash")) +
  theme_classic() +
  ylab("CRPS") +
  xlab("Lead time (hours)") +
  scale_x_continuous(breaks = 24 * c(0:8)) +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 14, hjust = 0.5), legend.title = element_text(size = 16), legend.text = element_text(size = 12))
ggsave("2_generated_plots/4_comparison/fig14a_crps_combined_t2m.png", width = 7, height = 5)

# Plot results by location
results %>%
  group_by(lead_time, model, location) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  ggplot(aes(lead_time, CRPS, colour = model)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Separated EMOS - Running window" = "#ffa300",
      "Continuous EMOS - Running window" = "#e6d800",
      "Separated EMOS - Seasonality in model" = "#0bb4ff",
      "Continuous EMOS - Seasonality in model" = "#b3d4ff"
    )
  ) +
  theme_classic() +
  ylab("CRPS score") +
  xlab("Lead time (hours)") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  facet_wrap(~location)
# ggsave("2_generated_plots/4_comparison/crps_combined_by_location_t2m.png", width = 7, height = 5)


# Calculate and plot Energy and p Variogram score

calc_energy_score_normal <- function(mu, sigma, obs, n = 50) {
  samples <- matrix(nrow = length(mu), ncol = n)
  for (i in 1:length(mu)) {
    samples[i, ] <- rnorm(n = n, mean = mu[i], sd = sigma[i])
  }
  return(es_sample(y = obs, dat = samples))
}

calc_pvar_score_normal <- function(mu, sigma, obs, n = 50) {
  samples <- matrix(nrow = length(mu), ncol = n)
  for (i in 1:length(mu)) {
    samples[i, ] <- rnorm(n = n, mean = mu[i], sd = sigma[i])
  }
  return(vs_sample(y = obs, dat = samples))
}

# Plot energy score
results %>%
  group_by(model, location, issue_time) %>%
  summarise(es = calc_energy_score_normal(mu = predicted_mu, sigma = predicted_sigma, obs = observation, n = 500), .groups = "drop") %>%
  group_by(model, location) %>%
  summarise(es = mean(es), .groups = "drop") %>%
  mutate(
    `Model type` = if_else(grepl("Separated", model), "Separated (S-)", "Continuous (C-)"),
    `Training type` = if_else(grepl("Running", model), "Running Window \n(-RWIN)", "Seasonality in model \n(-SWM)"),
    model = interaction(`Model type`, `Training type`, sep = " - ")
  ) %>%
  ggplot(aes(`Training type`, es, fill = `Model type`)) +
  geom_boxplot() +
  scale_fill_manual(name = "EMOS type", values = c("Separated (S-)" = "#ffa300", "Continuous (C-)" = "#0bb4ff")) +
  # geom_line(aes(group = interaction(`Training type`, location)), alpha = 0.5, col = "darkgrey") +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  ylab("Energy score")
ggsave("2_generated_plots/4_comparison/energy_score_t2m.png", width = 7, height = 5)

# Plot p variogram score
results %>%
  group_by(model, location, issue_time) %>%
  summarise(pvar = calc_pvar_score_normal(mu = predicted_mu, sigma = predicted_sigma, obs = observation, n = 500), .groups = "drop") %>%
  group_by(model, location) %>%
  summarise(pvar = mean(pvar), .groups = "drop") %>%
  mutate(
    `Model type` = if_else(grepl("Separated", model), "Separated (S-)", "Continuous (C-)"),
    `Training type` = if_else(grepl("Running", model), "Running Window \n(-RWIN)", "Seasonality in model \n(-SWM)"),
    model = interaction(`Model type`, `Training type`, sep = " - ")
  ) %>%
  ggplot(aes(`Training type`, pvar, fill = `Model type`)) +
  geom_boxplot() +
  scale_fill_manual(name = "EMOS type", values = c("Separated (S-)" = "#ffa300", "Continuous (C-)" = "#0bb4ff")) +
  # geom_line(aes(group = interaction(`Training type`, location)), alpha = 0.5, col = "darkgrey") +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  ylab("p Variogram score (p = 0.5)")
ggsave("2_generated_plots/4_comparison/p_variogram_score_t2m.png", width = 7, height = 5)


# Calculate and analyse twCRPS

vec_twCRPS_normal <- function(mu, sigma, y, t) {
  twCRPS <- rep(0, length(y))

  s <- (t - mu) / sigma
  z <- (y - mu) / sigma

  indices_bigger <- (y > t)

  twCRPS[indices_bigger] <- sigma[indices_bigger] * .twCRPS_y_bigger_t(s[indices_bigger], z[indices_bigger])
  twCRPS[!indices_bigger] <- sigma[!indices_bigger] * .twCRPS_y_smaller_t(s[!indices_bigger], z[!indices_bigger])

  return(twCRPS)
}


twCRPS_normal <- function(mu, sigma, y, t) {
  s <- (t - mu) / sigma
  z <- (y - mu) / sigma

  if (y > t) {
    return(
      sigma * .twCRPS_y_bigger_t(s, z)
    )
  } else {
    return(
      sigma * .twCRPS_y_smaller_t(s, z)
    )
  }
}


.twCRPS_y_bigger_t <- function(s, z) {
  return(
    -s * pnorm(s)^2 + z * (2 * pnorm(z) - 1) + 2 * (dnorm(z) - dnorm(s) * pnorm(s)) - (1 / sqrt(pi)) * (1 - pnorm(sqrt(2) * s))
  )
}

.twCRPS_y_smaller_t <- function(s, z) {
  return(
    -s * (1 - pnorm(s))^2 + 2 * dnorm(s) * (1 - pnorm(s)) - (1 / sqrt(pi)) * (1 - pnorm(sqrt(2) * s))
  )
}

# Calculate twCRPS
results_twcrps <- list()
for (q in seq(from = 0.85, to = 0.95, by = 0.01)) {
  results_twcrps[[as.character(q)]] <- results %>%
    mutate(
      twCRPS = vec_twCRPS_normal(
        mu = predicted_mu,
        sigma = predicted_sigma,
        y = observation,
        t = quantile(observation, q)
      ),
      t = quantile(observation, q),
      q = q
    ) %>%
    group_by(location, lead_time, model) %>%
    summarise(twCRPS = mean(twCRPS), t = mean(t), q = mean(q), .groups = "drop")
}
results_twcrps <- bind_rows(results_twcrps)

# Plot skill score wrapped by lead time
results_twcrps %>%
  group_by(model, q, t, lead_time) %>%
  summarise(twCRPS = mean(twCRPS), .groups = "drop") %>%
  pivot_wider(names_from = "model", values_from = "twCRPS") %>%
  mutate(across(contains("EMOS"), ~ 1 - .x / `Separated EMOS - Running window`)) %>%
  pivot_longer(contains("EMOS"), names_to = "model", values_to = "twCRPS") %>%
  filter(model != "Separated EMOS - Running window") %>%
  ggplot(aes(x = q, y = twCRPS, col = model)) +
  geom_line() +
  xlab("Quantile") +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  xlab("twCRPSS") +
  facet_wrap(~lead_time)

# 2. Wind speed ------------------------------------------------------------------

seasonality_in_model_predictions <- read_csv("1_generated_data/2_seasonality_in_model_predictions_wind_speed.csv") %>%
  dplyr::rename(valid_time = prediction_time)

running_window_predictions <- read_csv("1_generated_data/3_running_window_predictions_wind_speed.csv") %>%
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
  ) %>%
  drop_na() %>%
  mutate(
    CRPS = crps(y = observation, family = "normal", mean = predicted_mu, sd = predicted_sigma),
    PIT = pnorm(observation, mean = predicted_mu, sd = predicted_sigma),
    MSE = (observation - ensemble_mean)^2
  )

# Extract best models
seasonality_in_model_predictions <- seasonality_in_model_predictions %>%
  filter(model %in% c("Lead time continuous model 1", "Lead time separated EMOS")) %>%
  mutate(
    model = case_when(
      model == "Lead time separated EMOS" ~ "Separated EMOS - Seasonality in model",
      model == "Lead time continuous model 1" ~ "Continuous EMOS - Seasonality in model",
      TRUE ~ NA_character_
    )
  )

running_window_predictions <- running_window_predictions %>%
  filter(model %in% c("Lead time continuous model 2", "Lead time separated EMOS - log link")) %>%
  mutate(
    model = case_when(
      model == "Lead time separated EMOS - log link" ~ "Separated EMOS - Running window",
      model == "Lead time continuous model 2" ~ "Continuous EMOS - Running window",
      TRUE ~ NA_character_
    )
  )

# Restrict running window onto range of seasonality in model predictions
running_window_predictions <- running_window_predictions %>%
  left_join(
    seasonality_in_model_predictions %>%
      distinct(location, issue_time, lead_time, forecast_period, valid_time) %>%
      mutate(in_seasonality_in_model_predictions = TRUE),
    by = c("location", "issue_time", "lead_time", "forecast_period", "valid_time")
  ) %>%
  filter(in_seasonality_in_model_predictions == TRUE)

# Merge dataframes
results <- rbind(
  seasonality_in_model_predictions %>% dplyr::select(location, issue_time, lead_time, forecast_period, valid_time, doy, observation, ensemble_mean, ensemble_sd, model, predicted_mu, predicted_sigma, CRPS, PIT, MSE),
  running_window_predictions %>% dplyr::select(location, issue_time, lead_time, forecast_period, valid_time, doy, observation, ensemble_mean, ensemble_sd, model, predicted_mu, predicted_sigma, CRPS, PIT, MSE)
)

# Plot results
results %>%
  mutate(
    Seasonality = if_else(grepl("Seasonality", model), "Seasonality in Model (-SWM)", "Running window (-RWIN)"),
    EMOS_type = if_else(grepl("Continuous", model), "Continuous (C-)", "Separated (S-)")
  ) %>%
  group_by(lead_time, Seasonality, EMOS_type) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  ggplot(aes(lead_time, CRPS, colour = EMOS_type, linetype = Seasonality)) +
  geom_line(linewidth = 1) +
  scale_color_manual(name = "EMOS type", values = c("Separated (S-)" = "#000000", "Continuous (C-)" = "#E69F00")) +
  scale_linetype_manual(values = c("Seasonality in Model (-SWM)" = "solid", "Running window (-RWIN)" = "dotdash")) +
  theme_classic() +
  ylab("CRPS") +
  xlab("Lead time (hours)") +
  scale_x_continuous(breaks = 24 * c(0:8)) +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5))
ggsave("2_generated_plots/4_comparison/fig14b_crps_combined_wind_speed.png", width = 7, height = 5)

# Plot results by location
results %>%
  group_by(lead_time, model, location) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  ggplot(aes(lead_time, CRPS, colour = model)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Separated EMOS - Running window" = "#ffa300",
      "Continuous EMOS - Running window" = "#e6d800",
      "Separated EMOS - Seasonality in model" = "#0bb4ff",
      "Continuous EMOS - Seasonality in model" = "#b3d4ff"
    )
  ) +
  theme_classic() +
  ylab("CRPS score") +
  xlab("Lead time (hours)") +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  facet_wrap(~location)
# ggsave("2_generated_plots/4_comparison/crps_combined_by_location_wind_speed.png", width = 7, height = 5)


# Calculate and plot Energy and p Variogram score

# Initialize truncated distribution
library("gamlss")
library("gamlss.tr")
gen.trun(par = c(0), family = "NO", name = "tr", type = "left")

calc_energy_score_trunc_normal <- function(mu, sigma, obs, n = 50) {
  samples <- matrix(nrow = length(mu), ncol = n)
  for (i in 1:length(mu)) {
    samples[i, ] <- rNOtr(n = n, mu = mu[i], sigma = sigma[i])
  }
  return(es_sample(y = obs, dat = samples))
}

calc_pvar_score_trunc_normal <- function(mu, sigma, obs, n = 50) {
  samples <- matrix(nrow = length(mu), ncol = n)
  for (i in 1:length(mu)) {
    samples[i, ] <- rNOtr(n = n, mu = mu[i], sigma = sigma[i])
  }
  return(vs_sample(y = obs, dat = samples))
}

# Plot energy score
results %>%
  group_by(model, location, issue_time) %>%
  summarise(es = calc_energy_score_trunc_normal(mu = predicted_mu, sigma = predicted_sigma, obs = observation, n = 500), .groups = "drop") %>%
  group_by(model, location) %>%
  summarise(es = mean(es), .groups = "drop") %>%
  mutate(
    `Model type` = if_else(grepl("Separated", model), "Separated (S-)", "Continuous (C-)"),
    `Training type` = if_else(grepl("Running", model), "Running Window \n(-RWIN)", "Seasonality in model \n(-SWM)"),
    model = interaction(`Model type`, `Training type`, sep = " - ")
  ) %>%
  ggplot(aes(`Training type`, es, fill = `Model type`)) +
  geom_boxplot() +
  scale_fill_manual(name = "EMOS type", values = c("Separated (S-)" = "#ffa300", "Continuous (C-)" = "#0bb4ff")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  ylab("Energy score")
ggsave("2_generated_plots/4_comparison/fig15a_energy_score_wind_speed.png", width = 7, height = 5)

# Plot p variogram score
results %>%
  group_by(model, location, issue_time) %>%
  summarise(pvar = calc_pvar_score_trunc_normal(mu = predicted_mu, sigma = predicted_sigma, obs = observation, n = 500), .groups = "drop") %>%
  group_by(model, location) %>%
  summarise(pvar = mean(pvar), .groups = "drop") %>%
  mutate(
    `Model type` = if_else(grepl("Separated", model), "Separated (S-)", "Continuous (C-)"),
    `Training type` = if_else(grepl("Running", model), "Running Window \n(-RWIN)", "Seasonality in model \n(-SWM)"),
    model = interaction(`Model type`, `Training type`, sep = " - ")
  ) %>%
  ggplot(aes(`Training type`, pvar, fill = `Model type`)) +
  geom_boxplot() +
  scale_fill_manual(name = "EMOS type", values = c("Separated (S-)" = "#ffa300", "Continuous (C-)" = "#0bb4ff")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  ylab("p Variogram score (p = 0.5)")
ggsave("2_generated_plots/4_comparison/fig15b_p_variogram_score_wind_speed.png", width = 7, height = 5)


# twCRPS
vec_twCRPS_trunc_normal <- function(mu, sigma, y, t) {
  twCRPS <- rep(0, length(y))

  s <- (t - mu) / sigma
  z <- (y - mu) / sigma
  alpha <- -mu / sigma

  indices_bigger <- (y > t)

  twCRPS[indices_bigger] <- sigma[indices_bigger] * .trunc_twCRPS_y_bigger_t(s[indices_bigger], z[indices_bigger], alpha[indices_bigger])
  twCRPS[!indices_bigger] <- sigma[!indices_bigger] * .trunc_twCRPS_y_smaller_t(s[!indices_bigger], z[!indices_bigger], alpha[!indices_bigger])

  return(twCRPS)
}


twCRPS_trunc_normal <- function(mu, sigma, y, t) {
  alpha <- -mu / sigma

  s <- (t - mu) / sigma
  z <- (y - mu) / sigma

  if (y > t) {
    return(
      sigma * .trunc_twCRPS_y_bigger_t(s, z, alpha)
    )
  } else {
    return(
      sigma * .trunc_twCRPS_y_smaller_t(s, z, alpha)
    )
  }
}


Phi_quotient <- function(x, alpha) {
  return(
    (pnorm(x) - pnorm(alpha)) / (1 - pnorm(alpha))
  )
}

phi_quotient <- function(x, alpha) {
  return(
    dnorm(x) / (1 - pnorm(alpha))
  )
}

.trunc_twCRPS_y_smaller_t <- function(s, z, alpha) {
  term_1 <- Phi_quotient(s, alpha)
  term_2 <- phi_quotient(s, alpha)
  term_3 <- (1 / sqrt(pi)) * (1 - pnorm(sqrt(2) * s)) / ((1 - pnorm(alpha))^2)

  return(
    -s * (1 - term_1)^2 + 2 * term_2 * (1 - term_1) - term_3
  )
}

.trunc_twCRPS_y_bigger_t <- function(s, z, alpha) {
  term_1 <- Phi_quotient(s, alpha)
  term_1_z <- Phi_quotient(z, alpha)
  term_2 <- phi_quotient(s, alpha)
  term_2_z <- phi_quotient(z, alpha)
  term_3 <- (1 / sqrt(pi)) * (1 - pnorm(sqrt(2) * s)) / ((1 - pnorm(alpha))^2)

  return(
    -s * (term_1)^2 + z * (2 * term_1_z - 1) + 2 * (term_2_z - term_2 * term_1) - term_3
  )
}

# Calculate twCRPS
results_twcrps <- list()
for (q in seq(from = 0.5, to = 0.95, by = 0.01)) {
  results_twcrps[[as.character(q)]] <- results %>%
    mutate(
      twCRPS = vec_twCRPS_trunc_normal(
        mu = predicted_mu,
        sigma = predicted_sigma,
        y = observation,
        t = quantile(observation, q)
      ),
      t = quantile(observation, q),
      q = q
    ) %>%
    group_by(location, lead_time, model) %>%
    summarise(twCRPS = mean(twCRPS, na.rm = T), t = mean(t, na.rm = T), q = mean(q, na.rm = T), .groups = "drop")
}
results_twcrps <- bind_rows(results_twcrps)

# Plot skill score wrapped by lead time
results_twcrps %>%
  group_by(model, q, t, lead_time) %>%
  summarise(twCRPS = mean(twCRPS), .groups = "drop") %>%
  pivot_wider(names_from = "model", values_from = "twCRPS") %>%
  mutate(across(contains("EMOS"), ~ 1 - .x / `Separated EMOS - Running window`)) %>%
  pivot_longer(contains("EMOS"), names_to = "model", values_to = "twCRPS") %>%
  filter(model != "Separated EMOS - Running window") %>%
  ggplot(aes(x = t, y = twCRPS, col = model)) +
  geom_line(linewidth = 1.2) +
  ylab("twCRPSS") +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  xlab("Lead time (hours") +
  facet_wrap(~lead_time)

# Plot skill score merged over lead times
results_twcrps %>%
  group_by(model, q, t) %>%
  summarise(twCRPS = mean(twCRPS), .groups = "drop") %>%
  pivot_wider(names_from = "model", values_from = "twCRPS") %>%
  mutate(across(contains("EMOS"), ~ 1 - .x / `Separated EMOS - Running window`)) %>%
  pivot_longer(contains("EMOS"), names_to = "model", values_to = "twCRPS") %>%
  filter(model != "Separated EMOS - Running window") %>%
  mutate(type = if_else(grepl("Continuous", model), "Continuous (C-)", "Separated (S-)"), training = if_else(grepl("Running", model), "Running window (-RWIN)", "Seasonality in Model (-SWM)")) %>%
  ggplot(aes(x = t, y = twCRPS, col = type, linetype = training)) +
  geom_line(linewidth = 1.2) +
  ylab("twCRPSS") +
  scale_color_manual(name = "EMOS type", values = c("Separated (S-)" = "#000000", "Continuous (C-)" = "#E69F00")) +
  scale_linetype_manual(name = "Seasonality", values = c("Seasonality in Model (-SWM)" = "solid", "Running window (-RWIN)" = "dotdash")) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5)) +
  xlab("Threshold t (m/s)")
ggsave("2_generated_plots/4_comparison/fig16_twcrps_wind_speed.png", width = 7, height = 5)
