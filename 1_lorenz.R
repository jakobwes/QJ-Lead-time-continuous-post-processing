library("tidyverse")
library("crch")
library("latex2exp")


# Training data
train <- read_csv("0_data/nwp_train_mean_sd.csv") %>%
  filter(variable == 0) %>%
  filter(lead_time_i < 30) %>%
  mutate(log_ensemble_sd = log(ensemble_sd))

# Testing data
test <- read_csv("0_data/nwp_test_mean_sd.csv") %>%
  # filter(variable == 0) %>%
  filter(lead_time_i < 30) %>%
  mutate(log_ensemble_sd = log(ensemble_sd))


# Lead time separated models
train_lead_time_separated_ngr_models <- function(collapsed_ensembles, method = "crps", link_scale = "log") {
  models <- collapsed_ensembles %>%
    group_by(
      lead_time_i
    ) %>%
    do(
      model = crch(
        observation ~ ensemble_mean | log(ensemble_sd),
        link.scale = link_scale, data = ., type = method,
      )
    )
}

lead_time_separated_models <- train_lead_time_separated_ngr_models(train, link_scale = "log")


# 1. Plot lead time dependence of parameters ------------------------------

parameters <- lead_time_separated_models %>%
  mutate(
    alpha_mean = coef(model, "location")[1],
    beta_mean = coef(model, "location")[2],
    gamma_mean = coef(model, "scale")[1],
    delta_mean = coef(model, "scale")[2],
    alpha_lower = confint(model, level = 0.95)[1, 1],
    beta_lower = confint(model, level = 0.95)[2, 1],
    gamma_lower = confint(model, level = 0.95)[3, 1],
    delta_lower = confint(model, level = 0.95)[4, 1],
    alpha_higher = confint(model, level = 0.95)[1, 2],
    beta_higher = confint(model, level = 0.95)[2, 2],
    gamma_higher = confint(model, level = 0.95)[3, 2],
    delta_higher = confint(model, level = 0.95)[4, 2]
  ) %>%
  mutate(lead_time = lead_time_i / 2) %>%
  dplyr::select(c(lead_time, starts_with("alpha"), starts_with("beta"), starts_with("gamma"), starts_with("delta"))) %>%
  pivot_longer(c(starts_with("alpha"), starts_with("beta"), starts_with("gamma"), starts_with("delta")), names_to = c("parameter", "type"), names_sep = "_", values_to = "value") %>%
  pivot_wider(names_from = "type", values_from = "value")

# Alpha
p1 <- parameters %>%
  filter(parameter == "alpha") %>%
  ggplot(aes(lead_time, mean)) +
  geom_line(col = "red") +
  geom_ribbon(aes(ymin = lower, ymax = higher), alpha = 0.2) +
  theme_classic() +
  ylab(TeX("$\\alpha_t$")) +
  xlab("Lead time (days)") +
  ggtitle(TeX("$\\alpha_t$ as a function of lead time")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12), plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))

# Beta
p2 <- parameters %>%
  filter(parameter == "beta") %>%
  ggplot(aes(lead_time, mean)) +
  geom_line(col = "red") +
  geom_ribbon(aes(ymin = lower, ymax = higher), alpha = 0.2) +
  theme_classic() +
  ylab(TeX("$\\beta_t$")) +
  xlab("Lead time (days)") +
  ggtitle(TeX("$\\beta_t$ as a function of lead time")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12), plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))

# Gamma
p3 <- parameters %>%
  filter(parameter == "gamma") %>%
  ggplot(aes(lead_time, mean)) +
  geom_line(col = "red") +
  geom_ribbon(aes(ymin = lower, ymax = higher), alpha = 0.2) +
  theme_classic() +
  ylab(TeX("$\\gamma_t$")) +
  xlab("Lead time (days)") +
  ggtitle(TeX("$\\gamma_t$ as a function of lead time")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12), plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))

# Delta
p4 <- parameters %>%
  filter(parameter == "delta") %>%
  ggplot(aes(lead_time, mean)) +
  geom_line(col = "red") +
  geom_ribbon(aes(ymin = lower, ymax = higher), alpha = 0.2) +
  theme_classic() +
  ylab(TeX("$\\delta_t$")) +
  xlab("Lead time (days)") +
  ggtitle(TeX("$\\delta_t$ as a function of lead time")) +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12), plot.title = element_text(size = 14, hjust = 0.5, face = "bold"))

library("ggpubr")

ggpubr::ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
ggsave("2_generated_plots/1_lorenz/fig2_lorenz_parameters_over_lead_time.png", width = 7, height = 5)


# 2. Lead time continuous models ------------------------------------------

library("bamlss")

lead_time_continuous_model_1 <- bamlss(
  observation ~ ensemble_mean + s(lead_time) | log_ensemble_sd + s(lead_time),
  family = "gaussian",
  data = train,
  sampler = FALSE
)

lead_time_continuous_model_2 <- bamlss(
  observation ~ s(lead_time, by = ensemble_mean) + s(lead_time) | s(lead_time, by = log_ensemble_sd) + s(lead_time),
  family = "gaussian",
  data = train,
  sampler = FALSE
)

# Generate predictions lead time separated models
.predict_mu_ngr_model <- function(data, model) {
  predict(model, newdata = data, type = "location")
}

.predict_sigma_ngr_model <- function(data, model) {
  predict(model, newdata = data, type = "scale")
}

.predict_cdf_transform <- function(data, observation, model) {
  predict(model, newdata = data, at = observation, type = "probability")
}

.predict_crps_scores <- function(data, observation, model) {
  predict(model, newdata = data, at = observation, type = "crps")
}

# Prediction function for lead time separated model
predict_lead_time_separated_ngr_models <- function(test, models) {
  test %>%
    group_by(lead_time_i) %>%
    do(
      mutate(.,
        predicted_mean_separated = .predict_mu_ngr_model(., get(first(as.character(.$lead_time_i)), models)[[1]]),
        predicted_sd_separated = .predict_sigma_ngr_model(., get(first(as.character(.$lead_time_i)), models)[[1]]),
        PIT_separated = .predict_cdf_transform(., .$observation, get(first(as.character(.$lead_time_i)), models)[[1]]),
        CRPS_separated = .predict_crps_scores(., .$observation, get(first(as.character(.$lead_time_i)), models)[[1]]),
      )
    ) %>%
    ungroup()
}
lead_time_separated_models <- split(lead_time_separated_models$model, lead_time_separated_models$lead_time_i)
predictions_separated <- predict_lead_time_separated_ngr_models(test, lead_time_separated_models)


# Generate predictions lead time continuous models
library("scoringRules")

predictions_continuous <- test

predictions_model_1 <- predict(
  lead_time_continuous_model_1,
  type = "parameter",
  newdata = test
)
predictions_continuous$predicted_mean_continuous_1 <- predictions_model_1$mu
predictions_continuous$predicted_sd_continuous_1 <- predictions_model_1$sigma

predictions_model_2 <- predict(
  lead_time_continuous_model_2,
  type = "parameter",
  newdata = test
)
predictions_continuous$predicted_mean_continuous_2 <- predictions_model_2$mu
predictions_continuous$predicted_sd_continuous_2 <- predictions_model_2$sigma

library("gamlss")

predictions_continuous <- predictions_continuous %>% mutate(
  PIT_continuous_1 = pNO(observation, mu = predicted_mean_continuous_1, sigma = predicted_sd_continuous_1),
  CRPS_continuous_1 = crps(y = observation, family = "normal", mean = predicted_mean_continuous_1, sd = predicted_sd_continuous_1),
  PIT_continuous_2 = pNO(observation, mu = predicted_mean_continuous_2, sigma = predicted_sd_continuous_2),
  CRPS_continuous_2 = crps(y = observation, family = "normal", mean = predicted_mean_continuous_2, sd = predicted_sd_continuous_2)
)


# Merge lead time continuous and separated prediction
predictions_test <- predictions_separated %>% left_join(
  predictions_continuous %>% dplyr::select(c(variable, nwp_initialisation, lead_time_i, starts_with("predicted_mean_"), starts_with("predicted_sd_"), starts_with("PIT_"), starts_with("CRPS_"))),
  by = c(
    "variable",
    "nwp_initialisation",
    "lead_time_i"
  )
)


# Write to file
write_csv(predictions_test, "1_generated_data/lorenz_predictions_continuous_and_separated_models.csv")

predictions_test <- read_csv("1_generated_data/lorenz_predictions_continuous_and_separated_models.csv")

# Plot CRPS of predictions
p6 <- predictions_test %>%
  dplyr::select(lead_time_i, CRPS_separated, CRPS_continuous_1, CRPS_continuous_2) %>%
  pivot_longer(c(CRPS_separated, CRPS_continuous_1, CRPS_continuous_2), names_to = "type", values_to = "CRPS") %>%
  group_by(lead_time_i, type) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  mutate(
    lead_time = lead_time_i / 2,
    type = case_when(
      type == "CRPS_separated" ~ "Lead-time-separated",
      type == "CRPS_continuous_1" ~ "Lead-time-continuous 1",
      type == "CRPS_continuous_2" ~ "Lead-time-continuous 2",
      TRUE ~ NA_character_
    )
  ) %>%
  ggplot(aes(lead_time, CRPS, colour = type)) +
  geom_line() +
  theme_classic() +
  ylab("CRPS") +
  xlab("Lead time (days)") +
  scale_colour_discrete(name = "EMOS model") +
  theme(axis.text = element_text(size = 16), axis.title = element_text(size = 16), plot.title = element_text(size = 14, hjust = 0.5), legend.title = element_text(size = 16), legend.text = element_text(size = 12))
p6
ggsave("2_generated_plots/1_lorenz/fig4_lorenz_crps_score_lead_time_continuous_and_separated_models.png", p6, width = 7, height = 5)


# Plot PIT histogram
# Note: This only works if an equal amount of PIT values are in either of the three categories, which is the case here.
p7 <- predictions_test %>%
  filter(lead_time_i == 9) %>%
  dplyr::select(starts_with("PIT_")) %>%
  pivot_longer(starts_with("PIT_"), names_to = "type", values_to = "PIT") %>%
  mutate(
    type = case_when(
      type == "PIT_separated" ~ "Lead-time-separated EMOS",
      type == "PIT_continuous_1" ~ "Lead-time-continuous EMOS 1",
      type == "PIT_continuous_2" ~ "Lead-time-continuous EMOS 2",
      TRUE ~ NA_character_
    )
  ) %>%
  ggplot(aes(x = PIT)) +
  geom_histogram(bins = 20, boundary = 0, aes(y = 3 * 20 * after_stat(..count.. / sum(..count..)))) +
  ylab("") +
  scale_x_continuous(limits = c(0, 1)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1.0) +
  facet_wrap(~type) +
  theme_classic() +
  theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), plot.title = element_text(size = 14, hjust = 0.5), axis.text.y = element_blank(), axis.ticks.y = element_blank(), strip.text = element_text(size=14))
p7
ggsave("2_generated_plots/1_lorenz/fig5_lorenz_PIT_lead_time_5.png", p7, width = 10, height = 5)


# Get reliability index
get_reliability_index <- function(x) {
  histogram <- hist(x, breaks = 20, plot = FALSE)
  return(
    sum(abs(histogram$counts / length(x) - 1 / 20))
  )
}

predictions_test %>%
  filter(lead_time_i == 9) %>%
  pull(PIT_separated) %>%
  get_reliability_index()
predictions_test %>%
  filter(lead_time_i == 9) %>%
  pull(PIT_continuous_1) %>%
  get_reliability_index()
predictions_test %>%
  filter(lead_time_i == 9) %>%
  pull(PIT_continuous_2) %>%
  get_reliability_index()
