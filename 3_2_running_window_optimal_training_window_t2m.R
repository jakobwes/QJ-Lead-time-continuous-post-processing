library("tidyverse")
library("crch")
library("latex2exp")
library("lubridate")
library("bamlss")

df <- read_csv("0_data/t2m_merged_forecast_with_obs_short.csv")

df <- df %>% 
  drop_na() %>%
  mutate(
    lead_time = as.numeric(difftime(prediction_time, issue_time, units = "hours")),
    doy = as.numeric(difftime(prediction_time, make_date(year(prediction_time), 1, 1), units = "days")),
    doy_sin = sin(2*pi*doy/366),
    doy_cos = cos(2*pi*doy/366),
    tod = as.factor(doy - floor(doy)),
    log_ensemble_sd = log(ensemble_sd)
  ) %>%
  dplyr::select(
    location, issue_time, lead_time, forecast_period, prediction_time, doy, observation, ensemble_mean, ensemble_sd, doy_sin, doy_cos, tod, log_ensemble_sd
  ) %>% 
  rename(valid_time = prediction_time) %>%
  distinct()

df <- df %>% filter(location == 20)

df <- df %>%
  arrange(issue_time, lead_time) 


source("3_helpers_t2m.R")


# 1. Get optimal training window  -------------------------------

library("foreach")

n.cores <- parallel::detectCores() - 1
my.cluster <- parallel::makeCluster(n.cores, type = "PSOCK")
doParallel::registerDoParallel(cl = my.cluster)
foreach::getDoParRegistered()


# Lead time separated model 
results_separated <- foreach(
  issue_time_prediction = unique(df$issue_time),
  .packages = c("tidyverse", "crch"),
  .errorhandling = "remove"
) %dopar% {
  
  
  predictions <- list()
  training_window_lengths <- c(5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80)
  
  for(i in 1:length(training_window_lengths)){
    
    training_window_length <- training_window_lengths[i]
    
    # Get train and testing dataset
    train <- .get_train(df, issue_time_prediction, training_window_length) %>% filter(lead_time == 48)
    test <- .get_test(df, issue_time_prediction) %>% filter(lead_time == 48)
    
    # Check if we are not within the first training_window_length days
    if (nrow(train) >= training_window_length) {
      
      predictions_one_window_length <- .train_and_predict_ngr(train, test)
      predictions_one_window_length$training_window_length <- training_window_length
      predictions[[i]] <- predictions_one_window_length
      
    }
  }
  
  bind_rows(predictions)
  
}  


# Lead time continuous model 1
results_continuous <- foreach(
  issue_time_prediction = unique(df$issue_time),
  .packages = c("tidyverse", "crch", "bamlss"),
  .errorhandling = "remove"
) %dopar% {
  
  predictions <- list()
  training_window_lengths <- c(5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80)
  
  for(i in 1:length(training_window_lengths)){
    
    training_window_length <- training_window_lengths[i]

    predictions_one_window_length <- .predict_lead_time_continuous_one_issue_date_crps(
      issue_time_prediction,
      df = df,
      formula = observation ~ ensemble_mean + factor(tod) + lead_time | log_ensemble_sd + lead_time + factor(tod),
      training_window_length = training_window_length
    )
    predictions_one_window_length$training_window_length <- training_window_length
    
    predictions[[i]] <- predictions_one_window_length
    
  }
  
  bind_rows(predictions)
  
}

# Lead time continuous model 1 -- Staggered window
results_continuous_staggered <- foreach(
  issue_time_prediction = unique(df$issue_time),
  .packages = c("tidyverse", "crch", "bamlss"),
  .errorhandling = "remove"
) %dopar% {
  
  predictions <- list()
  training_window_lengths <- c(5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80)
  
  for(i in 1:length(training_window_lengths)){
    
    training_window_length <- training_window_lengths[i]
    
    predictions_one_window_length <- .predict_lead_time_continuous_one_issue_date_crps(
      issue_time_prediction,
      df = df,
      formula = observation ~ ensemble_mean + factor(tod) + lead_time | log_ensemble_sd + lead_time + factor(tod),
      training_window_length = training_window_length,
      train_mode = "staggered"
    )
    predictions_one_window_length$training_window_length <- training_window_length
    
    predictions[[i]] <- predictions_one_window_length
    
  }
  
  bind_rows(predictions)
  
}

results_separated <- bind_rows(results_separated)
results_continuous <- bind_rows(results_continuous) %>% filter(lead_time == 48)
results_continuous_staggered <- bind_rows(results_continuous_staggered) %>% filter(lead_time == 48)

results_separated <- results_separated %>% rename(predicted_mu_lead_time_separated = predicted_mu, predicted_sigma_lead_time_separated = predicted_sigma)
results_continuous <- results_continuous %>% rename(predicted_mu_lead_time_continuous_model = predicted_mu, predicted_sigma_lead_time_continuous_model = predicted_sigma)
results_continuous_staggered <- results_continuous_staggered %>% rename(predicted_mu_lead_time_continuous_model_staggered = predicted_mu, predicted_sigma_lead_time_continuous_model_staggered = predicted_sigma)

results <- results_separated %>% left_join(
  results_continuous %>% dplyr::select(c(issue_time, lead_time, forecast_period, valid_time, training_window_length, starts_with("predicted"))), by = c("issue_time", "lead_time", "forecast_period", "valid_time", "training_window_length")
) %>% left_join(
  results_continuous_staggered %>% dplyr::select(c(issue_time, lead_time, forecast_period, valid_time, training_window_length, starts_with("predicted"))), by = c("issue_time", "lead_time", "forecast_period", "valid_time", "training_window_length")
)

results <- results %>% 
  drop_na() %>%
  pivot_longer(starts_with("predicted_"), names_to = c("type", "model"), values_to = "value", names_sep = "_lead_time_") %>%
  pivot_wider(names_from = "type", values_from = "value") %>%
  mutate(
    model = case_when(
      model == "separated" ~ "Lead time separated EMOS",
      model == "continuous_model_staggered" ~ "Lead time continuous model -- Staggered window",
      model == "continuous_model" ~ "Lead time continuous model",
      TRUE ~ NA_character_
    )
  )

library ("scoringRules")

results <- results %>%
  mutate(
    CRPS = crps(y = observation, family = "normal", mean = predicted_mu, sd = predicted_sigma),
    PIT = pnorm(observation, mean = predicted_mu, sd = predicted_sigma),
    MSE = (observation - ensemble_mean)^2
  )

results %>% write_csv("1_generated_data/3_running_window_optimal_training_window_size_t2m.csv")


# 2. Evaluate and plot optimal training window ----------------------------

results <- read_csv("1_generated_data/3_running_window_optimal_training_window_size_t2m.csv")


# Plot CRPS of predictions
p1 <- results %>% 
  group_by(training_window_length, model) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  filter(training_window_length > 5, model != "Lead time continuous model -- Staggered window") %>%
  mutate(model = case_when(
    model == "Lead time separated EMOS" ~ "Lead time separated \n(S-RWIN)",
    model == "Lead time continuous model" ~ "Lead time continuous \n(C-RWIN)",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(training_window_length, CRPS, colour = model)) +
  scale_color_manual(
    name = "EMOS type",
    values = c(
      "Lead time separated \n(S-RWIN)" = "#000000",
      "Lead time continuous \n(C-RWIN)" = "#E69F00",
      "Lead time continuous model -- Staggered window" = "#56B4E9"
    )) +
  geom_line() + 
  geom_point() +
  theme_classic() + 
  ylab("CRPS for 48h predictions") + 
  xlab("Training window size") + 
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70, 80)) +
  ggtitle(TeX("CRPS for 48h predictions as a function of the training window size")) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14), plot.title = element_text(size = 14))
p1
ggsave("2_generated_plots/3_running_window/t2m_dependence_on_training_window_size.png", width = 7, height = 5)


# 3. Not in paper ---------------------------------------------------------

# Get optimal window size for each lead time (lead time continuous)
results <- foreach(
  issue_time_prediction = unique(df$issue_time),
  .packages = c("tidyverse", "bamlss"),
  .errorhandling = "remove"
) %dopar% {
  
  predictions <- list()
  training_window_lengths <- c(5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80)
  
  for(i in 1:length(training_window_lengths)){
    
    training_window_length <- training_window_lengths[i]
    
    predictions_one_window_length <- .predict_lead_time_continuous_one_issue_date(
      issue_time_prediction,
      df = df,
      formula = list(observation ~ ensemble_mean + lead_time + factor(tod), sigma ~ log_ensemble_sd + lead_time + factor(tod)),
      training_window_length = training_window_length
    )
    predictions_one_window_length$training_window_length <- training_window_length
    
    predictions[[i]] <- predictions_one_window_length
    
  }
  
  bind_rows(predictions)
  
}

results <- bind_rows(results) %>% drop_na()
test <- results %>%
  filter(!is.infinite(predicted_mu), !is.infinite(predicted_sigma)) %>%
  mutate(
    CRPS = crps(y = observation, family = "normal", mean = predicted_mu, sd = predicted_sigma),
    PIT = pnorm(observation, mean = predicted_mu, sd = predicted_sigma),
    MSE = (observation - ensemble_mean)^2
  ) %>%
  group_by(training_window_length, lead_time) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  group_by(lead_time) %>%
  summarise(optimal_training_window = training_window_length[CRPS == min(CRPS)], min_CRPS = min(CRPS), .groups = "drop") 

test %>% ggplot(aes(lead_time, optimal_training_window)) + geom_line()


# CRPS with optimal training window size
# Compare 25 day training window with separated lead time results 30 day training window
results_separated <- foreach(
  issue_time_prediction = unique(df$issue_time),
  .packages = c("tidyverse", "crch"),
  .errorhandling = "remove"
) %dopar% {
  predictions <- .predict_lead_time_separated_one_issue_date(
    issue_time_prediction,
    df = df,
    training_window_length = 40
  )
  
  predictions
  
}

results_continuous_1 <- foreach(
  issue_time_prediction = unique(df$issue_time),
  .packages = c("tidyverse", "bamlss"),
  .errorhandling = "remove"
) %dopar% {
  predictions <- .predict_lead_time_continuous_one_issue_date(
    issue_time_prediction,
    df = df,
    formula = list(observation ~ ensemble_mean + factor(tod) + lead_time, sigma ~ log_ensemble_sd + lead_time + factor(tod)),
    training_window_length = 25
  )
  
  predictions
}

results_continuous_2 <- foreach(
  issue_time_prediction = unique(df$issue_time),
  .packages = c("tidyverse", "bamlss"),
  .errorhandling = "remove"
) %dopar% {
  predictions <- .predict_lead_time_continuous_one_issue_date(
    issue_time_prediction,
    df = df,
    formula = list(observation ~ ensemble_mean + factor(tod) + lead_time, sigma ~ log_ensemble_sd + lead_time + factor(tod)),
    training_window_length = 40
  )
  
  predictions
}

results_separated <- bind_rows(results_separated)
results_continuous_1 <- bind_rows(results_continuous_1)
results_continuous_2 <- bind_rows(results_continuous_2)

results_separated <- results_separated %>% rename(predicted_mu_lead_time_separated = predicted_mu, predicted_sigma_lead_time_separated = predicted_sigma)
results_continuous_1 <- results_continuous_1 %>% rename(predicted_mu_lead_time_continuous_1 = predicted_mu, predicted_sigma_lead_time_continuous_1 = predicted_sigma)
results_continuous_2 <- results_continuous_2 %>% rename(predicted_mu_lead_time_continuous_2 = predicted_mu, predicted_sigma_lead_time_continuous_2 = predicted_sigma)

results <- results_separated %>% left_join(
  results_continuous_1 %>% dplyr::select(c(issue_time, lead_time, forecast_period, valid_time, starts_with("predicted"))), by = c("issue_time", "lead_time", "forecast_period", "valid_time")
) %>% left_join(
  results_continuous_2 %>% dplyr::select(c(issue_time, lead_time, forecast_period, valid_time, starts_with("predicted"))), by = c("issue_time", "lead_time", "forecast_period", "valid_time")
) %>% 
  drop_na() %>%
  pivot_longer(starts_with("predicted_"), names_to = c("type", "model"), values_to = "value", names_sep = "_lead_time_") %>%
  pivot_wider(names_from = "type", values_from = "value") %>%
  mutate(
    model = case_when(
      model == "separated" ~ "Lead time separated EMOS",
      model == "continuous_1" ~ "Lead time continuous model -- Optimal window",
      model == "continuous_2" ~ "Lead time continuous model -- 40 days",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    CRPS = crps(y = observation, family = "normal", mean = predicted_mu, sd = predicted_sigma),
    PIT = pnorm(observation, mean = predicted_mu, sd = predicted_sigma),
    MSE = (observation - ensemble_mean)^2
  )


# Plot CRPS score of predictions
results %>% 
  group_by(lead_time, model) %>%
  summarise(CRPS = mean(CRPS), .groups = "drop") %>%
  ggplot(aes(lead_time, CRPS, colour = model)) +
  scale_color_manual(
    values = c(
      "Lead time separated EMOS" = "#000000",
      "Lead time continuous model -- Optimal window" = "#E69F00",
      "Lead time continuous model -- 40 days" = "#56B4E9"
    )) +
  geom_line(linewidth = 1.2) + 
  geom_point() +
  theme_classic() + 
  ylab("CRPS score") + 
  xlab("Lead time") + 
  ggtitle(TeX("CRPS score as a function of the lead time")) + 
  theme(axis.text=element_text(size=14),axis.title=element_text(size=14), plot.title = element_text(size = 14,hjust = 0.5))
