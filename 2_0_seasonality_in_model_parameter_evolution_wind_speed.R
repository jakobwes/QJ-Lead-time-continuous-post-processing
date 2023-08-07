library("tidyverse")
library("crch")
library("latex2exp")
library("lubridate")

df <- read_csv("0_data/wind_speed_merged_forecast_with_obs_short.csv")

# Zero observation values are most likely due to rounding and are replaced by the smallest positive one. This affects 17 values.
df$observation[df$observation == 0] <- min(df$observation[df$observation > 0])

df <- df %>% 
  drop_na() %>%
  mutate(
    lead_time = as.numeric(difftime(prediction_time, issue_time, units = "hours")),
    doy = as.numeric(difftime(prediction_time, make_date(year(prediction_time), 1, 1), units = "days")),
    doy_sin = sin(2*pi*doy/366),
    doy_cos = cos(2*pi*doy/366),
    tod = as.factor(doy - floor(doy))
  ) %>%
  dplyr::select(
    location, issue_time, lead_time, forecast_period, prediction_time, doy, observation, ensemble_mean, ensemble_sd, doy_sin, doy_cos, tod
  ) %>% 
  distinct()

# Select one station
df <- df %>% filter(location == 20)

# Training data
train <- df %>%
  filter(year(issue_time) < 2021)

# Testing data
test <- df %>%
  filter(year(issue_time) >= 2021)

# Remove full data
rm(df)


# 1. Parameter evolution one station --------------------------------------

# Lead time separated models
predictions_lead_time_separated <- list()

start <- Sys.time()

lead_times <- unique(train$lead_time)

# Get model parameters
model_parameters <- data.frame(lead_time = lead_times)
model_parameters$mu_intercept <- 0
model_parameters$mu_mult <- 0
model_parameters$sigma_intercept <- 0
model_parameters$sigma_mult <- 0


for(i in 1:length(lead_times)){
  
  lead_time_i <- lead_times[i]
  
  print(paste0("------ Lead time ", lead_time_i, "h ------"))
  
  # Get train and test df
  train_df <- train %>% filter(lead_time == lead_time_i)  
  test_df <- test %>% filter(lead_time == lead_time_i)  
  
  # Fit model
  fit <-  crch(
    formula = observation ~ ensemble_mean + doy_sin + doy_cos | log(ensemble_sd) + doy_sin + doy_cos,
    data = train_df,
    link.scale = "log",
    dist = "gaussian",
    type = "crps",
    truncated = TRUE,
    left = 0
  )
  
  # Get parameters
  model_parameters$mu_intercept[i] <- coef(fit, "location")["(Intercept)"]
  model_parameters$mu_mult[i] <- coef(fit, "location")["ensemble_mean"]
  model_parameters$sigma_intercept[i] <- coef(fit, "scale")["(Intercept)"]
  model_parameters$sigma_mult[i] <- coef(fit, "scale")["log(ensemble_sd)"]
  
}

end <- Sys.time()

print(paste0(difftime(end, start, units = "secs"), "s elapsed for model training and prediction"))
predictions_lead_time_separated <- bind_rows(predictions_lead_time_separated)

# Visualize parameters: alpha
p1 <- model_parameters %>% 
  pivot_longer(c(mu_intercept, mu_mult, sigma_intercept, sigma_mult), names_to = "type", values_to = "vals") %>%
  filter(type == "mu_intercept") %>%
  ggplot(aes(lead_time, vals)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + 
  theme_classic() + 
  ylab(TeX("$\\alpha_t$")) + 
  xlab("Lead time (hours)") + 
  scale_x_continuous(breaks = 24*c(0:8)) + 
  ggtitle(TeX("$\\alpha_t$ as a function of lead time")) + 
  theme(axis.text=element_text(size=11),axis.title=element_text(size=11), plot.title = element_text(size = 11,hjust = 0.5, face = "bold"))

# Visualize parameters: beta
p2 <- model_parameters %>% 
  pivot_longer(c(mu_intercept, mu_mult, sigma_intercept, sigma_mult), names_to = "type", values_to = "vals") %>%
  filter(type == "mu_mult") %>%
  ggplot(aes(lead_time, vals)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + 
  theme_classic() + 
  ylab(TeX("$\\beta-t$")) + 
  xlab("Lead time (hours)") + 
  scale_x_continuous(breaks = 24*c(0:8)) + 
  ggtitle(TeX("$\\beta_t$ as a function of lead time")) + 
  theme(axis.text=element_text(size=11),axis.title=element_text(size=11), plot.title = element_text(size = 11,hjust = 0.5, face = "bold"))

# Visualize parameters: gamma
p3 <- model_parameters %>% 
  pivot_longer(c(mu_intercept, mu_mult, sigma_intercept, sigma_mult), names_to = "type", values_to = "vals") %>%
  filter(type == "sigma_intercept") %>%
  ggplot(aes(lead_time, vals)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + 
  theme_classic() + 
  ylab(TeX("$\\gamma_t$")) + 
  xlab("Lead time (hours)") + 
  scale_x_continuous(breaks = 24*c(0:8)) + 
  ggtitle(TeX("$\\gamma_t$ as a function of lead time")) + 
  theme(axis.text=element_text(size=11),axis.title=element_text(size=11), plot.title = element_text(size = 11,hjust = 0.5, face = "bold"))

# Visualize parameters: delta
p4 <- model_parameters %>% 
  pivot_longer(c(mu_intercept, mu_mult, sigma_intercept, sigma_mult), names_to = "type", values_to = "vals") %>%
  filter(type == "sigma_mult") %>%
  ggplot(aes(lead_time, vals)) + 
  geom_point() + 
  geom_line() + 
  geom_smooth(method=lm, se=FALSE, linetype="dashed", color="darkred") + 
  theme_classic() + 
  ylab(TeX("$\\delta_t$")) + 
  xlab("Lead time (hours)") + 
  scale_x_continuous(breaks = 24*c(0:8)) + 
  ggtitle(TeX("$\\delta_t$ as a function of lead time")) + 
  theme(axis.text=element_text(size=11),axis.title=element_text(size=11), plot.title = element_text(size = 11,hjust = 0.5, face = "bold"))

library("ggpubr")

ggpubr::ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2)
ggsave("2_generated_plots/2_seasonality_in_model/wind_speed_parameters_over_lead_time.png", width = 7, height = 5)

