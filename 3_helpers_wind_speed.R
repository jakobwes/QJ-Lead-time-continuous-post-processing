# Helpers to apply for one issue date -----------------------------------------------

.train_and_predict_ngr <- function(train, test, how = "ML"){
  
  # Use bamlss and ML fit for lognormal, because not available in crch
  if(how == "ML"){
    fit <- bamlss(
      formula = list(observation ~ ensemble_mean, sigma ~ log_ensemble_sd),
      data = train,
      sampler = FALSE, 
      family = NOtr
    )
    
    predictions <-  predict(fit, newdata = test, type = "parameter")
    
    test$predicted_mu <- predictions$mu
    test$predicted_sigma <- predictions$sigma
    
  } else if(how == "CRPS"){
    fit <- crch(
      formula = observation ~ ensemble_mean | log_ensemble_sd,
      data = train,
      link.scale = "log",
      dist = "gaussian",
      type = "crps",
      truncated = TRUE,
      left = 0
    )
    
    mu <- predict(fit, newdata = test, type = "location")
    sigma <- predict(fit, newdata = test, type = "scale")
    
    test$predicted_mu <- mu
    test$predicted_sigma <- sigma
    
  } else {
    
    stop("Wrong argument for how")
    
  }
  
  
  return(test)
  
}


.get_train <- function(df, issue_time_prediction, training_window_length, mode = "full"){
  
  if(mode == "full"){
    df %>%
      filter(
        valid_time < issue_time_prediction &
          valid_time >= (issue_time_prediction - training_window_length)
      )
  } else if(mode == "staggered"){
    df %>%
      filter(
        valid_time < issue_time_prediction &
          issue_time >= (issue_time_prediction - training_window_length)
      )
  } else {
    stop("mode needs to be one of c(\"staggered\", \"full\")")
  }
  
}

.get_test <- function(df, issue_time_prediction){
  df %>%
    filter(issue_time == issue_time_prediction)
}

.predict_lead_time_continuous_one_issue_date <- function(issue_time_prediction, df, formula, training_window_length = 30, train_mode = "full"){
  
  # Get train and testing dataset
  train <- .get_train(df, issue_time_prediction, training_window_length, mode = train_mode)
  test <- .get_test(df, issue_time_prediction)
  
  # Check if we are not within the first training_window_length days
  if (length(unique(train$issue_time)) >= training_window_length) {
    
    # Lead time continuous model
    fit <- bamlss(
      formula,
      data = train,
      sampler = FALSE, 
      family = NOtr
    )
    
    predictions <-  predict(fit, newdata = test, type = "parameter")
    
    test$predicted_mu <- predictions$mu
    test$predicted_sigma <- predictions$sigma
    
    return(test)
  }
}

.predict_lead_time_continuous_one_issue_date_crps <- function(issue_time_prediction, df, formula, training_window_length = 30, train_mode = "full"){
  
  # Get train and testing dataset
  train <- .get_train(df, issue_time_prediction, training_window_length, mode = train_mode)
  test <- .get_test(df, issue_time_prediction)
  
  # Check if we are not within the first training_window_length days
  if (length(unique(train$issue_time)) >= training_window_length) {
    
    # Lead time continuous model
    fit <- crch(
      formula,
      data = train,
      link.scale = "log",
      dist = "gaussian",
      type = "crps",
      truncated = TRUE,
      left = 0
    )
    
    mu <- predict(fit, newdata = test, type = "location")
    sigma <- predict(fit, newdata = test, type = "scale")
    
    test$predicted_mu <- mu
    test$predicted_sigma <- sigma
    
    return(test)
  }
}

.predict_lead_time_separated_one_issue_date<- function(issue_time_prediction, df, training_window_length = 30, how = "ML"){
  
  # Get train and testing dataset
  train_all_lead_times <- .get_train(df, issue_time_prediction, training_window_length)
  test_all_lead_times <- .get_test(df, issue_time_prediction)
  
  # Check if we are not within the first training_window_length days
  if (length(unique(train_all_lead_times$issue_time)) >= training_window_length) {
    
    results_all_lead_times <- list()
    
    # Predictions lead time separated model
    for (chosen_lead_time in unique(test_all_lead_times$lead_time)) {
      
      train_one_lead_time <- train_all_lead_times %>% filter(lead_time == chosen_lead_time)
      test_one_lead_time <- test_all_lead_times %>% filter(lead_time == chosen_lead_time)
      
      predictions_one_lead_time <- .train_and_predict_ngr(train_one_lead_time, test_one_lead_time, how = how)
      
      results_all_lead_times[[as.character(chosen_lead_time)]] <- predictions_one_lead_time
    }
    
    predictions_separated <- bind_rows(results_all_lead_times)
    
    return(predictions_separated)
  }
}
