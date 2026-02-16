
#### Function to call Stan visit model return the aggregated results ####
bayes_visits <- function(data,chains,iter_sampling,iter_warmup){

  ## Libraries
  library(cmdstanr)

  ## Point to Stan Model ##
  stan_model <- cmdstan_model(stan_file = "stan_scripts/stan_model_visits.stan")

  ## Prepare Data for Stan model ##
  stan_data <- list(
    N = nrow(data),
    Visits = as.numeric(as.character(data$visit)),
    Treatment = data$treatment,
    Newbie = data$newbie,
    History = data$history,
    Gender = data$gender,
    Recency = data$recency,
    Web = if_else(data$channel == "Web",1,0),
    Phone = if_else(data$channel == "Phone",1,0),
    Urban = if_else(data$zip_code == "Urban",1,0),
    Surburban = if_else(data$zip_code == "Surburban",1,0)
  )
  ## Fit Stan Model ##
  stan_fit <- stan_model$sample(
    data = stan_data,
    seed = 123,
    chains = 4,
    iter_sampling = 1000,
    iter_warmup = 1000
  )

  ## Save the model
  stan_fit$save_object("models/bayes_models/visit_model.rds")

  ## Return the model
  return(stan_fit)
}
