
#### Function to call Stan visit model return the aggrerated results ####
bayes_visits <- function(data){

  ## Libraries
  library(cmdstanr)

  ## Point to Stan Model ##
  stan_model <- cmdstan_model(stan_file = "stan_scripts/stan_model_visits.stan")

  ## Prepare Data for Stan model ##
  stan_data <- list(
    N = nrow(generative_model),
    Visits = generative_model$visits,
    Treatment = generative_model$treatment,
    Newbie = generative_model$newbie,
    History = generative_model$history,
    Gender = generative_model$gender,
    Recency = generative_model$recency,
    Web = if_else(generative_model$channel == "Web",1,0),
    Phone = if_else(generative_model$channel == "Phone",1,0),
    Urban = if_else(generative_model$zip_code == "Urban",1,0),
    Surburban = if_else(generative_model$zip_code == "Surburban",1,0)
  )

  ## Fit Stan Model ##
  stan_fit <- stan_model$sample(
    data = stan_data,
    seed = 123,
    chains = 4,
    iter_sampling = 1000,
    iter_warmup = 1000
  )

  summary <- stan_fit$summary()









}
