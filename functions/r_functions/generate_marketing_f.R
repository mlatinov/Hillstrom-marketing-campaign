
#### Function for Generative Model (MC Validation variant) ####
generate_marketing <- function(){

  # Set seed
  set.seed(123)

  ## Simulate Gender from Bernoulli dist with male edge
  gender <- rbinom(n = n, size = 1, prob = 0.6)

  ## Simulate Newbie Bernoulli dist with even chances
  newbie <- rbinom(n = n, size = 1, prob = 0.5)

  ## Simulate Zip Code
  zip_code <- sample(
    x = c("Surburban","Urban","Rural"),
    size = n,
    replace = TRUE,
    prob = c(0.4,0.4,0.2)
  )

  ## Simulate Channel
  channel <- sample(
    x = c("Phone","Web","Multichannel"),
    size = n,
    replace = TRUE,
    prob = c(0.4,0.4,0.2)
  )

  ## Simulate History
  history <- simulate_history(n = n, newbie_vector = newbie)

  ## Simulate Recency
  recency <- simulate_recency(n = n, newbie_vector = newbie)

  ## Simulate Treatment
  treatment <- simulate_treatment(
    n = n,
    history_var = history,
    gender_var = gender,
    channel_var = channel,
    zip_code_var = zip_code,
    recency_var = recency,
    newbie_var = newbie
  )

  ## Simulate Visits
  visits <- simulate_visits(
    n = n,
    treatment_var = treatment,
    channel_var = channel,
    newbie_var = newbie,
    history_var = history,
    recency_var = recency,
    gender_var = gender,
    zip_code_var = zip_code
  )

  ## Simulate Conversion
  conversion <- simulate_conversion(
    n = n,
    visits_var = visits,
    zip_code_var = zip_code,
    recency_var = recency,
    channel_var = channel,
    newbie_var = newbie,
    history_var = history,
    gender_var = gender
  )

  ## Simulate Spending
  spending <- simulate_spending(
    n = n,
    channel_var = channel,
    conversion_var = conversion,
    zip_code_var = zip_code,
    recency_var = recency,
    history_var = history,
    gender_var = gender
  )

  ## Combine all and return dataframe
  simulated_data <- data.frame(
    gender,
    newbie,
    zip_code,
    channel,
    history,
    recency,
    treatment,
    visits,
    conversion,
    spending
  )

  # Return the simulated data
  return(simulated_data)

}
