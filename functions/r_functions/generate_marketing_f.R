
#### Function for Generative Model (MC Validation variant) ####
generate_marketing <- function(){

  ## Simulate Gender from Bernoulli dist with male edge
  gender <- rbinom(n = n, size = 1, prob = 0.6)

  ## Simulate Newbie Bernoulli dist with even chances
  newbie <- rbinom(n = n, size = 1, prob = 0.5)

  ## Simulate Zip Code
  # Level 0 Surburban ; Level 1 Urban ; Level 2 Rural
  zip_code <- sample(x = c(0,1,2), size = n, replace = TRUE, prob = c(0.4,0.4,0.2))

  ## Simulate Channel
  # Level 0 Phone ; Level 1 Web ; Level 2 Multichannel
  channel <- sample(x = c(0,1,2), size = n,replace = TRUE,prob = c(0.4,0.4,0.2))

  ## Simulate History
  history <- simulate_history(n = n, newbie_vector = newbie)

  ## Simulate Recency
  recency <- simulate_recency(n = n, newbie_vector = newbie)

  ## Simulate Treatment
  treatment <- simulate_treatment()

  ## Simulate Visits
  visits <- simulate_visits()

  ## Simulate Conversion
  conversion <- simulate_conversion()

  ## Simulate Spending
  spending <- simulate_spending()


}
