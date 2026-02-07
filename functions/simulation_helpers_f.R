
#### Functions to help with the simulation generative model ####

### Simulate History from Gamma Distribution
simulate_history <- function(
    n = n,
    newbie_vector = newbie,
    n_hypr_alpha_mu = 180,
    n_hypr_alpha_sd = 2,
    n_hypr_beta_newbie_mu = -115,
    n_hypr_beta_newbie_sd = 10,
    e_hypr_epsilon_rate = 1,
    g_shape = 1
  ){

  ### Distribution hyperparameters

  # Baseline history Sampled from Normal Distribution
  alpha <- rnorm(
    n = n,
    mean = n_hypr_alpha_mu,
    sd = n_hypr_alpha_sd
  )

  # The effect of not being Newbie to History sampled from Normal Distribution
  beta_newbie <- rnorm(
    n = n,
    mean = n_hypr_beta_newbie_mu,
    sd = n_hypr_beta_newbie_sd
  )

  # Introduce Randomness from Exponential Distribution
  epsilon <- rexp(n = n, rate = e_hypr_epsilon_rate)

  # Mean is calculated by simple linear equation
  mu <- alpha + beta_newbie * newbie + epsilon

  ### Gamma Distribution Parameters

  # Calculate the rate as the shape dev by the mu
  rate <- g_shape / mu

  # Sample from Gamma distribution
  history <- rgamma(n, shape = g_shape, rate = rate)

  ## Return History
  return(history)
}

#### Simulate Recency with Latent Model approach
simulate_recency <- function(
    n = n,
    newbie_vector = newbie,
    n_hypr_latent_mu_mu = 0,
    n_hypr_latent_mu_sd = 2,
    e_hypr_latent_sd_rate = 1,
    n_hypr_beta_newbie_mu = -0.150,
    n_hypr_beta_newbie_sd = 2
    ){

  # Empirical distribution
  base_counts <- c(8952, 7537, 5904, 5077, 4510, 4605,
                   4078, 3495, 6441, 7565, 3504, 2332)
  cum_props <- cumsum(base_counts) / sum(base_counts)

  # Cutpoints
  cutpoints <- qnorm(cum_props[-12])

  # Parameters for Normal latent scores
  latent_mu <- rnorm(n = n, mean = 0, sd = 2)  # Center at 0
  latent_sd <- rexp(n = n, rate = 1)           # SD = 1 (standard normal)

  # Newbie effect on latent  scale
  beta_newbie <- rnorm(n = n, mean = -0.150, sd = 2)

  # Generate latent scores
  latent_scores <- rnorm(
    n = n,
    mean = latent_mu + beta_newbie * newbie,
    sd = latent_sd
  )

  # Assign categories
  recency <- findInterval(latent_scores, cutpoints) + 1

  ## Return recency
  return(recency)
}

#### Function to Simulate Treatment assignments ####
simulate_treatment <- function(
    n = n,
    gender_var,
    channel_var,
    zip_code_var,
    recency_var,
    history_var,
    newbie_var,
    n_hypr_beta_channel_Phone_mu = 0.01294,
    n_hypr_beta_channel_Web_mu = 0.01602,
    n_hypr_beta_gender_mu = -0.01329,
    n_hypr_beta_zip_Surburban_mu = -0.02815,
    n_hypr_beta_zip_Urban_mu = -0.02094,
    n_hypr_beta_recency_mu = 0.001711,
    n_hypr_beta_newbie_mu = 0.001671,
    n_hypr_beta_history_mu = 2.756e-05,
    n_alpha_mu = 20
  ){

  # Gender Effect
  beta_gender <- rnorm(n = n, mean = n_hypr_beta_gender_mu, sd = 0.2)

  # Channel Effect
  beta_channel_effect <- c(
    channelPhone = rnorm(n = 1,mean = n_hypr_beta_channel_Phone_mu, sd = 0.1),
    channelWeb = rnorm(n = 1, mean = n_hypr_beta_channel_Web_mu, sd = 0.1)
  )

  # Channel Matrix
  beta_chanel_matrix <- model.matrix(~channel_var)[,-1]

  # Multiply the effect and the matrix to get a vector of effects
  channel_effect <- as.vector(beta_chanel_matrix  %*% beta_channel_effect)

  # Zip code Effect
  beta_zip_code_effect <- c(
    Surburban = rnorm(n = 1, mean = n_hypr_beta_zip_Surburban_mu, sd = 0.2),
    Urban = rnorm(n = 1, mean = n_hypr_beta_zip_Urban_mu, sd  = 0.2)
  )

  # Zip Code Matrix
  beta_zip_code_matrix <- model.matrix(~zip_code_var)[,-1]

  # Multiple the effect and the matrix to get a vector of effects
  zip_code_effect <- as.vector(beta_zip_code_matrix %*% beta_zip_code_effect)

  # Recency Effect
  beta_recency <- rnorm(n = n, mean = n_hypr_beta_recency_mu, sd = 0.2)

  # Newbie Effect
  beta_newbie <- rnorm(n = n, mean = n_hypr_beta_newbie_mu, sd = 0.2)

  # History Effect
  beta_history <- rnorm(n = n, mean = n_hypr_beta_history_mu, sd = 0.2)

  # Baseline prob
  alpha <- rnorm(n = n, mean = b_alpha_mu, sd = 0.1)

  ## Treatment Comes from Binomial Distribution with size = 1 ##
  p <- plogis(
    alpha +
      beta_gender * gender_var +
      channel_effect +
      zip_code_effect +
      beta_recency * recency_var +
      beta_newbie * newbie_var +
      beta_history * history_var
    )

  # Sample Treatment from Binomial  distribution
  treatment <- rbinom(n = n, size = 1, prob = p)

  # Return treatment
  return(treatment)
}

#### Function to Simulate Visits ####
simulate_visits <- function(
    n = n,
    treatment_var,
    channel_var,
    newbie_var,
    history_var,
    recency_var,
    gender_var,
    zip_code_var,
    n_hypr_beta_treatment_effect_mu = 0.5238,
    n_hypr_beta_channel_Phone_mu = -0.4,
    n_hypr_beta_channel_Web_mu = -0.2,
    n_hypr_beta_newbie_mu = -0.6,
    n_hypr_beta_history_mu = -3.903e-05,
    n_hypr_beta_recency_mu = -0.08,
    n_hypr_beta_gender_mu = 0.1,
    n_hypr_beta_zip_Surburban_mu = -0.45,
    n_hypr_beta_zip_Urban_mu = -0.4,
    n_alpha_mu = -6
    ){

  # Treatment Effect (Important What we are trying to recover later) #
  treatment_effect <- rnorm(
    n = n,
    mean = n_hypr_beta_treatment_effect_mu,
    sd = 0.1
  )

  # Channel Effect
  channel_effect <- c(
    Phone = rnorm(n = 1, mean = n_hypr_beta_channel_Phone_mu, sd = 0.2),
    Web = rnorm(n = 1, mean =  n_hypr_beta_channel_Web_mu, sd = 0.2)
  )

  # Chanel matrix
  channel_matrix <- model.matrix(~channel_var)[,-1]

  # Multiply the Channel effect by the Channel matrix
  beta_channel_effect <- as.vector(channel_matrix %*% channel_effect)

  # Newbie Effect
  beta_newbie <- rnorm(n = n, mean = n_hypr_beta_newbie_mu, sd = 0.1)

  # History Effect
  beta_history <- rnorm(n = n, mean = n_hypr_beta_history_mu , sd = 0.1)

  # Recency Effect
  beta_recency <- rnorm(n = n, mean = n_hypr_beta_recency_mu, sd = 0.1)

  # Gender Effect
  beta_gender <- rnorm(n = n, mean = n_hypr_beta_gender_mu, sd = 0.1)

  # Zip Code Effect
  zip_code_effect <- c(
    Surburban = rnorm(n = 1, mean = n_hypr_zip_Surburban_mu, sd = 0.1),
    Urban = rnorm(n = 1, mean = n_hypr_zip_Urban_mu , sd = 0.1)
  )

  # Zip code matrix
  zip_code_matrix <- model.matrix(~zip_code_var)[,-1]

  # Multiple the zip code effect by the zip_code matrix
  zip_code_effect <- as.vector(zip_code_matrix %*% zip_code_effect)

  # Baseline prob alpha
  alpha <- rnorm(n = n, mean = n_alpha_mu, sd = 0.1)

  # The prob p is equal to
  p <- plogis(
    alpha +
    zip_code_effect +
    beta_gender * gender_var +
    beta_recency * recency_var +
    beta_history * history_var +
    beta_newbie * newbie_var +
    beta_channel_effect +
    treatment_effect * treatment_var
  )

  # Sample Visits from Binomial distribution
  visits <- rbinom(n = n, size = 1, prob = p)

  # Return Visits
  return(visits)
}

#### Function to Simulate Conversion ####
simulate_conversion <- function(
    n = n,
    visits_var,
    zip_code_var,
    recency_var,
    channel_var,
    newbie_var,
    history_var,
    gender_var,
    n_hypr_beta_visits_mu = -0.1,
    n_hypr_beta_zip_Surburban_mu = -2,
    n_hypr_beta_zip_Urban_mu = -1.5,
    n_hypr_beta_recency_mu = -3,
    n_hypr_beta_channel_Phone_mu = -3,
    n_hypr_beta_channel_Web_mu = -2,
    n_hypr_beta_newbie_mu = -4,
    n_hypr_beta_history_mu = -0.0005,
    n_hypr_beta_gender_mu = 0.01,
    n_alpha_mu = -10
    ){

  # Visits Effect
  beta_visits_effect <- rnorm(n = n, mean = n_hypr_beta_visits_mu ,sd = 0.1)

  # Zip code Effect
  zip_code_effect <- c(
    Surburban  = rnorm(n = 1, mean = n_hypr_beta_zip_Surburban_mu, sd = 0.1),
    Urban = rnorm(n = 1, mean = n_hypr_beta_zip_Urban_mu, sd = 0.1)
  )
  # Zip code matrix
  zip_code_matrix <- model.matrix(~zip_code_var)[,-1]
  # Multiple the effect by the matrix
  zip_code_beta <- as.vector(zip_code_matrix %*% zip_code_effect)

  # Recency Effect
  beta_recency_effect <- rnorm(n = n, mean = n_hypr_beta_recency_mu, sd = 0.1)

  # Channel Effect
  channel_effect <- c(
    Phone = rnorm(n = 1, mean = n_hypr_beta_channel_Phone_mu, sd = 0.2),
    Web = rnorm(n = 1, mean = n_hypr_beta_channel_Web_mu, sd = 0.2)
  )
  # Channel Matrix
  channel_matrix <- model.matrix(~channel_var)[,-1]
  # Multiple the matrix by the effect
  beta_channel <- as.vector(channel_matrix %*% channel_effect)

  # Newbie Effect
  beta_newbie <- rnorm(n = n, mean = n_hypr_beta_newbie_mu, sd = 0.1)

  # History Effect
  beta_history <- rnorm(n = n, mean = n_hypr_beta_history_mu, sd = 0.1)

  # Gender Effect
  beta_gender <- rnorm(n = n, mean = n_hypr_beta_gender_mu, sd = 0.1)

  # Baseline prob
  alpha <- rnorm(n = n, mean = n_alpha_mu, sd = 0.1)

  # The prob p is equal to
  p <- plogis(
    alpha +
    beta_visits_effect * visits_var +
    zip_code_beta +
    beta_recency_effect * recency_var +
    beta_channel +
    beta_newbie * newbie_var +
    beta_history * history_var +
    beta_gender * gender_var
  )

  ## Sample Conversion from Binomial Distribution
  conversion <- rbinom(n = n, size = 1, prob = p)

  # Return Conversion
  return(conversion)
}

#### Function to simulate Spending ####
simulate_spending <- function(
  n = n,
  g_shape = 10,
  conversion_var,
  zip_code_var,
  recency_var,
  channel_var,
  history_var,
  gender_var,
  n_hypr_beta_converion_mu = 100,
  n_hypr_beta_recency_mu = -0.07,
  n_hypr_beta_zip_Surburan_mu = -0.2,
  n_hypr_beta_zip_Urban_mu = -0.14,
  n_hypr_beta_channel_Phone_mu = -0.5,
  n_hypr_beta_channel_Web_mu = -0.3,
  n_hypr_beta_gender_mu = 0.26,
  n_hypr_beta_history_mu = 0.0013,
  n_alpha_mu = 20
  ){

  # Conversion effect
  beta_conversion <- rnorm(n = n, mean = n_hypr_beta_converion_mu, sd = 1)

  # Zip code effect
  zip_code_effect <- c(
    Surburban = rnorm(n = 1,mean = n_hypr_beta_zip_Surburan_mu, sd = 1),
    Urban = rnorm(n = 1, mean = n_hypr_beta_zip_Urban_mu, sd = 1)
  )

  # Zip code Matrix
  zip_code_matrix <- model.matrix(~zip_code_var)[,-1]

  # Multiple the effect vector by the matrix
  zip_code_beta <- as.vector(zip_code_matrix %*% zip_code_effect)

  # Recency effect
  beta_recency <- rnorm(n = n, mean = n_hypr_beta_recency_mu, sd = 0.1)

  # Channel effect
  channel_effect <- c(
    Phone = rnorm(n = 1, mean = n_hypr_beta_channel_Phone_mu, sd = 1),
    Web = rnorm(n = 1, mean = n_hypr_beta_channel_Web_mu, sd = 1)
  )
  # Channel Matrix
  channel_matrix <- model.matrix(~channel_var)[,-1]
  # Multiple the effect vector by the matrix
  beta_channel <- as.vector(channel_matrix %*% channel_effect)

  # History effect
  beta_history <- rnorm(n = n, mean = n_hypr_beta_history_mu, sd = 0.1)

  # Gender effect
  beta_gender <- rnorm(n = n, mean = n_hypr_beta_gender_mu, sd = 1)

  # Baseline spending
  alpha <- rnorm(n = n, mean = n_alpha_mu, sd = 0.1)

  # Mean is calculated by simple linear equation
  mu <- c(
    alpha +
    beta_conversion * conversion_var +
    zip_code_beta +
    beta_recency * recency_var +
    beta_channel +
    beta_history * history_var +
    beta_gender * gender_var
  )

  # Calculate the rate as the shape dev by the mu
  rate <- pmax(g_shape / mu, 0)

  ## Sample Spending from Gamma distribution
  spending <- rgamma(n = n,shape = g_shape,rate = rate)

  # Return spending
  return(spending)
}




