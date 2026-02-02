
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

  # The effect of being Newbie to History sampled from Normal Distribution
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
  rate <- shape / mu

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
    mean = latent_mu + newbie_effect * newbie,
    sd = latent_sd
  )

  # Assign categories
  recency <- findInterval(latent_scores, cutpoints) + 1

  ## Return recency
  return(recency)
}

#### Function to Simulate Treatment assignments ####
simulate_treatment <- function(
    n = n){


















}











