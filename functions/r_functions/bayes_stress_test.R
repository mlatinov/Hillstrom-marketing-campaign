
#### Function to Stress Test Bayes Estimators with simulated model ####
bayes_stress_test <- function(
    R,
    sample_sizes,
    estimator_path,
    treatment_visit_effect = 0.5238,
    chains = 4,
    iter_sampling = 1000,
    iter_warmup = 1000
    ){

  ### Libraries ####
  library(posterior)

  ### Source Functions ###
  source("functions/r_helpers.R")

  ## Collect the Results
  results <- list()
  row_id <- 1

  ## Loop over the diff sample sizes
  for (i in 1:length(sample_sizes)) {

    ## Loop over every one R times
    for (j in 1:R) {

      ## Generate a dataset with the sample size
      data <- generate_marketing(
        n = sample_sizes[i],
        treatment_visit_effect = treatment_visit_effect
      )

      ## Prep the data for Stan
      stan_data <- prepare_stan_data(data)

      ## Fit the Bayes Mode
      stan_fit <- fit_stan_model(
        stan_model_path = estimator_path,
        data = stan_data,
        chains = chains,
        iter_sampling = iter_sampling,
        iter_warmup = iter_warmup
      )

      ## Extract the draws for the effect
      estemated_estimands_df <-
        as_draws_df(stan_fit$draws("beta_treatment"))

      ## Calculate Bias Var and MSE for the Treatment effect
      results[[row_id]] <- bayes_calc_stress(
        draws = estemated_estimands_df$beta_treatment,
        true_effect = treatment_visit_effect,
        sample_size = sample_sizes[i]
      )
      row_id <- row_id + 1
    }
  }
  ## Combine the results
  results_df <- do.call(rbind, results)

  ## Aggregate and plot the results
  strees_profile <- bayes_stress_plots(results = results_df)

  ## Return the Plots
  return(stress_profile)
}
