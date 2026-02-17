
#### Function to Summarize and present the results from the bayes visit model ####
bayes_visit_summary <- function(
    draws,
    output_dir = "_results/insights/bayes_models/visits_model"
  ){

  ## Source R helper Functions
  source("functions/r_helpers.R")

  ## Create a dir and path file vector
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c()

  ## Marginal Posterior Distributions
  # Treatment Effect (Beta Treatment) & ATE (Average Treatment Effect)
  effect_posterior_path <- plot_posterior(
    output_dir = output_dir,
    plot_name = "ate_treatment_effect_posterior.png",
    draws = draws,
    pars = c("ATE","beta_treatment")
  )
  paths <- c(paths,effect_posterior_path)

  # NNT(Numbers Needed To Treat) &
  # PN(Probability of Necessity) &
  # RR(Risk Ratio) &
  # OR (Odds Ratio)
  causal_effects_paths <- plot_posterior(
    output_dir = output_dir,
    plot_name = "causal_effect_posterior.png",
    draws = draws,
    pars = c("NNT","PN","RR","OR")
  )
  paths <- c(paths,causal_effects_paths)

  # Return paths
  return(paths)
}
