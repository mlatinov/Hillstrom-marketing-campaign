
#### Function to do Diagnostics for Bayesian Stan Model ####
bayes_diagnostics <- function(){

  ## Libraries ##
  library(posterior)
  library(bayesplot)

  # Get draws from stan fit model
  draws <- as_draws_df(bayes_visit_model)

  ## MCMC Convergence Diagnostics

  # Rhat ESS Bulk ESS tail Mean MSCE mean
  summary_draws <- summarise_draws(
    draws,
    mean,
    sd,
    mcse = mcse_mean,
    ess_bulk,
    rhat
  )

  # Trace Plots
  trace_p <- mcmc_trace(draws,pars = c("beta_treatment","alpha"))

  ## Sampler Diagnostics

  # Visualizing divergences
  divergences_p <- mcmc_pairs(draws,c("beta_treatment","alpha"))

  # BFMI (Bayesian Fraction of Missing Information)
  bfmi_vals <- bayes_visit_model$diagnostic_summary()$ebfmi

  ## Posterior Predictive Checks (PPC)
  y_rep_clean <- y_rep[, grep("^y_rep", colnames(y_rep))]
  y_subset <- as.numeric(as.character(data_clean$visit[1:1000]))

  # Mean Prop
  ppc_mean <- ppc_stat(y_subset, y_rep_clean, stat = "mean") +
    ggtitle("PPC: Mean")

  # PPC bars
  ppc_bars <- ppc_bars(y_subset,y_rep_clean)

  ## Residual Diagnostics
  resid_diag <- ppc_error_binned(y_subset, y_rep_clean) +
    ggtitle("Error Binned")

  ## Model Comparison Summaries







}
