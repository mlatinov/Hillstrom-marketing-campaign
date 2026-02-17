
#### Function to do Diagnostics for Bayesian Stan Model ####
bayes_diagnostics <- function(
    stan_model,
    y_obs = NULL,
    output_dir = "_results/diagnostics/bayes_models/visits_model"
    ){

  ## Libraries
  library(posterior)
  library(bayesplot)
  library(loo)

  # Create a directory and path file vector
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- c()

  # Global plot theme
  theme_set(theme_minimal())

  # Get draws
  draws <- as_draws(stan_model)

  # MCMC Summary
  summ <- summarise_draws(draws)

  summary_path <- file.path(output_dir, "mcmc_summary.csv")
  write.csv(summ, summary_path, row.names = FALSE)
  paths <- c(paths, summary_path)

  # Trace Plot
  trace_p <- mcmc_trace(draws, pars = c("beta_treatment","alpha"))
  trace_path <- file.path(output_dir, "traceplot.png")
  ggsave(trace_path, trace_p, width = 6, height = 4)
  paths <- c(paths, trace_path)
  rm(trace_p)

  # Sampler Pairs / Divergences
  divergences_p <- mcmc_pairs(draws, pars = c("beta_treatment","alpha"))
  pair_path <- file.path(output_dir, "divergences.png")
  ggsave(pair_path, divergences_p, width = 6, height = 6)
  paths <- c(paths, pair_path)
  rm(divergences_p)

  # Posterior Predictive Checks
  if (!is.null(y_obs)) {
    y_rep <- as_draws_matrix(stan_model$draws("y_rep"))
    ppc_mean <- ppc_stat(y_obs, y_rep, stat = "mean") +
    ggtitle("PPC: Mean")

    ppc_mean_path <- file.path(output_dir, "ppc_mean.png")
    ggsave(ppc_mean_path, ppc_mean, width = 6, height = 4)
    paths <- c(paths, ppc_mean_path)
    rm(ppc_mean, y_rep)
  }
  # LOO / Model Comparison
  if (any(grepl("^loocv\\[", stan_model$metadata()$variables))) {
    loo_result <- stan_model$loo("loocv")
    loo_est <- as.data.frame(loo_result$estimates)

    loo_path <- file.path(output_dir, "loo_summary.csv")
    write.csv(loo_est, loo_path, row.names = FALSE)
    paths <- c(paths, loo_path)
  }

  return(paths)
}

