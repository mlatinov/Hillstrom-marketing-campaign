
#### Global Libraries ####
library(targets)
library(tidyverse)

## Source Function
tar_source("functions/r_functions/")

## Pipeline
list(

  #### Load the Raw Data ####
  tar_target(
    name = data_raw,
    command = read_csv("data/marketing_data.csv")
  ),
  #### Clean the data ####
  tar_target(
    name = data_clean,
    command = clear_raw(data_raw)
  ),
  #### Create a DAG and return adjustment sets ####
  tar_target(
    name = marketing_dag,
    command = create_dag()
  ),
  #### Exploratory Data Analysis ####
  tar_target(
    name = eda_marketing,
    command = explore_data(data_clean)
  ),
  #### Validation Monte Carlo Bias Estimation ####
  tar_target(
    name = stress_test_bayes_visits,
    command = bayes_stress_test(
      R = 3,
      iter_sampling = 4000,
      iter_warmup = 2000,
      chains = 4,
      treatment_visit_effect = 0.05,
      sample_sizes = seq(1000,6000,1000),
      estimator_path = "stan_scripts/stan_model_visits.stan"
    )
  ),
  tar_target(
    name = bayes_visit_model,
    command = bayes_visits(sample_n(data_clean,size = 1000)),
    memory = "transient",
    format = "rds"
  ),
  tar_target(
    name = bayes_visit_model_diagnostics,
    command = bayes_diagnostics(
      y_obs = sample(as.numeric(as.character(data_clean$visit)),1000),
      stan_model = bayes_visit_model,
      output_dir = "_results/diagnostics/bayes_models/visits_model"
    ),
    memory = "transient"
  ),
  tar_target(
    name = bayes_visit_model_summary,
    command = bayes_visit_summary(
      output_dir = "_results/insights/bayes_models/visits_model",
      draws = as_draws(bayes_visit_model)
      ),
    memory = "transient"
  )

  #### Machine Learning Visits Models for ATE estimation ####
  #### Bayesian Models Conversion Model ####
  #### Machine Learning Conversion Models ####
  #### Bayesian Models Spending Model ####
  #### Machine Learning Spending Models ####
  #### Update The Generative Simulation Model ####
  #### Predictive Machine Learning Model ####























)
