
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
  #### Generative Simulation Model (Validation Monte Carlo)####
  tar_target(
    name = generative_model,
    command = generate_marketing(n = 1000)
  ),
  #### Bayesian Models Visits Target for ATE estimation ####
  tar_target(
    name = bayes_visit_model_sim,
    command = bayes_visits(generative_model)
  ),
  tar_target(
    name = bayes_visit_model,
    command = bayes_visits(data_clean)
  ),
  tar_target(
    name = stress_test_bayes_visits,
    command = bayes_stress_test(
      R = 10,
      iter_sampling = 4000,
      iter_warmup = 2000,
      chains = 4,
      treatment_visit_effect = 0.05,
      sample_sizes = seq(1000,6000,1000),
      estimator_path = "stan_scripts/stan_model_visits.stan"
    )
  ),
  tar_target(
    name = bayes_visit_model_diagnostics,
    command = bayes_diagnostics(bayes_visit_model)
  ),
  tar_target(
    name = bayes_visit_model_summary,
    command = bayes_visit_summary(bayes_visit_model)
  )

  #### Machine Learning Visits Models for ATE estimation ####
  #### Bayesian Models Conversion Model ####
  #### Machine Learning Conversion Models ####
  #### Bayesian Models Spending Model ####
  #### Machine Learning Spending Models ####
  #### Update The Generative Simulation Model ####
  #### Predictive Machine Learning Model ####























)
