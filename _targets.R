
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
    command = generate_marketing()
  )

  #### Bayesian Models Visits Target for ATE estimation ####
  #### Machine Learning Models for ATE estimation ####
  #### Predictive Machine Learning Model ####


)
