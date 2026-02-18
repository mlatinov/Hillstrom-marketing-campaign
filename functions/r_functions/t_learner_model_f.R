
#### Function to Create a T Meta Learner Model ####
t_learner_model <- function(
    data,
    output_dir_diagnostics = "_results/diagnostics/Causal_ML/visits_model/T_learner",
    output_dir_insights = "_results/insights/Causal_ML/visits_model/T_learner"
  ){

  ## Libraries ##
  library(tidymodels)

  # Source r.helpers functions
  source("functions/r_helpers.R")

  # Create dir for the output paths and collect all paths in a vector
  dir.create(output_dir_diagnostics, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir_insights, recursive = TRUE, showWarnings = FALSE)
  paths <- c()

  # Split the data into control and treatment &
  # Create a Train and Test Split for each
  model_data <- prep_causal_ml_df(data = data)

  # Model Specifications
  spec_control <- rand_forest(
    trees = tune(),
    mtry = tune(),
    min_n = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")

  spec_treatment <- rand_forest(
    trees = tune(),
    mtry = tune(),
    min_n = tune()
    ) %>%
    set_mode("classification") %>%
    set_engine("ranger")

  # Create a Recipes
  recipes <- causal_ml_recipes_visit(
    treatment_train_df = model_data$treatment$training_df,
    control_train_df = model_data$control$training_df
  )

  # Create a workflow
  control_wf <- workflow() %>%
    add_model(spec_control) %>%
    add_recipe(recipes$control_recipe)

  treatment_wf <- workflow() %>%
    add_model(spec_treatment) %>%
    add_recipe(recipes$treatment_recipe)

  # Resamples
  control_resamples_cv <- vfold_cv(
    data = model_data$control$training_df,
    v = 5
  )
  treatment_resample_cv <- vfold_cv(
    data = model_data$treatment$training_df,
    v = 5
  )

  # Parameters Ranges to tune
  parameters <- parameters(list(
    mtry = mtry(range = c(3, 10)),
    trees = trees(range = c(500, 2000)),
    min_n = min_n(range = c(5, 50))
    )
  )

  # Tune the models and save their performance
  control_tune <- causal_ml_aov_tune(
    resamples = control_resamples_cv,
    workflow = control_wf,
    parameters = parameters
  )
  treatment_tune <- causal_ml_aov_tune(
    resamples = treatment_resample_cv,
    workflow = treatment_wf,
    parameters = parameters
  )
  model_performance <- bind_rows(
    control_tune_performance = control_tune$performance_cv,
    treatment_tune_performance = treatment_tune$performance_cv,
  )
  models_performance_path <-
    file.path(output_dir_diagnostics,"tune_performance.csv")
  write.csv(model_performance,models_performance_path)
  paths <- c(paths, models_performance_path)

  # Finalize workflows
  control_wf_final <- finalize_workflow(
    control_wf,
    parameters = control_tune$best_params
  )
  treatment_wf_final <- finalize_workflow(
    treatment_wf,
    parameters = treatment_tune$best_params
  )

  # Fit the models
  control_fit <- fit(
    control_wf_final,
    data = model_data$control$training_df
  )
  treatment_fit <- fit(
    treatment_wf_final,
    data = model_data$treatment$training_df
  )

  # Predict class probabilities on test data (positive class = "1")
  control_pred <- predict(
    control_fit,
    new_data = model_data$control$testing_df,
    type = "prob"
  )$.pred_1

  treatment_pred <- predict(
    treatment_fit,
    new_data = model_data$treatment$testing_df,
    type = "prob"
  )$.pred_1

  # Compute and plot the AUC Curves
  control_roc_df <- model_data$control$testing_df %>%
    select(visit) %>%
    mutate(.pred_1 = control_pred)

  treatment_roc_df <- model_data$treatment$testing_df %>%
    select(visit) %>%
    mutate(.pred_1 = treatment_pred)

  roc_control <- roc_auc(
    data = control_roc_df,
    .pred_1,
    truth = visit,
    event_level = "second"
  )
  roc_treatment <- roc_auc(
    data = treatment_roc_df,
    .pred_1,
    truth = visit,
    event_level = "second"
  )
  roc_test_performance <- bind_rows(
    control   = roc_control,
    treatment = roc_treatment,
    .id = "group"
  )
  roc_test_performance_path <-
    file.path(output_dir_diagnostics,"roc_summary.csv")
  write_csv(roc_test_performance, roc_test_performance_path)
  paths <- c(paths, roc_test_performance_path)

  ## Calc Causal Estimands
  causal_estimands <- calc_causal_estimands(
    y_hat_0 = control_pred,
    y_hat_1 = treatment_pred
  )
  causal_estimands_path <-
    file.path(output_dir_insights,"causal_estimands.csv")
  write_csv(x = causal_estimands, file = causal_estimands_path)
  paths <- c(paths, causal_estimands_path)

  # Return all collected output paths
  return(paths)
}
