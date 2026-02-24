
#### Function for Double Roburst Dr ML model ####
dr_learner_model <- function(
    data,
    output_dir_insights = "_results/insights/Causal_ML/visits_model/DR_learner",
    output_dir_diagnostics = "_results/diagnostics/Causal_ML/visits_model/DR_learner"
    ){
  ## Source Helpers
  source("functions/r_helpers.R")
  library(tidymodels)
  dir.create(output_dir_insights, recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir_diagnostics, recursive = TRUE, showWarnings = FALSE)
  paths <- c()

  ## Prepare Causal Data
  causal_data <- prep_causal_ml_df(data)

  ## Outcome Models Spec
  dr_1_model_spec <- rand_forest(
    min_n = tune(),
    mtry = tune(),
    trees = tune()
    ) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  dr_0_model_spec <- rand_forest(
    min_n = tune(),
    mtry = tune(),
    trees = tune()
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")

  ## Outcome models workflows
  recipe <- causal_ml_recipes_visit(
    treatment_train_df = causal_data$treatment$training_df,
    control_train_df = causal_data$control$testing_df
  )
  wf_dr_1 <- workflow() %>%
    add_model(dr_1_model_spec) %>%
    add_recipe(recipe$treatment_recipe)
  wf_dr_0 <- workflow() %>%
    add_model(dr_0_model_spec) %>%
    add_recipe(recipe$control_recipe)

  ## Tune the models via Anova Tuning
  resamples_1 <- vfold_cv(data = causal_data$treatment$training_df, v = 5)
  resamples_0 <- vfold_cv(data = causal_data$control$training_df, v = 5)

  # Parameters Ranges to tune
  parameters <- parameters(list(
    mtry = mtry(range = c(3, 10)),
    trees = trees(range = c(500, 2000)),
    min_n = min_n(range = c(5, 50))
  ))

  # Tune results
  wf_dr_1_tune <- causal_ml_aov_tune(
    resamples = resamples_1,
    workflow = wf_dr_1,
    parameters = parameters
  )

  wf_dr_0_tune <- causal_ml_aov_tune(
    resamples = resamples_0,
    workflow = wf_dr_0,
    parameters = parameters
  )
  performance_combined <- bind_rows(
    list(
      treatment_model = wf_dr_1_tune$performance,
      control_model = wf_dr_0_tune$performance
    ),
    .id = "group"
  )
  model_performance_path <-
    file.path(output_dir_diagnostics,"tune_performace.csv")
  write_csv(performance_combined, model_performance_path)
  paths <- c(paths, model_performance_path)

  # Finalize the workflows
  wf_dr_1_final <- finalize_workflow(
    parameters = wf_dr_1_tune$best_params,
    x = wf_dr_1
  )
  wf_dr_0_final <- finalize_workflow(
    parameters = wf_dr_0_tune$best_params,
    x = wf_dr_0
  )

  # Fit the models to the training data
  fit_dr_1 <- fit(wf_dr_1_final, data = causal_data$treatment$training_df)
  fit_dr_0 <- fit(wf_dr_0_final, data = causal_data$control$training_df)

  # Predict on the testing data and summarize the performance
  dr_1_pred <- predict(
    object = fit_dr_1,
    new_data = causal_data$treatment$testing_df,
    type = "prob"
  )$.pred_1
  dr_0_pred <- predict(
    object = fit_dr_0,
    new_data = causal_data$control$testing_df,
    type = "prob"
  )$.pred_1

  # Compute and plot the AUC Curves
  control_roc_df <- causal_data$control$testing_df %>%
    select(visit) %>%
    mutate(.pred_1 = dr_0_pred)

  treatment_roc_df <- causal_data$treatment$testing_df %>%
    select(visit) %>%
    mutate(.pred_1 = dr_1_pred)

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

  ### Construct Pseudo-Outcome
  train_full <- bind_rows(
    causal_data$control$training_df,
    causal_data$treatment$training_df
  )
  test_full <- bind_rows(
    causal_data$treatment$testing_df,
    causal_data$control$testing_df
  )
  pred_1 <- predict(fit_dr_1, new_data = train_full, type = "prob")$.pred_1
  pred_0 <- predict(fit_dr_0, new_data = train_full, type = "prob")$.pred_1

  ## Propensity Scores by fitting a glm model to the data and extract the .pred_1
  propensity <- estemate_propensity(train_full)

  ## Calculate the doubly robust target
  data_cate_train <- train_full %>%
    mutate(
      phi = ((
        as.numeric(as.character(train_full$visit)) - propensity)
        /(propensity*(1-propensity))) *
        (as.numeric(as.character(train_full$visit)) -
        ifelse(as.numeric(as.character(train_full$visit))  == 1, pred_1, pred_0)) +
        (pred_1 - pred_0)
    )

  ### Final CATE Model
  dr_cate_model <- cate_model(data_cate_train)

  # Predict on the full test_set
  tau_hat_test <- predict(dr_cate_model, new_data = test_full)$.pred

  ## Causal Estemations
  mu0_hat_test <- predict(fit_dr_0, test_full, type = "prob")$.pred_1
  mu1_hat_test <- mu0_hat_test + tau_hat_test

  # compute causal estimands
  causal_estimands <- calc_causal_estimands(
    y_hat_0 = mu0_hat_test,
    y_hat_1 = mu1_hat_test
  )
  causal_estimands_path <- file.path(output_dir_insights,"causal_estimands.csv")
  readr::write_csv(x = causal_estimands, file = causal_estimands_path)
  paths <- c(paths, causal_estimands_path)

  # Return all collected paths to the results
  return(paths)
}
