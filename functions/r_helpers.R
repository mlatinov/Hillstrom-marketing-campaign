
#### Function to automate Univariate Analysis ####
univar_analysis <- function(variable,data) {

  ## Libraries ##
  library(patchwork)

  ## Histogram plot
  histogram <- ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram() +
    theme_minimal() +
    labs(
      title = paste0("Histogram of ", variable),
      x = variable,
      y = "Count"
      )

  ## Boxplot
  boxplot <- ggplot(data, aes(x = .data[[variable]])) +
    geom_boxplot() +
    theme_minimal() +
    labs(
      title = paste0("Boxplot of ", variable),
      x = variable,
      y = variable
      )

  ## Combine the plots
  combined <- histogram + boxplot

  ## Return the combined plot
  return(combined)
}

#### Functions to automate Bivariate analysis ####
biv_continuous_factor <- function(data, continuous_var, factor){

  ## Libraries ##
  library(patchwork)

  ## Bar plot
  total <- sum(data[[continuous_var]])
  bar_plot <- data %>%
    group_by(.data[[factor]]) %>%
    summarise(
      total_by_category = sum(.data[[continuous_var]]),
      percent_of_total = (sum(.data[[continuous_var]]) / total ) * 100
    ) %>%
    as.data.frame() %>%
    ggplot(aes(
      x = fct_reorder(.f = .data[[factor]],percent_of_total,.desc = TRUE),
      y = percent_of_total,
      fill = .data[[factor]]
    ))+
    geom_col() +
    theme_minimal()+
    scale_fill_viridis_d(option = "A",begin = 0.2 , end = 0.8)+
    labs(
      title = paste0("Procent of Total ", continuous_var , " by " , factor ),
      x = factor,
      y = "Procent of Total"
    )

  ## Boxplots
  boxplot <-
    data %>%
    filter(.data[[continuous_var]] > 0) %>%
    ggplot(aes(x = .data[[continuous_var]], fill = .data[[factor]]))+
    geom_boxplot()+
    theme_minimal()+
    scale_fill_viridis_d(option = "A",begin = 0.2 , end = 0.8)+
    labs(
      title = paste0("Boxplot (does not include 0) ", continuous_var , " by " , factor ),
      x = continuous_var,
      fill = factor
    )

  ## Density Distributions
  density <-
    data %>%
    filter(.data[[continuous_var]] > 0) %>%
    ggplot(aes(x = .data[[continuous_var]] , fill = .data[[factor]]))+
    geom_density(alpha = 0.4)+
    theme_minimal()+
    scale_fill_viridis_d(option = "A",begin = 0.2 , end = 0.8)+
    labs(
      title = paste0("Distributions (does not include 0) ", continuous_var , " by " , factor ),
      x = continuous_var,
      fill = factor
    )

  ## Combine all the plots
  combined <- (bar_plot + boxplot) / density

  # Return the combined plot
  return(combined)
}

biv_factor_factor <- function(data , factor_one, factor_two){

  ## Libraries ##
  library(patchwork)

  # Stacked Bar Chart
  stacked_bar_chart <-
    ggplot(
      data = data, aes(x = .data[[factor_one]],fill = .data[[factor_two]]))+
    geom_bar(position = "stack")+
    theme_minimal()+
    scale_fill_viridis_d(option = "A",begin = 0.2 , end = 0.8)+
    labs(
      title = paste0("Stacked Bar Chart of ", factor_one , " by ", factor_two),
      x = factor_one,
      y = "Counts",
      fill = factor_two
    )

  # Contingency table
  contingency_table <- data %>%
    group_by(.data[[factor_one]],.data[[factor_two]]) %>%
    summarise(
      n = n()
    ) %>%
    ungroup() %>%
    group_by(.data[[factor_one]]) %>%
    mutate(
      sum = sum(n),
      percent = (n / sum) * 100
    ) %>%
    as.data.frame() %>%
    ggplot(aes(x = .data[[factor_one]],y = .data[[factor_two]],fill = percent))+
    geom_tile()+
    theme_minimal()+
    scale_fill_viridis_c(option = "A",begin = 0.2 , end = 0.8)+
    labs(
      title = paste0("Percent Contingency table " , factor_one , " and " ,factor_two),
      x = factor_one,
      y = factor_two,
      fill = "Count"
    )

  ## Combine plots
  combined <- stacked_bar_chart + contingency_table

  ## Return combined plots
  return(combined)
}

#### Function to Prepare data for Bayesian Modeling in Stan  ####
prepare_stan_data <- function(data){

  ## For Stan the data have to be a named list
  stan_data <- list(
    N = nrow(data),
    Visits = data$visits,
    Treatment = data$treatment,
    Newbie = data$newbie,
    History = data$history,
    Gender = data$gender,
    Recency = data$recency,
    ## Ref encoding for the indicator variables with L > 2
    Web = if_else(data$channel == "Web",1,0),
    Phone = if_else(data$channel == "Phone",1,0),
    Urban = if_else(data$zip_code == "Urban",1,0),
    Surburban = if_else(data$zip_code == "Surburban",1,0)
  )
  # Return a named list
  return(stan_data)
}

#### Function to fit a Bayes Model ####
fit_stan_model <- function(
    stan_model_path = "stan_scripts/stan_model_visits.stan",
    data,
    chains = 4,
    iter_sampling = 1000,
    iter_warmup = 1000
    ){

  ## Libraries ##
  library(cmdstanr)

  # Compile the model
  stan_model <- cmdstan_model(stan_file = "stan_scripts/stan_model_visits.stan")

  ## Fit Stan Model ##
  stan_fit <- stan_model$sample(
    data = data,
    seed = 123,
    chains = chains,
    iter_sampling = iter_sampling,
    iter_warmup = iter_warmup
  )

  # Return the stan fit object
  return(stan_fit)

}

#### Function to Calc Bias Var and MSE from Bayes draws ####
bayes_calc_stress <- function(draws,true_effect,sample_size){

  ## Calculate Bias
  bias <- mean(draws) - true_effect

  ## Calculate Variance
  variance <- var(draws)

  ## Calculate Bias-Variance Trade-off
  mse <- (mean(draws) - true_effect)^2 + var(draws)

  ## Combine them in a dataframe and add the sample size for this measures
  stress_data <- data.frame(
    bias,
    variance,
    mse,
    sample_size
  )

  # Return the stress data
  return(stress_data)

}

#### Function to Aggregate and plot the results from the Monte Carlo Stress Test
bayes_stress_plots <- function(results){

  ## Libraries
  library(patchwork)

  ## Global themes
  theme_set(theme_minimal())

  ## Aggregate the results by their sample sizes
  aggregated_results <- results_df %>%
    group_by(sample_size) %>%
    summarise(
      mean_bias = mean(bias),
      mean_variance = mean(variance),
      mean_mse = mean(mse)
    )

  ## Plot Bias as a function of Sample Size
  bias_size <- ggplot(
    data = aggregated_results, aes(x = sample_size, y = mean_bias))+
    geom_point()+
    geom_smooth()+
    labs(
      title = "Bias VS Sample Size",
      x = "Sample Size",
      y = "Mean Bias"
    )+
    theme(
      title = element_text(size = 15,family = "mono"))

  ## Plot Variance as a function of Sample Size
  variance_size <- ggplot(
    data = aggregated_results, aes(x = sample_size, y = mean_variance))+
    geom_point()+
    geom_smooth()+
    labs(
      title = "Variance VS Sample Size",
      x = "Sample Size",
      y = "Mean Variance"
    )+
    theme(
      title = element_text(size = 15,family = "mono"))

  ## Plot Bias Variance Trade-off
  mse_size <- ggplot(
    data = aggregated_results, aes(x = sample_size, y = mean_mse))+
    geom_point()+
    geom_smooth()+
    labs(
      title = "Bias-Variance Trade-Off VS Sample Size",
      x = "Sample Size",
      y = "Mean MSE"
    )+
    theme(
      title = element_text(size = 15,family = "mono"))

  ## Combine the Result into one Stress Profile Plot
  stress_profile <- (bias_size + variance_size) / mse_size

  ## Return the Stress Profile
  return(stress_profile)
}

#### Function to plot posterior distributions and save them in dir ####
plot_posterior <- function(
    output_dir,
    plot_name,
    draws,
    pars
  ){

  # Libraries
  library(bayesplot)
  library(posterior)

  # Plot Posterior Dens Overlay
  plot <- mcmc_dens_overlay(draws,pars)+
    theme_minimal()

  # Save the plot and remove it from the env
  plot_posterior_path <- file.path(output_dir, plot_name)
  ggsave(plot_posterior_path, plot, width = 6, height = 4)
  rm(plot)

  # Return the path to the plot
  return(plot_posterior_path)
}

#### Function to prep data for Causal ML Modeling ####
prep_causal_ml_df <- function(data){

  # Create a train and test splits
  split <- initial_split(data = data, prop = 0.8)
  train_df <- training(split)
  testing_df <- testing(split)

  # Filter the data to create control and treatment train and test sets
  control_df_train <- train_df[train_df[["treatment"]] == 0,]
  control_df_test <- testing_df[testing_df[["treatment"]] == 0,]

  treatment_df_train <- train_df[train_df[["treatment"]] == 1,]
  treatment_df_test <- testing_df[testing_df[["treatment"]] == 1,]

  # Return a list with the datasets
  return(list(
    control = list(
      training_df = control_df_train,
      testing_df = control_df_test
    ),
    treatment = list(
      training_df = treatment_df_train,
      testing_df = treatment_df_test
    )
  ))
}
#### Function for Causal ML Recipes for Visit Model ####
causal_ml_recipes_visit <- function(control_train_df,treatment_train_df){

  # Control recipe
  control_recipe <- recipe(visit ~ . , data = control_train_df) %>%

    # Remove Variables
    step_rm(
      removals = c("history_segment","conversion","spend","segment")
    ) %>%

    # Normalize History
    step_normalize(history) %>%

    # Encode the Categorical variables
    step_dummy(all_nominal_predictors())

  # Treatment Recipe
  treatment_recipe <- recipe(visit ~ . , data = treatment_train_df) %>%

    # Remove Variables
    step_rm(
      removals = c("history_segment","conversion","spend","segment")
    ) %>%

    # Normalize History
    step_normalize(history) %>%

    # Encode the Categorical variables
    step_dummy(all_nominal_predictors())

  # Return the recipes
  return(list(
    control_recipe = control_recipe,
    treatment_recipe = treatment_recipe
  ))
}

#### Function to perform Tune Race Anova Tuning ####
causal_ml_aov_tune <- function(
    parameters,
    workflow,
    resamples
  ){

  # Define control for ANOVA race
  aov_control <- finetune::control_race(
    burn_in = 3,
    save_workflow = TRUE,
    save_pred = TRUE,
    verbose = FALSE
  )

  # Set metric
  metric <- metric_set(roc_auc)

  # Search Parameter Grid LHC
  lhc_grid <- grid_space_filling(parameters, size = 40)

  # Perform the Tuning
  tune_race <- finetune::tune_race_anova(
    object = workflow,
    resamples = resamples,
    grid = lhc_grid,
    metrics = metric,
    control = aov_control
  )

  # Extract and the return the top performance
  performance_cv <- collect_metrics(tune_race)

  best_model_params <- tune_race %>%
    show_best(n = 1) %>%
    select(-.metric, -.estimator, -mean, -n, -std_err, -.config)

  return(list(
    performance = performance_cv,
    best_params = best_model_params
  ))
}

#### Function to Calc Causal Estimands ####
calc_causal_estimands <- function(y_hat_1,y_hat_0){

  ## Average Treatment Effect
  ATE = mean(y_hat_1 - y_hat_0)

  ## Risk Ratio
  RR = mean(y_hat_1) / mean(y_hat_0)

  ## Odds Ratio
  OR = (mean(y_hat_1)/(1-mean(y_hat_1))) / (mean(y_hat_0)/(1-mean(y_hat_0)))

  ## Number Needed to Treat
  NNT = 1/ATE

  # Bind rows and return one df
  causal_estimands <- bind_rows(
    Average_Treatment_Effect = ATE,
    Risk_Ratio = RR,
    Odds_Ratio = OR,
    Numbers_Needed_to_Treat = NNT
  )

  # Return the causal_estimands
  return(causal_estimands)

}








