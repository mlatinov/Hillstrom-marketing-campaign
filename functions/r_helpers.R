
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
