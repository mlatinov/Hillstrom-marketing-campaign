
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

  ## Bar plot
  data %>%
    group_by(zip_code) %>%
    summarise(
      total_by_category = sum(spend),
      percent_of_total = (sum(spend) / sum(data$spend)) * 100
    ) %>%
    as.data.frame() %>%
    ggplot(aes(
      x = fct_reorder(.f = zip_code,percent_of_total,.desc = TRUE),
      y = percent_of_total
      ))+
    geom_col() +
    theme_minimal()+
    scale_fill_viridis_d(option = "A")+
    labs(
      title = paste0("Procent of Total ", continuous_var , "by" , factor ,),
      x = factor,
      y = "Procent of Total"
    )

  ## Correlation heatmap

  ## Boxplots

  ## Density Distributions

}

biv_factor_factor <- function(data , factor_one, factor_two){

  ## Corrlatation heatmap




}
