
#### Function to Perform Exploratory Data Analysis ####
explore_data <- function(clean_data){

  #### Source Functions helper
  source("functions/r_helpers.R")

  #### Univariate Analysis for Continues variables ####
  uni_spend <- univar_analysis(variable = "spend",data = data_clean)
  uni_history <- univar_analysis(variable = "history",data = data_clean)

  #### Bivariate Analysis ####

  #### Multivariate Analysis ####























}
