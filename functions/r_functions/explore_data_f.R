
#### Function to Perform Exploratory Data Analysis ####
explore_data <- function(data_clean){

  #### Source Functions helper
  source("functions/r_helpers.R")

  #### Univariate Analysis for Continues variables ####
  uni_spend <- univar_analysis(variable = "spend",data = data_clean)
  uni_history <- univar_analysis(variable = "history",data = data_clean)

  #### Bivariate Analysis ####

  ## Spend ~ F
  biv_spend_zipcode <- biv_continuous_factor(
    data = data_clean,
    factor = "zip_code",
    continuous_var = "spend"
  )

  biv_spend_channel <- biv_continuous_factor(
    data = data_clean,
    factor = "channel",
    continuous_var = "spend"
  )

  biv_spend_gender <- biv_continuous_factor(
    data = data_clean,
    factor = "gender",
    continuous_var = "spend"
  )

  ## Visit ~ F
  biv_visit_channel <- biv_factor_factor(
    data = data_clean,
    factor_one = "channel",
    factor_two = "visit"
  )

  biv_visit_newbie <- biv_factor_factor(
    data = data_clean,
    factor_one = "newbie",
    factor_two = "visit"
  )

  biv_visit_gender <- biv_factor_factor(
    data = data_clean,
    factor_one = "visit",
    factor_two = "gender"
  )

  biv_visit_zip_code <- biv_factor_factor(
    data = data_clean,
    factor_one = "zip_code",
    factor_two = "visit"
  )

  #### Return ###
  return(list(
    univariate_num_analysis = list(
      spend_var = uni_spend,
      history_var = uni_history
    ),
    bivariate_analysis = list(
      biv_visit_channel = biv_visit_channel,
      biv_visit_newbie = biv_visit_newbie,
      biv_visit_gender = biv_visit_gender,
      biv_visit_zip_code = biv_visit_zip_code
    )
  ))
}
