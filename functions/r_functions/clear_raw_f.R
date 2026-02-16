
#### Function to Clean the Raw data ####
clear_raw <- function(data_raw){

  data_raw %>%

  mutate(
    ## Clean the history segment
    history_segment = case_when(
      history_segment == "1) $0 - $100" ~ "seq_0_100",
      history_segment == "2) $100 - $200" ~ "seq_100_200",
      history_segment == "3) $200 - $350" ~ "seq_200_350",
      history_segment == "4) $350 - $500" ~ "seq_350_500",
      history_segment == "5) $500 - $750" ~ "seq_500_750",
      history_segment == "6) $750 - $1,000" ~ "seq_750_1000",
      history_segment == "7) $1,000 +" ~ "seq_1000_plus"
      ),
    ## Combine men and woman in one column
    gender = if_else(mens == "1" ,true = "1",false = "0"),
    ## Clean the names of segment column
    segment = case_when(
      segment == "Womens E-Mail" ~ "woman_email",
      segment == "No E-Mail" ~ "no_email",
      segment == "Mens E-Mail" ~ "men_email"
    ),
    ## Add indicator column Email Vs None
    treatment = if_else(segment == "no_email",0 ,1)
  ) %>%
    ## Drop the means and womens columns
    select(-mens,-womens) %>%
    ## Change the data types from char to factors for plots and modeling
    mutate(
      across(where(is.character),as.factor)
    ) %>%
    ## Change the indicator variables to factors
    mutate(
      across(c("newbie","visit","conversion","treatment"),as.factor)
    )
}
