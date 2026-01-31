
#### Function to Create a Directed Acyclic Graf and State Causal Assumptions
create_dag <- function(){

  ## Libraries
  library(caugi)

  #### Causal DAG ####
  causal_dag <-   caugi(

    #### Main Causal Chains ####

    ## Treatment_received effects the visits
    treatment_received %-->% visits,
    ## Visits effect conversion
    visits %-->% conversion,
    ## Conversion effect the spending
    conversion %-->% spending,

    #### Potential Confounders ####
    zip_code %-->% treatment_received,
    zip_code %-->% visits,
    zip_code %-->% conversion,
    zip_code %-->% spending,

    recency %-->% treatment_received,
    recency %-->% visits,
    recency %-->% conversion,
    recency %-->% spending,

    channel %-->% treatment_received,
    channel %-->% visits,
    channel %-->% conversion,
    channel %-->% spending,

    newbie %-->% treatment_received,
    newbie %-->% visits,
    newbie %-->% conversion,
    newbie %-->% recency,
    newbie %-->% history,

    history %-->% treatment_received,
    history %-->% visits,
    history %-->% conversion,
    history %-->% spending,

    gender %-->% treatment_received,
    gender %-->% visits,
    gender %-->% conversion,
    gender %-->% spending

  )

  #### Adjustment sets
  adj_set_outcome_visits <- adjustment_set(
    cg = causal_dag,
    Y = "visits",
    X = "treatment_received"
    )

  adj_set_outcome_conversion <- adjustment_set(
    cg = causal_dag,
    Y = "conversion",
    X = "treatment_received"
  )

  adj_set_outcome_spending <- adjustment_set(
    cg = causal_dag,
    Y = "spending",
    X = "treatment_received"
  )

  #### Return the Adjustment sets
  return(
    list(
      adjustment_set_visit = adj_set_outcome_visits,
      adjustment_set_convertion = adj_set_outcome_conversion,
      adj_set_outcome_spending = adj_set_outcome_spending
    )
  )
}
