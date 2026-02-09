// Functions for Causal Estemands
functions{
  // Average Treatment Effect
  real calc_ATE(int N, vector y_hat_1, vector y_hat_0){
    return mean(y_hat_1 - y_hat_0);
  }
  // Risk Ratio
  real calc_RR(int N, vector y_hat_1, vector y_hat_0){
    return mean(y_hat_1) / mean(y_hat_0);
  }
  // Odds Ratio
  real calc_OR(int N, vector y_hat_1, vector y_hat_0){
    return (mean(y_hat_1)/(1-mean(y_hat_1))) / (mean(y_hat_0)/(1-mean(y_hat_0)));
  }
  // Number Needed to Treat
  real calc_NNT(real ATE){
    return 1/ATE;
  }
  // Prob of Necessity
real calc_PN(int N, vector y_hat_1, vector y_hat_0){
  real count = 0;
  real total_y1 = 0;
  for (i in 1:N) {
    int y1_bin = y_hat_1[i] > 0.5 ? 1 : 0;
    int y0_bin = y_hat_0[i] > 0.5 ? 1 : 0;

    if (y1_bin == 1) {
      total_y1 += 1;
      if (y0_bin == 0)
        count += 1;
    }
  }
  if (total_y1 > 0)
    return count / total_y1;
  else
    return 0;
}

// Prob of Sufficiency
real calc_PS(int N, vector y_hat_1, vector y_hat_0){
  real count = 0;
  real total_y0 = 0;
  for (i in 1:N) {
    int y1_bin = y_hat_1[i] > 0.5 ? 1 : 0;
    int y0_bin = y_hat_0[i] > 0.5 ? 1 : 0;

    if (y0_bin == 0) {
      total_y0 += 1;
      if (y1_bin == 1)
        count += 1;
    }
  }
  if (total_y0 > 0)
    return count / total_y0;
  else
    return 0;
  }
}

// Input data
data {
  int<lower=0> N;                       // Number of observations
  array[N] int<lower=0,upper=1> Visits; // Target
  vector[N] Treatment;                  // Treatment Received

  // Predictors
  vector[N] Newbie;
  vector[N] History;
  vector[N] Gender;
  vector[N] Recency;
  // Ref level Multichannel
  vector[N] Web;
  vector[N] Phone;
  // Ref level Rural
  vector[N] Urban;
  vector[N] Surburban;
}

// Model Parameters
parameters {
  real alpha; // Baseline Visits
  real beta_treatment; // Treatment Effect

  // Predictors Effects
  real beta_newbie;
  real beta_history;
  real beta_gender;
  real beta_recency;
  real beta_web;
  real beta_phone;
  real beta_urban;
  real beta_surburban;
}
// Model
model {
  // Priors
  alpha ~ normal(-6,1);
  beta_treatment ~ normal(0.5,1);
  beta_newbie ~ normal(-0.6,1);
  beta_history ~ normal(-0.00004,1);
  beta_gender ~ normal(0.1,1);
  beta_recency ~ normal(0.05,1);
  beta_web ~ normal(-0.2,1);
  beta_phone ~ normal(-0.4,1);
  beta_urban ~ normal(-0.4,1);
  beta_surburban ~ normal(-0.5,1);

  // Model Likelihood
  Visits ~ bernoulli_logit(
    alpha
    + beta_treatment * Treatment
    + beta_newbie * Newbie
    + beta_history * History
    + beta_gender * Gender
    + beta_recency * Recency
    + beta_web * Web
    + beta_phone * Phone
    + beta_urban * Urban
    + beta_surburban * Surburban
  );
}
// Additional Calc
generated quantities{

  // Specify the repetive part of the formula in eta
  vector[N] eta;
  for (i in 1:N) {
    eta[i] =
    beta_newbie * Newbie[i]
    + beta_history * History[i]
    + beta_gender * Gender[i]
    + beta_recency * Recency[i]
    + beta_web * Web[i]
    + beta_phone * Phone[i]
    + beta_urban * Urban[i]
    + beta_surburban * Surburban[i];
  }
  // Posterior rep
  vector[N] y_rep;
  // LOO-CV
  vector[N] loocv;

  // Posterior P values
  int<lower=0, upper=1> mean_qt;
  int<lower=0, upper=1> sd_qt;

  for(i in 1:N){
    // Posterior Predictive Distribution
    y_rep[i] = bernoulli_rng(
      inv_logit(alpha + beta_treatment * Treatment[i] + eta[i])
    );
    // Log-likelihood for LOO-CV
    loocv[i] = bernoulli_logit_lpmf(
      Visits[i] | alpha + beta_treatment * Treatment[i] + eta[i]
    );
  }
  // Posterior ‘’p-values’’
  mean_qt = mean(y_rep) > mean(Visits);
  sd_qt = sd(y_rep) > sd(Visits);

  // Counterfactuals
  vector[N] y_hat_0;
  vector[N] y_hat_1;

  for (i in 1:N) {
    // do(T = 0)
    y_hat_0[i] = inv_logit(
      alpha
      + beta_treatment * 0
      + eta[i]
    );

    // do(T = 1)
    y_hat_1[i] = inv_logit(
      alpha
      + beta_treatment * 1
      + eta[i]
    );
  }
  // Calculate ATE RR OR NNT PN PS
  real ATE = calc_ATE(N, y_hat_1, y_hat_0);
  real NNT = calc_NNT(ATE);
  real RR = calc_RR(N ,y_hat_1, y_hat_0);
  real OR = calc_OR(N ,y_hat_1, y_hat_0);
  real PN = calc_PN(N ,y_hat_1, y_hat_0);
  real PS = calc_PS(N ,y_hat_1, y_hat_0);

}


