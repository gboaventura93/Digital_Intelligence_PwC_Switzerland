loss_data <- data.frame(
  Year = c(2016, 2016, 2016, 2017, 2018, 2018, 2019, 2019, 2020),
  WIS = c(3.1, 2.1, 10.5, 2.0, 0, 0, 230.5, 51.0, 0.5),
  Non_WIS = c(4.5, 0, 0, 0, 125.3, 0, 0.4, 0, 0)
)

inflation_rate <- 1.03

fit_poisson <- function(data) {
  mean_claims <- mean(data) * inflation_rate ^ (2021 - max(loss_data$Year))
  lambda <- mean_claims
  return(lambda)
}

fit_pareto <- function(data) {
  data <- data * inflation_rate ^ (2021 - max(loss_data$Year))
  mean_claims <- mean(data)
  shape <- 1.5
  location <- mean_claims * (shape - 1) / shape
  return(list(shape = shape, location = location))
}

lambda_WIS <- fit_poisson(loss_data$WIS)
params_WIS <- fit_pareto(loss_data$WIS)

lambda_non_WIS <- fit_poisson(loss_data$Non_WIS)
params_non_WIS <- fit_pareto(loss_data$Non_WIS)

# Results
lambda_WIS #WIS frequency
params_WIS$shape #WIS severity shape
params_WIS$location#WIS severity location 

lambda_non_WIS #Non-WIS frequency
params_non_WIS$shape #Non-WIS severity shape
params_non_WIS$location #Non-WIS severity location

E_N_WIS <- lambda_WIS
E_N_non_WIS <- lambda_non_WIS

E_X_WIS <- params_WIS$location / (params_WIS$shape - 1)
E_X_non_WIS <- params_non_WIS$location / (params_non_WIS$shape - 1)

E_S_WIS <- E_N_WIS * E_X_WIS
E_S_non_WIS <- E_N_non_WIS * E_X_non_WIS

E_S_aggregate <- E_S_WIS + E_S_non_WIS

# Results
E_S_WIS #Value WIS in mCHF
E_S_non_WIS #Value Non-WIS in mCHF
E_S_aggregate #Aggregate

Var_N_WIS <- lambda_WIS
Var_X_WIS <- (params_WIS$location^2 * params_WIS$shape) / ((params_WIS$shape - 2)^2 * (params_WIS$shape - 1))
Var_S_WIS <- E_N_WIS * Var_X_WIS + Var_N_WIS * E_X_WIS^2

Var_N_non_WIS <- lambda_non_WIS
Var_X_non_WIS <- (params_non_WIS$location^2 * params_non_WIS$shape) / ((params_non_WIS$shape - 2)^2 * (params_non_WIS$shape - 1))
Var_S_non_WIS <- E_N_non_WIS * Var_X_non_WIS + Var_N_non_WIS * E_X_non_WIS^2

VaR_80_WIS <- sqrt(Var_S_WIS) * 0.84
VaR_80_non_WIS <- sqrt(Var_S_non_WIS) * 0.84

#Results
VaR_80_WIS
VaR_80_non_WIS

E_N_WIS_doubled <- 2 * E_N_WIS
E_N_non_WIS_doubled <- 2 * E_N_non_WIS

E_S_WIS_doubled <- E_N_WIS_doubled * E_X_WIS
E_S_non_WIS_doubled <- E_N_non_WIS_doubled * E_X_non_WIS

E_S_aggregate_doubled <- E_S_WIS_doubled + E_S_non_WIS_doubled

impact_on_expected_loss <- E_S_aggregate_doubled - E_S_aggregate

# Results
impact_on_expected_loss

