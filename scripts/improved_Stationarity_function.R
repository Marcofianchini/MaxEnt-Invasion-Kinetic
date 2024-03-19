stationarity_test <- function(series, max_lag = 5, selectlags = "BIC") {
  significance_levels <- c(1, 5, 10) # Significance levels to test against
  cval_indices <- c(1, 2, 3) # Indices for critical values corresponding to significance levels
  
  # Loop through significance levels and test for stationarity
  for (i in 1:length(significance_levels)) {
    cval_index <- cval_indices[i]
    
    # Test for strict stationarity with type='none'
    test_none <- urca::ur.df(y = series, type = 'none', lags = max_lag, selectlags = selectlags)
    tau_none <- test_none@teststat[1]
    pvalue_none <- test_none@cval[cval_index] # Adjust to select the correct critical value
    
    if (tau_none < pvalue_none) {
      return(list(
        type = "Stationary",
        lag = nrow(coef(test_none@testreg)) - 1,
        tau = tau_none,
        significance = paste0(significance_levels[i], '%')
      ))
    }
    
    # Test for drift stationarity with type='drift'
    test_drift <- urca::ur.df(y = series, type = 'drift', lags = max_lag, selectlags = selectlags)
    tau_drift <- test_drift@teststat[1]
    pvalue_drift <- test_drift@cval[cval_index, 1] # Adjust to select the correct critical value
    
    if (tau_drift < pvalue_drift) {
      return(list(
        type = "Drift-stationary",
        lag = nrow(coef(test_drift@testreg)) - 2, # Adjust for 'drift'
        tau = tau_drift,
        significance = paste0(significance_levels[i], '%')
      ))
    }
    
    # Test for trend stationarity with type='trend'
    test_trend <- urca::ur.df(y = series, type = 'trend', lags = max_lag, selectlags = selectlags)
    tau_trend <- test_trend@teststat[1]
    phi2 <- test_trend@teststat[2]
    phi3 <- test_trend@teststat[3]
    pvalue_trend <- test_trend@cval[cval_index, 1] # Adjust to select the correct critical value
    
    if (tau_trend < pvalue_trend) {
      return(list(
        type = "Trend-stationary",
        lag = nrow(coef(test_trend@testreg)) - 3, # Adjust for 'trend'
        tau = tau_trend,
        phi2 = phi2,
        phi3 = phi3,
        significance = paste0(significance_levels[i], '%')
      ))
    }
  }
  
  # If none of the tests reject the null hypothesis, return NA to indicate non-stationarity
  return(list(
    type = "Non-stationary",
    significance = NA
  ))
}

calculate_area <- function(raster, value ) {
  # Get the number of layers
  nlayers <- nlyr(raster)
  
  # Initialize a vector to store the areas
  areas <- numeric(nlayers)
  
  # Loop through the layers and calculate the area for each one
  for (i in 1:nlayers) {
    layer <- raster[[i]]
    print(sum(values(layer == value),na.rm = T))
    areas[i] <- sum(values(layer == value),na.rm = T) * res(layer)[1] * res(layer)[2]
  }
  
  return(areas)
}