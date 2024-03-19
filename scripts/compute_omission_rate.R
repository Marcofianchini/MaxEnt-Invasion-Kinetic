computeOmissionRate <- function(truepres_envs_path, e_mx, is_weighted = FALSE, truepres_weights_path = NULL) {
  
  # Load necessary library
  library(dismo)
  
  # Read the environmental variables for presence
  true.pres.envs <- read.csv(truepres_envs_path)
  
   # Extracting occurrence and background environmental variables from e_mx
  occ.envs <- e_mx@occs[, 3:ncol(e_mx@occs)]
  # bg.envs <- e_mx@bg[, 3:ncol(e_mx@occs)] # Uncomment if background predictions are needed in future extensions
  
  # Initialize a dataframe to store the omission rates and model names
  test.or10p <- data.frame(or10= rep(NA, length(e_mx@models)),
                           th10= rep(NA, length(e_mx@models)),
                           model= rep(NA, length(e_mx@models)))
  
  # Validate and set weights
  if (is_weighted) {
    true.pres.weights <- read.csv(truepres_weights_path)[,1]
    
    if (length(true.pres.weights) != nrow(true.pres.envs)) {
      stop("The number of weights does not match the number of presence points.")
    }
  } else {
    true.pres.weights <- rep(1, nrow(true.pres.envs)) # Default to equal weights if not specified
  }
  
  # Calculate omission rates for each model
  for (i in seq_along(e_mx@models)) {
    ppred <- dismo::predict(e_mx@models[[i]], true.pres.envs, args = "outputformat=cloglog")
    cv.occ.pred <- dismo::predict(e_mx@models[[i]], occ.envs, args = "outputformat=cloglog")
    # Calculate the threshold for the 10th percentile
    th.10p <- quantile(cv.occ.pred, probs = 0.1)
    # Calculate weighted omission rate
    weightedOmissions <- sum((ppred < th.10p) * true.pres.weights)
    totalWeight <- sum(true.pres.weights)
    or.10p <- weightedOmissions / totalWeight
    
    # Store the omission rate and the model name in the test.or10p dataframe
    test.or10p[i, 'or10'] <- or.10p
    test.or10p[i, 'th10'] <- th.10p
    test.or10p[i, 'model'] <- names(e_mx@models)[[i]]
    # Optional: Call garbage collector to manage memory - use if running into memory issues
    # rJava::.jcall("java/lang/System", "V", "gc")
  }
  
  # Assign the calculated omission rates to the results in e_mx
  if(is_weighted){
    e_mx@results$test_wOR10p <- test.or10p[, 'or10']
    e_mx@results$test_wth10p <- test.or10p[, 'th10']
  }
  else{
    e_mx@results$test_OR10p <- test.or10p[, 'or10']
    e_mx@results$test_th10p <- test.or10p[, 'th10']
  }
  
  # Return the updated e_mx object
  return(e_mx)
}
