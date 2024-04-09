remove_nas <- function(df) {
  # Remove columns with only NA values
  cols_to_remove <- colSums(is.na(df)) == nrow(df)
  df <- df[, !cols_to_remove]
  num_cols_removed <- sum(cols_to_remove)
  
  # Remove rows with only NA values
  rows_to_remove <- rowSums(is.na(df)) == ncol(df)
  df <- df[!rows_to_remove, ]
  num_rows_removed <- sum(rows_to_remove)
  
  return(list(df = df, num_cols_removed = num_cols_removed, num_rows_removed = num_rows_removed))
}


computeBackgroundWeights <- function(presence_points, background_points, maxDist, alpha = 1) {
  # Check if presence_points and background_points are SpatVector objects
  if (!inherits(presence_points, "SpatVector")) {
    stop("presence_points must be a SpatVector object")
  }
  if (!inherits(background_points, "SpatVector")) {
    stop("background_points must be a SpatVector object")
  }
  
  # Initialize weights vector for background points
  weights <- numeric(nrow(background_points))
  
  # Calculate the distances from each background point to the nearest presence point
  for (i in 1:nrow(background_points)) {
    # Get the distance of the i-th background point to all presence points
    distances <- distance(presence_points, background_points[i, ])
    
    # Convert the distances to a numeric vector
    distances_vec <- as.vector(distances)
    
    # Get the distances that are less than maxDist
    relevant_distances <- distances_vec[distances_vec < maxDist]
    
    # Calculate weights using the 'epsilon' approach
    if (length(relevant_distances) > 0) {
      epsilon <- sum((1 - (relevant_distances / maxDist))^alpha)
      weights[i] <- (1 + epsilon) / 2  # Adjusting weights to be higher for closer points and to scale to 0.5 at maxDist
    } else {
      weights[i] <- 0.5
    }
  }
  
  return(weights)
}


duplicate_rows <- function(occs, frequency_column) {
  # Check if the frequency column exists
  if (!frequency_column %in% names(occs)) {
    stop("frequency column not found in the data frame")
  }
  
  # Check if all frequencys are non-negative integers
  if (any(occs[[frequency_column]] < 0 | occs[[frequency_column]] != round(occs[[frequency_column]]))) {
    stop("frequencys must be non-negative integers")
  }
  
  # Duplicate rows based on the frequency column
  occs_duplicated <- occs[rep(1:nrow(occs), occs[[frequency_column]]), ]
  
  # Reset row names
  rownames(occs_duplicated) <- NULL
  
  return(occs_duplicated)
}


source(paste0(wd,'scripts/proc.R'))

#calculate 10percentile threshold as in ENMeval 
calc.10p.trainThresh <- function(pred.train) {
  n <- length(pred.train)
  if(n < 10) {
    pct90.train <- floor(n * 0.9)
  }else{
    pct90.train <- ceiling(n * 0.9)
  }
  pct10.train.thr <- rev(sort(pred.train))[pct90.train]
  return(pct10.train.thr)
}

# compute omission rate on test set
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
