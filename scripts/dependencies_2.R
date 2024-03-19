calculate_scores <- function(df_models, df_metrics, is_weighted, use_relative_I = T,verbose = FALSE, num_classes = 10) {
  # Ensure the models data frame has a 'tune.args' column
  if (!"tune.args" %in% names(df_models)) {
    stop("df_models must have a 'tune.args' column.")
  }
  print(paste0('Ranking models based on ', num_classes, ' percentile classes'))
  # Initialize a data frame to store scores
  scores_df <- data.frame(tune.args = df_models$tune.args)
  
  # Iterate over each metric in df_metrics
  for (metric_row in 1:nrow(df_metrics)) {
    metric_name <- df_metrics$metric_name[metric_row]
    worst_value <- df_metrics$worst_value[metric_row]
    best_value <- df_metrics$best_value[metric_row]
    weight <- df_metrics$W[metric_row]
    
    # Find the corresponding column in df_models
    metric_col <- grep(paste0(metric_name), names(df_models), value = TRUE)
    if (length(metric_col) >1) {
      metric_col <- grep(paste0('\\b',metric_name,'\\b'), names(df_models), value = TRUE)
      print(paste0('forcing grep to take the metric: ', metric_col))
    }
   
    if (length(metric_col) == 0) {
      if (verbose) print(paste("Skipping metric:", metric_name, "as it's not found in df_models"))
      next # Skip if no matching metric found
    }
    
    # Extract the metric values for all models
    metric_values <- df_models[[metric_col]]
    
    # manage NA and NaN
    metric_values[!is.finite(metric_values)] <- worst_value
    
    # Compute Absolute Importance (AI) for each model
    ai_values <- if(best_value > worst_value) {
      (metric_values - worst_value) / (best_value - worst_value)
    } else {
      (worst_value - metric_values) / (worst_value - best_value)
    }
    
    # Compute Relative Importance (RI) for each model
    # Calculate percentile ranks
    percentiles <- quantile(metric_values, probs = seq(0, 1, length.out = num_classes + 1))
    
    # Assign points based on percentile rank
    ri_values <- rep(NA, length(metric_values))
    for (i in 1:num_classes) {
      lower_bound <- percentiles[i]
      upper_bound <- percentiles[i + 1]
      points <- num_classes - i + 1 # Adjust points based on class
      ri_values[metric_values >= lower_bound & metric_values < upper_bound] <- points
    }
    # Compute final score for each model
    if(isTRUE(use_relative_I)) {
      final_scores <- ai_values * ri_values
    } else{
      final_scores <- ai_values
    }
    
    if (is_weighted) {
      final_scores <- final_scores * weight
    }
    
    # Update the scores in scores_df
    scores_df[[metric_name]] <- final_scores
    
    # Debugging output
    if (verbose) {
      for (model_row in 1:nrow(df_models)) {
        print(paste("tune.args:", df_models$tune.args[model_row], 
                    "Metric:", metric_name, 
                    "AI:", ai_values[model_row], "RI:", ri_values[model_row], 
                    "Final Score:", final_scores[model_row]))
      }
    }
  }
  
  # Calculate sum of scores
  scores_df$Sum_Scores <- rowSums(scores_df[,-1], na.rm = TRUE) # Excluding the first column (Model names)
  
  return(scores_df)
}


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
adjustPointsToRasterMultilayer <- function(points, raster, threshold) {
  # Ensure points and raster have the same CRS
  if (terra::crs(points) != terra::crs(raster)) {
    points <- terra::project(points, terra::crs(raster))
  }
  # Create a mask where all layers have non-NA values
  mask <- terra::rast(raster[[1]], vals=1)
  
  for (i in 1:terra::nlyr(raster)) {
    mask[is.na(raster[[i]])] <- NA
  }
  valid_raster <- mask
  
  # Initialize an empty data.frame for results
  results <- data.frame(orig_x = numeric(0), orig_y = numeric(0), 
                        new_x = numeric(0), new_y = numeric(0), 
                        distance = numeric(0))
  
  # Loop through each point
  for (i in 1:length(points)) {
    # Original point coordinates
    orig_coords <- as.matrix(terra::geom(points[i]))[1, c("x", "y")]
    
    # Calculate distance from the point to the nearest non-NA cell in the valid raster
    dist_raster <- terra::distance(valid_raster, points[i])
    
    # Initialize min_dist to a large number and valid extraction flag to FALSE
    min_dist <- Inf
    valid_extraction <- FALSE
    cell_center <- c(NA, NA)
    
    # Sort distances and iterate to find a valid location within threshold
    dist_values <- sort(values(dist_raster), na.last = TRUE)
    for (dist_value in dist_values) {
      if (dist_value > threshold) break # Exit if distance exceeds threshold
      
      cell_index <- which(values(dist_raster) == dist_value)[1] # Get first index of this distance
      potential_cell_center <- terra::xyFromCell(valid_raster, cell_index)
      
      # Extract values from original raster at this potential new location
      extracted_values <- terra::extract(raster, terra::vect(potential_cell_center, crs = terra::crs(raster)))
      
      # Check if any layer has NA
      if (!any(is.na(extracted_values))) {
        valid_extraction = TRUE
        min_dist = dist_value
        cell_center <- potential_cell_center
        break # Valid location found
      }
    }
    
    # Update results based on validity of extraction
    if (!valid_extraction || is.na(min_dist) || min_dist > threshold) {
      results <- rbind(results, c(orig_coords[1], orig_coords[2], NA, NA, NA))
    } else {
      results <- rbind(results, c(orig_coords[1], orig_coords[2], cell_center[1], cell_center[2], min_dist))
    }
  }
  
  # Name the columns appropriately
  colnames(results) <- c("orig_x", "orig_y", "new_x", "new_y", "distance")
  results <- cbind(results, as.data.frame(points))
  return(results)
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


proc <- function(vars) {
  # Definition of sediWeighted function, replacing the defunct 'enmSdm' package's version
  
  sediWeighted <- function(
    pres,
    contrast,
    presWeight = rep(1, length(pres)),
    contrastWeight = rep(1, length(contrast)),
    thresholds = seq(0, 1, by=0.01),
    delta = 0.001,
    na.rm = FALSE,
    bg = NULL,
    bgWeight = NULL,
    ...
  ) {
    
    if (missing(contrast) & !is.null(bg)) contrast <- bg
    if (missing(contrastWeight) & !is.null(bgWeight)) contrastWeight <- bgWeight
    
    # if all NAs
    if (all(is.na(pres)) | all(is.na(contrast)) | all(is.na(presWeight)) | all(is.na(contrastWeight))) return(NA)
    
    # catch errors
    if (length(presWeight) != length(pres)) stop('You must have the same number of presence predictions and presence weights ("pres" and "presWeight").')
    if (length(contrastWeight) != length(contrast)) stop('You must have the same number of absence/background predictions and absence/background weights ("contrast" and "contrastWeight").')
    
    # remove NAs
    if (na.rm) {
      
      cleanedPres <- omnibus::naOmitMulti(pres, presWeight)
      pres <- cleanedPres[[1]]
      presWeight <- cleanedPres[[2]]
      
      cleanedContrast <- omnibus::naOmitMulti(contrast, contrastWeight)
      contrast <- cleanedContrast[[1]]
      contrastWeight <- cleanedContrast[[2]]
      
    }
    
    sumPresWeights <- sum(presWeight)
    sumContrastWeights <- sum(contrastWeight)
    
    # SEDI, true positive rate, true negative rate, false negative rate
    sedi <- tpr <- tnr <- fnr <- rep(NA, length(thresholds))
    
    for (i in seq_along(thresholds)) {
      
      thisThresh <- thresholds[i]
      
      # which presences/contrast sites are CORRECTLY predicted at this threshold
      whichCorrectPres <- which(pres >= thisThresh)
      whichCorrectContrast <- which(contrast < thisThresh)
      
      numCorrectPres <- length(whichCorrectPres)
      numCorrectContrast <- length(whichCorrectContrast)
      
      anyCorrectPres <- (numCorrectPres > 0)
      anyCorrectContrast <- (numCorrectContrast > 0)
      
      # which presences/contrast sites are INCORRECTLY predicted at this threshold
      whichIncorrectPres <- which(pres < thisThresh)
      whichIncorrectContrast <- which(contrast >= thisThresh)
      
      numIncorrectPres <- length(whichIncorrectPres)
      numIncorrectContrast <- length(whichIncorrectContrast)
      
      anyIncorrectPres <- (numIncorrectPres > 0)
      anyIncorrectContrast <- (numIncorrectContrast > 0)
      
      # weights of CORRECTLY predicted predictions
      correctPresWeights <- if (anyCorrectPres) {
        sum(presWeight[whichCorrectPres])
      } else {
        0
      }
      
      correctContrastWeights <- if (anyCorrectContrast) {
        sum(contrastWeight[whichCorrectContrast])
      } else {
        0
      }
      
      # weights of INCORRECTLY predicted predictions
      incorrectPresWeights <- if (anyIncorrectPres) {
        sum(presWeight[whichIncorrectPres])
      } else {
        0
      }
      
      incorrectContrastWeights <- if (anyIncorrectContrast) {
        sum(contrastWeight[whichIncorrectContrast])
      } else {
        0
      }
      
      # true positive/negative rates
      tpr[i] <- correctPresWeights / sumPresWeights
      tnr[i] <- correctContrastWeights / sumContrastWeights
      
      # false positive/negative rates
      fnr[i] <- incorrectContrastWeights / sumContrastWeights
      
    }
    
    # SEDI
    if (any(tpr == 0)) tpr[tpr == 0] <- delta
    if (any(fnr == 0)) fnr[fnr == 0] <- delta
    if (any(tpr == 1)) tpr[tpr == 1] <- 1 - delta
    if (any(fnr == 1)) fnr[fnr == 1] <- 1 - delta
    
    numer <- log10(fnr) - log10(tpr) - log10(1 - fnr) + log10(1 - tpr)
    denom <- log10(fnr) + log10(tpr) + log10(1 - fnr) + log10(1 - tpr)
    sedi <- numer / denom
    
    sedi
    
  }
  
  # if doesn't work, set manually the working directory as specified in the main.file 
  wd<-"C:/Users/User/Documents/Rstudio/data_to_submit/"
  
  k<-vars$k_num
  print(k)   # for debug, uncomment to see if the cv.enm patch worked. it will print the fold values currently analyzed.
  model<-vars$mod.k
  # if k ==5 print a statement in a log file 
  if(k==5){
    cat(paste0("done!\n"), file=paste0(wd,'/data/','log.txt'), append=TRUE)
    cat(paste0(Sys.time(),"\n"), file=paste0(wd,'/data/','log.txt'), append=TRUE)
  }
  
  rnms<-row.names(model@results)
  
  # compute statistics against the withheld dataset
  rcmed.pres.envs<-read.csv(paste0(wd,'/temp/',"rcmed_pres_envs.csv"))
  rcmed.pres.weights<-read.csv(paste0(wd,'/temp/',"rcmed_pres_weights.csv"))[,1]
  rcmed.abs.weights<-read.csv(paste0(wd,'/temp/',"rcmed_abs_weights.csv"))[,1]
  rcmed.abs.envs<-read.csv(paste0(wd,'/temp/',"rcmed_abs_envs.csv"))
  ppred<-dismo::predict(model,rcmed.pres.envs,args = "outputformat=cloglog")
  apred<-dismo::predict(model,rcmed.abs.envs, args = "outputformat=cloglog")
  
  occ_k_weights<-read.csv(paste0(wd,'/temp/','final_occs.csv'))
  bg_k_weights<-read.csv(paste0(wd,'/temp/','final_bg.csv'))
  
  threshold_select <- "Maximum.training.sensitivity.plus.specificity.Cloglog.threshold"
  threshold_ix<- which(rnms==threshold_select)
  threshold <- model@results[threshold_ix]
  names(threshold)<- threshold_select
  
  CBI <- enmSdmX::evalContBoyce(pres = ppred, contrast =  apred,graph = F)
  wCBI <- enmSdmX::evalContBoyce(ppred,apred,presWeight = rcmed.pres.weights, contrastWeight = rcmed.abs.weights,graph = F)
  AUC <- enmSdmX::evalAUC(pres = ppred, contrast = apred)
  wAUC <- enmSdmX::evalAUC(pres = ppred, contrast = apred, presWeight = rcmed.pres.weights,contrastWeight = rcmed.abs.weights)
  SEDI <- sediWeighted(pres = ppred, contrast = apred, thresholds = threshold)
  wSEDI <- sediWeighted(pres = ppred, contrast = apred, presWeight = rcmed.pres.weights,contrastWeight = rcmed.abs.weights, thresholds = threshold)
  # pROC-AUC-RATIO(heavy and sometimes fail. ensure it doesn't block the analysis)
  #proc <- tryCatch({kuenm::kuenm_proc(ppred, c(vars$bg.train.pred, apred) , threshold = 10, iterations = 100)}, error = function(e){
  #  #print('oh NO!')
  #  return(NULL)})
  #test.proc_auc_ratio <- ifelse(is.null(proc),rep(NA,length(SEDI)),proc$pROC_summary[1])
  #test.proc_pval <- ifelse(is.null(proc),rep(NA,length(SEDI)),proc$pROC_summary[2])
  
  out.test <- data.frame(
    test.SEDI= SEDI,
    test.wSEDI= wSEDI,
    test.AUC = AUC,
    test.wAUC = wAUC,
    test.CBI = CBI,
    test.wCBI = wCBI,
    #threshold_name = ix,
    row.names = NULL
  )
  
  #### DO THE SAME ANALYSIS FOR CROSS-VALIDATION
  
  ppred <- vars$occs.val.pred
  apred <- vars$bg.val.pred
  
  CBI <- enmSdmX::evalContBoyce(ppred,apred, graph = F) 
  wCBI <- enmSdmX::evalContBoyce(ppred,apred, presWeight = occ_k_weights[occ_k_weights$k==k,'weight'], contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight'], graph = F)
  AUC <- enmSdmX::evalAUC(pres = ppred, contrast = apred) 
  wAUC <- enmSdmX::evalAUC(pres = ppred, contrast = apred, presWeight = occ_k_weights[occ_k_weights$k==k,'weight'],contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight'])
  SEDI <- sediWeighted(pres = ppred, contrast = apred, thresholds = threshold)
  wSEDI <- sediWeighted(pres = ppred, contrast = apred, thresholds = threshold, presWeight = occ_k_weights[occ_k_weights$k==k,'weight'],contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight'])
  wAUC.train <- enmSdmX::evalAUC(pres = vars$occs.train.pred, contrast = vars$bg.train.pred , presWeight = occ_k_weights[occ_k_weights$k != k, "weight"], contrastWeight = bg_k_weights[bg_k_weights$k != k, "weight"])
  wAUC.diff <- wAUC.train - wAUC
  # pROC-AUC-RATIO(heavy and sometimes fail. ensure it doesn't block the analysis)
  #cv.proc <- tryCatch({kuenm::kuenm_proc(vars$occs.val.pred, c(vars$bg.train.pred, vars$bg.val.pred), threshold = 10, iterations = 100)}, error = function(e){
  #  #print('oh NO!')
  #  return(NULL)})
  #cv.proc_auc_ratio <- ifelse(is.null(cv.proc),rep(NA,length(SEDI)),cv.proc$pROC_summary[1])
  #cv.proc_pval <- ifelse(is.null(cv.proc),rep(NA,length(SEDI)),cv.proc$pROC_summary[2])
  
  
  #prepare the final output
  out <- data.frame(
    #cv.proc_auc_ratio = cv.proc_auc_ratio, 
    #cv.proc_pval = cv.proc_pval,
    cv.AUC= AUC,
    cv.wAUC= wAUC,
    cv.wAUC.diff = wAUC.diff,
    cv.CBI = CBI,
    cv.wCBI = wCBI,
    cv.SEDI= SEDI,
    cv.wSEDI= wSEDI,
    #test.proc_auc_ratio = test.proc_auc_ratio, 
    #test.proc_pval = test.proc_pval,
    out.test,
    threshold_value = threshold,
    #threshold_name = threshold_select,
    row.names = NULL
  )
  #print(head(out))
  return(out)
}

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
