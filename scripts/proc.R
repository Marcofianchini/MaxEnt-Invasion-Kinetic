
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
