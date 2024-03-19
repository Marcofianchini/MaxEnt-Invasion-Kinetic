####################################
# A custom function to compute the additional performance metrics while fitting the models in ENMevaluate call. 
# !!!!!!!!!!!!!!!!!!!!!!!!!!BE CAREFUL!!!!!!!!!!!!!!!!!!!!!!!!!!!
#
# YOU NEED TO FIRST MODIFY INTERNAL FUNCTION USING trace(ENMeval:::cv.enm, edit = T): 
# remove all code and paste the code you find in cv.enm_mod.R and save. 
#
# PS: sediWeighted was a function used in old 'enmSdm' package, now defunct and back-incompatible. So It will be defined inside proc to ensure it works properly.
file.edit(paste0(wd,'scripts/','cv.enm_mod.R'))
trace(ENMeval:::cv.enm, edit = T)


proc <- function(vars) {
  
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
  #wd<-
  
  k<-vars$k_num
  model<-vars$mod.k
  
  rnms<-row.names(model@results)
  
  # compute statistics against the withheld dataset
  rcmed.points<-read.csv(paste0(wd,'/data/',"rcmed_abundances_adjusted_proj.csv"))
  rcmed.envs<-read.csv(paste0(wd,'/data/',"rcmed_envs_19_proj.csv"))
  rcmed.pred<-dismo::predict(model,rcmed.envs)
  rcmed.pres.envs<-read.csv(paste0(wd,'/data/',"rcmed_pres_envs_19_proj.csv"))
  rcmed.pres.weights<-readRDS(paste0(wd,'/data/',"rcmed_pres_weights_19_proj.RDS"))
  rcmed.abs.weights<-readRDS(paste0(wd,'/data/',"rcmed_abs_weights_19_proj.RDS"))
  rcmed.abs.envs<-read.csv(paste0(wd,'/data/',"rcmed_abs_envs_19_proj.csv"))
  ppred<-dismo::predict(model,rcmed.pres.envs)
  apred<-dismo::predict(model,rcmed.abs.envs)
  
  occ_k_weights<-read.csv(paste0(wd,'/data/','occ_k_weights.csv'))
  bg_k_weights<-read.csv(paste0(wd,'/data/','bg_k_weights.csv'))
  
  threshold_select <- "Maximum.training.sensitivity.plus.specificity.Cloglog.threshold"
  threshold_ix<- which(rnms==threshold_select)
  threshold <- model@results[threshold_ix]
  names(threshold)<- threshold_select
  pred.test<-ifelse(rcmed.pred>threshold,1,0)
  truth<-ifelse(rcmed.points$layer>0,1,0)
  
  ## for stability/debugging #
  if(sum(pred.test)==0){
    #print('no presence points from current model. adding 1 "fake" presence for statistics stability')
    pred.test<-c(1,rep(0,length(pred.test)-1))
  }
  if(sum(pred.test)==length(pred.test)){
    #print('only presence points from current model. adding 1 "fake" absence for statistics stability')
    pred.test<-c(0,rep(1,length(pred.test)-1))
  }
  ## 
  
  CBI <- enmSdmX::evalContBoyce(pres = ppred, contrast =  apred,graph = F)
  wCBI <- enmSdmX::evalContBoyce(ppred,apred,presWeight = rcmed.pres.weights, contrastWeight = rcmed.abs.weights,graph = F)
  AUC <- enmSdmX::evalAUC(pres = ppred, contrast = apred)
  wAUC <- enmSdmX::evalAUC(pres = ppred, contrast = apred, presWeight = rcmed.pres.weights,contrastWeight = rcmed.abs.weights)
  SEDI <- sediWeighted(pres = ppred, contrast = apred, thresholds = threshold)
  wSEDI <- sediWeighted(pres = ppred, contrast = apred, presWeight = rcmed.pres.weights,contrastWeight = rcmed.abs.weights, thresholds = threshold)
  
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
  
  occs.val.pred.class <- ifelse(vars$occs.val.pred >= threshold, 1, 0)
  bg.val.pred.class <- ifelse(vars$bg.val.pred >= threshold, 1, 0)
  true.classes <- c(rep(1, nrow(vars$occs.val.z)), rep(0, nrow(vars$bg.val.z)))
  predicted.classes <- c(occs.val.pred.class, bg.val.pred.class)
  ppred <- occs.val.pred.class
  apred <- bg.val.pred.class
  
  ## for stability/debugging #
  if(sum(predicted.classes)==0){
    #print('no presence points from current model. adding 1 "fake" presence for statistics stability')
    predicted.classes<-c(1,rep(0,length(predicted.classes)-1))
  }
  if(sum(predicted.classes)==length(predicted.classes)){
    #print('only presence points from current model. adding 1 "fake" absence for statistics stability')
    predicted.classes<-c(0,rep(1,length(predicted.classes)-1))
  }
  ##
  
  CBI <- enmSdmX::evalContBoyce(ppred,apred, graph = F) #contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight']
  wCBI <- enmSdmX::evalContBoyce(ppred,apred, presWeight = occ_k_weights[occ_k_weights$k==k,'weight'], contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight'], graph = F) #contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight']
  AUC <- enmSdmX::evalAUC(pres = ppred, contrast = apred) #contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight']
  wAUC <- enmSdmX::evalAUC(pres = ppred, contrast = apred, presWeight = occ_k_weights[occ_k_weights$k==k,'weight'],contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight']) #contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight']
  SEDI <- sediWeighted(pres = ppred, contrast = apred, thresholds = threshold)#,contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight']
  wSEDI <- sediWeighted(pres = ppred, contrast = apred, thresholds = threshold, presWeight = occ_k_weights[occ_k_weights$k==k,'weight'],contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight'])#,contrastWeight = bg_k_weights[bg_k_weights$k==k,'weight']
  
  # NOW LET'S COMPUTE pROC-AUC-RATIO 
  ##proc_auc_ratio is REALLY heavy and sometimes fail. ensure it doesn't block the analysis
  proc <- tryCatch({kuenm::kuenm_proc(ppred, c(apred), threshold = 10, iterations = 1000)}, error = function(e){
    #print('oh NO!')
    return(NULL)})
  out.test <- out
  cv.proc <- tryCatch({kuenm::kuenm_proc(vars$occs.val.pred, c(vars$bg.train.pred, vars$bg.val.pred), threshold = 10, iterations = 1000)}, error = function(e){
    #print('oh NO!')
    return(NULL)})
  cv.proc_auc_ratio <- ifelse(is.null(cv.proc),rep(NA,length(SEDI)),cv.proc$pROC_summary[1])
  cv.proc_pval <- ifelse(is.null(cv.proc),rep(NA,length(SEDI)),cv.proc$pROC_summary[2])
  test.proc_auc_ratio <- ifelse(is.null(proc),rep(NA,length(SEDI)),proc$pROC_summary[1])
  test.proc_pval <- ifelse(is.null(proc),rep(NA,length(SEDI)),proc$pROC_summary[2])
  #######
  
  #prepare the final output
  out <- data.frame(
    cv.proc_auc_ratio = cv.proc_auc_ratio, 
    cv.proc_pval = cv.proc_pval,
    cv.AUC= AUC,
    cv.wAUC= wAUC,
    cv.CBI = CBI,
    cv.wCBI = wCBI,
    cv.SEDI= SEDI,
    cv.wSEDI= wSEDI,
    test.proc_auc_ratio = test.proc_auc_ratio, 
    test.proc_pval = test.proc_pval,
    out.test,
    threshold_value = threshold,
    threshold_name = threshold_name,
    row.names = NULL
  )
  
  return(out)
}



occs <- read.csv(wd,'/data/','occ_k_weights.csv')
bg <- read.csv(wd,'/data/','bg_k_weights.csv')
