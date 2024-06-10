criteria <- c("tune.args",'score','cv.score','cv.score.w','cv.diff.score','test.score','test.weighted.score','test.diff.score','test.AUC.avg', 'test.wAUC.avg', 'test.CBI.avg', 'test.wCBI.avg', 'test.SEDI.avg', 'test.wSEDI.avg','test_OR10p', 'AUC.test.diff.avg' ,'wAUC.test.diff.avg', 'AICc','test_wOR10p', 'weighted.auc.diff', 'weighted.cbi.diff','weighted.sedi.diff', 'weighted.or10p.diff','cv.wAUC.avg','cv.wCBI.avg','cv.wSEDI.avg','cv.wAUC.diff.avg','or.10p.avg','AICc','t.proc','cv.proc')

name.vector <- c('AUC','CBI','SEDI','AUCdiff','wAUC','wCBI','wSEDI','wAUCdiff','OR10p', 'AICc','pROC')
                 

# Extract results from model and identify best models based on different criteria
bb <- e.mx@results

model.or10p <- bb %>% filter(or.10p.avg == min(or.10p.avg, na.rm = T)) %>% first() %>% select(tune.args)
model.aic <- bb %>% filter(AICc == min(AICc,na.rm=T)) %>% first() %>% select(tune.args)
model.auc <- bb %>% filter(cv.AUC.avg == max(cv.AUC.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.cbi <- bb %>% filter(cv.CBI.avg == max(cv.CBI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.sedi <- bb %>% filter(cv.SEDI.avg == max(cv.SEDI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.adiff <- bb %>% filter(auc.diff.avg == min(auc.diff.avg,na.rm=T)) %>% first() %>% select(tune.args)

model.wauc <- bb %>% filter(cv.wAUC.avg == max(cv.wAUC.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wcbi <- bb %>% filter(cv.wCBI.avg == max(cv.wCBI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wsedi <- bb %>% filter(cv.wSEDI.avg == max(cv.wSEDI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wadiff <- bb %>% filter(cv.wAUC.diff.avg == min(cv.wAUC.diff.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.proc <- bb %>% filter(cv.proc == max(cv.proc,na.rm=T)) %>% first() %>% select(tune.args)

opt.or10p <- bb %>% filter(tune.args== model.or10p$tune.args)
opt.aic <- bb %>% filter(tune.args== model.aic$tune.args)
opt.proc <- bb %>% filter(tune.args== model.proc$tune.args)
opt.auc <- bb %>% filter(tune.args== model.auc$tune.args)
opt.cbi <- bb %>% filter(tune.args== model.cbi$tune.args)
opt.sedi <- bb %>% filter(tune.args== model.sedi$tune.args)
opt.adiff <- bb %>% filter(tune.args== model.adiff$tune.args)
opt.wauc <- bb %>% filter(tune.args== model.wauc$tune.args)
opt.wcbi <- bb %>% filter(tune.args== model.wcbi$tune.args)
opt.wsedi <- bb %>% filter(tune.args== model.wsedi$tune.args)
opt.wadiff <- bb %>% filter(tune.args== model.wadiff$tune.args)

# Create a data.frame of the candidates 
mod.selection <- rbind(opt.auc,opt.cbi,opt.sedi,opt.adiff,opt.wauc,opt.wcbi,opt.wsedi,opt.wadiff,opt.or10p,opt.aic,opt.proc)
mod.selection <- cbind(name.vector,mod.selection)
mod.rankings <- mod.selection %>% select(name.vector,criteria)

# Create a dataframe of ranks for each metric considered 
# Define the sorting order for each metric 
for(i in 3:ncol(mod.rankings)){
  if(colnames(mod.rankings[i]) %in% c("test.AUC.avg", "test.wAUC.avg", "test.CBI.avg", "test.wCBI.avg", "test.SEDI.avg", "test.wSEDI.avg",'score',
                                      "cv.wAUC.avg", "cv.wCBI.avg", "cv.wSEDI.avg", "cv.score",'t.proc','cv.proc','test.score','test.weighted.score','test.diff.score')){
    d <- T
  } else {
    d <- F
  }
  if(d){
    mod.rankings[,i] <- rank(-mod.rankings[,i], ties.method = "first")
  } else {
    mod.rankings[,i] <- rank(mod.rankings[,i], ties.method = "first")
  }
}

# Reorder by decreasing final TOPSIS score
mod.rankings <- mod.rankings %>% arrange(desc(-score))
# TOPSISall = overall TOPSIS
# TOPSIScv = TOPSIS based on cross-validation metrics
# TOPSISt = TOPSIS based on test metrics
# TOPSIStw = TOPSIS based on weighted test metrics
# TOPSIStdiff = TOPSIS based on unweighted-weighted test metrics differences
names(mod.rankings) <- c(names(mod.rankings)[1:2],c('TOPSISall','TOPSIScv','TOPSIScvw','TOPSIScvdiff','TOPSISt','TOPSIStw','TOPSIStdiff'),names(mod.rankings)[10:ncol(mod.rankings)])
mod.rankings


# Save the results
write.csv(mod.rankings, file = paste0('results/',expname,'_',"model_rankings.csv"), row.names = F)
bestopt <- mod.selection %>% filter(score == max(score,na.rm=T)) %>% first()
bestmod <- e.mx@models[[bestopt$tune.args]]
saveRDS(bestopt,paste0('results/',expname,'_','bestopt.rds'))
saveRDS(bestmod,paste0('results/',expname,'_','bestmod.rds'))    


mod.scores <- mod.selection %>% select(name.vector,c('tune.args','score'), 
                                       c('cv.wAUC.avg','test.wAUC.avg'),
                                       c('cv.proc','t.proc','t.proc.pval'),
                                       c('cv.wSEDI.avg','test.wSEDI.avg'),
                                       c('or.10p.avg','test_OR10p'),
                                       c('cv.wAUC.diff.avg','wAUC.test.diff.avg'),
                                       c('AICc','delta.AICc'),
                                       c('cv.wCBI.avg','test.wCBI.avg')
                                       )
mod.scores <- mod.scores %>% arrange(desc(score))
mod.scores[,3:ncol(mod.scores)] <- apply(mod.scores[,-c(1,2)],2,FUN=function(x)round(x,2))
names(mod.scores) <- c(names(mod.scores)[1:2],c('TOPSIS'),c('cv.wAUC.avg','test.wAUC.avg','cv.proc','t.proc','t.proc.pval','cv.wSEDI.avg','test.wSEDI.avg','or.10p.avg','test_OR10p','cv.wAUC.diff.avg','wAUC.test.diff.avg','AICc','delta.AICc','cv.wCBI.avg','test.wCBI.avg'))
mod.scores
write.csv(mod.scores, file = paste0('results/',expname,'_',"model_scores.csv"), row.names = F)                       



########################################
# Extract the thresholds 
threshold <- "Maximum.training.sensitivity.plus.specificity.Cloglog.threshold"
maxSSS.test.th <- bestopt$threshold_value.avg
maxSSS.th <- bestmod@results[threshold,]
maxSSS.th.values <- e.mx@results.partitions %>% filter(tune.args == bestopt$tune.args) %>% select(threshold_value) %>% unlist()
maxSSS.th.values <- c(maxSSS.th.values,maxSSS.th,maxSSS.test.th)
maxSSS.th.range <- range(maxSSS.th.values)

# Create the classification matrices 
th<-maxSSS.th
th_lower <- maxSSS.th.range[2]
th_upper <- maxSSS.th.range[1]
int_mx<-matrix(c(0,th/3,0,
                 th/3,2*th/3,1,
                 2*th/3,th,2,
                 th,1,3),
               ncol=3, byrow=T)
int_mx_lower<-matrix(c(0,th_lower/3,0,
                       th_lower/3,2*th_lower/3,1,
                       2*th_lower/3,th_lower,2,
                       th_lower,1,3),
                     ncol=3, byrow=T)
int_mx_upper<-matrix(c(0,th_upper/3,0,
                       th_upper/3,2*th_upper/3,1,
                       2*th_upper/3,th_upper,2,
                       th_upper,1,3),
                     ncol=3, byrow=T)
# 
# # Save the results
saveRDS(int_mx,paste0('results/',expname,'_','int_mx.rds'))
saveRDS(int_mx_lower,paste0('results/',expname,'_','int_mx_lower.rds'))
saveRDS(int_mx_upper,paste0('results/',expname,'_','int_mx_upper.rds'))
