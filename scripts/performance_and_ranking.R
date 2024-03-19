library(ggplot2)
library(dplyr)
plt.lst<-list()

variables <- c("test.AUC.avg", "test.wAUC.avg", "test.CBI.avg", "test.wCBI.avg", "test.SEDI.avg", "test.wSEDI.avg","test_OR10p", "AUC.test.diff.avg" ,"wAUC.test.diff.avg", "AICc",'test_wOR10p', 'weighted.auc.diff', 'weighted.cbi.diff','weighted.sedi.diff', 'weighted.or10p.diff')  # replace with your variables

name.vector <- c('AUC','CBI','SEDI','AUCdiff','OR10p', 'AICc', 'wAUC','wCBI','wSEDI','wAUCdiff','wOR10p')



# here you can check the ranking of the models based on the variable of interest
bb <- e.mx@results
model.auc <- bb %>% filter(auc.val.avg == max(auc.val.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.cbi <- bb %>% filter(cbi.val.avg == max(cbi.val.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.sedi <- bb %>% filter(cv.SEDI.avg == max(cv.SEDI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.adiff <- bb %>% filter(auc.diff.avg == min(auc.diff.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.or10p <- bb %>% filter(or.10p.avg == min(or.10p.avg, na.rm = T)) %>% first() %>% select(tune.args)
model.aic <- bb %>% filter(AICc == min(AICc,na.rm=T)) %>% first() %>% select(tune.args)
# weighted metrics
model.wauc <- bb %>% filter(cv.wAUC.avg == max(cv.wAUC.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wcbi <- bb %>% filter(cv.wCBI.avg == max(cv.wCBI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wsedi <- bb %>% filter(cv.wSEDI.avg == max(cv.wSEDI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wadiff <- bb %>% filter(cv.wAUC.diff.avg == min(cv.wAUC.diff.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wor10p <- bb %>% filter(or.10p.avg == min(or.10p.avg, na.rm = T)) %>% first() %>% select(tune.args)


# 
for(variable in variables){
  if(variable %in% c("test.AUC.avg", "test.wAUC.avg", "test.CBI.avg", "test.wCBI.avg", "test.SEDI.avg", "test.wSEDI.avg")){
    d <- T
  } else {
    d <- F
  }
  
  bb <- e.mx@results
  bb<-bb[order(bb[,variable],decreasing = d),]
  bb$rank <- 1:nrow(bb)
  #print(bb[1,])
  opt.auc <- bb %>% filter(tune.args== model.auc$tune.args)
  opt.cbi <- bb %>% filter(tune.args== model.cbi$tune.args)
  opt.sedi <- bb %>% filter(tune.args== model.sedi$tune.args)
  opt.adiff <- bb %>% filter(tune.args== model.adiff$tune.args)
  opt.or10p <- bb %>% filter(tune.args== model.or10p$tune.args)
  opt.aic <- bb %>% filter(tune.args== model.aic$tune.args)
  
  opt.wauc <- bb %>% filter(tune.args== model.wauc$tune.args)
  opt.wcbi <- bb %>% filter(tune.args== model.wcbi$tune.args)
  opt.wsedi <- bb %>% filter(tune.args== model.wsedi$tune.args)
  opt.wadiff <- bb %>% filter(tune.args== model.wadiff$tune.args)
  opt.wor10p <- bb %>% filter(tune.args== model.wor10p$tune.args)
  
  
  # report modname as paste of fc and rm, selecname, and rank
  bestmodels <- data.frame(modname = c(
                                       paste0(opt.auc$fc,opt.auc$rm),
                                       paste0(opt.cbi$fc,opt.cbi$rm),
                                       paste0(opt.sedi$fc,opt.sedi$rm),
                                       paste0(opt.adiff$fc,opt.adiff$rm),
                                       paste0(opt.or10p$fc,opt.or10p$rm),
                                       paste0(opt.aic$fc,opt.aic$rm),
                                       paste0(opt.wauc$fc,opt.wauc$rm),
                                       paste0(opt.wcbi$fc,opt.wcbi$rm),
                                       paste0(opt.wsedi$fc,opt.wsedi$rm),
                                       paste0(opt.wadiff$fc,opt.wadiff$rm),
                                       paste0(opt.wor10p$fc,opt.wor10p$rm)), selecname = name.vector, 
                           rank = c(opt.auc$rank,opt.cbi$rank,opt.sedi$rank,opt.adiff$rank,opt.or10p$rank,opt.aic$rank,opt.wauc$rank,opt.wcbi$rank,opt.wsedi$rank,opt.wadiff$rank,opt.wor10p$rank))
  bestmodels <- bestmodels[order(bestmodels$rank),]
  print(paste0(variable, " ranking"))
  print(bestmodels)
  print('---------------------------------------------------------------------------------')
  print('---------------------------------------------------------------------------------')
}

bestopt <-  bb %>% filter(tune.args== model.or10p$tune.args)


