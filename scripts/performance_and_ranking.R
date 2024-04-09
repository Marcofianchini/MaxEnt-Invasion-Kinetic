# here you can check the ranking of the models based on the variable of interest
# Then we will perform TOPSIS analysis on test results to decide which model is better at reproducing independent test data. We will build a rank of TOPSIS values to see how the model performs relatively to the others 
variables <- c("test.AUC.avg", "test.wAUC.avg", "test.CBI.avg", "test.wCBI.avg", "test.SEDI.avg", "test.wSEDI.avg","test_OR10p", "AUC.test.diff.avg" ,"wAUC.test.diff.avg", "AICc",'test_wOR10p', 'weighted.auc.diff', 'weighted.cbi.diff','weighted.sedi.diff', 'weighted.or10p.diff')  # replace with your variables

name.vector <- c('AUC','CBI','SEDI','AUCdiff','OR10p', 'AICc', 'wAUC','wCBI','wSEDI','wAUCdiff')

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

allmod <- e.mx@results[,c('fc','rm','tune.args')]
allrank <- data.frame(modname = c(
  paste0(opt.auc$fc,opt.auc$rm),
  paste0(opt.cbi$fc,opt.cbi$rm),
  paste0(opt.sedi$fc,opt.sedi$rm),
  paste0(opt.adiff$fc,opt.adiff$rm),
  paste0(opt.or10p$fc,opt.or10p$rm),
  paste0(opt.aic$fc,opt.aic$rm),
  paste0(opt.wauc$fc,opt.wauc$rm),
  paste0(opt.wcbi$fc,opt.wcbi$rm),
  paste0(opt.wsedi$fc,opt.wsedi$rm),
  paste0(opt.wadiff$fc,opt.wadiff$rm)
), selecname = name.vector)
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
                                       paste0(opt.wadiff$fc,opt.wadiff$rm)
                                       ), selecname = name.vector, 
                           rank = c(opt.auc$rank,opt.cbi$rank,opt.sedi$rank,opt.adiff$rank,opt.or10p$rank,opt.aic$rank,opt.wauc$rank,opt.wcbi$rank,opt.wsedi$rank,opt.wadiff$rank))
  bestmodels <- bestmodels[order(bestmodels$rank),]
  # bind the ranking to the allrank data frame with the variable name as a column
  allrank<- left_join(allrank, bestmodels, by = c('modname','selecname'))
  allmod <- left_join(allmod, bb[,c('tune.args','rank')], by = c('tune.args'))
  print(paste0(variable, " ranking"))
  print(bestmodels)
  print('---------------------------------------------------------------------------------')
  print('---------------------------------------------------------------------------------')
}

names(allrank) <- c('modname','selecname',variables)
names(allmod) <- c('fc','rm','tune.args',variables)


# use topsis to define the best model
to.topsis <- allrank[,variables]
topsis.impacts <- rep('-',ncol(to.topsis))

topsis_res <- topsis(as.matrix(to.topsis),impacts = topsis.impacts, weights = rep(1,ncol(to.topsis)))
topsis_df <- as.data.frame(topsis_res)
topsis_df$modname <- allrank$modname
topsis_df$selecname <- allrank$selecname
topsis_df <- topsis_df %>% arrange(rank)
head(topsis_df) # the best model is OR10p 

all.to.topsis <- allmod[,variables]
all_topsis_res <- topsis(as.matrix(all.to.topsis),impacts = topsis.impacts, weights = rep(1,ncol(all.to.topsis)))
all_topsis_df <- as.data.frame(all_topsis_res)
all_topsis_df$fc <- allmod$fc
all_topsis_df$rm <- allmod$rm
all_topsis_df$tune.args <- allmod$tune.args
all_topsis_df <- all_topsis_df %>% arrange(rank) %>% select(tune.args,rank, score)
head(all_topsis_df)
e.mx@results$rank <- NULL
e.mx@results$score <- NULL
e.mx@results <- left_join(e.mx@results, all_topsis_df, by = 'tune.args')


bestopt <-  bb %>% filter(tune.args== opt.or10p$tune.args)
bestmod <- e.mx@models[[opt.or10p$tune.args]]

saveRDS(e.mx, file = paste0(wd,'results/',expname,'_','emx_final.rds'))

