####################################
# Preprocessing Instructions:
# Before running this script, you need to modify two internal functions of the ENMeval package.
# 1. Open the file 'cv.enm_mod.R' located in your working directory under 'scripts/'.
# 2. Use trace(ENMeval:::cv.enm, edit = TRUE) in the R console to edit the cv.enm function directly.
#    Remove all existing code within the function and replace it with the contents from 'cv.enm_mod.R'.
# 3. Save the changes. This step is crucial for the script to run as intended.
####################################


file.edit(paste0(wd,'scripts/','cv.enm_mod.R'))
trace(ENMeval:::cv.enm, edit = T)

# Load inputs and parameters
occ_k_weights <- read.csv(paste0(wd,'data/',expname,'_','final_occs.csv'))
bg_k_weights <- read.csv(paste0(wd,'data/',expname,'_','final_bg.csv'))
occs<-occ_k_weights[,1:2]
bg<-bg_k_weights[,1:2]
user.grp<-list(occs.grp = occ_k_weights$k,
               bg.grp = bg_k_weights$k,
               weights = c(occ_k_weights$weight,bg_k_weights$weight)
)
envs.final <- rast(paste0(wd,'rasters/',expname,'_','envs.tif'))
envs.final$biozone<-as.numeric(envs.final$biozone)
envs.final$substrate<-as.numeric(envs.final$substrate)
#names(envs.final) <- nm
# Create the tuning "grid" of hyperparameters
ftrs <- c("L", "Q", "H","P",'T')
fc <- unlist(lapply(1:5, function(x) combn(ftrs, x, paste0, collapse = "")))
rm <- seq(0.5,20,0.5)


occs.envs<- na.omit(terra::extract(envs.final,vect(occs,geom = c('x','y'),crs = eckertIV),xy = T, ID=F)[,c('x','y',names(envs.final))])
bg.envs<- na.omit(terra::extract(envs.final,vect(bg,geom = c('x','y'),crs = eckertIV),xy = T, ID=F)[,c('x','y',names(envs.final))])
write.csv(occs.envs[,names(envs.final)],paste0(wd,'temp/','occs_envs.csv'))
write.csv(bg.envs[,names(envs.final)],paste0(wd,'temp/','bg_envs.csv'))
# Fit the models.
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!! this will take a LONG time to run !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
e.mx<- ENMeval::ENMevaluate(occs = occs,
                            other.settings = list(validation.bg='full') , # 
                            envs = envs.final,
                            categoricals = c("substrate","biozone"),
                            bg=bg,
                            doClamp = F,
                            user.grp = user.grp,
                            tune.args = list(fc = fc, rm = rm), #
                            partitions = 'user',
                            user.eval = proc,
                            algorithm = 'maxent.jar',
                            taxon.name = 'Caulerpa cylindracea',
                            overlap = F,
                            parallel = T,
                            parallelType = 'doParallel',
                            numCores = 6
)
saveRDS(e.mx,paste0(wd,'results/',expname,'_','emx_final.rds'))
# compute postproc metrics.
source(paste0(wd,'scripts/','compute_omission_rate.R'))
e.mx <- computeOmissionRate(truepres_envs_path = paste0(wd,'temp/',"rcmed_pres_envs.csv"), e.mx,is_weighted = F)
e.mx <- computeOmissionRate(truepres_envs_path = paste0(wd,'temp/',"rcmed_pres_envs.csv"), e.mx,is_weighted = T,truepres_weights_path = paste0(wd,'temp/',"rcmed_pres_weights.csv"))
e.mx@results$AUC.test.diff.avg<-    abs(e.mx@results$cv.AUC.avg-e.mx@results$test.AUC.avg)
e.mx@results$wAUC.test.diff.avg<-   abs(e.mx@results$cv.wAUC.avg-e.mx@results$test.wAUC.avg)
e.mx@results$weighted.auc.diff <-   abs(e.mx@results$test.AUC.avg - e.mx@results$test.wAUC.avg)
e.mx@results$weighted.cbi.diff <-   abs(e.mx@results$test.CBI.avg - e.mx@results$test.wCBI.avg)
e.mx@results$weighted.sedi.diff <-  abs(e.mx@results$test.SEDI.avg - e.mx@results$test.wSEDI.avg)
e.mx@results$weighted.or10p.diff <- abs(e.mx@results$test_OR10p - e.mx@results$test_wOR10p)
 # save 
saveRDS(e.mx, file = paste0(wd,'results/',expname,'_','emx_final.rds'))

#e.mx<- readRDS(paste0(wd,'results/',expname,'_','emx_final.rds')) # to re-read 

#############################################################################
