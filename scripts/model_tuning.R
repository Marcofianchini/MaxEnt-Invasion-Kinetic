####################################
# Preprocessing Instructions:
# Before running this script, you need to modify two internal functions of the ENMeval package.
# 1. Open the file 'cv.enm_mod.R' located in your working directory under 'scripts/'.
# 2. Use trace(ENMeval:::cv.enm, edit = TRUE) in the R console to edit the cv.enm function directly.
#    Remove all existing code within the function and replace it with the contents from 'cv.enm_mod.R'.
# 3. Save the changes. This step is crucial for the script to run as intended.
####################################

# you can do it manually: 
# file.edit(paste0('scripts/','cv.enm_mod.R'))
# trace(ENMeval:::cv.enm, edit = T)

# modify cv.enm automatically (it should work without problems)
source(paste0('scripts/','cv.enm_mod.R'))
old.cvenm <- getFromNamespace("cv.enm",'ENMeval')
assignInNamespace(x = "cv.enm", value = cv.enm_mod, ns = "ENMeval")


# if needed, unquote to modify proc
#file.edit(paste0('scripts/','proc.R'))
source(paste0('scripts/','proc.R'))

# Load inputs and parameters
occ_k_weights <- read.csv(paste0('data/',expname,'_','final_occs.csv'))
bg_k_weights <- read.csv(paste0('data/',expname,'_','final_bg.csv'))
occs<-occ_k_weights[,1:2]
bg<-bg_k_weights[,1:2]
user.grp<-list(occs.grp = occ_k_weights$k,
               bg.grp = bg_k_weights$k,
               weights = c(occ_k_weights$weight,bg_k_weights$weight)
)
envs.final <- rast(paste0('rasters/',expname,'_','envs.tif'))
envs.final$biozone<-as.numeric(envs.final$biozone)
envs.final$substrate<-as.numeric(envs.final$substrate)
vnm <- names(envs.final)
vnm<- vnm[!vnm %in% c('biozone','substrate')]
#names(envs.final) <- nm
# Create the tuning "grid" of hyperparameters
ftrs <- c("L", "Q", "H","P",'T')
fc <- unlist(lapply(1:5, function(x) combn(ftrs, x, paste0, collapse = "")))
rm <- seq(0.5,20,0.5)

# write environmental data.frames for occurrences, background and test presence/absence sites 
occs.envs<- na.omit(terra::extract(envs.final,vect(occs,geom = c('x','y'),crs = eckertIV),xy = T, ID=F)[,c('x','y',names(envs.final))])
bg.envs<- na.omit(terra::extract(envs.final,vect(bg,geom = c('x','y'),crs = eckertIV),xy = T, ID=F)[,c('x','y',names(envs.final))])

# If you want to clamp variables in respect of each fold, uncomment the following lines
#occs.k.envs<- as.data.frame(na.omit(terra::extract(envs.final,vect(occ_k_weights,geom = c('x','y'),crs = eckertIV),xy = T, ID=F, bind =T)))
#bg.k.envs<-   as.data.frame(na.omit(terra::extract(envs.final,vect(bg_k_weights,geom = c('x','y'),crs = eckertIV),xy = T, ID=F,bind=T)))
#
## clamp the variables in respect of each fold 
#for(i in 1:5){
#  ref.vals <- rbind(occs.k.envs[occs.k.envs$k != i, vnm], bg.k.envs[bg.k.envs$k != i, vnm])
#  occs.k.envs[occs.k.envs$k == i, vnm] <- clamp.vars(orig.vals = occs.k.envs[occs.k.envs$k == i, vnm] ,ref.vals = ref.vals, left = vnm, right = vnm)
#  bg.k.envs[bg.k.envs$k == i, vnm] <- clamp.vars(orig.vals = bg.k.envs[bg.k.envs$k == i, vnm] ,ref.vals = ref.vals, left = vnm, right = vnm)
#}
#occs.envs <- occs.k.envs[,][,c('x','y',names(envs.final))]
#bg.envs <- bg.k.envs[,][,c('x','y',names(envs.final))]

write.csv(occs.envs[,names(envs.final)],paste0('temp/','occs_envs.csv'),row.names = F)
write.csv(bg.envs[,names(envs.final)],paste0('temp/','bg_envs.csv'),row.names = F)
rcmed.pres.envs <- terra::extract(as.numeric(envs.final), rcmed.pres_sf, ID=F)
rcmed.abs.envs <- terra::extract(as.numeric(envs.final), rcmed.abs_sf, ID=F)
write.csv(rcmed.pres.envs, paste0( 'data/',expname,'_', 'rcmed_pres_envs.csv'), row.names = F)
write.csv(rcmed.abs.envs, paste0(  'data/',expname,'_', 'rcmed_abs_envs.csv'), row.names = F)
write.csv(rcmed.pres.envs, paste0( 'temp/', 'rcmed_pres_envs.csv'), row.names = F)
write.csv(rcmed.abs.envs, paste0( 'temp/', 'rcmed_abs_envs.csv'), row.names = F)
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
                            parallel = F,
                            parallelType = 'doParallel',
                            numCores = 8
)
e.mx@results
#saveRDS(e.mx,paste0('results/',expname,'_','emx_final.rds'))
# compute postproc metrics.
source(paste0('scripts/','compute_omission_rate.R'))
e.mx <- computeOmissionRate(truepres_envs_path = paste0('temp/',"rcmed_pres_envs.csv"), e.mx,is_weighted = F)
e.mx <- computeOmissionRate(truepres_envs_path = paste0('temp/',"rcmed_pres_envs.csv"), e.mx,is_weighted = T,truepres_weights_path = paste0('temp/',"rcmed_pres_weights.csv"))

# perform TOPSIS analysis do decide the best model according to test set 
file.edit(paste0('scripts/','modified_topsis_analysis.R')) #open and/or source it
source(paste0('scripts/','modified_topsis_analysis.R'))
# save 
saveRDS(e.mx, file = paste0('results/',expname,'_','emx_final.rds'))

#e.mx<- readRDS(paste0('results/',expname,'_','emx_final.rds')) # to re-read 

#############################################################################
bb <- e.mx@results
# write bb to csv 
write.csv(bb, file = paste0('results/',expname,'_','emx_results.csv'), row.names = F)
