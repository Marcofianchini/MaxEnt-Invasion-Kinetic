############################################################################################################
# this script perform the variable inflation factor (VIF) analysis to test for multicollinearity among continuous, environmental variables.
#
# Loading environmental variables from raster files
envs <- terra::rast(paste0(wd, 'rasters/', 'present_raster.tif'))
envs <- project(envs,eckertIV)
bathymetry <- terra::rast(paste0(wd, 'rasters/', "coast_220m.tif"))
# Masking the environmental variables  
envs <- terra::mask(envs, project(bathymetry, eckertIV))

# Converting the masked environmental variables raster to a data frame, removing NA values
envs.df <- as.data.frame(envs, na.rm = TRUE)

# Performing variable inflation factor (VIF) analysis to test for multicollinearity among variables
# Note: VIF analysis excludes categorical variables
vif.results <- usdm::vifstep(envs.df, th = 2.5, size = Inf, keep = c('mean_KE','mean_nh4','mean_po4','mean_no3')) 

# Outputting the VIF results to a text file
sink(paste0(wd, 'results/',expname,'_', 'vif_results.txt'))
print(vif.results)
sink() # Stop diverting output to the file

# Environmental variables with acceptable VIF scores
variables.selected <- vif.results@results$Variables
envs.final<- envs[[variables.selected]]
vnm <- names(envs.final)

# Adding the substrate and biozone rasters to the final environmental variables raster
substrate <- as.numeric(terra::rast(paste0(wd, 'rasters/', "substrate.tif")))
biozone <- as.numeric(terra::rast(paste0(wd, 'rasters/', "biozone.tif")))
names(substrate) <- 'substrate'
names(biozone) <- 'biozone'
substrate_legend <- read.csv(paste0(wd, 'results/',expname,'_', 'substrate_legend.csv'))
biozone_legend <- read.csv(paste0(wd, 'results/',expname,'_', 'biozone_legend.csv'))
envs.final<- c(envs.final,as.factor(as.numeric(biozone)),as.factor(as.numeric(substrate)))
envs.final <- mask(envs.final, biozone)
vnm<- vnm[!vnm %in% c('biozone','substrate')]
writeRaster(envs.final, filename = paste0(wd, 'rasters/',expname,'_', 'envs.tif'), overwrite = TRUE)

# Now that we have the variables, we can create the mean raster scenario for the future
source(paste0(wd,'scripts/','create_mean_spatraster.R'))
fut.cond <- createMeanSpatRaster(paste0(wd,'rasters/predictors/'),vnm, 2030,2050)
fut.cond<-terra::project(fut.cond,envs.final)
fut.cond<-mask(fut.cond,envs.final[[1]])
fut.cond$biozone<- envs.final$biozone
fut.cond$substrate<- envs.final$substrate
writeRaster(fut.cond, filename = paste0(wd, 'rasters/',expname,'_', 'future3050_raster.tif'), overwrite = TRUE)
