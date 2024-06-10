############################################################################################################
# this script perform the variable inflation factor (VIF) analysis to test for multicollinearity among continuous, environmental variables.
#

# Loading environmental variables from raster files
envs <- terra::rast(paste0(  'rasters/', 'present_raster.tif'))
envs <- project(envs,eckertIV)
bathymetry <- terra::rast(paste0(  'rasters/', "coast_220m.tif"))
# Masking the environmental variables  
envs <- terra::mask(envs, project(bathymetry, eckertIV))

# Converting the masked environmental variables raster to a data frame, removing NA values
envs.df <- as.data.frame(envs, na.rm = TRUE)

# Performing variable inflation factor (VIF) analysis to test for multicollinearity among variables
# Note: VIF analysis excludes categorical variables
vifs <- usdm::vif(envs.df, size = Inf)  # 
vif.sort <- vifs[order(vifs$VIF, decreasing = F),]
vif.sort
vif.step <- usdm::vifstep(envs.df,th = 2.5,method = 'spearman', size = Inf)
vif.step
vif.cor <- usdm::vifcor(envs.df,th = 0.5, size = Inf)
vif.cor

# variables to keep are chosen according to their VIF and their physiological importance for the species (see chapter 1.2)
# selecting variables only on VIF or correlation criteria lead to models with lower predictive power
variables.selected <- c('max_thetao','min_thetao','max_so','mean_po4','mean_nppv','min_KE')
envs.final<- envs[[variables.selected]]
vnm <- names(envs.final)
envs.df <- as.data.frame(envs.final, na.rm = TRUE)
vif.results <- usdm::vif(envs.df, size = Inf)  #
# Outputting the VIF results to a text file
sink(paste0('results/',expname,'_', 'vif_results.txt'))
print(vif.results)
sink() # 

# Adding the substrate and biozone rasters to the final environmental variables raster
substrate <- as.numeric(terra::rast(paste0(  'rasters/', "substrate.tif")))
biozone <- as.numeric(terra::rast(paste0(  'rasters/', "biozone.tif")))
names(substrate) <- 'substrate'
names(biozone) <- 'biozone'
substrate_legend <- read.csv(paste0(  'data/', 'substrate_legend.csv'))
biozone_legend <- read.csv(paste0(  'data/', 'biozone_legend.csv'))
envs.final<- c(envs.final,as.factor(as.numeric(biozone)),as.factor(as.numeric(substrate)))
envs.final <- mask(envs.final, biozone)
vnm<- vnm[!vnm %in% c('biozone','substrate')]
writeRaster(envs.final, filename = paste0(  'rasters/',expname,'_', 'envs.tif'), overwrite = TRUE)

