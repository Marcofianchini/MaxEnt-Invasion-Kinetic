# Now that we have the variables, we can create the mean raster scenario for the future
vnm <- read.csv(paste0('data/', 'predictors_names.csv'))[,1]
source(paste0( 'scripts/','create_mean_spatraster.R'))

# check the predictors folder
pres.cond <- createMeanSpatRaster(paste0('predictors/'),vnm, 2000,2020)
pres.cond <- terra::project(pres.cond, eckertIV)
pres.cond$biozone <- rast(paste0(  'rasters/', "biozone.tif"))
pres.cond$substrate <- rast(paste0(  'rasters/', "substrate.tif"))
writeRaster(pres.cond, filename = paste0(  'rasters/', 'present0020_raster.tif'), overwrite = TRUE)

fut.cond <- createMeanSpatRaster(paste0('predictors/'),vnm, 2030,2050)
fut.cond<-terra::project(fut.cond,eckertIV)
fut.cond$biozone<- rast(paste0(  'rasters/', "biozone.tif"))
fut.cond$substrate<- rast(paste0(  'rasters/', "substrate.tif"))
writeRaster(fut.cond, filename = paste0(  'rasters/', 'future3050_raster.tif'), overwrite = TRUE)

for(y in 2000:2050){
  f.rst <- createMeanSpatRaster(paste0('predictors/'),vnm, y,y)
  f.rst <- terra::project(f.rst, eckertIV)
  f.rst$biozone <-rast(paste0(  'rasters/', "biozone.tif"))
  f.rst$substrate <- rast(paste0(  'rasters/', "substrate.tif"))
  writeRaster(f.rst, filename = paste0('rasters/','envs_', as.character(y), '.tif'), overwrite = TRUE)
  print(paste0(y, ' envs stack created'))
}

