# Define the time span
yrs <- 2000:2050

# Define color palette 
pal <- viridis::magma(100)

bathymetry <- terra::rast(paste0(wd, 'rasters/', "coast_220m.tif"))
bathymetry <- project(bathymetry, eckertIV)
envs.final <- rast(paste0(wd,'rasters/',expname,'_','envs.tif'))
envs.final$biozone<-as.numeric(envs.final$biozone)
envs.final$substrate<-as.numeric(envs.final$substrate)
vnm <- names(envs.final)
vnm <- vnm[!vnm %in% c('biozone','substrate')]

# Select the best model
bestopt <- e.mx@results %>% filter(or.10p.avg == min(or.10p.avg, na.rm = T)) %>% first()
bestmod <- e.mx@models[[bestopt$tune.args]]

# Initialize an empty raster stack for storing yearly prediction maps
best_yearly_prediction_maps <- rast()
source(paste0(wd,'scripts/','create_mean_spatraster.R'))
# Predict distributions for each year using the selected model
for(y in yrs) {
  print(y)
  yr_pred <- createMeanSpatRaster(paste0(wd,'rasters/predictors/'),vnm, y,y)
  yr_pred <- project(yr_pred, eckertIV)
  yr_pred <- mask(yr_pred, bathymetry)
  yr_pred$biozone <- envs.final$biozone
  yr_pred$substrate <- envs.final$substrate
  names(yr_pred) <- names(envs.final)
  yr_pred <- mask(yr_pred, envs.final[[1]])
  print('envs loaded')
  yr_pred <- dismo::predict(bestmod, yr_pred, args = "outputformat=cloglog")
  best_yearly_prediction_maps[[paste0('OR10p_', as.character(y))]] <- yr_pred
}
print('Yearly predictions ready')

# Predict distributions for the specific future scenario
scenario_predicted_map <- rast()
fls <- paste0(wd, 'rasters/',expname,'_', 'future3050_raster', '.tif')
yr_pred <- rast(fls)
print('Environmental variables for scenario loaded')
yr_pred <- dismo::predict(bestmod, yr_pred, args = "outputformat=cloglog")
pres_pred <- dismo::predict(bestmod, envs.final, args = "outputformat=cloglog")
scenario_predicted_map[['OR10p_present_scenario']] <- pres_pred
scenario_predicted_map[['OR10p_future_scenario']] <- yr_pred

# Plot the mean predictions for different periods and scenarios
par(mfrow = c(2, 2))
plot(mean(best_yearly_prediction_maps[[1:21]]), main = 'Mean 2000-2020', col = rev(pal))
plot(scenario_predicted_map[['OR10p_present_scenario']], main = 'Present Scenario', col = rev(pal))
plot(mean(best_yearly_prediction_maps[[31:50]]), main = 'Mean 2030-2050', col = rev(pal))
plot(scenario_predicted_map[['OR10p_future_scenario']], main = 'Future Scenario', col = rev(pal))
