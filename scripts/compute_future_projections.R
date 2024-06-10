dir.create(paste0('rasters/','selected_model_predictions/'))
# Define the time span
yrs <- 2000:2050

# Define color palette 
pal <- viridis::magma(100)

envs.final <- rast(paste0('rasters/',expname,'_','envs.tif'))
envs.final$biozone<-as.numeric(envs.final$biozone)
envs.final$substrate<-as.numeric(envs.final$substrate)
vnm <- names(envs.final)
vnm <- vnm[!vnm %in% c('biozone','substrate')]

# Select the best model
bestmod <- readRDS(paste0('results/',expname,'_','bestmod.rds'))
bestopt <- readRDS(paste0('results/',expname,'_','bestopt.rds'))
# Initialize an empty raster stack for storing yearly prediction maps
best_yearly_prediction_maps <- rast()
# Predict distributions for each year using the selected model
for(y in yrs) {
  print(y)
  yr_pred <- rast(paste0('rasters/','envs_', as.character(y), '.tif'))
  print('envs loaded')
  yr_pred <- dismo::predict(bestmod, yr_pred, args = "outputformat=cloglog")
  best_yearly_prediction_maps[[paste0('p_', as.character(y))]] <- yr_pred
}
writeRaster(best_yearly_prediction_maps, filename = paste0('rasters/selected_model_predictions/', 'p_annual', '.tif'), overwrite = TRUE)
print('Yearly predictions ready')

# Predict distributions for the specific future scenario
scenario_predicted_map <- rast()
fls <- paste0( 'rasters/', 'future3050_raster', '.tif')
yr_pred <- rast(fls)
print('Environmental variables for scenario loaded')
yr_pred <- dismo::predict(bestmod, yr_pred, args = "outputformat=cloglog")
pres_pred <- dismo::predict(bestmod, envs.final, args = "outputformat=cloglog")
scenario_predicted_map[['p_present_scenario']] <- pres_pred
scenario_predicted_map[['p_future_scenario']] <- yr_pred
writeRaster(scenario_predicted_map, filename = paste0('rasters/selected_model_predictions/', 'p_scenario', '.tif'), overwrite = TRUE)

# Plot the mean predictions for different periods and scenarios
par(mfrow = c(2, 2))
plot(mean(best_yearly_prediction_maps[[1:21]]), main = 'Mean 2000-2020', col = rev(pal))
plot(scenario_predicted_map[['p_present_scenario']], main = 'Present Scenario', col = rev(pal))
plot(mean(best_yearly_prediction_maps[[31:50]]), main = 'Mean 2030-2050', col = rev(pal))
plot(scenario_predicted_map[['p_future_scenario']], main = 'Future Scenario', col = rev(pal))
par(mfrow = c(1, 1))
