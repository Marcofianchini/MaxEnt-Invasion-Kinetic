# Function to create a spatRaster of mean values for specified variables and years
createMeanSpatRaster <- function(base_path, variables, start_year, end_year) {
  # Initialize an empty list to hold the mean rasters
  mean_rasters <- list()
  
  # Loop through each variable
  for (varname in variables) {
    # Generate the pattern to match files for the current variable and years 
    files <- paste0(base_path, varname, "_", c(start_year:end_year), ".asc")
    
    # Load these files as a SpatRaster
    rasters <- rast(files)
    
    # Calculate the mean across all years for the current variable
    mean_raster <- mean(rasters)
    
    # Add the mean raster to the list
    mean_rasters[[varname]] <- mean_raster
  }
  
  # Combine the mean rasters into a single SpatRaster
  combined_mean_raster <- rast(mean_rasters)
  
  return(combined_mean_raster)
}
