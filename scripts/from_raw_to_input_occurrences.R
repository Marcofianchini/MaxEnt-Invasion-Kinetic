# Download/Import occurrences
source(paste0('scripts/','import_occurrences.R'))

# Adjust the occurrences to remove duplicates and adjust coordinates
occs.final <- unique(occs.final)
occs.adj <- rSDM::points2nearestcell(as(occs.final,'Spatial'), raster(envs.final$biozone), distance = sqrt(2)*res(envs.final)[1], showmap = F)
occs.final <- vect(occs.adj,geom= c('x','y'),crs = eckertIV)
occs.final$presence <- 1

# keep points that are inside the raster 
occs.filtered <- terra::extract(envs.final, occs.final,bind = T, xy = TRUE)
occs.df <- data.frame(na.omit(occs.filtered)[,c('x','y')],Observed=1)
occs.final <- vect(occs.df, geom = c('x','y') , crs = eckertIV)
occs.r <- terra::rasterize(occs.final, envs.final,field='Observed',fun = 'count')
occs.r[occs.r<1] <- NA
occs.df <- as.data.frame(occs.r, xy = TRUE)
names(occs.df) <- c('x','y','count')
occs.final <- vect(occs.df, geom = c('x','y'), crs = eckertIV)
distances_to_occurrences <- terra::distance(envs.final[[1]], occs.final, unit = 'm')
distances_to_occurrences[distances_to_occurrences>200000] <- NA

envs.m <- terra::mask(envs.final, distances_to_occurrences)

# mask out distances that are too close from test points from envs.m
#rcmed.points <- read.csv('data/','rcmed_abundance.csv')
distances_to_testpoints <- terra::distance(envs.final[[1]], vect(rcmed.points, geom = c('x','y'), crs = eckertIV), unit = 'm')
distances_to_testpoints[distances_to_testpoints>20000] <- NA
envs.m <- terra::mask(envs.m, distances_to_testpoints,inverse=T)
writeRaster(envs.m, paste0('rasters/',expname,'_','calibration_space','.tif'), overwrite=T)
v.ranges <- lapply(mask(envs.final,envs.m),function(x){range(values(x),na.rm = T)} )
v.ranges <- t(as.data.frame(v.ranges))
v.ranges[,1]<- v.ranges[,1]-0.001*v.ranges[,1]
v.ranges[,2]<- v.ranges[,2]+0.001*v.ranges[,2]
rownames(v.ranges)<-names(envs.final)
write.csv(v.ranges, paste0('data/',expname,'_','variable_ranges.csv'),row.names = F)
# remove occs points that are too close to test points
occs.m <- terra::extract(envs.m, occs.final, bind=T)
occs.to.write <- na.omit(as.data.frame(occs.m, geom='XY'))[,c('x','y','count')]
# get background points
bg <- na.omit(as.data.frame(envs.m, xy = TRUE))[, 1:2]

write.csv(bg, paste0('data/',expname,'_','background.csv'), row.names = FALSE)
write.csv(occs.to.write, paste0('data/',expname,'_','occurrences_counts.csv'), row.names = FALSE)

