rcmed.points <- read.csv(paste0(wd,'data/',expname,'_','rcmed_abundance_compact.csv'))
# Define the data from Boujadira et al. (2010, doi: 10.3391/ai.2010.5.S1.020) in an appropriate format to be transformed into decimal 
Boujadira_df <- data.frame( 
  Location = c("Tamentfoust", "Sidi-Fredj", "Bou Ismail", "Salamandre Station 1", "Station 2", "Station 3", "Station 4", "Stidia Station 1", "Station 2", "Station 3", "Station 4"),
  Latitude  = c("36 48 29.64", "36 45 55.2", "36 41 39.23", "35 55 12.46", "35 55 11.62", "35 55 08.07", "35 55 06.15", "35 50 1.45", "35 50 2.29", "35 50 3.96", "35 50 4.22"),
  Longitude = c("03 13 52.56", "02 50 50.82", "02 47 50.67", "00 03 28.32", "00 03 28.30", "00 03 27.20", "00 03 27.09", "00 00 49.79", "00 00 49.12", "00 00 48.94", "00 00 47.30")
)

Boujadira_decimalLongitude <- as.numeric(measurements::conv_unit(Boujadira_df$Longitude,from='deg_min_sec', to = "dec_deg"))
Boujadira_decimalLatitude <- as.numeric(measurements::conv_unit(Boujadira_df$Latitude,from='deg_min_sec', to = "dec_deg"))

Boujadira_points <- data.frame(Boujadira_decimalLongitude, Boujadira_decimalLatitude)

colnames(Boujadira_points)<-c("decimalLongitude","decimalLatitude")
Boujadira.pts<- vect(Boujadira_points,geom= c('decimalLongitude','decimalLatitude'),crs='EPSG:4326')
Boujadira.pts <- project(Boujadira.pts, eckertIV)
# keep coordinates and year of observation. filter years after 2020
occs.original <- read.delim(paste0(wd,'data/',"0004580-240216155721649/0004580-240216155721649.csv"))
occs.original <- occs.original %>% filter(!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(year))
occs.original <- occs.original %>% filter(year <= 2020)
occs.original <- occs.original %>% select(decimalLongitude, decimalLatitude, year)
# keep only that columns
occs.sp <- vect(occs.original,geom = c('decimalLongitude', 'decimalLatitude'),crs = 'EPSG:4326')
occs.p <- project(occs.sp, eckertIV)

# add easin points
easin.original <- vect(paste0(wd,'data/','easin-layer-export.json'))
easin.p <- project(easin.original, eckertIV)
easin.r <- mask(envs.final, easin.p)
easin.pts <- as.data.frame(easin.r, xy=T)[,c('x','y')]
easin.p <- vect(easin.pts, geom = c('x','y'), crs =eckertIV)

# 
occs.final <- rbind(occs.p,easin.p, Boujadira.pts)
occs.final <- crop(occs.final, envs.final)

# keep points that are inside the raster 
occs.filtered <- terra::extract(envs.final, occs.final, xy = TRUE)
occs.df <- data.frame(na.omit(occs.filtered)[,c('x','y')],Observed=1)
occs.final <- vect(occs.df, geom = c('x','y') , crs = eckertIV)
occs.r <- terra::rasterize(occs.final, envs.final,field='Observed',fun = 'count')
occs.r[occs.r<1] <- NA
occs.df<- as.data.frame(occs.r, xy = TRUE)
occs.final <- vect(occs.df, geom = c('x','y'), crs = eckertIV)
distances_to_occurrences <- terra::distance(envs.final[[1]], occs.final, unit = 'm')
distances_to_occurrences[distances_to_occurrences>200000] <- NA

envs.m <- terra::mask(envs.final, distances_to_occurrences)

# mask out distances that are too close from test points from envs.m
#rcmed.points <- read.csv(wd,'data/','rcmed_abundance.csv')
distances_to_testpoints <- terra::distance(envs.final[[1]], vect(rcmed.points, geom = c('x','y'), crs = eckertIV), unit = 'm')
distances_to_testpoints[distances_to_testpoints>20000] <- NA
envs.m <- terra::mask(envs.m, distances_to_testpoints,inverse=T)
v.ranges <- lapply(mask(envs.final,envs.m),function(x){range(values(x),na.rm = T)} )
v.ranges <- t(as.data.frame(v.ranges))
v.ranges[,1]<- v.ranges[,1]-0.001*v.ranges[,1]
v.ranges[,2]<- v.ranges[,2]+0.001*v.ranges[,2]
rownames(v.ranges)<-names(envs.final)
write.csv(v.ranges, paste0(wd,'data/',expname,'_','variable_ranges.csv'),row.names = F)
# remove occs points that are too close to test points
occs.m <- terra::extract(envs.m, occs.final, bind=T)
occs.to.write <- na.omit(as.data.frame(occs.m, geom='XY'))[,c('x','y','count')]
# get background points
bg <- na.omit(as.data.frame(envs.m, xy = TRUE))[, 1:2]

write.csv(bg, paste0(wd,'data/',expname,'_','background.csv'), row.names = FALSE)
write.csv(occs.to.write, paste0(wd,'data/',expname,'_','occurrences_counts.csv'), row.names = FALSE)
