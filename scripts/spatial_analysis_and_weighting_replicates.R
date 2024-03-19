############################################################################################################
# this script perform the spatial autocorrelation analysis and the spatial blocking using blockCV package.
# The script also computes the distance-based weights for the presence and absence points to compute weighted metrics.
# 
# NOTE: due to package updates following the (unlucky) recent dismission of some key spatial packages (https://r-spatial.org/r/2022/04/12/evolution.html) during the analysis,
# folds (and then weights) may differ from the ones original analysis. I prefer to provide the updated code that is meant to be stable for a longer time,
# to promote its future use. Different partitioning doesn't affect the results of the original paper, anyway.
############################################################################################################
# COMPUTE FOR RCMED POINTS 
# Reading all the rcmed points data to find an appropriate distance band (darange)
rcmed.points <- read.csv(paste0(wd, 'data/', 'rcmed_abundance.csv'))
rcmed.points.sp<-vect(rcmed.points, geom = c("x", "y"), crs = eckertIV)
sar.rcmed.points <- blockCV::cv_spatial_autocor(r = envs.final[[1:12]], x = rcmed.points.sp,column = 'measurementValue', num_sample = Inf)
rcmed.darange <- sar.rcmed.points$range
# Computing the distance-based weights for the presence and absence points on the flatten data 
rcmed.points <- read.csv(paste0(wd, 'data/', 'rcmed_abundance_compact.csv'))
rcmed.points.sp<-vect(rcmed.points, geom = c("x", "y"), crs = eckertIV)
# Separating presence and absence points
rcmed.pres <- rcmed.points[rcmed.points$measurementValue > 0, ]
rcmed.abs <- rcmed.points[rcmed.points$measurementValue == 0, ]

# Converting presence and absence data to sf objects
rcmed.pres_sf <- vect(rcmed.pres, geom = c("x", "y"), crs = eckertIV)
rcmed.abs_sf <- vect(rcmed.abs, geom = c("x", "y"), crs = eckertIV)
################################################################################################
alpha_pres <- 1
test.p.w <- as.data.frame(terra::extract(envs.final, rcmed.pres_sf, bind=T),geom='XY')[,c('x','y','measurementValue')]
test.p.weight <- enmSdmX::weightByDist(rcmed.pres_sf, maxDist = rcmed.darange, alpha = alpha_pres)*(test.p.w$measurementValue)
test.p.weight.n <- test.p.weight/sum(test.p.weight)

test.a.w <- as.data.frame(terra::extract(envs.final, rcmed.abs_sf, bind=T),geom='XY')[,c('x','y','measurementValue')]
test.a.weight <- computeBackgroundWeights(rcmed.pres_sf,rcmed.abs_sf, maxDist = rcmed.darange, alpha = alpha_pres)
test.a.weight.n <- test.a.weight/sum(test.a.weight)

distances_to_nearest <- sapply(1:nrow(rcmed.abs_sf), function(i) {
  min(terra::distance(rcmed.pres_sf, rcmed.abs_sf[i, ]))
})

presence_count_within_maxDist <- sapply(1:nrow(rcmed.abs_sf), function(i) {
  sum(terra::distance(rcmed.pres_sf, rcmed.abs_sf[i, ]) < rcmed.darange)
})


p<-ggplot(data.frame(distance = distances_to_nearest, weight = test.a.weight.n, presence_count = presence_count_within_maxDist), 
          aes(x = distance, y = weight, color = presence_count)) +
  geom_point() +
  #xlim(0, db_true+2000) +
  scale_alpha(test.a.weight.n,range = c(0.5, 0.95)) +
  scale_size(test.a.weight.n,range = c(1, 0.7)) +
  scale_color_viridis_c(name = "PP within SAC range", option = "D") +
  labs(title = "Background Weights vs. Distance", 
       x = "Distance to Nearest Presence Point", 
       y = "Weight") +
  theme_bw() +
  #make pretty plot
  theme(plot.title = element_text(hjust = 0.5, size = 21),
        legend.position = "bottom",
        legend.title = element_text(size = 12, color = grey(0.1)),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 13),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        title = element_text(face = "bold", size = rel(1.1)),
        axis.title.y = element_text(size = rel(0.9),vjust = 1),
        axis.title.x = element_text(size = rel(0.9),vjust = 1),
        axis.text.x = element_text(size = rel(1.2), vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = rel(1.2))) +
  guides(color = guide_colourbar(barwidth = rel(10),
                                 label.position = "bottom", 
                                 title.position = "left",
                                 title.hjust = 0.5, label.hjust = 0.5,
                                 label.theme = element_text(size = 9, face = 'bold'))) # Customize colorbar


png(paste0(wd,'plots/','background_weights_vs_distance.png'))
print(p)
dev.off()
# do the same plot for presences weights 
distances_to_nearest_presence <- sapply(1:nrow(rcmed.pres_sf), function(i) {
  distances <- distance(rcmed.pres_sf, rcmed.pres_sf[i, ])
  distances[i] <- Inf # Set the distance from the point to itself as Infinity
  min(distances)
})

presence_count_within_maxDist_presence <- sapply(1:nrow(rcmed.pres_sf), function(i) {
  distances <- distance(rcmed.pres_sf, rcmed.pres_sf[i, ])
  sum(distances < rcmed.darange) - 1 # Subtract 1 to exclude the point itself
})

p <- ggplot(data.frame(distance = distances_to_nearest_presence, weight = test.p.weight.n, presence_count = presence_count_within_maxDist_presence), 
            aes(x = distance, y = weight, color = test.p.w$measurementValue)) +
  geom_point() +
  #xlim(0, db_true) +
  scale_alpha(test.p.weight.n,range = c(0.5, 0.95)) +
  scale_size(test.p.weight.n,range = c(1, 0.7)) +
  scale_color_viridis_c(name = "Abundance value", option = "D") +
  labs(title = "Presence Weights vs. Distance", 
       x = "Distance to Nearest Presence Point", 
       y = "Weight") +
  theme_bw() +
  #make pretty plot
  theme(plot.title = element_text(hjust = 0.5, size = 21),
        legend.position = "bottom",
        legend.title = element_text(size = 12, color = grey(0.1)),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 17),
        axis.text = element_text(size = 13),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        title = element_text(face = "bold", size = rel(1.1)),
        axis.title.y = element_text(size = rel(0.9),vjust = 1),
        axis.title.x = element_text(size = rel(0.9),vjust = 1),
        axis.text.x = element_text(size = rel(1.2), vjust = 0.5, hjust=1),
        axis.text.y = element_text(size = rel(1.2))) +
  guides(color = guide_colourbar(barwidth = rel(10),
                                 label.position = "bottom", 
                                 title.position = "left",
                                 title.hjust = 0.5, label.hjust = 0.5,
                                 label.theme = element_text(size = 9, face = 'bold'))) # Customize colorbar


png(paste0(wd,'plots/','presence_weights_vs_distance.png'))
print(p)
dev.off()

write.csv(test.p.weight.n, paste0(wd, 'data/', 'rcmed_pres_weights.csv'), row.names = F)
write.csv(test.a.weight.n, paste0(wd, 'data/', 'rcmed_abs_weights.csv'), row.names = F)


################################################################################################
# COMPUTE FOR TRAINING POINTS 
occs <- read.csv(paste0(wd, 'data/', 'occurrences_counts.csv'))

bg <- read.csv(paste0(wd, 'data/', 'background.csv'))
# Combining occurrence and background points with a presence/absence flag
occs.bg <- rbind(data.frame(occs[,c('x','y')], pres = 1), data.frame(bg, pres = 0))

# Converting the combined data frame to an sf object with specified coordinates and CRS
occs.bg.sf <- sf::st_as_sf(occs.bg, coords = c("x", "y"), crs = sp::CRS(eckertIV))

# Determining the spatial autocorrelation range for the spatial blocking
sar.points <- blockCV::cv_spatial_autocor(r = envs.final[[1:12]], x = occs.bg.sf, column = 'pres', num_sample = Inf)
darange <- sar.points$range

occs$original_index <- 1:nrow(occs)
original_occs <- occs 
occs <- duplicate_rows(original_occs,'count')
occs.sf <- sf::st_as_sf(na.omit(data.frame(occs[,c('x','y')],presence=1)), coords = c('x','y'), crs = eckertIV)
occsBg.sf <- sf::st_as_sf(
  rbind(na.omit(data.frame(occs[,c('x','y')],pres=1)), data.frame(bg,pres=0)), 
  coords = c("x","y"),
  crs = eckertIV)
kfolds <- 5
sb <- blockCV::cv_spatial(x = occsBg.sf, r = envs.final[[1]], size = darange , k = kfolds, selection =  'random', column = 'pres', seed = 123, iteration = 1000)
occs.grp <- sb$folds_ids[1:nrow(na.omit(occs))]
bg.grp <- sb$folds_ids[(nrow(na.omit(occs))+1):length(sb$folds_ids)]
# 3. User partitions.
user.grp <- list(occs.grp = occs.grp, 
                 bg.grp = bg.grp)
occs.to.weight <- data.frame(occs, k = occs.grp)
bg.to.weight <- data.frame(bg, k = bg.grp)
# Weight by distance and abundance
bg.spt<-vect(bg.to.weight, geom = c('x','y'), eckertIV)
occs.spt<- vect(occs.to.weight, geom = c('x','y'), eckertIV)

cv_distsw<-list()
# Modify the weight calculation
for (i in 1:kfolds) {
  # Obtain unique indices of the original occurrences for the current fold
  original_indices <- unique(as.data.frame(occs.spt[occs.spt$k == i, "original_index"])[,1])
  
  # Create a subset of original_occs based on these indices
  original_subset <- subset(original_occs, original_index %in% original_indices)
  original_subset.spt <- vect(original_subset, geom = c('x', 'y'), eckertIV)
  
  # Calculate weights for the subset of original occurrences
  weights_for_original <- enmSdmX::weightByDist(original_subset.spt, maxDist = darange, alpha = alpha_pres)
  
  # Normalize weights
  normalized_weights_for_original <- weights_for_original
  
  # Create a mapping of original index to weight
  weight_mapping <- setNames(normalized_weights_for_original, original_subset$original_index)
  
  # Map these weights back to the duplicated rows in occs
  occs_fold <- occs.spt[occs.spt$k == i, ]
  occs_fold$weight <- weight_mapping[as.character(occs_fold$original_index)]
  occs_fold$weight <- occs_fold$weight/sum(occs_fold$weight)
  
  # Store the weighted occurrences in the list
  cv_distsw[[i]] <- occs_fold
}

cv_distsbg<-list()
for(i in 1:kfolds){
  #cv_distsbg[[i]]<-computeBackgroundWeights(occs.spt[occs.spt$k==i],bg.spt[bg.spt$k==i], maxDist =  distance_band, alpha = alpha_abs)
  cv_distsbg[[i]]<-enmSdmX::weightByDist(bg.spt[bg.spt$k==i], maxDist =  darange, alpha = alpha_pres)
  cv_distsbg[[i]]<- cv_distsbg[[i]]/sum(cv_distsbg[[i]])
}

occ_k<-data.frame(occs.to.weight[,c('x','y','k')])
bg_k<-data.frame(bg.to.weight[,c('x','y','k')])
occ_k_weights<-list()
bg_k_weights<-list()
for(i in 1:kfolds){
  occ_k_weights[[i]]<-data.frame(occ_k[occ_k$k==i,],weight=cv_distsw[[i]])
  bg_k_weights[[i]]<-data.frame(bg_k[bg_k$k==i,],weight=cv_distsbg[[i]])
}

occ_k_weights<-do.call('rbind',occ_k_weights)
bg_k_weights<-do.call('rbind',bg_k_weights)

user.grp <- list(occs.grp = occs.grp, 
                 bg.grp = bg.grp,
                 weights = c(occ_k_weights$weight.weight,bg_k_weights$weight)
)
final_occs <- occ_k_weights[,c('x','y','k','weight.weight')]
final_bg <- bg_k_weights[,c('x','y','k','weight')]
names(final_occs)<- c('x','y','k','weight')
names(final_bg)<- c('x','y','k','weight')



# Prepare the data for model tuning and evaluating 
write.csv(final_occs, paste0(wd, 'data/', 'final_occs.csv'), row.names = F)
write.csv(final_bg, paste0(wd, 'data/', 'final_bg.csv'), row.names = F)


rcmed.pres.envs <- terra::extract(envs.final, rcmed.pres_sf, ID=F)
rcmed.abs.envs <- terra::extract(envs.final, rcmed.abs_sf, ID=F)

write.csv(rcmed.pres.envs, paste0(wd, 'data/', 'rcmed_pres_envs.csv'), row.names = F)
write.csv(rcmed.abs.envs, paste0(wd, 'data/', 'rcmed_abs_envs.csv'), row.names = F)



