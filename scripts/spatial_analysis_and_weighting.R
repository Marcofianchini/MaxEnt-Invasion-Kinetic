############################################################################################################
# this script perform the spatial autocorrelation analysis and the spatial blocking using blockCV package.
# The script also computes the distance-based weights for the presence and absence points to compute weighted metrics.
# 
# NOTE: due to package updates following the (unlucky) recent dismission of some key spatial packages (https://r-spatial.org/r/2022/04/12/evolution.html) during the analysis,
# folds (and then weights) may differ from the ones original analysis. I prefer to provide the updated code that is meant to be stable for a longer time,
# to promote its future use. Different partitioning doesn't affect the results of the original paper, anyway.
############################################################################################################
# helper function
adjust_weights <- function(weights, desired_ratio) {
  if (desired_ratio <= 1) {
    stop("Desired ratio must be greater than 1")
  }
  
  # Normalize weights to range from 0 to 1
  min_weight <- min(weights)
  max_weight <- max(weights)
  scaled_weights <- (weights - min_weight) / (max_weight - min_weight)
  
  # Adjust weights to have the desired max/min ratio
  adjusted_weights <- scaled_weights * (desired_ratio - 1) + 1
  
  # Normalize the weights to sum to 1
  final_weights <- adjusted_weights / sum(adjusted_weights)
  
  return(final_weights)
}
#
############################################################################################################
# COMPUTE FOR RCMED POINTS 
# Reading all the rcmed points data to find an appropriate distance band (darange)
rcmed.points <- read.csv(paste0( 'data/',expname,'_', 'rcmed_abundance.csv'))
rcmed.points.sp<-vect(rcmed.points, geom = c("x", "y"), crs = eckertIV)
if(file.exists(paste0('results/',expname,'_', 'sar.rds'))){
  sar <- readRDS(paste0('results/',expname,'_', 'sar.rds'))
} else {
  sar <- blockCV::cv_spatial_autocor(r = envs.final[[vnm]], num_sample = Inf)
  saveRDS(sar, paste0('results/',expname,'_', 'sar.rds'))
  
}

darange <- sar$range*1.5
# Computing the distance-based weights for the presence and absence points on the flatten data 
rcmed.points <- read.csv(paste0( 'data/',expname,'_', 'rcmed_abundance_compact.csv'))
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
test.p.weight <- enmSdmX::weightByDist(rcmed.pres_sf, maxDist = darange, alpha = alpha_pres)*(test.p.w$measurementValue/max(test.p.w$measurementValue))
test.p.weight.n <- test.p.weight/sum(test.p.weight)
#scale the values to ensure that the sum of the weights is 1 and the max:min ratio is 5 
#test.p.weight.n <- adjust_weights(test.p.weight, 10)

test.a.w <- as.data.frame(terra::extract(envs.final, rcmed.abs_sf, bind=T),geom='XY')[,c('x','y','measurementValue')]
test.a.weight <- computeBackgroundWeights(rcmed.pres_sf,rcmed.abs_sf, maxDist = darange, alpha = alpha_pres)
test.a.weight.n <- test.a.weight/sum(test.a.weight)
#scale the values to ensure that the sum of the weights is 1 and the max:min ratio is 5 
#test.a.weight.n <- adjust_weights(test.a.weight, 10)

distances_to_nearest <- sapply(1:nrow(rcmed.abs_sf), function(i) {
  min(terra::distance(rcmed.pres_sf, rcmed.abs_sf[i, ]))
})

presence_count_within_maxDist <- sapply(1:nrow(rcmed.abs_sf), function(i) {
  sum(terra::distance(rcmed.pres_sf, rcmed.abs_sf[i, ]) < darange)
})


p<-ggplot(data.frame(distance = distances_to_nearest, weight = test.a.weight.n, presence_count = presence_count_within_maxDist), 
          aes(x = distance, y = weight, fill = presence_count)) +
  geom_point(size = 6, shape = 21) +
  geom_vline(xintercept = darange, linetype = 'dashed') +
  scale_alpha(test.a.weight.n,range = c(0.5, 0.95)) +
  scale_size(test.a.weight.n,range = c(1, 0.7)) +
  scale_fill_viridis_c(name = "PP within SAC range", option = "D") +
  labs(title = "Absence Weights vs. Distance", 
       x = "Distance to Nearest Presence Point", 
       y = "Weight") +
  theme_bw() +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = rel(2.1), face = 'bold'),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.title.y = element_blank(),
        axis.title.y = element_text(face = "bold", size = rel(1.1)),
        axis.text.y = element_text(size = rel(2.2), face = 'bold'),
        axis.title.x = element_text(size = rel(1.1),vjust = 1),
        title = element_text(face = "bold", size = rel(1.9)),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = rel(1.4), color = grey(0.2,1)),
        legend.title = element_text(face = "bold", size = rel(1.1)),
        legend.text.align = 0) +
  xlim(0,darange*1.2) +
  guides(color = guide_colourbar(barwidth = 400,
                                 label.position = "bottom", 
                                 title.position = "left",
                                 title.hjust = 0.5, label.hjust = 0.5,
                                 label.theme = element_text(size = 18, face = 'bold'))) # Customize colorbar


png(paste0('plots/',expname ,'background_weights_vs_distance.png'))
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
  sum(distances < darange) - 1 # Subtract 1 to exclude the point itself
})

p <- ggplot(data.frame(distance = distances_to_nearest_presence, weight = test.p.weight.n, presence_count = presence_count_within_maxDist_presence), 
            aes(x = distance, y = weight, fill = test.p.w$measurementValue)) +
  geom_point(size = 6, color = 'black',shape = 21) +
  geom_vline(xintercept = darange, linetype = 'dashed') +
  scale_alpha(test.p.weight.n,range = c(0.5, 0.95)) +
  scale_size(test.p.weight.n,range = c(1, 0.7)) +
  scale_fill_viridis_c(name = "Abundance value", option = "D") +
  labs(title = "Presence Weights vs. Distance", 
       x = "Distance to Nearest Presence Point", 
       y = "Weight") +
  theme_bw() +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = rel(2.1), face = 'bold'),
        #axis.text.x = element_blank(),
        #axis.title.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #axis.title.y = element_blank(),
        axis.title.y = element_text(face = "bold", size = rel(1.1)),
        axis.text.y = element_text(size = rel(2.2), face = 'bold'),
        axis.title.x = element_text(size = rel(1.1),vjust = 1),
        title = element_text(face = "bold", size = rel(1.9)),
        legend.position = "bottom",
        legend.text = element_text(face = "bold", size = rel(1.4), color = grey(0.2,1)),
        legend.title = element_text(face = "bold", size = rel(1.1)),
        legend.text.align = 0) +
  xlim(0,darange*1.2) +
  guides(color = guide_colourbar(barwidth = 400,
                                 label.position = "bottom", 
                                 title.position = "left",
                                 title.hjust = 0.5, label.hjust = 0.5,
                                 label.theme = element_text(size = 18, face = 'bold'))) # Customize colorbar


png(paste0('plots/',expname,'presence_weights_vs_distance.png'))
print(p)
dev.off()

#
write.csv(test.p.weight.n, paste0( 'data/',expname,'_', 'rcmed_pres_weights.csv'), row.names = F)
write.csv(test.a.weight.n, paste0( 'data/',expname,'_', 'rcmed_abs_weights.csv'), row.names = F)
#write the same file in temp, without expname
write.csv(test.p.weight.n, paste0( 'temp/', 'rcmed_pres_weights.csv'), row.names = F)
write.csv(test.a.weight.n, paste0( 'temp/', 'rcmed_abs_weights.csv'), row.names = F)
###############################################################################################
# COMPUTE FOR TRAINING POINTS 
occs <- read.csv(paste0('data/',expname,'_', 'occurrences_counts.csv'))

bg <- read.csv(paste0( 'data/',expname,'_', 'background.csv'))
# Combining occurrence and background points with a presence/absence flag
occs.bg <- rbind(data.frame(occs[,c('x','y')], pres = 1), data.frame(bg, pres = 0))

# Converting the combined data frame to an sf object with specified coordinates and CRS
occs.bg.sf <- sf::st_as_sf(occs.bg, coords = c("x", "y"), crs = sp::CRS(eckertIV))


darange <- sar$range*1.5
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

# weight occurrences according to their spatial distribution
for(i in 1:kfolds){
  weight <-enmSdmX::weightByDist(occs.spt[occs.spt$k==i], maxDist = darange, alpha =  1)
  weight.n <- weight/sum(weight)
  cv_distsw[[i]]<- weight.n
}
cv_distsbg<-list()
for(i in 1:kfolds){
  weight <-enmSdmX::weightByDist(bg.spt[bg.spt$k==i], maxDist = darange, alpha =  1)
  weight.n <- weight/sum(weight)
  cv_distsbg[[i]]<- weight.n
}

occ_k<-data.frame(occs,k=occs.grp)
bg_k<-data.frame(bg,k=bg.grp)
occ_k_weights<-list()
bg_k_weights<-list()
for(i in 1:kfolds){
  occ_k_weights[[i]]<-data.frame(occ_k[occ_k$k==i,],weight=cv_distsw[[i]])
  bg_k_weights[[i]]<-data.frame(bg_k[bg_k$k==i,],weight=cv_distsbg[[i]])
}

occ_k_weights<-do.call('rbind',occ_k_weights)
bg_k_weights<-do.call('rbind',bg_k_weights)

final_occs <- occ_k_weights[,c('x','y','k','weight')]
final_bg <- bg_k_weights[,c('x','y','k','weight')]

# Prepare the data for model tuning and evaluating 
write.csv(final_occs, paste0( 'data/',expname,'_', 'final_occs.csv'), row.names = F)
write.csv(final_bg, paste0( 'data/',expname,'_', 'final_bg.csv'), row.names = F)
write.csv(final_occs, paste0( 'temp/', 'final_occs.csv'), row.names = F)
write.csv(final_bg, paste0( 'temp/', 'final_bg.csv'), row.names = F)

