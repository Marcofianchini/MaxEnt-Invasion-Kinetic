vnm <- names(envs.final)
# because of the computational cost, we will only run the analysis for significative variables
important.vnm <- names(importances)[round(importances,0)>=5]
important.vnm<- important.vnm[!important.vnm %in% c('biozone','substrate')]

# load and prepare future raster 
g_in <- rast(paste0(wd,'rasters/',expname,'_','future3050_raster.tif'))
g_in <- g_in[[important.vnm]]
mmop_analysis_d<-mop(m = envs.final[[important.vnm]],
                     g = g_in,
                     type = 'detailed',
                     distance = 'mahalanobis',
                     where_distance = 'all',
                     percent = 100,
                     comp_each = 4000,
                     calculate_distance = T,
                     rescale_distance = F,
                     parallel = T,
                     n_cores = 4,
                     fix_NA = T)
r <- mmop_analysis_d$mop_distances
# Determine the decile thresholds for given significance levels

# create a series of breaks defined by different significance levels 
significance_levels <- c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0.05)
threshold_chi_squared <- qchisq(1 - significance_levels, df=length(vnm), n = length(vnm))
# create a raster to highlight strict extrapolation
r_strict <- raster::cut(raster(r), breaks = c(0,threshold_chi_squared[10],Inf), include.lowest = TRUE, ordered_result=T)

# classify mop map using the thresholds 
r_combination <- raster::cut(raster(r), breaks = c(threshold_chi_squared,Inf), include.lowest = TRUE)
# mask out the strict extrapolation
r_strict_mask <- r_strict
r_strict_mask[r_strict_mask==1]<-NA
r_range <- mask(r_combination,r_strict_mask,inverse=T)
# create color ramp
class_colors <- c(brewer.pal(length(unique(threshold_chi_squared)),'RdYlBu')[3:(length(unique(threshold_chi_squared)))])

par(mfrow=c(2,1))
plot(rast(r_range), xlab='x',ylab='y',colNA=grey(0.5,0.1),col=rev(class_colors),legend=F, main ='percentile mop class')
plot(rast(r_strict), col = c('white','black'),colNA=grey(0.5,0.1), main = 'strict extrapolation (black area)',legend = F)

par(mfrow=c(1,1))
