  
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
  # Our degrees of freedom is the number of variables
  threshold_chi_squared <- qchisq(1 - significance_levels, df= length(names(envs.final)))
  # classify mop map using the thresholds 
  r_combination <- raster::cut(raster(r), breaks = c(0,threshold_chi_squared[1:9],Inf), include.lowest = TRUE)
  # create color ramp
  class_colors<-c(viridisLite::viridis(9),'red')
  
  rast_poly <- as.polygons(rast(r_combination))
  #class_colors <- c(brewer.pal(length(unique(threshold_chi_squared)),'RdYlBu')[2:(length(unique(threshold_chi_squared)))])
  class_colors<-c(viridisLite::viridis(9),'red')
  class_colors.n <- setNames(class_colors, nm = c('0',rep('',7),'1','Strict\nExtr.'))
  # Load and prepare some data
  med_mask<-rgdal::readOGR(paste0(wd,'shapefiles/',"med_mask.shp"))
  med_mask<-terra::project(vect(med_mask),eckertIV)
  library(tidyterra)
  
  png(paste0(wd,'plots/',expname,'_','mop.png'), width = 1600, height = 900)
  p1 <- ggplot(rast_poly, aes(fill = factor(layer))) +
    geom_spatvector(data = med_mask, color = 'black', fill = blues9[3], linewidth = 1) +
    geom_spatvector(data = rast_poly, aes(fill = factor(layer)), color = NA) +
    scale_fill_manual(values = class_colors, labels = names(class_colors.n)) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = grey(0.5, 1)),
          axis.text = element_text(size = 35),
          axis.title = element_text(size = 38,face = 'bold', vjust = -0.5),
          legend.position = c(1, 1), # Adjust as needed
          # make the x grid thinner
          #panel.grid = element_line(size = 0.02, linetype = 'solid', colour = "green"),
          panel.grid.major = element_line(size = 0.00001, linetype = 'solid', colour = "gray70"),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.direction = "vertical",
          legend.key = element_rect(size = 1),
          legend.key.size = unit(3.5, "lines"),
          legend.text = element_text(size = 26,face = "bold.italic"),
          legend.title = element_text(size = 34, face = 'bold'),
          legend.background = element_rect(fill = "white", colour = 'black')) +
    labs(x = "X", y = "Y") +
    guides(fill = guide_legend(title = "Environmental Distance",
                               override.aes = list(shape = 15), 
                               label.position = "bottom", 
                               title.position = "top", 
                               nrow = 1))
  
  print(p1)
  dev.off()
  # Save the plot
  #ggsave(filename = paste0(wd,'plots/','mop.png'), plot = p1, width = 16, height = 12, units = "in")

  strict_area <-calculate_area(rast(r_combination), 10)/10^6
  nonstrict_area <- calculate_area(rast(r_combination), 1:9)/10^6

  strict_area_percent <- strict_area*100/nonstrict_area
  