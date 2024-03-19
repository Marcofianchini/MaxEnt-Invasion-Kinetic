# Load and prepare some data
med_mask<-rgdal::readOGR(paste0(wd,'shapefiles/',"med_mask.shp"))
med_mask<-terra::project(vect(med_mask),eckertIV)

rast_poly <- as.polygons(present,dissolve=T, crs = eckertIV)
rast_poly$present <- factor(rast_poly$present, levels = c("high suitability", "medium suitability", "low suitability", "no suitability"))
cl.names <- cats(present)[[1]][,2]
  
class_colors <- rev(palette.colors(palette = "Okabe-Ito")[c('vermillion','yellow','bluishgreen','blue')])
class_colors.n <- setNames(class_colors,cl.names)

png(paste0(wd,'plots/','present_raster.png'), width = 1600, height = 900)
p3 <- ggplot(rast_poly, aes(fill = present)) +
  geom_spatvector(data = med_mask, color = 'black', fill = blues9[3], linewidth = 1) +
  geom_spatvector(data = rast_poly, aes(fill = present), color = NA) +
  scale_fill_manual(values = class_colors.n, labels = c('high', 'med','low','no')) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = grey(0.5, 1)),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 38,face = 'bold', vjust = -0.5),
        legend.position = c(1, 1), # Adjust as needed
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.direction = "vertical",
        legend.key = element_rect(size = 1),
        legend.key.size = unit(4, "lines"),
        legend.text = element_text(size = 28,face = "bold.italic"),
        legend.title = element_text(size = 34, face = 'bold'),
        legend.background = element_rect(fill = "white", colour = 'black')) +
  labs(x = "X", y = "Y") +
  guides(fill = guide_legend(title = "Suitability class",
                             override.aes = list(shape = 15), 
                             label.position = "bottom", 
                             title.position = "top", 
                             nrow = 1))

print(p3)
dev.off()
rast_poly <- as.polygons(present_upper,dissolve=T, crs = eckertIV)
rast_poly$present <- factor(rast_poly$present, levels = c("high suitability", "medium suitability", "low suitability", "no suitability"))
cl.names <- cats(present_upper)[[1]][,2]
class_colors <- rev(palette.colors(palette = "Okabe-Ito")[c('vermillion','yellow','bluishgreen','blue')])
class_colors.n <- setNames(class_colors,cl.names)

png(paste0(wd,'plots/','present_upper_raster.png'), width = 1600, height = 900)
p5 <- ggplot(rast_poly, aes(fill = present)) +
  geom_spatvector(data = med_mask, color = 'black', fill = blues9[3], linewidth = 1) +
  geom_spatvector(data = rast_poly, aes(fill = present), color = NA) +
  scale_fill_manual(values = class_colors.n, labels = c('high', 'med','low','no')) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = grey(0.5, 1)),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 38,face = 'bold', vjust = -0.5),
        legend.position = c(1, 1), # Adjust as needed
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.direction = "vertical",
        legend.key = element_rect(size = 1),
        legend.key.size = unit(4, "lines"),
        legend.text = element_text(size = 28,face = "bold.italic"),
        legend.title = element_text(size = 34, face = 'bold'),
        legend.background = element_rect(fill = "white", colour = 'black')) +
  labs(x = "X", y = "Y") +
  guides(fill = guide_legend(title = "Suitability class",
                             override.aes = list(shape = 15), 
                             label.position = "bottom", 
                             title.position = "top", 
                             nrow = 1))

print(p5)
dev.off()

rast_poly <- as.polygons(present_lower,dissolve=T)
rast_poly$present <- factor(rast_poly$present, levels = c("high suitability", "medium suitability", "low suitability", "no suitability"))
cl.names <- cats(present_upper)[[1]][,2]
class_colors <- rev(palette.colors(palette = "Okabe-Ito")[c('vermillion','yellow','bluishgreen','blue')])
class_colors.n <- setNames(class_colors,cl.names)


png(paste0(wd,'plots/','present_lower_raster.png'), width = 1600, height = 900)
p1 <- ggplot(rast_poly, aes(fill = present)) +
  geom_spatvector(data = med_mask, color = 'black', fill = blues9[3], linewidth = 1) +
  geom_spatvector(data = rast_poly, aes(fill = present), color = NA) +
  scale_fill_manual(values = class_colors.n, labels = c('high', 'med','low','no')) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = grey(0.5, 1)),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 38,face = 'bold', vjust = -0.5),
        legend.position = c(1, 1), # Adjust as needed
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.direction = "vertical",
        legend.key = element_rect(size = 1),
        legend.key.size = unit(4, "lines"),
        legend.text = element_text(size = 28,face = "bold.italic"),
        legend.title = element_text(size = 34, face = 'bold'),
        legend.background = element_rect(fill = "white", colour = 'black')) +
  labs(x = "X", y = "Y") +
  guides(fill = guide_legend(title = "Suitability class",
                             override.aes = list(shape = 15), 
                             label.position = "bottom", 
                             title.position = "top", 
                             nrow = 1))

print(p1)
dev.off()


rast_poly <- as.polygons(future_upper,dissolve=T)
rast_poly$future <- factor(rast_poly$future, levels = c("high suitability", "medium suitability", "low suitability", "no suitability"))
cl.names <- cats(present_upper)[[1]][,2]
class_colors <- rev(palette.colors(palette = "Okabe-Ito")[c('vermillion','yellow','bluishgreen','blue')])
class_colors.n <- setNames(class_colors,cl.names)
png(paste0(wd,'plots/','future_upper_raster.png'), width = 1600, height = 900)
p6 <- ggplot(rast_poly, aes(fill = future)) +
  geom_spatvector(data = med_mask, color = 'black', fill = blues9[3], linewidth = 1) +
  geom_spatvector(data = rast_poly, aes(fill = future), color = NA) +
  scale_fill_manual(values = class_colors.n, labels = c('high', 'med','low','no')) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = grey(0.5, 1)),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 38,face = 'bold', vjust = -0.5),
        legend.position = c(1, 1), # Adjust as needed
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.direction = "vertical",
        legend.key = element_rect(size = 1),
        legend.key.size = unit(4, "lines"),
        legend.text = element_text(size = 28,face = "bold.italic"),
        legend.title = element_text(size = 34, face = 'bold'),
        legend.background = element_rect(fill = "white", colour = 'black')) +
  labs(x = "X", y = "Y") +
  guides(fill = guide_legend(title = "Suitability class",
                             override.aes = list(shape = 15), 
                             label.position = "bottom", 
                             title.position = "top", 
                             nrow = 1))

print(p6)
dev.off()

rast_poly <- as.polygons(future_lower,dissolve=T)
rast_poly$future <- factor(rast_poly$future, levels = c("high suitability", "medium suitability", "low suitability", "no suitability"))
cl.names <- cats(present_upper)[[1]][,2]
class_colors <- rev(palette.colors(palette = "Okabe-Ito")[c('vermillion','yellow','bluishgreen','blue')])
class_colors.n <- setNames(class_colors,cl.names)
png(paste0(wd,'plots/','future_lower_raster.png'), width = 1600, height = 900)
p2 <- ggplot(rast_poly, aes(fill = future)) +
  geom_spatvector(data = med_mask, color = 'black', fill = blues9[3], linewidth = 1) +
  geom_spatvector(data = rast_poly, aes(fill = future), color = NA) +
  scale_fill_manual(values = class_colors.n, labels = c('high', 'med','low','no')) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = grey(0.5, 1)),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 38,face = 'bold', vjust = -0.5),
        legend.position = c(1, 1), # Adjust as needed
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.direction = "vertical",
        legend.key = element_rect(size = 1),
        legend.key.size = unit(4, "lines"),
        legend.text = element_text(size = 28,face = "bold.italic"),
        legend.title = element_text(size = 34, face = 'bold'),
        legend.background = element_rect(fill = "white", colour = 'black')) +
  labs(x = "X", y = "Y") +
  guides(fill = guide_legend(title = "Suitability class",
                             override.aes = list(shape = 15), 
                             label.position = "bottom", 
                             title.position = "top", 
                             nrow = 1))

print(p2)
dev.off()

rast_poly <- as.polygons(future,dissolve=T)
rast_poly$future <- factor(rast_poly$future, levels = c("high suitability", "medium suitability", "low suitability", "no suitability"))
cl.names <- cats(present_upper)[[1]][,2]
class_colors <- rev(palette.colors(palette = "Okabe-Ito")[c('vermillion','yellow','bluishgreen','blue')])
class_colors.n <- setNames(class_colors,cl.names)
png(paste0(wd,'plots/','future_raster.png'), width = 1600, height = 900)
p4 <- ggplot(rast_poly, aes(fill = future)) +
  geom_spatvector(data = med_mask, color = 'black', fill = blues9[3], linewidth = 1) +
  geom_spatvector(data = rast_poly, aes(fill = future), color = NA) +
  scale_fill_manual(values = class_colors.n, labels = c('high', 'med','low','no')) +
  theme_minimal() +
  guides(fill = guide_legend(title = "Suitability class",
                             override.aes = list(shape = 15), 
                             label.position = "bottom", 
                             title.position = "top", 
                             nrow = 1)) +
  theme(panel.background = element_rect(fill = grey(0.5, 1)),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 38,face = 'bold', vjust = -0.5),
        legend.position = c(1, 1), # Adjust as needed
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.direction = "vertical",
        legend.key = element_rect(size = 1),
        legend.key.size = unit(4, "lines"),
        legend.text = element_text(size = 28,face = "bold.italic"),
        legend.title = element_text(size = 34, face = 'bold'),
        legend.background = element_rect(fill = "white", colour = 'black'),
        # set ratio 
        
         ) +
  labs(x = "X", y = "Y") 

print(p4)
dev.off()
