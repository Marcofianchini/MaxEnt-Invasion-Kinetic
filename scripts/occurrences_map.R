pal <- c('1' =  "#009E73", '2' = "#E69F00", '3' = "#D55E00")


# Read data
occs.p <- read.csv(paste0(wd,'data/',expname,'_', 'occurrences_counts.csv'))
rcmed.p <- read.csv(paste0(wd,'data/',expname,'_', 'rcmed_abundance_compact.csv')) 


# Adjust color assignment for rcmed.p based on measurementValue
occs.p$col <- 1
rcmed.p$col <- ifelse(rcmed.p$measurementValue>0,2,3)



# Plotting
r<- rast(envs.final[[1]], vals = 1)
r<-mask(r,envs.final[[1]])
r.v <- as.polygons(r, crs = eckertIV, dissolve = TRUE)

png(paste0(wd,'plots/', 'point_map.png'), width = 1600,height=900)
p3 <- ggplot(r) +
  geom_spatvector(data = med_mask, color = NA, fill = blues9[3],alpha=1, linewidth = 1) +
  geom_spatvector(data = r.v, fill = blues9[6], color = 'grey20', linewidth=1.1) +
  geom_spatvector(data = med_msfd, fill = NA, color = 'grey10', linewidth = 1.5) +
  geom_point(data = occs.p,aes (x=x,y=y, fill = as.factor(col)),color = alpha('black',0.1), size = 3, shape = 22, alpha=0.85) +
  geom_point(data = rcmed.p,aes(x=x,y=y, fill = as.factor(col)),color = 'black', size = 4.5, shape = 23,alpha=0.9) +
  theme_minimal() + 
  scale_fill_manual(values = pal,
                    labels = c('Train', 'RCMED pres', 'RCMED abs'),
                    guide = guide_legend(title = 'Occurrences',
                                         override.aes = list(size = 8,8,8))) +
  theme(panel.background = element_rect(fill = grey(0.5, 1)),
        axis.text = element_text(size = 30),
        axis.title = element_text(size = 38,face = 'bold', vjust = -0.5),
        legend.position = c(1, 1), # Adjust as needed
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.direction = "vertical",
        #legend.key = element_rect(size = 1),
        #legend.key.size = unit(4, "lines"),
        legend.text = element_text(size = 28,face = "bold.italic"),
        legend.title = element_text(size = 34, face = 'bold'),
        legend.background = element_rect(fill = "white", colour = 'black')) +
  labs(x = "X", y = "Y") 
print(p3)
dev.off()

