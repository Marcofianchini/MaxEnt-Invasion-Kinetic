createPlotPCA <- function(df.base, df2, df3, df4, file_name) {
  
  # Remove NA/Inf values
  df <- df.base
  df2 <- na.omit(df2)
  df3 <- na.omit(df3)
  df4 <- na.omit(df4)
  
  # Perform PCA on df.base
  pca <- prcomp(df, scale. = T)
  pca.summary <- summary(pca)
  axis1.variance<-round(pca.summary$importance[2,1]*100,2)
  axis2.variance<-round(pca.summary$importance[2,2]*100,2)
  # Project the other data frames onto the PCA space of df.base
  df1.pca <- predict(pca, newdata = df)
  df2.pca <- predict(pca, newdata = df2)
  df3.pca <- predict(pca, newdata = df3)
  df4.pca <- predict(pca, newdata = df4)
  
  # Extract the first two principal components
  df$pca1 <- df1.pca[,1]
  df$pca2 <- df1.pca[,2]
  df2$pca1 <- df2.pca[,1]
  df2$pca2 <- df2.pca[,2]
  df3$pca1 <- df3.pca[,1]
  df3$pca2 <- df3.pca[,2]
  df4$pca1 <- df4.pca[,1]
  df4$pca2 <- df4.pca[,2]
  # Combine the PCA data frames
  df.pca <- bind_rows(
    cbind(df, dataset = 'df'),
    cbind(df2, dataset = 'df2'),
    cbind(df3, dataset = 'df3'),
    cbind(df4, dataset = 'df4')
  )
  df.pca$ID<-NULL
  
  # Define color
  df.pca <- df.pca %>%
    mutate(col = case_when(
      dataset == 'df' ~ "grey",
      dataset == 'df2' ~ "darkgrey",
      dataset == 'df3' ~ "orange",
      dataset == 'df4' ~ "red"
    ))
  
  df.pca <- na.omit(df.pca)
  
  png(file_name, width = 800, height = 600)
  # Create the plot
  par(mar=c(6,6,6,6), mgp = c(4, 1.5, 0))
  # add pca axis variance explained to x and y labs 
  plot(df.pca$pca1[df.pca$dataset == 'df'], df.pca$pca2[df.pca$dataset == 'df'],
       pch = 21, cex = 1, col = alpha('grey20',0.05),bg = adjustcolor(grey(0.7,1), alpha.f = 0.3),
       xlab = paste0('PC1','(',axis1.variance,'%)'), 
       ylab = paste0('PC2','(', axis2.variance,'%)'), cex.lab = 2, font.lab = 2,
       main = NULL, xlim = c(-5,6), ylim = c(-8,3), cex.axis= 2.5)
  
  points(df.pca$pca1[df.pca$dataset == 'df2'], df.pca$pca2[df.pca$dataset == 'df2'],
         pch = 21, cex = 0.9, col = alpha('grey10',0.15),bg = adjustcolor(blues9[4], alpha.f = 0.5))
  
  points(df.pca$pca1[df.pca$dataset == 'df3'], df.pca$pca2[df.pca$dataset == 'df3'],
         pch = 21, cex = 1.3, col = alpha('grey5',0.3),bg = adjustcolor("orange",alpha.f = 0.45))
  
  points(df.pca$pca1[df.pca$dataset == 'df4'], df.pca$pca2[df.pca$dataset == 'df4'],
         pch = 21, cex = 1.5, col = 'black', bg = adjustcolor("red", alpha.f = 0.7))
  
  # Add legend
  legend("bottomright", inset = c(0, 0), legend = c("Full environmental space", "Calibration space" ,"Training occurrences", "RCMED test occurrences"),
         col = c('darkgrey',blues9[4] ,"orange", "red"), pch = c(19,19, 16, 16), cex = 1.4,
         title = "Dataset", box.lty = 1, xjust = 1, yjust = 1, title.cex = 1.7, title.font = 2 )
  dev.off()
}


# remove EUNIS_biozone and EUNIS_substrate
fut.cond <- rast(paste0('rasters/',"future3050_raster.tif"))
vnm <- names(envs.final)
vnm<- vnm[!vnm %in% c('biozone','substrate')]
df.base <- rbind(as.data.frame(envs.final[[vnm]]),as.data.frame(fut.cond[[vnm]]))
df2 <- m_in <- as.data.frame(rast(paste0('rasters/',expname,'_','calibration_space','.tif'))[[vnm]]) 
df3 <- terra::extract(envs.final[[vnm]],occs, ID=F)
df4 <- read.csv(paste0('data/',expname,'_',"rcmed_pres_envs.csv"))[,vnm]

file_name <- paste0('plots/',expname,'_', 'PCA_space_plot.png')
createPlotPCA(df.base, df2, df3, df4, file_name)
