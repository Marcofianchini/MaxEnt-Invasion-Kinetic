print('Computing response curves')
bestmod <- readRDS(paste0('results/',expname,'_','bestmod.rds'))
bestopt <- readRDS(paste0('results/',expname,'_','bestopt.rds'))
a <- list()

for(nm in names(envs.final)){
  #if(!file.exists(paste0('results/',expname,'_','allresponses_',nm,'.RDS'))){
    for(i in 1:nrow(e.mx@results)){
      a[[names(e.mx@models)[i]]]<-dismo::response(e.mx@models[[i]],var=nm, range = 'pa',expand=0)
    }
    saveRDS(a,paste0('results/',expname,'_','allresponses_',nm,'.RDS'))
  #}
  gc()
  print(paste0(nm,' DONE!'))
}
xlims <- read.csv(paste0('data/',expname,'_','variable_ranges.csv'),)
rownames(xlims)<-names(envs.final)
xlims['mean_no3',]<-c(0,1.5)
vunit <- c('°C','°C','PSU','mmol/m^3','mg/m-2/day-1','m/s','.','.')
vunit <- setNames(vunit,names(envs.final))

#create extended names for titles from names(envs.final)
extended.names <- c('Maximum temperature','Minimum temperature','Maximum salinity','Mean phosphate concentration','Mean net primary production','Minimum Water Velocity',
                    'EUNIS biozone','EUNIS substrate')

extended.names <- setNames(extended.names,names(envs.final))
c <- 0
vnm <- names(envs.final)
vnm <- vnm[!vnm %in% c('biozone','substrate')]
for(nm in vnm){
  
  a<- readRDS(paste0('results/',expname,'_','allresponses_',nm,'.RDS'))
  bestline.OR10p<-dismo::response(e.mx@models[[bestopt$tune.args]],var=nm,range = 'pa',expand=0)
  # Each data frame should have two columns, for x and y values
  df<-lapply(a, function(x) as.data.frame(x))
  
  # Combine the dataframes together, adding an id column to identify them
  df_combined <- dplyr::bind_rows(df, .id = "id")
  
  df_summary <- df_combined %>%
    group_by(V1) %>%
    dplyr::summarise(mean_y = mean(p), sd_y = sd(p), min_y = min(p), max_y = max(p))
  
  # Generate upper and lower bounds for the shading
  df_summary <- df_summary %>%
    mutate(ymin = mean_y - sd_y, ymax = mean_y + sd_y)
  
  # Plot the data
  p <- ggplot(df_summary, aes(x = V1, y = mean_y)) +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = blues9[6], alpha = 0.6) +
    geom_line(data = df_combined, aes(x = V1, y = p, group = id), color = "grey50", alpha = 0.05, linewidth=1) + # Add individual response lines
    geom_line(size = 1.5, color='grey10',linetype=5) +   # Increase line size
    geom_line(aes(x=bestline.OR10p[,1],y=bestline.OR10p[,2]),size = 3, color='red') +   # Increase line size
    labs(
      title = paste0(extended.names[nm], ' (importance: ', round(bestmod@results[paste0(nm,'.permutation.importance'),],2),')'),    # Add a title
      x = paste0(nm,' [ ',vunit[nm],' ]'),  # Add x-axis label
      y = "Occurrence probability"   # Add y-axis label
    ) +
    #xlim(c(20.24036, 30.93629)) +  # Set x-axis limits
    xlim(as.numeric(xlims[nm,])) +  # Set x-axis limits
    ylim(c(0, 1)) +  # Set y-axis limits
    theme_minimal() +
    theme(
      axis.text.x = element_text( size = 38, face = 'bold'),
      axis.text.y = element_text(size = 38),
      axis.title = element_text(size = 37,face='bold'),
      axis.title.x = element_blank(),
      title = element_text(size = 38,face='bold')
    )
  
  png(paste0('plots/',expname,'_',nm,'.png'),width = 1600, height = 900)
  print(p)
  dev.off()
  print(paste0(nm,' plot DONE!'))
}


####################################################################
# do violin plots for categorical variables

# EUNIS_biozone variable
nm <- 'biozone'
biozone_legend <-read.csv(paste0('data/','biozone_legend.csv'))
a<- readRDS(paste0('results/',expname,'_','allresponses_',nm,'.RDS'))
bestpoint<-as.data.frame(dismo::response(e.mx@models[[bestopt$tune.args]],var=nm,range = 'pa',expand=0))
names(bestpoint)<-c('V1','p')
# Each data frame should have two columns, for x and y values
df<-lapply(a, function(x) as.data.frame(x))
  
  # Combine the dataframes together, adding an id column to identify them
  df_combined <- dplyr::bind_rows(df, .id = "id")
  # remove the levels missclassified (4,5)
  df_combined<-df_combined[!df_combined$V1 %in% c(4,5),]
  bestpoint<-bestpoint[!bestpoint$V1 %in% c(4,5),]
  
p <- ggplot(df_combined, aes(x = as.factor(V1), y = p)) +
  geom_violin(color = "black",fill = blues9[5], alpha = 0.55) + 
  geom_point(data = as.data.frame(bestpoint), aes(x = as.factor(V1), y = p), color = 'red', size = 5) + # Add the best model
  labs(
    title = paste0(extended.names[nm], ' (importance: ', round(bestmod@results[paste0(nm,'.permutation.importance'),],2),')'),    # Add a title
    #x = paste0(nm,' [ ',vunit[nm],' ]'),  # Add x-axis label
    y = "Occurrence probability"   # Add y-axis label
  ) +
  ylim(c(0,1)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text( size = 38, face = 'bold'),
    axis.text.y = element_text(size = 38),
    axis.title = element_text(size = 37,face='bold'),
    axis.title.x = element_blank(),
    title = element_text(size = 38,face='bold')
  ) + scale_x_discrete(labels = biozone_legend$category )  
  # Setting x-axis ticks to display names

  png(paste0('plots/',expname,'_',nm,'.png'),width = 1600, height = 900)
  print(p)
  
  dev.off()
###############################################
###############################################
###############################################
  nm <- 'substrate'
  substrate_legend <-read.csv(paste0('data/','substrate_legend.csv'))
  substrate_legend$category[11]<- 'Fine or Sandy mud or Muddy sand' # shorten the name to fit the plot
  substrate_legend$category[10]<- 'Coral. Platform' # shorten the name to fit the plot
  a<- readRDS(paste0('results/',expname,'_','allresponses_',nm,'.RDS'))
  bestpoint<-as.data.frame(dismo::response(e.mx@models[[bestopt$tune.args]],var=nm,range = 'pa',expand=0))
  names(bestpoint)<-c('V1','p')
  # Each data frame should have two columns, for x and y values
  df<-lapply(a, function(x) as.data.frame(x))
  
  # Combine the dataframes together, adding an id column to identify them
  df_combined <- dplyr::bind_rows(df, .id = "id")
  # keep only the relevant substrates
  df_combined<-df_combined[!df_combined$V1 %in% c(1),]
  bestpoint<-bestpoint[!bestpoint$V1 %in% c(1),]
  
  
  p <- ggplot(df_combined, aes(x = as.factor(V1), y = p)) +
    geom_violin(color = "black",fill = blues9[5], alpha = 0.55) + # Using the merged dataframe directly
    geom_point(data = as.data.frame(bestpoint), aes(x = as.factor(V1), y = p), color = 'red', size = 5) + # Add the best point
    labs(
      title = paste0(extended.names[nm], ' (importance: ', round(bestmod@results[paste0(nm,'.permutation.importance'),],2),')'),    # Add a title
      #x = paste0(nm,' [ ',vunit[nm],' ]'),  # Add x-axis label
      y = "Occurrence probability"   # Add y-axis label
    ) +
    ylim(c(0.1,1)) +
    theme_minimal() +
    theme(
      axis.text.x = element_text( size = 32, face = 'bold'),
      axis.text.y = element_text(size = 38),
      axis.title = element_text(size = 37,face='bold'),
      axis.title.x = element_blank(),
      title = element_text(size = 38,face='bold')
    ) + scale_x_discrete(labels = str_wrap(substrate_legend$category[2:length(substrate_legend$category)], width = 4 ))
  
    # Setting x-axis ticks to display names
    
    png(paste0('plots/',expname,'_',nm,'.png'),width = 1600, height = 900)
  print(p)
  
  dev.off()

print('Response Curves saved and plotted')

