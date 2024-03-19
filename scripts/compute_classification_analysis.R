################################
# helper functions
source(paste0(wd,'scripts/','improved_Stationarity_function.R'))
file.edit(paste0(wd,'scripts/','improved_Stationarity_function.R'))

####################################



rf_classified<-best_yearly_prediction_maps
rf_classified_base<-c(scenario_predicted_map$wQTI_present_scenario,scenario_predicted_map$wQTI_future_scenario)
th<-maxSSS
fnm<-rev(c('high','medium','low','no'))
thr<-round(th,2)
int_mx<-matrix(c(0,th/3,0,
                 th/3,2*th/3,1,
                 2*th/3,th,2,
                 th,1,3),
               ncol=3, byrow=T)

rf_classified_base<- as.factor(terra::classify(rf_classified_base,int_mx, include.lowest = T, right = T))
rf_classified<- as.factor(terra::classify(rf_classified,int_mx, include.lowest = T, right = T))
level_names <- c("no suitability", "low suitability", "medium suitability", "high suitability")

# Iterate through each layer
for(i in 1:nlyr(rf_classified_base)) {
  
  # Get the current layer
  layer <- rf_classified_base[[i]]
  
  # Rename the levels
  levels(layer) <- data.frame(ID=0:3,label=level_names)
  
  # Replace the original layer with the renamed one
  rf_classified_base[[i]] <- layer
}

for(i in 1:nlyr(rf_classified)) {
  
  # Get the current layer
  layer <- rf_classified[[i]]
  
  # Rename the levels
  levels(layer) <- data.frame(ID=0:3,label=level_names)
  
  # Replace the original layer with the renamed one
  rf_classified[[i]] <- layer
}
names(rf_classified_base)<-c('present','future')
names(rf_classified)<-seq(2000,2050,1)

hs_area<- calculate_area(rf_classified,3)/10^6
ms_area<- calculate_area(rf_classified,2)/10^6
ls_area<- calculate_area(rf_classified,1)/10^6
no_area<- calculate_area(rf_classified,0)/10^6

annual_areas<-data.frame(Year=yrs,
                  'High_suitability'= hs_area,
                  'Medium_suitability'=ms_area,
                  'Low_suitability'=ls_area,
                  'No_suitability'=no_area)

# Assuming your data frame is named 'df'
long_df <- annual_areas %>%
  gather(key = "Level", value = "Area_km2", -Year) %>%
  mutate(Level = recode(
    Level,
    High_suitability = "high suitability",
    Medium_suitability = "medium suitability",
    Low_suitability = "low suitability",
    No_suitability = "no suitability"
  ))

# Calculate the percent change
long_df <- long_df %>%
  group_by(Level) %>%
  arrange(Year) %>%
  mutate(Percent_Change = (Area_km2 - lag(Area_km2)) / lag(Area_km2) * 100)

# Group the data by 'Level'
grouped_df <- long_df %>%
  group_by(Level)


suitability_cols<-rev(RColorBrewer::brewer.pal(4,'RdYlGn'))
custom_colors <- c("high suitability" = suitability_cols[4], "medium suitability" = suitability_cols[3], "low suitability" = suitability_cols[2], "no suitability" = suitability_cols[1])



hs_area_base<- calculate_area(rf_classified_base,3)/10^6
ms_area_base<- calculate_area(rf_classified_base,2)/10^6
ls_area_base<- calculate_area(rf_classified_base,1)/10^6
no_area_base<- calculate_area(rf_classified_base,0)/10^6

pf<-list("high suitability" =hs_area_base,
         "medium suitability"=ms_area_base,
         "low suitability"=ls_area_base,
         "no suitability"=no_area_base)
pf.df <- as.data.frame(pf)
#compute difference and percentage change between present and future by level
pf.df[3,]<-pf.df[2,]-pf.df[1,]
pf.df[4,]<-pf.df[3,]/pf.df[1,]*100
rownames(pf.df)<-c('present','future','difference','percentage_change')
level_names <- c("no suitability", "low suitability", "medium suitability", "high suitability")
#compute difference and percentage change between present and future from yearly values in grouped_df for the periods 2000-2020, 2030-2050
grouped_df$Scenario <- ifelse(grouped_df$Year < 2021, "present", ifelse(grouped_df$Year > 2029, "future", NA))

# Compute Area mean and standard deviation for each level in each scenario
present_mean <- grouped_df %>%
  filter(Scenario == "present") %>%
  group_by(Level) %>%
  summarize(mean_Area_km2 = mean(Area_km2, na.rm=T),
            sd = sd(Area_km2,na.rm=T))

future_mean <- grouped_df %>% 
  filter(Scenario == "future") %>%
  group_by(Level) %>%
  summarize(mean_Area_km2 = mean(Area_km2,na.rm=T),
            sd = sd(Area_km2,na.rm=T))

present_mean
future_mean

# compute percent change between present and future mean area 

percent_change <- present_mean %>%
  left_join(future_mean, by = "Level") %>%
  mutate(percent_change = (mean_Area_km2.y - mean_Area_km2.x) / mean_Area_km2.x * 100)


################################################################################################################
present<- raster(rf_classified_base$present)
future<- raster(rf_classified_base$future)
################################################################################################################
################################################################################################################
################################################################################################################
# Create transition raster
transition <- present * 10 + future

# Reclassify matrix
reclass_matrix <- matrix(c(
  30, 3,
  31, 2,
  32, 1,
  33, 0,
  23, -1,
  13, -2,
  3, -3,
  0,  NA,  # the transitions we are not interested in
  1,  NA,
  2,  NA,
  10,  NA,
  11,  NA,
  12,  NA,
  20, NA,
  21, NA,
  22, NA
), ncol=2, byrow=TRUE)

# Reclassify transition raster
classified <- reclassify(transition, reclass_matrix)

# Convert to factor raster and assign levels
classified <- as.factor(rast(classified))
levels(classified) <- data.frame(ID=-3:3, label=c(
  'NS to HS',
  'LS to HS',
  'MS to HS',
  'HS unchanged',
  'HS to MS',
  'HS to LS',
  'HS to NS')
)
# Define a consistent color palette
full_colors <- setNames(rev(c("green", "lightgreen", "yellow", "purple", "orange", "red", "darkred")), -3:3)

available_classes <- sort(unique(values(classified, na.rm=T)))
# Map these classes to the predefined color palette
current_colors <- full_colors[as.character(available_classes)]

op <- par(no.readonly = TRUE)

# Adjust margins


# Plotting
png(filename = paste0(wd,'plots/', 'transition_map.png'), width = 1600, height = 1200)
# Plot classified raster
plot(classified,
     background= grey(0.5,1), col = current_colors,
     cex.lab=3, pax=list(cex.axis=2.8), axes=T, 
     xlab='x', ylab='y',
     legend=F, plg=list(cex=0.7, bty='o'), mar = c(1,3,1,2))

# Plot sea_mask over the classified raster
plot(med_mask, colNA=grey(0.5,1), col=alpha(blues9[3],1), buffer=F, xlab='x', ylab='y', axes=F, legend=F, cex=1.8, bty='o')

# Add classified raster again on top to make sure it's not covered by sea_mask
plot(class.poly, add=T, axes = F,xlab='x', ylab='y', cex.lab=1.8, pax = list(cex.axis=1.3), legend='topright', plg=list(cex=0.7, bty='o'), col = alpha(current_colors,0.5), lwd=0.1)

plot(classified,add=T,col = current_colors, axes=F, legend = 'topright',plg=list(cex=1.7, bty='o'))
lines(class.poly, col='black', lwd=0.01) 
dev.off()
par(op)
#names(pf)<-rev(level_names)
#################################################################



for(i in 1:4){
  print(level_names[i])
  
  data <- grouped_df %>% filter(Level == level_names[i])
  # Transform the data for modeling
  data$LogArea_km2 <- log(data$Area_km2)
  
  # Fit the linear model to the log-transformed data
  model <- lm(LogArea_km2 ~ Year, data = data)
  
  # Compute the fitted values on the original scale
  data$Fitted <- exp(predict(model))
  # Compute the trend line and statistical tests
  pv <- data.frame(v = data$Area_km2, t = data$Year)
  #########
  k.fit <- Kendall::MannKendall(pv$v)
  l.fit <- lm(v ~ t, data = pv)
  trend <- coef(l.fit)
  print(k.fit)
  yp <- pf[[level_names[i]]][1]
  yf <- pf[[level_names[i]]][2]
  yp.label <- yp
  yf.label <- yf
  if(i == 4){
    x.to.dodge <-2037
    y.to.dodge <- max(pv$v) - 0.1 * max(pv$v)
    xp.label <- 2010
    xf.label <- 2040
  } else if(i == 2){
    x.to.dodge <- 2010
    y.to.dodge <- min(pv$v) + 0.2 * min(pv$v)
    xp.label <- 2010
    xf.label <- 2040
  } else if(i == 3){
    x.to.dodge <- 2010
    y.to.dodge <- min(pv$v) + 0.5 * min(pv$v)
    xp.label <- 2010
    xf.label <- 2040
    } else if(i == 1){
    x.to.dodge <- 2037
    y.to.dodge <- min(pv$v) + 1.1 * min(pv$v)
    xp.label <- 2010
    xf.label <- 2040
    yp.label <- yp -0.05 * yp
    yf.label <- yf +0.04 * yf

  }
  ########
  urca_res<-stationarity_test(pv$v, max_lag = 10)
  if (!is.na(urca_res)[1]) {
    text <- paste0(
      "adf: ", urca_res$type, "\n",
      "Lag: ", urca_res$lag, " ",
      "Tau: ", round(urca_res$tau, 2), "\n"
    )
    
    if (urca_res$type == "Trend-stationary") {
      text <- paste0(
        text,
        "Phi2: ", round(urca_res$phi2, 2), " ",
        "Phi3: ", round(urca_res$phi3, 2), "\n"
      )
    }
    text <- paste0(text,"P[H0]: <", urca_res$significance, "\n")
  } else {
    text <- "No stationarity detected"
  }

  
  # Generate a sequence of breaks
  y_range <- round(1*range(pv$v),0)
  breaks_seq <- seq(from = y_range[1], to = y_range[2], length.out = 5)
  # Create the plot
  p <- ggplot(data, aes(x = Year, y = Area_km2, color = Level)) +
    scale_y_continuous(labels=function(x) scales::scientific(x,digits=2), breaks = breaks_seq) +
    geom_line(aes(y = Fitted), color = custom_colors[level_names[i]],linewidth=2, linetype=2) +
    geom_line(aes(group = Level),color='darkgrey',size=0.4, linetype=5) +  # Connect points with lines
    #geom_text(aes(label = c('',paste0(round(Percent_Change, 2), "%")[2:length(paste0(round(Percent_Change, 2), "%"))])), vjust = -1, nudge_y = 0.1, check_overlap = T, size=1.4, bg='white') + # Add annotation
    scale_color_manual(values = 'black') + # Set custom colors
    labs(title = NULL,
         x = "Year",
         y = "Area (km^2)") +
    theme_minimal() +
    #geom_smooth(method = 'loess', se = FALSE, color = 'red', linetype=2,size=1) +
    #stat_smooth(method = "lm", formula = Area_km2 ~ exp(Year), se = FALSE,color = custom_colors[i]) +
    geom_segment(aes(x = 2000, y = yp, xend = 2020, yend = yp), 
                 color = alpha("darkblue",0.55), size = 7, linetype=1) +
    geom_segment(aes(x = 2030, y = yf, xend = 2050, yend = yf), 
                 color = alpha("darkred",0.55),size = 7, linetype=1) +
    geom_label(label.padding = unit(0.15,'lines'), size = 8,fontface=2,aes(x = xp.label, y = yp.label), 
               color = "darkblue", label = 'present',bg = 'white') +
    geom_label(label.padding = unit(0.15,'lines'), size = 8, fontface = 2 ,aes(x = xf.label, y = yf.label), 
               color = "darkred",label = 'future',bg = 'white') +
    geom_label(label.size=0,color='black',fill = alpha('grey99',1),position =  'dodge2',aes(x = x.to.dodge , y = y.to.dodge, 
                                                     label = paste0(text,paste0("Kendall ", "tau:", round(k.fit$tau, 2), " p-val:", formatC(k.fit$sl, digits = 2, format = 'e')))), size = 13) +
    #geom_text(color='black',position =  'dodge2',aes(x = 2010, y = min(pv$v) - 0.01 * min(pv$v),
    #                                                 label =  +
    geom_label(color = custom_colors[level_names[i]],fill='white',label.size = 0,position =  'dodge2',aes(x = 2008, y = max(pv$v) + 0.06 * max(pv$v),
                                                                             label = level_names[i]), size = 18) +
    geom_point(size = 10) +  # Plot points
    theme(legend.position = "none",
          axis.text.x = element_text( size = 40, face = 'bold'),
          axis.text.y = element_text(size = 40, face = "bold"), 
          axis.text = element_text(size = 30, face = "bold"), # Bigger, bold axis text
          axis.title = element_text(size = 48,face = "bold"), # Bold axis titles
          ) 
  

  png(paste0(wd,'plots/',fnm[i],'_plot.png'), width = 1600, height = 1200)
  print(p)
  dev.off()
}

