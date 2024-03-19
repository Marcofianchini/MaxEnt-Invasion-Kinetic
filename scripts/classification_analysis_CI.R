# helper functions
#file.edit(paste0(wd,'scripts/','improved_Stationarity_function.R'))
source(paste0(wd,'scripts/','improved_Stationarity_function.R'))

####################################
rf_classified.original<-best_yearly_prediction_maps
rf_classified_base.original<-c(scenario_predicted_map$OR10p_present_scenario,scenario_predicted_map$OR10p_future_scenario)
fnm<-rev(c('high','medium','low','no'))
threshold <- "Maximum.training.sensitivity.plus.specificity.Cloglog.threshold"
maxSSS <- bestmod@results[threshold,]
maxSSS.values <- c(e.mx@results.partitions %>% filter(tune.args == bestopt$tune.args) %>% select(threshold_value) %>% unlist(), maxSSS)
# compute median absolute difference of maxSSS.values
maxSSS.mad <- mad(maxSSS.values, constant = 1) 
th<-maxSSS
thr <- round(th, 2)
th_lower <- th + maxSSS.mad
th_upper <- th - maxSSS.mad
int_mx<-matrix(c(0,th/3,0,
                 th/3,2*th/3,1,
                 2*th/3,th,2,
                 th,1,3),
               ncol=3, byrow=T)
int_mx_lower<-matrix(c(0,th_lower/3,0,
                       th_lower/3,2*th_lower/3,1,
                       2*th_lower/3,th_lower,2,
                       th_lower,1,3),
                     ncol=3, byrow=T)
int_mx_upper<-matrix(c(0,th_upper/3,0,
                       th_upper/3,2*th_upper/3,1,
                       2*th_upper/3,th_upper,2,
                       th_upper,1,3),
                     ncol=3, byrow=T)

rf_classified_base<- as.factor(terra::classify(rf_classified_base.original,int_mx, include.lowest = T, right = T))
rf_classified_base_lower<- as.factor(terra::classify(rf_classified_base.original,int_mx_lower, include.lowest = T, right = T))
rf_classified_base_upper<- as.factor(terra::classify(rf_classified_base.original,int_mx_upper, include.lowest = T, right = T))
rf_classified<- as.factor(terra::classify(rf_classified.original,int_mx, include.lowest = T, right = T))
rf_classified_lower<- as.factor(terra::classify(rf_classified.original,int_mx_lower, include.lowest = T, right = T))
rf_classified_upper<- as.factor(terra::classify(rf_classified.original,int_mx_upper, include.lowest = T, right = T))
level_names <- c("no suitability", "low suitability", "medium suitability", "high suitability")

# Iterate through each layer for base, lower and upper 
for(i in 1:nlyr(rf_classified_base)) {
  
  # Get the current layer
  layer <- rf_classified_base[[i]]
  layer_lower <- rf_classified_base_lower[[i]]
  layer_upper <- rf_classified_base_upper[[i]]
  
  # Rename the levels
  levels(layer) <- data.frame(ID=0:3,label=level_names)
  levels(layer_lower) <- data.frame(ID=0:3,label=level_names)
  levels(layer_upper) <- data.frame(ID=0:3,label=level_names)
  
  # Replace the original layer with the renamed one
  rf_classified_base[[i]] <- layer
  rf_classified_base_lower[[i]] <- layer_lower
  rf_classified_base_upper[[i]] <- layer_upper
}

for(i in 1:nlyr(rf_classified)) {
  
  # Get the current layer
  layer <- rf_classified[[i]]
  layer_lower <- rf_classified_lower[[i]]
  layer_upper <- rf_classified_upper[[i]]
  
  # Rename the levels
  levels(layer) <- data.frame(ID=0:3,label=level_names)
  levels(layer_lower) <- data.frame(ID=0:3,label=level_names)
  levels(layer_upper) <- data.frame(ID=0:3,label=level_names)
  
  
  # Replace the original layer with the renamed one
  rf_classified[[i]] <- layer
  rf_classified_lower[[i]] <- layer_lower
  rf_classified_upper[[i]] <- layer_upper
}

names(rf_classified_base)<-c('present','future')
names(rf_classified_base_lower)<-c('present','future')
names(rf_classified_base_upper)<-c('present','future')
names(rf_classified)<-seq(2000,2050,1)
names(rf_classified_lower)<-seq(2000,2050,1)
names(rf_classified_upper)<-seq(2000,2050,1)

hs_area<- calculate_area(rf_classified,3)/10^6
hs_area_lower<- calculate_area(rf_classified_lower,3)/10^6
hs_area_upper<- calculate_area(rf_classified_upper,3)/10^6
ms_area<- calculate_area(rf_classified,2)/10^6
ms_area_lower<- calculate_area(rf_classified_lower,2)/10^6
ms_area_upper<- calculate_area(rf_classified_upper,2)/10^6
ls_area<- calculate_area(rf_classified,1)/10^6
ls_area_lower<- calculate_area(rf_classified_lower,1)/10^6
ls_area_upper<- calculate_area(rf_classified_upper,1)/10^6
no_area<- calculate_area(rf_classified,0)/10^6
no_area_lower<- calculate_area(rf_classified_lower,0)/10^6
no_area_upper<- calculate_area(rf_classified_upper,0)/10^6

hs_area_base<- calculate_area(rf_classified_base,3)/10^6
hs_area_base_lower<- calculate_area(rf_classified_base_lower,3)/10^6
hs_area_base_upper<- calculate_area(rf_classified_base_upper,3)/10^6
ms_area_base<- calculate_area(rf_classified_base,2)/10^6
ms_area_base_lower<- calculate_area(rf_classified_base_lower,2)/10^6
ms_area_base_upper<- calculate_area(rf_classified_base_upper,2)/10^6
ls_area_base<- calculate_area(rf_classified_base,1)/10^6
ls_area_base_lower<- calculate_area(rf_classified_base_lower,1)/10^6
ls_area_base_upper<- calculate_area(rf_classified_base_upper,1)/10^6
no_area_base<- calculate_area(rf_classified_base,0)/10^6
no_area_base_lower<- calculate_area(rf_classified_base_lower,0)/10^6
no_area_base_upper<- calculate_area(rf_classified_base_upper,0)/10^6

# create 3 annual_areas dataframes one for lower,upper, and normal
annual_areas_lower<-data.frame(Year=yrs,
                               'High_suitability'= hs_area_lower,
                               'Medium_suitability'=ms_area_lower,
                               'Low_suitability'=ls_area_lower,
                               'No_suitability'=no_area_lower
)
annual_areas_upper<-data.frame(Year=yrs,
                               'High_suitability'= hs_area_upper,
                               'Medium_suitability'=ms_area_upper,
                               'Low_suitability'=ls_area_upper,
                               'No_suitability'=no_area_upper
)
annual_areas <- data.frame(Year=yrs,
                          'High_suitability'= hs_area,
                          'Medium_suitability'=ms_area,
                          'Low_suitability'=ls_area,
                          'No_suitability'=no_area
)
 
#create 3 pf for upper, lower, normal
pf_lower<-list("high suitability" =hs_area_base_lower,
               "medium suitability"=ms_area_base_lower,
               "low suitability"=ls_area_base_lower,
               "no suitability"=no_area_base_lower
)
pf_upper<-list("high suitability" =hs_area_base_upper,
               "medium suitability"=ms_area_base_upper,
               "low suitability"=ls_area_base_upper,
               "no suitability"=no_area_base_upper
)
pf<-list("high suitability" =hs_area_base,
         "medium suitability"=ms_area_base,
         "low suitability"=ls_area_base,
         "no suitability"=no_area_base
)


long_df <- annual_areas %>%
  gather(key = "Level", value = "Area_km2", -Year) %>%
  mutate(Level = recode(
    Level,
    High_suitability = "high suitability",
    Medium_suitability = "medium suitability",
    Low_suitability = "low suitability",
    No_suitability = "no suitability"
  ))

long_df_lower <- annual_areas_lower %>%
  gather(key = "Level", value = "Area_km2", -Year) %>%
  mutate(Level = recode(
    Level,
    High_suitability = "high suitability",
    Medium_suitability = "medium suitability",
    Low_suitability = "low suitability",
    No_suitability = "no suitability"
  ))

long_df_upper <- annual_areas_upper %>%
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

long_df_lower <- long_df_lower %>%
  group_by(Level) %>%
  arrange(Year) %>%
  mutate(Percent_Change = (Area_km2 - lag(Area_km2)) / lag(Area_km2) * 100)

long_df_upper <- long_df_upper %>%
  group_by(Level) %>%
  arrange(Year) %>%
  mutate(Percent_Change = (Area_km2 - lag(Area_km2)) / lag(Area_km2) * 100)

# Group the data by 'Level'
grouped_df <- long_df %>%
  group_by(Level)

grouped_df_lower <- long_df_lower %>%
  group_by(Level)

grouped_df_upper <- long_df_upper %>%
  group_by(Level)

#compute difference and percentage change between present and future by level
pf.df <- as.data.frame(pf)
pf.df_lower <- as.data.frame(pf_lower)
pf.df_upper <- as.data.frame(pf_upper)

pf.df[3,]<-pf.df[2,]-pf.df[1,]
pf.df[4,]<-pf.df[3,]/pf.df[1,]*100
pf.df_lower[3,]<-pf.df_lower[2,]-pf.df_lower[1,]
pf.df_lower[4,]<-pf.df_lower[3,]/pf.df_lower[1,]*100
pf.df_upper[3,]<-pf.df_upper[2,]-pf.df_upper[1,]
pf.df_upper[4,]<-pf.df_upper[3,]/pf.df_upper[1,]*100

rownames(pf.df)<-c('present','future','difference','percentage_change')
rownames(pf.df_lower)<-c('present','future','difference','percentage_change')
rownames(pf.df_upper)<-c('present','future','difference','percentage_change')

level_names <- c("no suitability", "low suitability", "medium suitability", "high suitability")
#compute difference and percentage change between present and future from yearly values in grouped_df for the periods 2000-2020, 2030-2050
grouped_df$Scenario <- ifelse(grouped_df$Year < 2021, "present", ifelse(grouped_df$Year > 2029, "future", NA))
grouped_df_lower$Scenario <- ifelse(grouped_df_lower$Year < 2021, "present", ifelse(grouped_df_lower$Year > 2029, "future", NA))
grouped_df_upper$Scenario <- ifelse(grouped_df_upper$Year < 2021, "present", ifelse(grouped_df_upper$Year > 2029, "future", NA))
all_year_together <- rbind(grouped_df,grouped_df_lower,grouped_df_upper)
# Compute Area mean and standard deviation for each level in each scenario
present_mean <- grouped_df %>%
  filter(Scenario == "present") %>%
  group_by(Level) %>%
  summarize(mean_Area_km2 = mean(Area_km2, na.rm=T),
            mad = mad(Area_km2,na.rm=T, constant=1),
            sd = sd(Area_km2,na.rm=T))
present_mean_lower <- grouped_df_lower %>%
  filter(Scenario == "present") %>%
  group_by(Level) %>%
  summarize(mean_Area_km2 = mean(Area_km2, na.rm=T),
            mad = mad(Area_km2,na.rm=T, constant=1),
            sd = sd(Area_km2,na.rm=T))
present_mean_upper <- grouped_df_upper %>%
  filter(Scenario == "present") %>%
  group_by(Level) %>%
  summarize(mean_Area_km2 = mean(Area_km2, na.rm=T),
            mad = mad(Area_km2,na.rm=T, constant=1),
            sd = sd(Area_km2,na.rm=T))

future_mean <- grouped_df %>% 
  filter(Scenario == "future") %>%
  group_by(Level) %>%
  summarize(mean_Area_km2 = mean(Area_km2,na.rm=T),
            mad = mad(Area_km2,na.rm=T, constant= 1),
            sd = sd(Area_km2,na.rm=T))
future_mean_lower <- grouped_df_lower %>%
  filter(Scenario == "future") %>%
  group_by(Level) %>%
  summarize(mean_Area_km2 = mean(Area_km2, na.rm=T),
            mad = mad(Area_km2,na.rm=T, constant=1),
            sd= sd(Area_km2,na.rm=T))
future_mean_upper <- grouped_df_upper %>%
  filter(Scenario == "future") %>%
  group_by(Level) %>%
  summarize(mean_Area_km2 = mean(Area_km2, na.rm=T),
            mad = mad(Area_km2,na.rm=T, constant=1),
            sd = sd(Area_km2,na.rm=T))

# merge all the results together in a table 
present_mean <- present_mean %>%
  left_join(present_mean_lower, by = "Level") %>%
  left_join(present_mean_upper, by = "Level") %>%
  select(Level, mean_Area_km2.x, mean_Area_km2.y, mean_Area_km2)

data.frame(present_mean_upper)
pf.df_upper

data.frame(present_mean)
pf.df

data.frame(present_mean_lower)
pf.df_lower

data.frame(future_mean_upper)
pf.df_upper

data.frame(future_mean)
pf.df

data.frame(future_mean_lower)
pf.df_lower

rownames(int_mx) <- level_names
rownames(int_mx_lower) <- level_names
rownames(int_mx_upper) <- level_names
colnames(int_mx) <- c('lower bound', 'higher bound', 'class ID')
colnames(int_mx_lower) <- c('lower bound', 'higher bound', 'class ID')
colnames(int_mx_upper) <- c('lower bound', 'higher bound', 'class ID')

int_mx_upper 
int_mx
int_mx_lower
# compute percent change between present and future mean area 

percent_change <- present_mean %>%
  left_join(future_mean, by = "Level") %>%
  mutate(percent_change = (mean_Area_km2.y - mean_Area_km2.x) / mean_Area_km2.x * 100)


################################################################################################################
# Generate rasters for plotting
present<- rf_classified_base$present
present_upper <- rf_classified_base_upper$present
present_lower <- rf_classified_base_lower$present
future<- rf_classified_base$future
future_upper <- rf_classified_base_upper$future
future_lower <- rf_classified_base_lower$future
################################################################################################################
################################################################################################################
################################################################################################################


for(i in 1:4){
  print(level_names[i])
  
  data <- grouped_df %>% filter(Level == level_names[i])
  data_lower <- grouped_df_lower %>% filter(Level == level_names[i])
  data_upper <- grouped_df_upper %>% filter(Level == level_names[i])
  # Transform the data for modeling
  data$LogArea_km2 <- log(data$Area_km2)
  data_lower$LogArea_km2 <- log(data_lower$Area_km2)
  data_upper$LogArea_km2 <- log(data_upper$Area_km2)
  # Fit the linear model to the log-transformed data
  model <- lm(LogArea_km2 ~ Year, data = data)
  model_lower <- lm(LogArea_km2 ~ Year, data = data_lower)
  model_upper <- lm(LogArea_km2 ~ Year, data = data_upper)
  # Compute the fitted values on the original scale
  data$Fitted <- exp(predict(model))
  data_lower$Fitted <- exp(predict(model_lower))
  data_upper$Fitted <- exp(predict(model_upper))
  # Merge the results in data for plot 
  data$Area_km2_lower <-data_lower$Area_km2
  data$Area_km2_upper <-data_upper$Area_km2
  data$Fitted_lower <-data_lower$Fitted
  data$Fitted_upper <-data_upper$Fitted
  # Compute the trend line and statistical tests
  pv <- data.frame(v = data$Area_km2, Year = data$Year,lower = data_lower$Area_km2,upper = data_upper$Area_km2)
  #########
  k.fit <- Kendall::MannKendall(pv$v)
  l.fit <- lm(v ~ Year, data = pv)
  trend <- coef(l.fit)
  print(k.fit)
  yp <- pf[[level_names[i]]][1]
  yp_upper <- pf_upper[[level_names[i]]][1]
  yp_lower <- pf_lower[[level_names[i]]][1]
  yf <- pf[[level_names[i]]][2]
  yf_upper <- pf_upper[[level_names[i]]][2]
  yf_lower <- pf_lower[[level_names[i]]][2]
  yp.label <- yp
  yf.label <- yf
  if(i == 4){
    x.to.dodge <-2037
    y.to.dodge <- max(pv$v) + 0.2 * max(pv$v)
    xp.label <- 2010
    xf.label <- 2040
  } else if(i == 2){
    x.to.dodge <- 2012
    y.to.dodge <- max(pv$v)
    xp.label <- 2010
    xf.label <- 2040
    yp.label <- yp +0.01 * yp
  } else if(i == 3){
    #x.to.dodge <- 2010
    #y.to.dodge <- min(pv$v) + 0.5 * min(pv$v)
    #xp.label <- 2010
    #xf.label <- 2040
    x.to.dodge <-2037
    y.to.dodge <- max(pv$v)
    xp.label <- 2010
    xf.label <- 2040
    yp.label <- yp -0.03 * yp
  } else if(i == 1){
    x.to.dodge <- 2037
    y.to.dodge <- min(pv$v) + 1.5 * min(pv$v)
    xp.label <- 2010
    xf.label <- 2040
    yp.label <- yp +0.05 * yp
    yf.label <- yf +0.03 * yf
    
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
  adj.yf <-  max(pv$v) + 0.06 * max(pv$v)
  if(level_names[i] == 'medium suitability'){
    adj.yf<-1.3*max(pv$v)
  }
  if(level_names[i] == 'high suitability'){
    adj.yf<-1.6*max(pv$v)
  }
  # Create the plot
  p <- ggplot(data, aes(x = Year, y = Area_km2, color = Level)) +
    geom_line(aes(y = Fitted), color = class_colors.n[level_names[i]],linewidth=1.8,alpha=0.6, linetype=5) +
    geom_line(aes(group = Level),color='gray15',size=1.5, linetype=1) +  # Connect points with lines
    geom_ribbon(aes(ymin = Area_km2_lower, ymax = Area_km2_upper), fill = 'grey10',alpha=0.1, color = NA) +
    # add a rectangle for the present and future periods
    
   scale_color_manual(values = 'black') + # Set custom colors
    labs(title = NULL,
         x = "Year",
         y = "Area (km^2)") +
    theme_minimal() +
    geom_segment(aes(x = 2000, y = yp, xend = 2020, yend = yp), 
                 color = alpha("darkblue",0.55), size = 7, linetype=1) +
    geom_segment(aes(x = 2000, y = yp_lower, xend = 2020, yend = yp_lower), 
                 color = alpha("darkblue",0.01), size = 0.6, linetype=1,alpha=0.3) +
    geom_segment(aes(x = 2000, y = yp_upper, xend = 2020, yend = yp_upper),
                 color = alpha("darkblue",0.01), size = 0.6, linetype=1, alpha = 0.3) +
    geom_segment(aes(x = 2030, y = yf, xend = 2050, yend = yf), 
                 color = alpha("darkred",0.55),size = 7, linetype=1) +
    geom_segment(aes(x = 2030, y = yf_lower, xend = 2050, yend = yf_lower),
                 color = alpha("darkred",0.01), size = 0.6, linetype=1,alpha=0.3) +
    geom_segment(aes(x = 2030, y = yf_upper, xend = 2050, yend = yf_upper),
                 color = alpha("darkred",0.01), size = 0.6, linetype=1, alpha = 0.3) +
    geom_label(label.padding = unit(0.15,'lines'), size = 8,fontface=2,aes(x = xp.label, y = yp.label), 
               color = "darkblue", label = 'present',bg = 'white') +
    geom_label(label.padding = unit(0.15,'lines'), size = 8, fontface = 2 ,aes(x = xf.label, y = yf.label), 
               color = "darkred",label = 'future',bg = 'white') +
    geom_label(label.size=0,color='black',fill = alpha('grey99',1),position =  'dodge2',aes(x = x.to.dodge , y = y.to.dodge, 
                                                                                            label = paste0(text,paste0("Kendall ", "tau:", round(k.fit$tau, 2), " p-val:", formatC(k.fit$sl, digits = 2, format = 'e')))), size = 15) +
    #geom_text(color='black',position =  'dodge2',aes(x = 2010, y = min(pv$v) - 0.01 * min(pv$v),
    #                                                 label =  +
    geom_label(color = class_colors.n[level_names[i]],fill='white',label.size = 0,position =  'dodge2',aes(x = 2008, y = adj.yf,
                                                                                                          label = level_names[i]), size = 20) +
    geom_point(size = 6) +  # Plot points
    theme(legend.position = "none",
          axis.text.x = element_text( size = 40, face = 'bold'),
          axis.text.y = element_text(size = 40, face = "bold"), 
          axis.text = element_text(size = 30, face = "bold"), # Bigger, bold axis text
          axis.title = element_text(size = 48,face = "bold"), # Bold axis titles
    ) +
    scale_y_continuous(labels=function(x) scales::scientific(x,digits=2), n.breaks = NULL)
  #p <- p +  geom_rect(aes(xmin = 2000, xmax = 2020, ymin = yp_upper, ymax = yp_lower),color=NA,fill= alpha("blue",0.01),alpha = 0.01) +
  #  geom_rect(aes(xmin = 2030, xmax = 2050, ymin = yf_upper, ymax = yf_lower),fill= alpha("red",0.01),  color=NA, alpha = 0.01) 
  
  png(paste0(wd,'plots/',fnm[i],'_plot.png'), width = 1600, height = 1200)
  print(p)
  dev.off()
}

class_colors <- rev(palette.colors(palette = "Okabe-Ito")[c('vermillion','yellow','bluishgreen','blue')])
class_colors.n <- setNames(class_colors,cl.names)
