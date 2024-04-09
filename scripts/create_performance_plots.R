library(ggplot2)
library(dplyr)
variable<-'test_OR10p'
plt.lst<-list()

variables <- c("test.AUC.avg", "test.wAUC.avg", "test.CBI.avg", "test.wCBI.avg", "test.SEDI.avg", "test.wSEDI.avg","test_OR10p", "AUC.test.diff.avg" ,"wAUC.test.diff.avg", "AICc",'test_wOR10p', 'weighted.auc.diff', 'weighted.cbi.diff','weighted.sedi.diff', 'weighted.or10p.diff', 'score')  # replace with your variables

name.vector <- c('AUC','CBI','SEDI','AUCdiff','OR10p', 'AICc', 'wAUC','wCBI','wSEDI','wAUCdiff')



# here you can check the ranking of the models based on the variable of interest
bb <- e.mx@results
#bb<- left_join(bb, performances.merged, by = 'tune.args')
#model.topsis <- bb %>% filter(rank == min(rank,na.rm=T)) %>% first() %>% select(tune.args)
model.auc <- bb %>% filter(auc.val.avg == max(auc.val.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.cbi <- bb %>% filter(cbi.val.avg == max(cbi.val.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.sedi <- bb %>% filter(cv.SEDI.avg == max(cv.SEDI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.adiff <- bb %>% filter(auc.diff.avg == min(auc.diff.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.or10p <- bb %>% filter(or.10p.avg == min(or.10p.avg, na.rm = T)) %>% first() %>% select(tune.args)
model.aic <- bb %>% filter(AICc == min(AICc,na.rm=T)) %>% first() %>% select(tune.args)
# weighted metrics
model.wauc <- bb %>% filter(cv.wAUC.avg == max(cv.wAUC.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wcbi <- bb %>% filter(cv.wCBI.avg == max(cv.wCBI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wsedi <- bb %>% filter(cv.wSEDI.avg == max(cv.wSEDI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wadiff <- bb %>% filter(cv.wAUC.diff.avg == min(cv.wAUC.diff.avg,na.rm=T)) %>% first() %>% select(tune.args)


bestmodels <- data.frame(modname = c(
  #model.topsis$tune.args,
  model.auc$tune.args,
  model.cbi$tune.args,
  model.sedi$tune.args,
  model.adiff$tune.args,
  model.or10p$tune.args,
  model.aic$tune.args,
  model.wauc$tune.args,
  model.wcbi$tune.args,
  model.wsedi$tune.args,
  model.wadiff$tune.args
  ), selecname = name.vector)


titles <- setNames(c("AUC on test",
                     "Weighted AUC on test",
                     "CBI on test",
                     "Weighted CBI on test",
                     "SEDI on test",
                     "Weighted SEDI on test",
                     "Omission Rate 10th percentile on test",
                     "AUC difference between train and test",
                     "Weighted AUC difference between train and test",
                     "Akaike Information Criterion corrected",
                     "Weighted Omission Rate 10th percentile on test",
                     "AUC and wAUC absolute difference on test",
                     "CBI and wCBI absolute difference on test",
                     "SEDI and wSEDI absolute difference on test",
                     "OR10p and wOR10p absolute difference on test",
                     "Similarity to ideal solution (TOPSIS)")
                     ,variables)


for(variable in variables){
  if(variable %in% c("test.AUC.avg", "test.wAUC.avg", "test.CBI.avg", "test.wCBI.avg", "test.SEDI.avg", "test.wSEDI.avg",'test.score_unw','test.score_w','cv.score_unw','cv.score_w','score')){
    d <- T
  } else {
    d <- F
  }
  print(variable)
  bb<-bb[order(bb[,variable],decreasing = d),]
  bb$numbers<-1:nrow(bb)
  
  #opt.topsis <- bb %>% filter(rank == min(rank,na.rm=T)) %>%first()
  opt.auc <- bb %>% filter(auc.val.avg == max(auc.val.avg,na.rm=T)) %>%first()
  opt.cbi <- bb %>% filter(cbi.val.avg == max(cbi.val.avg,na.rm=T)) %>%first()
  opt.sedi <- bb %>% filter(cv.SEDI.avg == max(cv.SEDI.avg,na.rm=T)) %>%first()
  opt.adiff <- bb %>% filter(auc.diff.avg == min(auc.diff.avg,na.rm=T)) %>%first()
  opt.or10p <- bb %>% filter(or.10p.avg == min(or.10p.avg, na.rm = T)) %>%first()
  opt.aic <- bb %>% filter(AICc == min(AICc,na.rm=T)) %>%first()
  opt.wauc <- bb %>% filter(cv.wAUC.avg == max(cv.wAUC.avg,na.rm=T)) %>%first()
  opt.wcbi <- bb %>% filter(cv.wCBI.avg == max(cv.wCBI.avg,na.rm=T)) %>%first()
  opt.wsedi <- bb %>% filter(cv.wSEDI.avg == max(cv.wSEDI.avg,na.rm=T)) %>%first()
  opt.wadiff <- bb %>% filter(cv.wAUC.diff.avg == min(cv.wAUC.diff.avg,na.rm=T)) %>%first()
  
  mod.seq.topsis <- eval.models(e.mx)[[opt.topsis$tune.args]]
  mod.seq.auc <- eval.models(e.mx)[[opt.auc$tune.args]]
  mod.seq.cbi <- eval.models(e.mx)[[opt.cbi$tune.args]]
  mod.seq.sedi <- eval.models(e.mx)[[opt.sedi$tune.args]]
  mod.seq.adiff <- eval.models(e.mx)[[opt.adiff$tune.args]]
  mod.seq.or10p <- eval.models(e.mx)[[opt.or10p$tune.args]]
  mod.seq.aic <- eval.models(e.mx)[[opt.aic$tune.args]]
  mod.seq.wauc <- eval.models(e.mx)[[opt.wauc$tune.args]]
  mod.seq.wcbi <- eval.models(e.mx)[[opt.wcbi$tune.args]]
  mod.seq.wsedi <- eval.models(e.mx)[[opt.wsedi$tune.args]]
  mod.seq.wadiff <- eval.models(e.mx)[[opt.wadiff$tune.args]]
  
 
  # Create a new column that combines the modname and selecname
  bestmodels <- bestmodels %>%
    mutate(combined = paste(selecname, '-', modname))
  
  # Define your colors for each combined label
  combined_colors <- setNames(palette.colors(palette = "Okabe-Ito")[c(8,2:5,7,1,2:5)],
                              bestmodels$combined)
  cls<-c(palette.colors(palette = "Okabe-Ito")[c(8,2:5,7,1,2:5)])
  nms<-c('TOPSIS','AUC','CBI','SEDI','AUCdiff','OR10p', 'AICc', 'wAUC','wCBI','wSEDI','wAUCdiff')
  
  # Extracting the quantiles
  qntls <- quantile(bb[,variable], c(0.10, 0.25, 0.75, 0.90), na.rm = TRUE)
  
  
  library(ggrepel)
  
  # Determine the y values for the points and quantiles
  y_breaks <- c(bb[,variable][which(bb$fc == opt.auc$fc & bb$rm == opt.auc$rm)],
                bb[,variable][which(bb$fc == opt.cbi$fc & bb$rm == opt.cbi$rm)],
                bb[,variable][which(bb$fc == opt.sedi$fc & bb$rm == opt.sedi$rm)],
                bb[,variable][which(bb$fc == opt.adiff$fc & bb$rm == opt.adiff$rm)],
                bb[,variable][which(bb$fc == opt.or10p$fc & bb$rm == opt.or10p$rm)],
                bb[,variable][which(bb$fc == opt.aic$fc & bb$rm == opt.aic$rm)],
                bb[,variable][which(bb$fc == opt.wauc$fc & bb$rm == opt.wauc$rm)],
                bb[,variable][which(bb$fc == opt.wcbi$fc & bb$rm == opt.wcbi$rm)],
                bb[,variable][which(bb$fc == opt.wsedi$fc & bb$rm == opt.wsedi$rm)],
                bb[,variable][which(bb$fc == opt.wadiff$fc & bb$rm == opt.wadiff$rm)],
                qntls)
  # compute distance between models selected numbers to adjust point position on plot
  pt.selected <- rbind(opt.auc,opt.cbi,opt.sedi,opt.adiff,opt.or10p,opt.aic,opt.wauc,opt.wcbi,opt.wsedi,opt.wadiff)
  distance_matrix <- as.matrix(dist(pt.selected$numbers))
  # Adjust x positions based on the threshold
  #Initialize a vector to hold the adjustment for each point
  adjustments <- rep(0, nrow(distance_matrix))
  
  # Define the threshold for adjustments
  threshold.d <- 19
  # Function to calculate adjustments
  # Function to find the index of the closest pair of points and their distance
  find_closest_pair <- function(numbers, adjustments) {
    min_dist <- Inf
    closest_i <- NA
    closest_j <- NA
    for (i in 1:(length(numbers)-1)) {
      for (j in (i+1):length(numbers)) {
        distance <- abs(numbers[i] + adjustments[i] - (numbers[j] + adjustments[j]))
        if (distance < min_dist) {
          min_dist <- distance
          closest_i <- i
          closest_j <- j
        }
      }
    }
    return(list(min_dist = min_dist, i = closest_i, j = closest_j))
  }
  
  # Adjust positions starting from the closest ones
  adjust_closest_pairs <- function(numbers, adjustments, threshold.d) {
    while(TRUE) {
      closest_info <- find_closest_pair(numbers, adjustments)
      if (closest_info$min_dist >= threshold.d) {
        break  # Exit if the closest pair is already beyond the threshold
      }
      
      # Increment the distance by moving each point away from the other by 1 unit
      adjustments[closest_info$i] <- adjustments[closest_info$i] - 0.5
      adjustments[closest_info$j] <- adjustments[closest_info$j] + 0.5
    }
    return(adjustments)
  }
  # Apply the function to adjust the closest pairs
  adjustments <- rep(0, length(pt.selected$numbers))
  adjustments <- adjust_closest_pairs(pt.selected$numbers, adjustments, threshold.d)
  #adjustments <- sort(adjustments, decreasing = F)
  
  plt <- ggplot(data = bb, mapping = aes(x = numbers, y = eval(parse(text = variable)))) 
  if(variable == 'test_OR10p'){
    #plt <- plt + geom_hline(yintercept = 0.1,  color = "darkred",size = 1, linetype = 'dotdash') + ylim(c(0,1)) + geom_text(data = data.frame(numbers=200,test_OR10p = 0.08), aes(label = 'Theoretical value = 0.1', color = 'darkred'),size=4.5,fontface='bold',show.legend = F )
    plt <- plt + ylim(c(0,0.6))
    
  }
  if(variable != 'rank.test'){
    plt <- plt +
      geom_point(color=grey(0.9,1), shape = 17, size = 1)
  }
  plt <- plt +
    #geom_point(position = position_nudge(x=adjustments[1] ),alpha = 0.9,data = filter(bb, fc == opt.topsis$fc & rm == opt.topsis$rm), color= 'black',aes(fill = bestmodels[bestmodels$selecname=='TOPSIS','combined']), size = 4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments[2] ),alpha = 0.9,data = filter(bb, fc == opt.auc$fc & rm == opt.auc$rm),       color= 'black',aes(fill = bestmodels[bestmodels$selecname=='AUC','combined']), size =        4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments[3] ),alpha = 0.9,data = filter(bb, fc == opt.cbi$fc & rm == opt.cbi$rm),       color= 'black',aes(fill = bestmodels[bestmodels$selecname=='CBI','combined']), size =        4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments[4] ),alpha = 0.9,data = filter(bb, fc == opt.sedi$fc & rm == opt.sedi$rm),     color= 'black',aes(fill = bestmodels[bestmodels$selecname=='SEDI','combined']), size =     4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments[5] ),alpha = 0.9,data = filter(bb, fc == opt.adiff$fc & rm == opt.adiff$rm),   color= 'black',aes(fill = bestmodels[bestmodels$selecname=='AUCdiff','combined']), size =4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments[6] ),alpha = 0.9,data = filter(bb, fc == opt.or10p$fc & rm == opt.or10p$rm),   color= 'black',aes(fill = bestmodels[bestmodels$selecname=='OR10p','combined']), size =  4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments[7] ),alpha = 0.9,data = filter(bb, fc == opt.aic$fc & rm == opt.aic$rm),       color= 'black',aes(fill = bestmodels[bestmodels$selecname=='AICc','combined']), size =       4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments[8] ),alpha = 0.9,data = filter(bb, fc == opt.wauc$fc & rm == opt.wauc$rm),     color= 'black',aes(fill = bestmodels[bestmodels$selecname=='wAUC','combined']), size =4.2, shape = 23) +
    geom_point(position = position_nudge(x=adjustments[9] ),alpha = 0.9,data = filter(bb, fc == opt.wcbi$fc & rm == opt.wcbi$rm),     color= 'black',aes(fill = bestmodels[bestmodels$selecname=='wCBI','combined']), size =4.2, shape = 23) +
    geom_point(position = position_nudge(x=adjustments[10] ),alpha = 0.9,data = filter(bb, fc == opt.wsedi$fc & rm == opt.wsedi$rm),   color= 'black',aes(fill = bestmodels[bestmodels$selecname=='wSEDI','combined']), size =4.2, shape = 23) +
    geom_point(position = position_nudge(x=adjustments[11]),alpha = 0.9,data = filter(bb, fc == opt.wadiff$fc & rm == opt.wadiff$rm), color= 'black',aes(fill = bestmodels[bestmodels$selecname=='wAUCdiff','combined']), size =4.2, shape = 23) 
      
      
    
    if(variable == 'test_OR10p'){
      #plt <- plt + geom_hline(yintercept = 0.1,  color = "darkred",size = 1, linetype = 'dotdash') + ylim(c(0,1)) 
      #+ geom_text(data = data.frame(numbers=200,test_OR10p = 0.08), aes(label = 'Theoretical value = 0.1', color = 'darkred'),alpha=0.7,fontface='bold',size=1.5,show.legend = F )
      plt <- plt + ylim(c(0,0.6))
    } else if(variable == 'AICc') {
      plt <-  plt + ylim(c(min(y_breaks,na.rm = T),max(y_breaks,na.rm = T)))
    } 
    plt <- plt +
    scale_fill_manual(values = combined_colors) +
    guides(fill = guide_legend(title = "Criteria and model selected")) +
    geom_hline(yintercept = qntls[c(1, 4)],  linetype = "dashed", color = "black") +
    #geom_hline(yintercept = qntls[c(2, 3)],  linetype = "dotted", color = "black") +
    theme_bw() +
    theme(axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(size = rel(1.4)),
          #axis.text.x = element_blank(),
          #axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_blank(),
          #axis.title.y = element_text(face = "bold", size = rel(1.1)),
          axis.text.y = element_text(size = rel(1.8)),
          axis.title.x = element_text(size = rel(0.9),vjust = 1),
          title = element_text(face = "bold", size = rel(1.1)),
          legend.text = element_text(face = "bold", size = rel(1.1), color = grey(0.2,1)),
          legend.title = element_text(face = "bold", size = rel(1.1)),
          legend.text.align = 0) +
    labs(
      color = "criteria and model selected" # Replace with your actual legend title
    ) +
    ylab(variable) +
    xlab('performance') +
    scale_x_continuous(breaks = c(1,nrow(bb)),n.breaks = 2, label = c('best','worst')) +
    scale_y_continuous(n.breaks = 10, minor_breaks = waiver()) +  # Setting y-axis ticks
  
    ggtitle(titles[variable])
  
    if(variable == 'AICc'){
      plt <- plt + scale_y_continuous(breaks =seq(min(y_breaks,na.rm = T),max(y_breaks,na.rm = T),length.out = 10), minor_breaks = waiver()) + ylim(c(min(y_breaks,na.rm = T),max(y_breaks,na.rm = T)))
    } 
  library(grid)
  library(cowplot)
    plt_color_legend_only <- plt + guides(color=FALSE)  # Remove the fill guide
    color_legend <- get_legend(plt_color_legend_only)
    # Save legend
    ggsave(filename = paste0(wd,'plots/','selection_legend','.png'), plot = color_legend, width = 3, height = 3, dpi = 300)

  #}
  plt_no_legend <- plt + theme(legend.position = "none")
  # Save plot
  
  ggsave(filename = paste0(wd,'plots/',variable,'.png'),plot = plt_no_legend, width = 6, height = 4, dpi = 300)
  
  
  
}

bestmodel <- mod.seq.topsis
bestopt <- opt.topsis
