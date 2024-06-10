criteria <- c('score','cv.score','cv.score.w','cv.diff.score','test.score','test.weighted.score','test.diff.score',
              'test.AUC.avg', 'test.wAUC.avg', 'test.CBI.avg', 'test.wCBI.avg', 'test.SEDI.avg', 
              'test.wSEDI.avg','test_OR10p', 'AUC.test.diff.avg' ,'wAUC.test.diff.avg', 'AICc',
              'test_wOR10p', 'weighted.auc.diff', 'weighted.cbi.diff', 'weighted.sedi.diff', 'weighted.or10p.diff',
              'cv.AUC.avg','cv.CBI.avg','cv.SEDI.avg','auc.diff.avg',
              'cv.wAUC.avg','cv.wCBI.avg','cv.wSEDI.avg','cv.wAUC.diff.avg', 'or.10p.avg',
              't.proc','cv.proc')

name.vector <- c('AUC','CBI','SEDI','AUCdiff','wAUC','wCBI','wSEDI','wAUCdiff','OR10p', 'AICc','pROC')


# Extract results from model and identify best models based on different criteria
bb <- e.mx@results

model.or10p <- bb %>% filter(or.10p.avg == min(or.10p.avg, na.rm = T)) %>% first() %>% select(tune.args)
model.aic <- bb %>% filter(AICc == min(AICc,na.rm=T)) %>% first() %>% select(tune.args)
model.auc <- bb %>% filter(cv.AUC.avg == max(cv.AUC.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.cbi <- bb %>% filter(cv.CBI.avg == max(cv.CBI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.sedi <- bb %>% filter(cv.SEDI.avg == max(cv.SEDI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.adiff <- bb %>% filter(auc.diff.avg == min(auc.diff.avg,na.rm=T)) %>% first() %>% select(tune.args)

model.wauc <- bb %>% filter(cv.wAUC.avg == max(cv.wAUC.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wcbi <- bb %>% filter(cv.wCBI.avg == max(cv.wCBI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wsedi <- bb %>% filter(cv.wSEDI.avg == max(cv.wSEDI.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.wadiff <- bb %>% filter(cv.wAUC.diff.avg == min(cv.wAUC.diff.avg,na.rm=T)) %>% first() %>% select(tune.args)
model.proc <- bb %>% filter(cv.proc == max(cv.proc,na.rm=T)) %>% first() %>% select(tune.args)

opt.or10p <- bb %>% filter(tune.args== model.or10p$tune.args)
opt.aic <- bb %>% filter(tune.args== model.aic$tune.args)
opt.proc <- bb %>% filter(tune.args== model.proc$tune.args)
opt.auc <- bb %>% filter(tune.args== model.auc$tune.args)
opt.cbi <- bb %>% filter(tune.args== model.cbi$tune.args)
opt.sedi <- bb %>% filter(tune.args== model.sedi$tune.args)
opt.adiff <- bb %>% filter(tune.args== model.adiff$tune.args)
opt.wauc <- bb %>% filter(tune.args== model.wauc$tune.args)
opt.wcbi <- bb %>% filter(tune.args== model.wcbi$tune.args)
opt.wsedi <- bb %>% filter(tune.args== model.wsedi$tune.args)
opt.wadiff <- bb %>% filter(tune.args== model.wadiff$tune.args)



bestmodels <- data.frame(modname = c(
  model.auc$tune.args,
  model.cbi$tune.args,
  model.sedi$tune.args,
  model.adiff$tune.args,
  model.wauc$tune.args,
  model.wcbi$tune.args,
  model.wsedi$tune.args,
  model.wadiff$tune.args,
  model.or10p$tune.args,
  model.aic$tune.args,
  model.proc$tune.args
  ), selecname = name.vector)


titles <- setNames(c("TOPSIS score",
                     "TOPSIS score on cross-validation",
                     "TOPSISw score on cross-validation",
                     "TOPSISdiff score on cross-validation",
                     "TOPSISw score on test",
                     "TOPSIS score on weighted test",
                     "TOPSISdiff score on test",
                     "AUC on test",
                     "Weighted AUC on test",
                     "CBI on test",
                     "Weighted CBI on test",
                     "SEDI on test",
                     "Weighted SEDI on test",
                     "Omission Rate 10th percentile on test",
                     "AUC difference on test",
                     "Weighted AUC difference on test",
                     "Akaike Information Criterion",
                     "Weighted Omission Rate 10th percentile on test",
                     "Absolute AUC-wAUC difference on test",
                     "Absolute CBI-wCBI difference on test",
                     "Absolute SEDI-wSEDI difference on test",
                     "Absolute OR10p-wOR10p difference on test",
                     "AUC on cross-validation (unweighted)",
                     "CBI on cross-validation (unweighted)",
                     "SEDI on cross-validation (unweighted)",
                     "AUC difference on cross-validation (unweighted)",
                     "AUC on cross-validation",
                     "CBI on cross-validation",
                     "SEDI on cross-validation",
                     "AUC difference on cross-validation",
                     "Omission Rate 10th percentile on cross-validation",
                     "Partial ROC AUC ratio on test",
                     "Partial ROC AUC ratio on cross-validation"
                     )
                     ,criteria)


for(variable in criteria){
  if(variable %in% c("test.AUC.avg", "test.wAUC.avg", "test.CBI.avg", "test.wCBI.avg", "test.SEDI.avg", "test.wSEDI.avg",'score',
                     "cv.AUC.avg",'cv.CBI.avg','cv.SEDI.avg',
                     "cv.wAUC.avg", "cv.wCBI.avg", "cv.wSEDI.avg", "cv.score",'t.proc','cv.proc','test.score','test.weighted.score','test.diff.score')){
    d <- T
  } else {
    d <- F
  }
  print(variable)
  bb <- e.mx@results
  bb<-bb[order(bb[,variable],decreasing = d),]
  bb$numbers<-1:nrow(bb)
  
  opt.or10p <- bb %>% filter(tune.args== model.or10p$tune.args)
  opt.aic <- bb %>% filter(tune.args== model.aic$tune.args)
  opt.proc <- bb %>% filter(tune.args== model.proc$tune.args)
  opt.auc <- bb %>% filter(tune.args== model.auc$tune.args)
  opt.cbi <- bb %>% filter(tune.args== model.cbi$tune.args)
  opt.sedi <- bb %>% filter(tune.args== model.sedi$tune.args)
  opt.adiff <- bb %>% filter(tune.args== model.adiff$tune.args)
  opt.wauc <- bb %>% filter(tune.args== model.wauc$tune.args)
  opt.wcbi <- bb %>% filter(tune.args== model.wcbi$tune.args)
  opt.wsedi <- bb %>% filter(tune.args== model.wsedi$tune.args)
  opt.wadiff <- bb %>% filter(tune.args== model.wadiff$tune.args)
  
  
  mod.seq.auc <- eval.models(e.mx)[[opt.auc$tune.args]]
  mod.seq.cbi <- eval.models(e.mx)[[opt.cbi$tune.args]]
  mod.seq.sedi <- eval.models(e.mx)[[opt.sedi$tune.args]]
  mod.seq.adiff <- eval.models(e.mx)[[opt.adiff$tune.args]]
  mod.seq.wauc <- eval.models(e.mx)[[opt.wauc$tune.args]]
  mod.seq.wcbi <- eval.models(e.mx)[[opt.wcbi$tune.args]]
  mod.seq.wsedi <- eval.models(e.mx)[[opt.wsedi$tune.args]]
  mod.seq.wadiff <- eval.models(e.mx)[[opt.wadiff$tune.args]]
  mod.seq.or10p <- eval.models(e.mx)[[opt.or10p$tune.args]]
  mod.seq.aic <- eval.models(e.mx)[[opt.aic$tune.args]]
  mod.seq.proc <- eval.models(e.mx)[[opt.proc$tune.args]]
  
 
  # Create a new column that combines the modname and selecname
  bestmodels <- bestmodels %>%
    mutate(combined = paste(selecname, '-', modname))
  
  # Define your colors for each combined label
  combined_colors <- setNames(palette.colors(palette = "Okabe-Ito")[c(1:4,1:4,5:8)],
                              bestmodels$combined)
  cls<-c(palette.colors(palette = "Okabe-Ito")[c(1:4,1:4,5:8)])
  nms<-c('AUC','CBI','SEDI','AUCdiff','wAUC','wCBI','wSEDI','wAUCdiff','OR10p', 'AICc','pROC')
  
  # Extracting the quantiles
  qntls <- quantile(bb[,variable], c(0.10, 0.25, 0.75, 0.90), na.rm = TRUE)
  
  
  library(ggrepel)
  
  # Determine the y values for the points and quantiles
  y_breaks <- c(bb[,variable][which(bb$fc == opt.auc$fc & bb$rm == opt.auc$rm)],
                bb[,variable][which(bb$fc == opt.cbi$fc & bb$rm == opt.cbi$rm)],
                bb[,variable][which(bb$fc == opt.sedi$fc & bb$rm == opt.sedi$rm)],
                bb[,variable][which(bb$fc == opt.adiff$fc & bb$rm == opt.adiff$rm)],
                bb[,variable][which(bb$fc == opt.wauc$fc & bb$rm == opt.wauc$rm)],
                bb[,variable][which(bb$fc == opt.wcbi$fc & bb$rm == opt.wcbi$rm)],
                bb[,variable][which(bb$fc == opt.wsedi$fc & bb$rm == opt.wsedi$rm)],
                bb[,variable][which(bb$fc == opt.wadiff$fc & bb$rm == opt.wadiff$rm)],
                bb[,variable][which(bb$fc == opt.or10p$fc & bb$rm == opt.or10p$rm)],
                bb[,variable][which(bb$fc == opt.aic$fc & bb$rm == opt.aic$rm)],
                qntls)
  y_breaks <- round(y_breaks,2)
  # compute distance between models selected numbers to adjust point position on plot
  pt.selected <- rbind(opt.auc,opt.cbi,opt.sedi,opt.adiff,opt.wauc,opt.wcbi,opt.wsedi,opt.wadiff,opt.or10p,opt.aic,opt.proc)
  rownames(pt.selected)<-bestmodels$selecname
  pt.selected <- pt.selected %>% arrange(numbers)
  new.order<-rownames(pt.selected)
  distance_matrix <- as.matrix(dist(pt.selected$numbers))
  # Adjust x positions based on the threshold
  #Initialize a vector to hold the adjustment for each point
  adjustments <- rep(0, nrow(distance_matrix))
  
  # Define the threshold for adjustments
  threshold.d <- 20
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
  names(adjustments) <- new.order
  plt <- ggplot(data = bb, mapping = aes(x = numbers, y = eval(parse(text = variable)))) 
  if(variable == 'test_OR10p'){
    #plt <- plt + geom_hline(yintercept = 0.1,  color = "darkred",size = 1, linetype = 'dotdash') + ylim(c(0,1)) + geom_text(data = data.frame(numbers=200,test_OR10p = 0.08), aes(label = 'Theoretical value = 0.1', color = 'darkred'),size=4.5,fontface='bold',show.legend = F )
    plt <- plt + ylim(c(0,0.6))
    
  }
  plt <- plt +
      geom_point(color=grey(0.9,1), shape = 17, size = 1)
  plt <- plt +
    #geom_point(position = position_nudge(x=adjustments[1] ),alpha = 0.3,data = filter(bb, fc == opt.topsis$fc & rm == opt.topsis$rm), color= 'black',aes(fill = bestmodels[bestmodels$selecname=='TOPSIS','combined']), size = 4.2, shape = 25) +
    geom_point(position = position_nudge(x=adjustments['AUC'] ),alpha = 0.9,data = filter(bb, fc == opt.auc$fc & rm == opt.auc$rm),       color= 'black',aes(fill = bestmodels[bestmodels$selecname=='AUC','combined']), size =        4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments['CBI'] ),alpha = 0.9,data = filter(bb, fc == opt.cbi$fc & rm == opt.cbi$rm),       color= 'black',aes(fill = bestmodels[bestmodels$selecname=='CBI','combined']), size =        4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments['SEDI'] ),alpha = 0.9,data = filter(bb, fc == opt.sedi$fc & rm == opt.sedi$rm),     color= 'black',aes(fill = bestmodels[bestmodels$selecname=='SEDI','combined']), size =     4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments['AUCdiff'] ),alpha = 0.9,data = filter(bb, fc == opt.adiff$fc & rm == opt.adiff$rm),   color= 'black',aes(fill = bestmodels[bestmodels$selecname=='AUCdiff','combined']), size =4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments['wAUC'] ),alpha = 0.9,data = filter(bb, fc == opt.wauc$fc & rm == opt.wauc$rm),     color= 'black',aes(fill = bestmodels[bestmodels$selecname=='wAUC','combined']), size =     4.2, shape = 23) +
    geom_point(position = position_nudge(x=adjustments['wCBI'] ),alpha = 0.9,data = filter(bb, fc == opt.wcbi$fc & rm == opt.wcbi$rm),     color= 'black',aes(fill = bestmodels[bestmodels$selecname=='wCBI','combined']), size =     4.2, shape = 23) +
    geom_point(position = position_nudge(x=adjustments['wSEDI'] ),alpha = 0.9,data = filter(bb, fc == opt.wsedi$fc & rm == opt.wsedi$rm),   color= 'black',aes(fill = bestmodels[bestmodels$selecname=='wSEDI','combined']), size =   4.2, shape = 23) +
    geom_point(position = position_nudge(x=adjustments['wAUCdiff'] ),alpha = 0.9,data = filter(bb, fc == opt.wadiff$fc & rm == opt.wadiff$rm), color= 'black',aes(fill = bestmodels[bestmodels$selecname=='wAUCdiff','combined']), size = 4.2, shape = 23) +
    geom_point(position = position_nudge(x=adjustments['OR10p'] ),alpha = 0.9,data = filter(bb, fc == opt.or10p$fc & rm == opt.or10p$rm),   color= 'black',aes(fill = bestmodels[bestmodels$selecname=='OR10p','combined']), size =  4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments['AICc'] ),alpha = 0.9,data = filter(bb, fc == opt.aic$fc & rm == opt.aic$rm),       color= 'black',aes(fill = bestmodels[bestmodels$selecname=='AICc','combined']), size =       4.2, shape = 21) +
    geom_point(position = position_nudge(x=adjustments['pROC'] ),alpha = 0.9,data = filter(bb, fc == opt.proc$fc & rm == opt.proc$rm),     color= 'black',aes(fill = bestmodels[bestmodels$selecname=='pROC','combined']), size =     4.2, shape = 21)
      
      
    
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
    scale_y_continuous(labels=function(x) sprintf("%.2f", round(x,2)),n.breaks = 10, minor_breaks = waiver()) +  # Setting y-axis ticks
    ggtitle(titles[variable])
  
    if(variable == 'AICc'){
      plt <- plt + scale_y_continuous(breaks =seq(min(y_breaks,na.rm = T),max(y_breaks,na.rm = T),length.out = 10), minor_breaks = waiver()) + ylim(c(min(y_breaks,na.rm = T),max(y_breaks,na.rm = T)))
    } 
  library(grid)
  library(cowplot)
    plt_color_legend_only <- plt + guides(color=FALSE)  # Remove the fill guide
    color_legend <- get_legend(plt_color_legend_only)
    # Save legend
    ggsave(filename = paste0(getwd(),'/','plots/','selection_legend','.png'), plot = color_legend, width = 3, height = 3, dpi = 300)

  #}
  plt_no_legend <- plt + theme(legend.position = "none")
  # Save plot
  
  ggsave(filename = paste0(getwd(),'/','plots/',variable,'.png'),plot = plt_no_legend, width = 6, height = 4, dpi = 300)
  
  
  
}


