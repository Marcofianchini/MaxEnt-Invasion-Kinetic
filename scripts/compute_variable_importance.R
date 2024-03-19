print('Creating variable importance plots')

# Define the path to save the plots
path.to.plot <- paste0(wd, 'plots/')

# Define an experiment name for plot labeling
exp.name <- 'Caulerpa'

# Extract the model results from the ENMevaluation object
bb <- e.mx@results

# Identify the best models based on metrics
opt.wQTI <- bb %>% filter(wQTI == max(wQTI,na.rm=T))  %>% first()
opt.QTI <- bb %>% filter(QTI == max(QTI,na.rm=T)) %>% first()
opt.auc <- bb %>% filter(auc.val.avg == max(auc.val.avg,na.rm=T)) %>% first()
opt.cbi <- bb %>% filter(cbi.val.avg == max(cbi.val.avg,na.rm=T)) %>% first()
opt.sedi <- bb %>% filter(cv.SEDI.avg == max(cv.SEDI.avg,na.rm=T)) %>% first()
opt.proc <- bb %>% filter(cv.proc_auc_ratio.avg == max(cv.proc_auc_ratio.avg,na.rm=T)) %>% first()
opt.adiff <- bb %>% filter(auc.diff.avg == min(auc.diff.avg,na.rm=T)) %>% first()
opt.or10p <- bb %>% filter(or.10p.avg == min(or.10p.avg, na.rm = T)) %>% first()
opt.aic <- bb %>% filter(AICc == min(AICc,na.rm=T)) %>% first()
opt.models<- list(opt.wQTI, opt.QTI, opt.auc, opt.cbi, opt.sedi, opt.proc, opt.adiff, opt.or10p, opt.aic)
# Extract the models corresponding to the optimal metrics
mod.seq.wQTI <- eval.models(e.mx)[[opt.wQTI$tune.args]]
mod.seq.QTI <- eval.models(e.mx)[[opt.QTI$tune.args]]
mod.seq.auc <- eval.models(e.mx)[[opt.auc$tune.args]]
mod.seq.cbi <- eval.models(e.mx)[[opt.cbi$tune.args]]
mod.seq.sedi <- eval.models(e.mx)[[opt.sedi$tune.args]]
mod.seq.proc <- eval.models(e.mx)[[opt.proc$tune.args]]
mod.seq.adiff <- eval.models(e.mx)[[opt.adiff$tune.args]]
mod.seq.or10p <- eval.models(e.mx)[[opt.or10p$tune.args]]
mod.seq.aic <- eval.models(e.mx)[[opt.aic$tune.args]]
mod.seq<- list(mod.seq.wQTI, mod.seq.QTI, mod.seq.auc, mod.seq.cbi, mod.seq.sedi, mod.seq.proc, mod.seq.adiff, mod.seq.or10p, mod.seq.aic)

# Initialize an empty list to store the variable importance and contribution data
var_importance_list <- list()
var_contribution_list <- list()

# Initialize an empty list to store the variable importance data
var_importance_list <- list()
var_contribution_list <- list()

# Iterate over each model in the ENMevaluation object
for(i in seq_along(e.mx@models)) {
  model <- e.mx@models[[i]]
  
  # Extract variable importance for the current model
  var_importance <- model@results[grepl('importance',rownames(model@results)),]
  var_contribution <- model@results[grepl('contribution',rownames(model@results)),]
  # Add the model identifier (fc and rm values) to the data frame
  model_fc <- e.mx@results$fc[i]
  model_rm <- e.mx@results$rm[i]
  
  # Convert the data frames to named vectors and add them to the list
  var_importance_named <- c(setNames(var_importance, names(var_importance)))
  var_contribution_named <- c(setNames(var_contribution, names(var_contribution)))
  var_importance_list[[i]] <- var_importance_named
  var_contribution_list[[i]] <- var_contribution_named
}

# Convert the lists to data frames, matching variables by name
var_importance_df <- data.frame(fc=e.mx@results$fc,rm=e.mx@results$rm,bind_rows(var_importance_list))
var_contribution_df <- data.frame(fc=e.mx@results$fc,rm=e.mx@results$rm,bind_rows(var_contribution_list))

importances <- vector(length=19)
for(i in 3:21){
  print(i)
  print(colnames(var_importance_df[i]))
  importances[i-2] <- mean(var_importance_df[,i], na.rm=T)
}
importances <- setNames(importances,names(var_importance_df[3:21]))
names(importances) <- stringr::str_remove(names(importances), "\\.importance")
names(importances) <- stringr::str_remove(names(importances), "\\.permutation")
importances <- importances[order(importances, decreasing = TRUE)]
# Reshape the data from wide to long format
var_importance_long <- var_importance_df %>%
  tidyr::gather(key = "variable", value = "importance", -fc,-rm) %>%
  mutate(variable = stringr::str_remove(variable, "\\.importance")) %>%
  mutate(variable = stringr::str_remove(variable, "\\.permutation")) 

var_contribution_long <- var_contribution_df %>%
  tidyr::gather(key = "variable", value = "contribution", -fc,-rm) %>%
  mutate(variable = stringr::str_remove(variable, "\\.contribution"))

# Calculate the order of variables based on their mean importance or contribution
var_order_importance <- var_importance_long %>%
  group_by(variable) %>%
  dplyr::summarise(mean_importance = stats::median(importance, na.rm = TRUE)) %>%
  arrange(desc(mean_importance)) %>%
  pull(variable)

var_order_contribution <- var_contribution_long %>%
  group_by(variable) %>%
  summarise(mean_contribution = median(contribution, na.rm = TRUE)) %>%
  arrange(desc(mean_contribution)) %>%
  pull(variable)
# Extract variable importances for the best model according to each metric in a list: example = model@results[grepl('importance',rownames(model@results)),]
var_importance_by_bestmetrics <- list(
  wQTI = mod.seq.wQTI@results[grepl('importance',rownames(mod.seq.wQTI@results)),],
  QTI = mod.seq.QTI@results[grepl('importance',rownames(mod.seq.QTI@results)),],
  auc = mod.seq.auc@results[grepl('importance',rownames(mod.seq.auc@results)),],
  cbi = mod.seq.cbi@results[grepl('importance',rownames(mod.seq.cbi@results)),],
  sedi = mod.seq.sedi@results[grepl('importance',rownames(mod.seq.sedi@results)),],
  proc = mod.seq.proc@results[grepl('importance',rownames(mod.seq.proc@results)),],
  adiff = mod.seq.adiff@results[grepl('importance',rownames(mod.seq.adiff@results)),],
  or10p = mod.seq.or10p@results[grepl('importance',rownames(mod.seq.or10p@results)),],
  aic = mod.seq.aic@results[grepl('importance',rownames(mod.seq.aic@results)),]
)
# set names to each vector in the list using the names obtained by unique(var_contribution_long$variable)
var_importance_by_bestmetrics_named <- lapply(var_importance_by_bestmetrics, setNames, nm = unique(var_importance_long$variable))
# create a dataframe with a column reporting the metric name
var_importance_by_bestmetrics_named_df <- as.data.frame(var_importance_by_bestmetrics_named)
var_importance_by_bestmetrics_named_df$variable <- rownames(var_importance_by_bestmetrics_named_df)
# Create box plots and add the variable importance data for each metric to the plot as points
annotate.importances <- importances
p1<-ggplot(var_importance_long, aes(x = factor(variable, levels = var_order_importance), y = importance)) +
  geom_boxplot(fill = "darkgray", outlier.shape = 19, outlier.size = 0.7) +
  # add the variable importance data for wQTI to the plot as points
  geom_point(data = var_importance_by_bestmetrics_named_df, aes(x = factor(variable, levels = var_order_importance), y = wQTI), color = "red", size = 2) +
  theme_bw() +
  ylim(c(0,50))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.4, size = 14, face = 'bold'),
        axis.text.y = element_text(size = 14),
        axis.title = element_text(size = 14,face='bold'),
        title = element_text(size = 17,face='bold')) +
  labs(title = "Variable Importance", x = "Variable", y = "Permutation Importance") 

for(i in seq_along(annotate.importances)){
  vr <-as.factor(unique(var_importance_long$variable)[i])
  p1 <- p1 + annotate('text',x = vr, y = 50, label = as.character(round(annotate.importances[names(annotate.importances) %in% vr],1)), hjust = 0.5, vjust = 0, size = 5, color = 'black')
  # annotate values for wQTI importance
  p1 <- p1 + annotate('text',x = vr, y = 47, label = as.character(round(var_importance_by_bestmetrics_named_df$wQTI[i],1)), hjust = 0.5, vjust = 0, size = 5, color = 'red')
}

# Save the plot and print it
png(paste0(path.to.plot,exp.name,"_variable_permutation_importance.png"),width = 1100, height = 500)
print(p1)
dev.off()
print(p1)

# IMPORTANT NOTE: BIOZONE IS NOT CONSIDERED IN THE STUDY DUE TO PROBLEMS IN RECLASSIFICATION (BATHYAL HABITAT IS PRESENT DUE TO MISSCLASSIFICATION IN THE PHYSICAL MODEL BATHYMETRY). 
# THE VARIABLE AFFECTS THE MODEL BUT DOESN'T CONTRIBUTE TO DISCERN SUITABLE FROM NOT SUITABLE HABITATS, AS THEY ARE ALL SIGNALLED AS EQUALLY IMPORTANT.
########## 