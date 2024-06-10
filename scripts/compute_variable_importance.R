print('Creating variable importance plots')
bestmod <- readRDS(paste0('results/',expname,'_','bestmod.rds'))
bestopt <- readRDS(paste0('results/',expname,'_','bestopt.rds'))
############################################################################################################
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
# extract all the values named 'substrate.permutation.importance' from each list in var_importance_list
prova <- sapply(var_importance_list, function(x) x['substrate.permutation.importance'])

# Convert the lists to data frames, matching variables by name
var_importance_df <- data.frame(fc=e.mx@results$fc,rm=e.mx@results$rm,bind_rows(var_importance_list))
var_contribution_df <- data.frame(fc=e.mx@results$fc,rm=e.mx@results$rm,bind_rows(var_contribution_list))

importances <- vector(length=nlyr(envs.final))
for(i in 1:length(importances)){
  print(i)
  print(colnames(var_importance_df[i]))
  importances[i] <- median(var_importance_df[,i+2], na.rm=T)
}
importances <- setNames(importances,names(var_importance_df[3:ncol(var_importance_df)]))
names(importances) <- str_remove(names(importances), "\\.importance")
names(importances) <- str_remove(names(importances), "\\.permutation")
importances <- importances[order(importances, decreasing = TRUE)]
# Reshape the data from wide to long format
var_importance_long <- var_importance_df %>%
  gather(key = "variable", value = "importance", -fc,-rm) %>%
  mutate(variable = str_remove(variable, "\\.importance")) %>%
  mutate(variable = str_remove(variable, "\\.permutation")) 

var_contribution_long <- var_contribution_df %>%
  gather(key = "variable", value = "contribution", -fc,-rm) %>%
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
  bestmod = bestmod@results[grepl('importance',rownames(bestmod@results)),]
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
  # add the variable importance data for or10p to the plot as points
  geom_point(data = var_importance_by_bestmetrics_named_df, aes(x = factor(variable, levels = var_order_importance), y = bestmod), color = "red", size = 10, shape = 15) +
  theme_bw() +
  ylim(c(-15,60))+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.4, size = 30, face = 'bold'),
        axis.text.y = element_text(size = 35),
        axis.title = element_text(size = 35,face='bold'),
        axis.title.x = element_blank(),
        title = element_text(size = 37,face='bold')) +
  labs(title = "Variable Permutation Importance", x = "Variable", y = "Permutation Importance") 

for(i in seq_along(annotate.importances)){
  vr <-as.factor(unique(var_importance_long$variable)[i])
  p1 <- p1 + annotate('text',x = vr, y = -15, label = as.character(round(annotate.importances[names(annotate.importances) %in% vr],1)), hjust = 0.5, vjust = 0, size = 12, color = 'black')
  # annotate values for best modelL importance
  p1 <- p1 + annotate('text',x = vr, y = -10, label = as.character(round(var_importance_by_bestmetrics_named_df$bestmod[i],1)), hjust = 0.5, vjust = 0, size = 11, color = 'red')
}

png(paste0('plots/',"variable_permutation_importance.png"),width = 1600, height = 900)
print(p1)
dev.off()

print(p1)
