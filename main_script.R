#### DATA TO REPRODUCE VIF ANALYSIS AND SPATIAL AUTOCORRELATION ANALYSIS ####
############
# Set working directory
wd <- ('path_to_this_folder/')
setwd(wd)
# check libraries and try to install if missing 
source(paste0(wd,'scripts/','check_install_dependencies.R'))
source(paste0(wd,'scripts/','dependencies_2.R'))
# Set projection
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Set exp name
expname <- 'experiment_'


###############################################################################
# VIF ANALYSIS AND RASTER PREPARATION
file.edit(paste0(wd,'scripts/','VIF_analysis.R'))

# CLEANING AND PREPARING RCMED DATASET 
file.edit(paste0(wd,'scripts/','rcmed_cleaning.R'))

# CLEANING AND PREPARING OCCURRENCE DATA 
file.edit(paste0(wd,'scripts/','from_raw_to_input_occurrences.R'))

# SPATIAL ANALYSIS AND WEIGHTING 
file.edit(paste0(wd,'scripts/','spatial_analysis_and_weighting.R'))

# DATA TO REPRODUCE MODEL SELECTION AND PERFORMANCE METRICS FROM SCRATCH 
file.edit(paste0(wd,'scripts/','model_tuning.R'))

# SEE PERFORMANCES AND MODEL RANKING
file.edit(paste0(wd,'scripts/','performance_and_ranking.R'))

# VARIABLE IMPORTANCE AND RESPONSES 
file.edit(paste0(wd,'scripts/','compute_variable_importance.R'))
file.edit(paste0(wd,'scripts/','compute_response_curves.R'))

# MOBILITY ORIENTED PARITY ANALYSIS
file.edit(paste0(wd,'scripts/','compute_mop_percentile_maps.R'))

# PCA SPACE PLOT 
file.edit(paste0(wd,'scripts/','PCA_space_plot.R'))

# PRESENT AND FUTURE PDF CURVES
file.edit(paste0(wd,'scripts/','compute_pdfs.R'))

# FUTURE PROJECTIONS
file.edit(paste0(wd,'scripts/','compute_future_projections.R'))

# CLASSIFICATION ANALYSIS 
file.edit(paste0(wd,'scripts/','classification_analysis_CI.R'))

# SCENARIO CLASSIFICATION MAP PLOTS 
file.edit(paste0(wd,'scripts/','scenario_raster_plots.R'))

