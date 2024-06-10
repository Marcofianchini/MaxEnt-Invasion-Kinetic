########################################################################################################################
To reproduce the results in the article, open the file 'main_script.R' in the folder. Here you must set the working directory to this folder.
Then follow the instructions of the script to perform the analysis step by step.
Minor actions are (should be) required for the user and it will be indicated in the code when they are required.
The outputs will be saved in 'results', 'rasters' and 'plots' folders.

Actions that will be performed are: 

1) Setup of the general parameters 
2) Install/loading of the required R packages 
3) Spatial analysis to perform variable selection 
4) Import and cleaning of the RCMED dataset 
5) Import and cleaning of the occurrences dataset used in the cross-validation setup
6) Spatial autocorrelation analysis and generation of the site-weighting 
7) Model tuning (follow the instruction inside the script, some user interaction is required in this point to make it works properly) + Partial ROC AUC analysis
8) TOPSIS analysis to rank the models
9) Identification of the best model 
10) Variable Importance Analysis
11) Response curves analysis 
12) Compute Mobility Oriented Map analysis 
13) Compute Probability Density Functions 
14) Compute future projections 
15) Map classification, stationarity and trend analysis 

