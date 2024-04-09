#check if library is present, otherwise install it
if (!require("mop")) {
  install.packages("mop", dependencies = TRUE)
  library(mop)
}

if(!require("raster")){
  install.packages("raster", dependencies = TRUE)
  library(raster)
}

if(!require("terra")){
  install.packages("terra", dependencies = TRUE)
  library(terra)
}

if(!require('RColorBrewer')){
  install.packages('RColorBrewer', dependencies = TRUE)
  library(RColorBrewer)
}

if(!require('ggplot2')){
  install.packages('ggplot2', dependencies = TRUE)
  library(ggplot2)
}
if(!require('dplyr')){
  install.packages('dplyr', dependencies = TRUE)
  library(dplyr)
}

if(!require('tidyr')){
  install.packages('tidyr', dependencies = TRUE)
  library(tidyr)
}

if(!require('ggpubr')){
  install.packages('ggpubr', dependencies = TRUE)
  library(ggpubr)
}

if(!require('ggrepel')){
  install.packages('ggrepel', dependencies = TRUE)
  library(ggrepel)
}

if(!require('stringr')){
  install.packages('stringr', dependencies = TRUE)
  library(stringr)
}

if(!require('dismo')){
  install.packages('dismo', dependencies = TRUE)
  library(dismo)
}

if(!require('ENMeval')){
  install.packages('ENMeval', dependencies = TRUE)
  library(ENMeval)
}

if(!require('usdm')){
  install.packages('usdm', dependencies = TRUE)
  library(usdm)
}

if(!require('blockCV')){
  install.packages('blockCV', dependencies = TRUE)
  library(blockCV)
}

if(!require('enmSdmX')){
  install.packages('enmSdmX', dependencies = TRUE)
  library(enmSdmX)
}

if(!require('doParallel')){
  install.packages('doParallel', dependencies = TRUE)
  library(doParallel)
}

if(!require('foreach')){
  install.packages('foreach', dependencies = TRUE)
  library(foreach)
}

if(!require('rSDM')){
  devtools::install_github("Pakillo/rSDM")
  library(rSDM)
}

if(!require('tidyterra')){
  install.packages('tidyterra', dependencies = TRUE)
  library(tidyterra)
}

if(!require('urca')){
  install.packages('urca', dependencies = TRUE)
  library(urca)
}
if(!require('Kendall')){
  install.packages('Kendall', dependencies = TRUE)
  library(Kendall)
}
if(!require('topsis')){
  install.packages('topsis', dependencies = TRUE)
  library(topsis)
}

if(!require('measurements')){
  install.packages('measurements',dependencies= TRUE)
  library(measurements)
}

if(!require('automap')){
  install.packages('automap', dependencies = TRUE)
  library(automap)
  }
