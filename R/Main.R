# Title: Dynamic ensemble selection based on trend classification (DESTC)
# Author: Jair Paulino de Sales
# Date: Feb/2023

# Setup 
# Cleaning R environment
rm(list=ls()); graphics.off() 

# Importing libs
if(!require("trend")) install.packages("trend")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("matrixStats")) install.packages("matrixStats")

# Importing functions
source("R/Auxiliar.R")
source("R/DESTC.R")
source("R/PerformanceMetrics.R")

# Get countries in "/data" path
countries = list.dirs(path = "data/", full.names = FALSE, recursive = FALSE)

# Create DESTC to each country based on their valid and test pools
run(countries)
