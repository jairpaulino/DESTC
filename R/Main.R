# Title: Dynamic ensemble selection based on trend classification (DESTC)
# Author: Jair Paulino de Sales
# Date: Feb/2023

# Setup
# Cleaning R environment
rm(list=ls()); graphics.off()

# Importing libs
require("trend")
require("tidyverse")
require("matrixStats")

# Importing functions
source("R/destc.R")

# Get countries in "/data" path
countries = list.dirs(path = "data/", full.names = FALSE, recursive = FALSE)

# Create DESTC to each country based on their valid and test pools
run(countries)
