#This file downloads all required packages. 
#This project runs on R version 4.3.2 with all updated packages as of Dec 12, 2024.

#Package to run stata from R
install.packages("RStata")


#Install Corelogic construct packages
for (pkg in c("dplyr", "collapse", "vroom", "haven", "sf")) {
  
  install.packages(pkg)
  
}


#Install Regulation Construct packages
for (pkg in c("labelled", "spdep", "ClustGeo", "cluster", "doParallel",
              "matrixStats", "ggplot2", "rlang", "stringr", "readr",
              "here","purrr", "viridis", "estimatr")) {
  
  install.packages(pkg)
  
}


#Install Facts packages

for (pkg in c("patchwork", "mgcv", "gratia", "forcats", "googleway", "geosphere")) {
  
  install.packages(pkg)
  
}


#Install Calibration/Counterfactual/Instrument packages
for (pkg in c("compiler", "vtable", "terra", "rgdal", "exactextractr")) {
  
  install.packages(pkg)
}

