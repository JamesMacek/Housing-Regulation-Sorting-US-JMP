
#Calling parameters from the calibrate module
source("CodeV2/Calibrate/Parameters/GlobalParameters.R")

#Adding more parameters
Omega <- c(0.18, 0.18, 0.31, 0.31, 0.31, 0.46, 0.46) #Preferred specification from Full_IV_clusterBartlett.do
#(on full set of controls. Note: some controls are outcomes in this specification!)

Agglomeration_elast <- 0.05 #elasticity of city wages to population