
#Calling parameters from the calibrate module
source("CodeV2/Calibrate/Parameters/GlobalParameters.R")

#Adding more parameters
Omega <- c(0.135, 0.135, 0.285, 0.285, 0.285, 0.3, 0.3) #Preferred specification from Full_IV_clusterBartlett.do 

#Alternative specifications based on different binning strategy (differences in coeff. are much more apparent)
#Omega <- c(0.135, 0.135, 0.25, 0.25, 0.25, 0.38, 0.38) #almost identical predictions!

#Standard agglomeration elasticity
Agglomeration_elast <- 0.05 #elasticity of city wages to population

#Diamond (2016) pairwise implied agglomeration elasticities by skill (NOTE: pairwise elasticities rely on assumptions about sigma)
#See Fagelbaum and Gaubert (2020) for transformation
bySkill_agg_matrix <- cbind(c(0.003, 0.044), c(0.02, 0.053)) #pairwise agglomeration elasticities

#Zone-level migration elasticites for effective computation of solution...
wN_elast_ctfl <- wN_elast  #set equal to calibration version of this

#_______________________________________________________________________________________________
#For crude welfare measurement assuming households are renters who also own a national portfolio
#_______________________________________________________________________________________________
#Owner occupier rates (from ownerOccupier_byIncomeType.do)
ownerOccupier_rate <- c(0.375, 0.511, 0.618, 0.708, 0.793, 0.857, 0.89)
spendShares_targeted <- c(0.389, 0.232, 0.177, 0.153, 0.138, 0.122, 0.088) #spend shares from data to rationalize national equilibrium 
  #(used in calibration of supply shifters + consumption values and observed in the data)

