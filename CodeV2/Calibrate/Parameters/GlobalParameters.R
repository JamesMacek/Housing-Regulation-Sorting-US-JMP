###_________MAIN PARAMETERS_________########

rho = 8.5 #(Baum-Snow and Han, 2023)
rho_se = 1.2 #(Standard error from Baum-Snow and Han (2023) to check robustness of estimates of omega to rho)

theta = 4.15 #(Hornbeck and Moretti, 2022 (pooled estimate across education types))

#assumed beta if assuming homothetic preferences
beta = 0.2 #Aggregate spending share under no regulation (to be used in purely homothetic model)

#Preference parameters from Calibrate_PrefParameters_to_Sample.R
beta_StGeary <- 0.075 #housing expenditure share of top income types
min_hReq <-  4750 #minimum housing consumption requirement == something that costs ~ $450 a month in average location

#Elasticity of substitution from the skills-based version of the model
sigma <- 1.3 #Card (2003)


#______________________________________________________________________________________________________________________________________________

#________________________________OTHER CALIBRATION/DATA BASED ASSUMED PARAMETERS_______________
wN_elast <- 8.5 #Set this to a large number to mimic perfect substitutes within cities

# Stringency parameters
regulation_censoring <- 100000 #parameter by which to censor regulation for all neighborhoods that are more stringent
                               #since we also observe censored income distributions. 
                               #Corresponds to yearly payments made to consume a house on a minimal lot


Convert_value_to_yr_flow_cost <- 0.0525 # Use upper end of user cost distribution from Poterba and Sinai (2007). 
#Convert housing wealth to flow cost. Lower values imply smaller regulatory stringency.

