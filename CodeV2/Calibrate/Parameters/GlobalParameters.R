###_________PARAMETERS_________########
rho = 8.5 #(Baum-Snow and Han, 2023)
rho_se = 1.2 #(Standard error from Baum-Snow and Han (2023) to check robustness of estimates of omega to rho)

theta = 10/3 #(Hornbeck and Moretti, 2022 (pooled estimate across education types))

beta = 0.2 #Aggregate spending share under no regulation (to be used in purely homothetic model)

#Preference parameters from Calibrate_PrefParameters_to_Sample.R
beta_StGeary <- 0.08 #housing expenditure share of top income types
min_hReq <- 6000 #minimum housing consumption requirement == something that costs $500 a month at average price

#Elasticity of substitution from the skills-based version of the model
sigma <- 1.3 #Card (2003)

#________________________________OTHER CALIBRATION ASSUMED PARAMETERS_______________
wN_elast <- 35 #Set this to a large number to mimic perfect substitutes within cities
