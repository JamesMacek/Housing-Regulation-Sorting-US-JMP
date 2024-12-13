# This file runs the entire project from start to finish.
# Calls R and Stata code in correct sequence.

library(RStata) #to call Stata from R

chooseStataBin()  #Set your path to stata binary manually here in interactive mode
stataVersion <- 17 #Set your stata version


#####################################################

#Module 1: Constructing CoreLogic data
  
  #This module takes proprietary CoreLogic assessment data in delimited format and transforms into structured dataset.
  for (file in c("currentAssess_construct.R",
                 "transactions_construct.R")) {
  
    source(paste0("CodeV2/Construct_CoreLogic/", file))
    
  }

  #Compressing and appending corelogic data...

  for (file in c("compress_append_currentAssessments", #automatically passes R working directory to stata
                "compress_append_transactions",
                "collapse_stats_forClustering")) {
    
    stata(paste0("CodeV2/Construct_CoreLogic/", file),
          stata.version = stataVersion)
    
  }
#######################################################


#Module 2: Clean ACS (from IPUMS) and NaNDA datasets for empirical work

  for (file in c("ACS_BlockGroup_Construct.R",
                 "Construct_Dist_to_CBD.R",
                 "harmonizeBlockGroups.R", #Use for harmonization between 2010-2020 block groups
                 "NaNDA_Construct.R")) {
    
    
    
    source(paste0("CodeV2/Clean_ACS_NaNDA/", file))
    
    
  }


#######################################################

#Module 3: Construct regulation and test it


  #Cluster zoning districts
  source("CodeV2/Construct_Regulation/cluster_ZoningDistricts.R")
  
  #Assign regulation levels to block groups
  stata(paste0("CodeV2/Construct_Regulation/assign_Regulation.do"),
        stata.version = stataVersion)
  
  #Merge stringency measures with other datasets + other cleaning
  source("CodeV2/Construct_Regulation/Merge_stringency.R")

  #Validation measures (need not run)
  source("CodeV2/Construct_Regulation/validate_Regulation.R")
  
  #Reproduce zoning figures
  stata(paste0("CodeV2/Construct_CoreLogic/extract_Hayward_example.do"),
        stata.version = stataVersion)
  
  #
  source("CodeV2/Construct_Regulation/Zoning_figures_output.R")

######################################################

#Module 4: Construct Descriptive Facts at beginning of paper
  
  for (file in c("Facts_construct.R", "Facts_construct_robust.R",
                 "facts_Appendix.R")) {
    
    
    source(paste0("CodeV2/Facts/", file))
    
    
  }

######################################################

#Module 5: City Productivity and Housing Parameters
  
  #Get measures of city productivity, income bins for each location, etc from ACS household/tabulations data
  for (file in c("PUMA_MSA_crosswalk.do",
                 "Construct_CityWages.do", 
                 "Construct_LocalAbilityDistributions.do",
                 "IncomeDist_byCollegeShare.do")) {
    
    stata(paste0("CodeV2/Construct_CityProductivity/", file),
          stata.version = stataVersion)
    
  }
  
  
  #Housing Parameters
  for (file in c("PriceRentRatios.do",             #get price to rent by city
                 "ownerOccupier_byIncomeType.do",  #get share of owner occupiers by income bin
                 "SpendingShare_Housing.do",       #get spending shares by income bin
                 
                 "HedonicIndex.do","SecondaryHedonicIndex.do")) {    #Construct hedonic indices
     
    stata(paste0("CodeV2/Construct_HousingParameters/", file),
          stata.version = stataVersion)
    
  }

######################################################

#Module 6: Calibrate the model
  
  
  
  #Also check Calibrate_StoneGeary_parameters to see how well subsample targets aggregate spending on housing services
  # (at current choice of parameter values...)
  
  for (file in c("SolveProductivity_bySkill.R", #productivity for bySkill version of the model
                 "Calibrate_ConsumptionValues_SupplyShifters.R", #main calibration file
                 "Calibrate_Unobserved_Amenities_all.R", #all unobserved amenities
                 "Calibration_SummaryStatistics.R")) { #Generate summary statistics for calibrated objects in file
    
    
    source(paste0("CodeV2/Calibrate/", file))
    
    
  }
  

##################################################### 
 
  #Module 7: Run IV Estimation of Omega(z) parameters.
  
  for (file in c("Prepare_Slope_Raster.R",                       #Prepare measures of terrain slopes at block group level
                 "Construct_donut_instrument.R")) { #Generate summary statistics for calibrated objects in file
    
    
    source(paste0("CodeV2/Instrument/", file))
    
    
  }
  
  #Now, run all IV estimation, create all IV tables in paper
  stata(paste0("CodeV2/Instrument/run_all_IV_estimation.do"),
        stata.version = stataVersion)
  
  
  
######################################################

  #Module 8: Run Counterfactuals. Reproduces all results in the paper.
  
  #Solve equilibria
  for (file in c("Solve_All_Equilibria_Loop_allSpecs_FullDereg.R", #complete deregulation + extensions
                 "Solve_All_Equilibria_Loop_allSpecs_CityDereg.R", #city-by-city
                 "Ctfl_Dereg_DifferentFundamentals.R",             #Muting variation in fundamental amenities
                 "TargetingRegulation.R")                           #Permuted Policy
                 ) {
    
    source(paste0("CodeV2/Counterfactual/Analysis", file))
    
  }

  #Analyze equilibria
  
  for (file in c("AnalyzeWelfare.R",               #Main welfare results for compl. deregulation
                 "AnalyzeLabourProductivity.R",    #Measures changes to aggregate productivity
                 "AnalyzeSpatialDistribution.R",   #Measures changes to within/across city income distribution          
                 "AnalyzeDifferentFundamentals.R", #Measures effects of deregulation w/ no fundamentals
                 "Analyze_variousCities.R",        #Effects of unilateral deregulation in select cities
                 "Analyze_SanFrancisco.R",         #San Francisco results deep dive 
                 "Analyze_TargetRegulation.R")       #Analysis of targeted regulation file                     
  ) {
    
    source(paste0("CodeV2/Counterfactual/Analysis/", file))
    
  }
  


