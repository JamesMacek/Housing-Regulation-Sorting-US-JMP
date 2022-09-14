#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(rlang)
library(doParallel) #Parallelize for loops, solving housing market equilibria much faster. 
library(rlang) #For symbolic...
library(ggplot2)

#This do file solves for both an initial equilibrium (because of partial calibration) 
#And a counterfactual where all minimum lot sizes are halved. 
#Requires output from all other Calibration .R files. 

#PART 0: PRELIMINARIES

test_zero_equilibrium <- 1 #Set to 1 if you need to remove all lot sizes
test_initial_equilibrium <- 0 #Set to 1 if you want to check if the chosen parameters rationalize the data as an equilibrium.


#Setting up parallel cluster to solve housing market equilibria faster
n.cores <- detectCores() - 1 #11 threads (cores?) on my main machine.
my.cluster <- makeCluster(n.cores, type = "PSOCK") #needs to allow firewall.

#registering cluster
registerDoParallel(cl = my.cluster)

#checking to see if this is registered
getDoParRegistered() #TRUE.
getDoParWorkers()


#Joined data
Main <- read_dta("Data/Counterfactuals/JoinedDataForCounterfactuals.dta")

#Amenity Values
AmenityValues <- read_dta("Data/Counterfactuals/CalibratedAmenityValues.dta")
AmenityValues <- AmenityValues %>% select(-avg_commuteMins) #deleting duplicates

#Productivity
Productivity <- read_dta("Data/Counterfactuals/CalibratedProductivity.dta")
Productivity <- Productivity %>% select(CBSA, CBSA_NAME, CollegeProductivity, NoCollegeProductivity)

#Joining
Main <- left_join(Main, AmenityValues, by = c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME"))
Main <- left_join(Main, Productivity, by = c("CBSA", "CBSA_NAME"))
rm(AmenityValues, Productivity)

#UNIVARIATE PARAMETERS__________________________________________________________________________________
Beta <- 0.174 #Spending share on housing from CalibrateBeta.R

Omega <- 0 #IV estimate from SimpleInstrument.do 2.65 

Sigma <- 1.3 #Elasticity of substitution for labour types

#Migration elasticities converted to a nested logit semi-elasticity, from CalibrateMigrationElasticityToLogit.R


tot_housing_units <- sum(Main$tot_housing_units_cen_2010) #>100million


#More cleaning


#Calculating density of housing units in each location at initial values
for (i in 1:7) {
   #Total number of households in each group-location
   Main[paste("households_College", i, sep = "")] <- sum(Main[[paste("Ability_bin_College", i, sep = "") ]]*
                                                       Main$tot_housing_units_cen_2010*
                                                       Main$implied_college_share, na.rm = TRUE)*
                                                       Main[[paste("fr_households_College", i, sep="")]]
  
   Main[paste("households_NoCollege", i, sep = "")] <- sum(Main[[paste("Ability_bin_NoCollege", i, sep = "")]]*
                                                       Main$tot_housing_units_cen_2010*
                                                       (rep(1, nrow(Main)) - Main$implied_college_share), na.rm = TRUE)*
                                                       Main[[paste("fr_households_NoCollege", i, sep="")]]
}

#Calculating model-implied income per household at original values
Main <- Main %>% mutate(implied_hhavgincome = ((households_College1*ability_grp1 + households_College2*ability_grp2 +
                                                households_College3*ability_grp3 + households_College4*ability_grp4 + 
                                                households_College5*ability_grp5 + households_College6*ability_grp6 + 
                                                households_College7*ability_grp7)*CollegeWage +
                                           
                                               (households_NoCollege1*ability_grp1 + households_NoCollege2*ability_grp2 +
                                                households_NoCollege3*ability_grp3 + households_NoCollege4*ability_grp4 + 
                                                households_NoCollege5*ability_grp5 + households_NoCollege6*ability_grp6 + 
                                                households_NoCollege7*ability_grp7)*NoCollegeWage)/
                          
                                               (households_College1 + households_College2 +
                                                households_College3 + households_College4 + 
                                                households_College5 + households_College6 + 
                                                households_College7 +
                                                households_NoCollege1 + households_NoCollege2 +
                                                households_NoCollege3 + households_NoCollege4 + 
                                                households_NoCollege5 + households_NoCollege6 + 
                                                households_NoCollege7))

#Checking to see if this measure of average income agrees with Fact 2 (not the same due to income censoring)
#Test Fact 2
Test_Fact_2 <- 0

if (Test_Fact_2 == 1) {
load(file = "Data/US_Data/Output/CBSA_quantiles.Rdata")
load(file = "Data/US_Data/Output/Constructed_2010_Tract.Rdata") #For ranks
US_BLOCK_2010_JOINED <- US_BLOCK_2010_JOINED %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME,
                                                        CBSA_med_house_value, rank_density_CBSA)
US_BLOCK_2010_JOINED$CBSA <- as.double(US_BLOCK_2010_JOINED$CBSA)

Main <- left_join(Main, US_BLOCK_2010_JOINED, by = c("State", "County", "Tract", "BlockGroup", "CBSA", "CBSA_NAME"))

Main <- Main %>% group_by(CBSA) %>% mutate(demeaned_imphhavgincome = log(implied_hhavgincome) - mean(log(implied_hhavgincome), na.rm = TRUE))

#HOLDS!
ggplot() + 
  geom_smooth(method = 'loess', span=1, data = 
                Main[Main$CBSA_med_house_value > as.numeric(quantile_CBSA_houseval["75.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES 
              aes(x=rank_density_CBSA, y=demeaned_imphhavgincome, color = 'Top 25%')) +
  geom_smooth(method = 'loess', span=1, data = 
                Main[Main$CBSA_med_house_value < as.numeric(quantile_CBSA_houseval["25.0%"]),], se = FALSE, #SET SE = FALSE FOR RAM ISSUES
              aes(x=rank_density_CBSA, y=demeaned_imphhavgincome, color = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) + 
  xlab("Ranked housing unit density (Block Group level)") +
  ylab("Log Average Income (demeaned by MSA)")
} #SET THIS TO 1 IF YOU WANT TO REPLICATE FACT 2 with model measure of income per capita.

#Constructing exogenous amenities by subtracting out neighbor component. 
for (i in 1:7) {
  
   # Adjust amenities for income per capita (the exogenous component)
   Main[paste("exogenous_Amenity_College", i, sep = "")] <- Main[paste("withincity_amenity_College", i, sep = "")]/(Main$implied_hhavgincome^(Omega))
   Main[paste("exogenous_Amenity_NoCollege", i, sep = "")] <- Main[paste("withincity_amenity_NoCollege", i, sep = "")]/(Main$implied_hhavgincome^(Omega))
}





#Dataframe storing equilibrium objects (that 'mostly' change each iteration), including initial population distributions as per our guess.
equilibrium_objects <- Main %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME,
                                       households_College1, households_College2, households_College3,
                                       households_College4, households_College5, households_College6,
                                       households_College7,
                                       households_NoCollege1, households_NoCollege2, households_NoCollege3,
                                       households_NoCollege4, households_NoCollege5, households_NoCollege6,
                                       households_NoCollege7,
                                       ability_grp1, ability_grp2, ability_grp3, ability_grp4, ability_grp5,
                                       ability_grp6, ability_grp7)

#Calculating total number of households 
for (i in 1:7) {
  equilibrium_objects[paste("tot_households_College", i, sep = "")] <- sum(equilibrium_objects[paste("households_College", i, sep = "")])
  equilibrium_objects[paste("tot_households_NoCollege", i, sep = "")] <- sum(equilibrium_objects[paste("households_NoCollege", i, sep = "")])
}


#counts of block groups by MSA
equilibrium_objects <- equilibrium_objects %>% group_by(CBSA) %>% mutate(MSAcount = n())

#FUNCTIONS.

#__________________CALC WAGES________________________________________________________________________________________________________________
#Following function takes the data frames above and calculates what the wage by education and MSA must be.
CalculateWages <- function(eq_objectsDF, MainDF, sub_elast) { #eq_objects store stuff that changes each iteration, Main for most invariant objects.
  
  #Determining labour supply at current iteration
  eq_objectsDF <- eq_objectsDF %>% group_by(CBSA) %>% 
                         mutate(lab_supply_College = sum(households_College1*ability_grp1) + sum(households_College2*ability_grp2) +
                                                     sum(households_College3*ability_grp3) + sum(households_College4*ability_grp4) + 
                                                     sum(households_College5*ability_grp5) + sum(households_College6*ability_grp6) + 
                                                     sum(households_College7*ability_grp7))
  eq_objectsDF <- eq_objectsDF %>% group_by(CBSA) %>% 
                         mutate(lab_supply_NoCollege = sum(households_NoCollege1*ability_grp1) + sum(households_NoCollege2*ability_grp2) +
                                                       sum(households_NoCollege3*ability_grp3) + sum(households_NoCollege4*ability_grp4) + 
                                                       sum(households_NoCollege5*ability_grp5) + sum(households_NoCollege6*ability_grp6) + 
                                                       sum(households_NoCollege7*ability_grp7))
  
  #Calculating relative wages to solve labour demand equation.
  Rel_collegeWage <- ((eq_objectsDF$lab_supply_College/eq_objectsDF$lab_supply_NoCollege)^(-(1/sub_elast)))*
                        ((MainDF$CollegeProductivity/MainDF$NoCollegeProductivity)^((sub_elast - 1)/sub_elast)) 
  
  #Calculating wages in levels given our choice of normalization
  College_wage <- (((Rel_collegeWage)^(sub_elast - 1))*MainDF$NoCollegeProductivity^(sub_elast - 1) + MainDF$CollegeProductivity^(sub_elast - 1))^(1/(sub_elast - 1))
  
  NoCollege_wage <- College_wage/Rel_collegeWage
  
  return(list(College_wage, NoCollege_wage))
}

#Testing function
testWages <- CalculateWages(equilibrium_objects, Main, Sigma) #Appears to work, and can extract vectors by refering to [[1]] and [[2]] in list.
#Should equal our observed wages
testWages[[1]] -  Main$CollegeWage #Equals up to machine precision
testWages[[2]] - Main$NoCollegeWage #Same deal.

#Put this in data frame
equilibrium_objects["CollegeWage"] <- Main$CollegeWage 
equilibrium_objects["NoCollegeWage"] <- Main$NoCollegeWage
rm(testWages)

#____________________________________CLEAR HOUSING MARKETS_____________________________________________________________________________________
#function to clear housing markets. This returns the squared excess demand for each row, in a vector.
ClearHousingMarkets <- function(price, eq_objectsDF, MainDF, house_spshare, i) { #First argument will be optimized over, i is the row of the dataframe
  
  #1: calculating lot size stringency at current prices
  Incstr_lot_size <- MainDF$DensityRestriction[i]*MainDF$lambda[i]*(price^(MainDF$converted_elasticities[i] + 1))
  
  #Calculating vector of spending
  Spending <- 0 #initialize running total of spending
  
  for (j in 1:7) {
    ability_grp <- paste("ability_grp", j, sep = "") #name of ability group
    households_College <- paste("households_College", j, sep = "")
    households_NoCollege <- paste("households_NoCollege", j, sep = "")
    
    #Spending by college workers
    
    Spending_max <- max(Incstr_lot_size, 
                         house_spshare*eq_objectsDF[[ability_grp]][i]*eq_objectsDF$CollegeWage[i]) 
    
    if (Spending_max > eq_objectsDF[[ability_grp]][i]*eq_objectsDF$CollegeWage[i]) {
      Spending_max <- eq_objectsDF[[ability_grp]][i]*eq_objectsDF$CollegeWage[i] #setting spending = entire income and leave with reservation utility.
    }
     #deleting if group priced out of market.
    
    Spending <- Spending + Spending_max*eq_objectsDF[[households_College]][i] #increasing spending by current number of HH's
    
    #Spending by non-college workers
    Spending_max <- max(Incstr_lot_size, 
                         house_spshare*eq_objectsDF[[ability_grp]][i]*eq_objectsDF$NoCollegeWage[i]) #parralel max is the rowwise maximimum
    
    if (Spending_max > eq_objectsDF[[ability_grp]][i]*eq_objectsDF$NoCollegeWage[i]) { #If workers priced out, set this to "homeless"--i.e. does not spend anything
    
      Spending_max <- eq_objectsDF[[ability_grp]][i]*eq_objectsDF$NoCollegeWage[i]
    }
    
    Spending <- Spending + Spending_max*eq_objectsDF[[households_NoCollege]][i] #increasing spending by current number of HH's
  }
  
  #Calculating supply vector
  SupplyValue <- MainDF$res_land_acres[i]*MainDF$lambda[i]*(price^(MainDF$converted_elasticities[i] + 1))
  
  return(Spending - SupplyValue) #returning excess demand across all markets.
  
}


#Testing function (use Optimize)
test <- ClearHousingMarkets(1, equilibrium_objects,
                            Main, Beta, 1)
rm(test)

testLoop <- 0 #set to one if you want to run the following block of code. 
if (testLoop == 1) {
  
  StartTime <- Sys.time()
  
  #using parallelized loop:
  housePrice <- foreach (row = 1:nrow(equilibrium_objects), .combine = 'c') %dopar% {
                           uniroot(f = ClearHousingMarkets,
                                    eq_objectsDF = equilibrium_objects,
                                    MainDF = Main, 
                                    house_spshare = Beta,
                                    i = row,
                                    interval = c(0, 10000),
                                    extendInt = "downX",
                                    maxiter = 1000, tol = .Machine$double.eps)$root #searching over this interval of housing prices.
  
  }
  
  FinishTime <- Sys.time() #approx. four minutes for an entire set of 171,000 prices. This will greatly speed up code. 
  
  ##Testing to see if we solved for our correct equilibrium (note Main$hedonicPrice clears markets as shown in CalibrateLambda.R)
  max(abs(housePrice - Main$hedonicPrice)) #equals 0 up to on average 6 decimal places. very close.
  rm(housePrice)
}

#Updating initial equilibrium housing prices
equilibrium_objects["housePrice"] <- Main$hedonicPrice


#Second housing function that works whenever DensityRestriction is zero for all markets. Can be computed instantly.
#returns a vector of equilibrium prices
ClearHousingMarkets0 <- function(eq_objectsDF, MainDF, house_spshare) {
  
  Spending <- rep(0, nrow(eq_objectsDF)) #starting running total
  
  #calculating total housing spending
  for (j in 1:7) {
    ability_grp <- paste("ability_grp", j, sep = "") #name of ability group
    households_College <- paste("households_College", j, sep = "")
    households_NoCollege <- paste("households_NoCollege", j, sep = "")
   
    Spending <- Spending + house_spshare*eq_objectsDF[[ability_grp]]*eq_objectsDF$CollegeWage*eq_objectsDF[[households_College]]
    Spending <- Spending + house_spshare*eq_objectsDF[[ability_grp]]*eq_objectsDF$NoCollegeWage*eq_objectsDF[[households_NoCollege]]
  }
  
  price <- (Spending/(MainDF$lambda*MainDF$res_land_acres))^(1/(MainDF$converted_elasticities + 1))
  return(price)
}

#testing
ClearHousingMarkets0(equilibrium_objects, Main, Beta)


#________________________________CONSTRUCT AMENITY INDICES_______________________________________________________
UpdateAmenities <- function(eq_objectsDF, MainDF, amenity_elasticity) {
  
  Amenities <- eq_objectsDF %>% select(State, County, Tract, BlockGroup) #instantiating output
  
  #Calculating implied income per capita
  
  eq_objectsDF <- eq_objectsDF %>% mutate(hh_avg_income = ((households_College1*ability_grp1 + households_College2*ability_grp2 +
                                                            households_College3*ability_grp3 + households_College4*ability_grp4 + 
                                                            households_College5*ability_grp5 + households_College6*ability_grp6 + 
                                                            households_College7*ability_grp7)*CollegeWage +
                                                            
                                                           (households_NoCollege1*ability_grp1 + households_NoCollege2*ability_grp2 +
                                                            households_NoCollege3*ability_grp3 + households_NoCollege4*ability_grp4 + 
                                                            households_NoCollege5*ability_grp5 + households_NoCollege6*ability_grp6 + 
                                                            households_NoCollege7*ability_grp7)*NoCollegeWage)/
                                                            
                                                            (households_College1 + households_College2 +
                                                             households_College3 + households_College4 + 
                                                             households_College5 + households_College6 + 
                                                             households_College7 +
                                                             households_NoCollege1 + households_NoCollege2 +
                                                             households_NoCollege3 + households_NoCollege4 + 
                                                             households_NoCollege5 + households_NoCollege6 + 
                                                             households_NoCollege7))
  
  for (i in 1:7) {
    
    Amenities[paste("withincity_amenity_College", i, sep = "")] <- Main[[paste("exogenous_Amenity_College", i, sep = "")]]*(eq_objectsDF$hh_avg_income^(amenity_elasticity))
    Amenities[paste("withincity_amenity_NoCollege", i, sep = "")] <- Main[[paste("exogenous_Amenity_NoCollege", i, sep = "")]]*(eq_objectsDF$hh_avg_income^(amenity_elasticity))
     
  }
  
  return(Amenities)
  
}

#Testing function
testAmenities <- UpdateAmenities(equilibrium_objects, Main, Omega)
max(abs(testAmenities$withincity_amenity_College1 - Main$withincity_amenity_College1)) #max difference of approximately 1.42085e^-14.
#Gets very close to actual amenities levels-- but some minor error due to machine precision. 
rm(testAmenities)

#Updating data frame
for (i in 1:7) {
  equilibrium_objects[paste("withincity_amenity_College", i, sep = "")] <- Main[paste("withincity_amenity_College", i, sep = "")]
  equilibrium_objects[paste("withincity_amenity_NoCollege", i, sep = "")] <- Main[paste("withincity_amenity_NoCollege", i, sep = "")]
  equilibrium_objects[paste("acrosscity_amenity_College", i, sep = "")] <- Main[paste("acrosscity_amenity_College", i, sep = "")]
  equilibrium_objects[paste("acrosscity_amenity_NoCollege", i, sep = "")] <- Main[paste("acrosscity_amenity_NoCollege", i, sep = "")]
}



#____________________________CONSTRUCT CONSUMPTION INDICES_____________________________________________________
UpdateConsumptionIndex <- function(eq_objectsDF, MainDF, house_spendsh) {
  
  #Data frame to return
  Consumption <- eq_objectsDF %>% select(State, County, Tract, BlockGroup)
  
  #Income stringency of lot size regulation in each block group
  eq_objectsDF["Incstr_lot_size"] <- MainDF$DensityRestriction*MainDF$lambda*(eq_objectsDF$housePrice^(MainDF$converted_elasticities + 1))
  
  for (i in 1:7) {
    ability_grp <- paste("ability_grp", i, sep = "") #measure of center for each income group
    
    #COLLEGE WORKERS
    Consumption[paste("consumption_val_College", i , sep="")] <- ifelse(house_spendsh*eq_objectsDF$CollegeWage*eq_objectsDF[[ability_grp]] > eq_objectsDF$Incstr_lot_size, 1, 0)* #If Unconstrained in that market
                                                                        (house_spendsh^(house_spendsh)*(1-house_spendsh)^(1-house_spendsh))*(eq_objectsDF$CollegeWage*eq_objectsDF[[ability_grp]])/(eq_objectsDF$housePrice^(house_spendsh)) +
                                                                 
                                                                 ifelse((house_spendsh*eq_objectsDF$CollegeWage*eq_objectsDF[[ability_grp]] <= eq_objectsDF$Incstr_lot_size) & #If constrained, but can afford
                                                                        (eq_objectsDF$CollegeWage*eq_objectsDF[[ability_grp]] > eq_objectsDF$Incstr_lot_size), 1, 0)*
                                                                         ((eq_objectsDF$Incstr_lot_size/eq_objectsDF$housePrice)^(house_spendsh))*((eq_objectsDF$CollegeWage*eq_objectsDF[[ability_grp]] - eq_objectsDF$Incstr_lot_size)^(1-house_spendsh))  
                                                       
    
    Consumption[[paste("consumption_val_College", i , sep="")]][is.nan(Consumption[[paste("consumption_val_College", i , sep="")]])] <- 0 #Replacing priced out households with 0 (all other households)
   
    #NON-COLLEGE WORKERS
    Consumption[paste("consumption_val_NoCollege", i , sep="")] <- ifelse(house_spendsh*eq_objectsDF$NoCollegeWage*eq_objectsDF[[ability_grp]] > eq_objectsDF$Incstr_lot_size, 1, 0)* #If Unconstrained in that market
                                                                        (house_spendsh^(house_spendsh)*(1-house_spendsh)^(1-house_spendsh))*(eq_objectsDF$NoCollegeWage*eq_objectsDF[[ability_grp]])/(eq_objectsDF$housePrice^(house_spendsh)) +
      
                                                                   ifelse((house_spendsh*eq_objectsDF$NoCollegeWage*eq_objectsDF[[ability_grp]] <= eq_objectsDF$Incstr_lot_size) & #If constrained, but can afford
                                                                          (eq_objectsDF$NoCollegeWage*eq_objectsDF[[ability_grp]] > eq_objectsDF$Incstr_lot_size), 1, 0)*
                                                                          ((eq_objectsDF$Incstr_lot_size/eq_objectsDF$housePrice)^(house_spendsh))*((eq_objectsDF$NoCollegeWage*eq_objectsDF[[ability_grp]] - eq_objectsDF$Incstr_lot_size)^(1-house_spendsh)) 
                                                                     
          
    Consumption[[paste("consumption_val_NoCollege", i , sep="")]][is.nan(Consumption[[paste("consumption_val_NoCollege", i , sep="")]])] <- 0 #Replacing priced out households with 0
  }
  
  return(Consumption)
  
}


#testing...
testConsumption <- UpdateConsumptionIndex(equilibrium_objects, Main, Beta)
max(abs(testConsumption$consumption_val_College3 - Main$consumption_val_College3))
max(abs(testConsumption$consumption_val_NoCollege2 - Main$consumption_val_NoCollege2))#Looks good up to machine precision error!
rm(testConsumption)

#Updating data frame with initial values.
for (i in 1:7) {
  equilibrium_objects[paste("consumption_val_College", i , sep="")] <- Main[paste("consumption_val_College", i , sep="")]
  equilibrium_objects[paste("consumption_val_NoCollege", i , sep="")] <- Main[paste("consumption_val_NoCollege", i , sep="")]
}


#_________________________________________________________________________________________________________________________________________________________________________________
#SOLVE COUNTERFACTUAL OF REMOVING MINIMUM LOT SIZES ENTIRELY. 

#Given an initial population (observed in the ACS), 
#Calculate implied wages, utility at those wages, housing prices
#This determines the future path of workers according to the migration equation
#Stop when reaching a fixed point in the migration function. 




#Breaking code if test_zero_equilibrium and test_initial_equilibrium are both 1
if (test_zero_equilibrium == 1 & test_initial_equilibrium == 1) {
  stop("Cannot test the zero and initial equilibrium simultaneously")
}


if (test_zero_equilibrium == 1) {
  Main["DensityRestriction"] <- rep(0, nrow(Main))
}

  
iter_no = 1 #Initial iteration

#Saving main data frame for use with other programs
if (test_initial_equilibrium == 1) {
save(Main, file = "Data/Counterfactuals/CounterfactualDataOutput/Main.Rdata")
}

#Instantiating household distribution given above values.
new_equilibrium_objects <- equilibrium_objects %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME)

Max_vector_College <- c(1, 1, 1, 1, 1, 1, 1)
Max_vector_NoCollege <- c(1, 1, 1, 1, 1, 1, 1) #-- for checking how close current iteration is to new iteration  
Max_vector <- max(pmax(Max_vector_College, Max_vector_NoCollege)) #initial distance under max norm

#Speed of adjustment -- set low if out-of-equilibrium dynamics are too crazy (depends on what you're trying to compute)
adjustment_speed <- 0.05

#START LOOP HERE
while (Max_vector > 0.99) { #TOLERANCE FOR Minimum Distance

  #MODULE 1.1: UPDATE WAGES AT CURRENT POP DISTRIBUTION
  equilibrium_objects["CollegeWage"] <- CalculateWages(equilibrium_objects, Main, Sigma)[[1]] #Extract vector labeled 1 in returned list of function
  equilibrium_objects["NoCollegeWage"] <- CalculateWages(equilibrium_objects, Main, Sigma)[[2]]
  
  #MODULE 1.2: UPDATE HOUSING PRICES TO CLEAR MARKETS (TAKES 1 MIN PER ITERATION ON ALL 11 THREADS)
  if (test_zero_equilibrium != 1 & test_initial_equilibrium != 1) { #use more difficult solution process if not testing an equilibrium with DensityRestriction = 0
    equilibrium_objects["housePrice"] <- foreach (row = 1:nrow(equilibrium_objects), .combine = 'c') %dopar% {
                                                            optimize(f = ClearHousingMarkets,
                                                                     eq_objectsDF = equilibrium_objects,
                                                                      MainDF = Main, 
                                                                      house_spshare = Beta,
                                                                      i = row,
                                                                      interval = c(0, 10000))$minimum #searching over this interval of housing prices.

    }
  }
  
  if (test_zero_equilibrium == 1) {
    equilibrium_objects["housePrice"] <- ClearHousingMarkets0(equilibrium_objects, Main, Beta) #use closed form solution whenever all DensityRestrictions are zero. Much faster.
  }
  
  #MODULE 1.3: UPDATE AMENITIES 
  temp <- UpdateAmenities(equilibrium_objects, Main, Omega) #temporary dataframe to store each column
  for (i in 1:7) {
    equilibrium_objects[paste("withincity_amenity_College", i, sep = "")] <- temp[paste("withincity_amenity_College", i, sep = "")]
    equilibrium_objects[paste("withincity_amenity_NoCollege", i, sep = "")] <- temp[paste("withincity_amenity_NoCollege", i, sep = "")]
  }
  
  #MODULE 1.4: UPDATE CONSUMPTION VALUES
  temp <- UpdateConsumptionIndex(equilibrium_objects, Main, Beta) #temporary dataframe to store each column
  for (i in 1:7) {
    equilibrium_objects[paste("consumption_val_College", i , sep="")] <- temp[paste("consumption_val_College", i , sep="")]
    equilibrium_objects[paste("consumption_val_NoCollege", i , sep="")] <- temp[paste("consumption_val_NoCollege", i , sep="")]
  }
  rm(temp)
  
    

  #Constructing new population distribution on current iteration
  for (i in 1:7) { #Looping over income groups...
  
    #COLLEGE WORKERS
    #first, calculate what the conditional welfare would be of someone who chose CBSA c 
    equilibrium_objects <- equilibrium_objects %>% group_by(CBSA) %>% mutate(!!sym(paste("wc_Amenity_College", i, sep = "")) := 
                                                                            log(sum(exp(me_within[i]*!!sym(paste("consumption_val_College", i , sep="")))*!!sym(paste("withincity_amenity_College", i, sep = "")))))
  
    #calculating fraction of workers in i conditional on CBSA
    equilibrium_objects[paste("wc_hhfr_College", i, sep = "")] <- ((exp(me_within[i]*equilibrium_objects[[paste("consumption_val_College", i , sep="")]])*
                                                                  equilibrium_objects[[paste("withincity_amenity_College", i, sep = "")]])/exp(equilibrium_objects[[paste("wc_Amenity_College", i, sep = "")]]))
    #should sum to 1 within MSAs...
  
    #Now, calculating the fraction of workers choosing CBSA c.Note, need to inversely weight by number of block groups in MSA since this data frame is not collapsed.
    equilibrium_objects[paste("ac_hhfr_College", i, sep = "")] <- (exp(me_across[i]*equilibrium_objects[[paste("wc_Amenity_College", i, sep = "")]])*equilibrium_objects[[paste("acrosscity_amenity_College", i, sep = "")]])/
                                                                (sum(exp(me_across[i]*equilibrium_objects[[paste("wc_Amenity_College", i, sep = "")]])*equilibrium_objects[[paste("acrosscity_amenity_College", i, sep = "")]]/equilibrium_objects$MSAcount))

    #Should sum to 1 across cities, and it does.    
      
    #Lastly, updating total number of households in equilibrium_objects
    new_equilibrium_objects[paste("households_College", i, sep = "")] <- equilibrium_objects[[paste("ac_hhfr_College", i, sep = "")]]*
                                                                   equilibrium_objects[[paste("wc_hhfr_College", i, sep = "")]]*
                                                                   equilibrium_objects[paste("tot_households_College", i, sep = "")] #total number of households chosen given amenities and consumption indices
    #Should sum to original value of total households (very close up to 9 decimal machine precision)
  
    #NON-COLLEGE WORKERS
    #first, calculate what the conditional welfare would be of someone who chose CBSA c 
    equilibrium_objects <- equilibrium_objects %>% group_by(CBSA) %>% mutate(!!sym(paste("wc_Amenity_NoCollege", i, sep = "")) := 
                                                                               log(sum(exp(me_within[i]*!!sym(paste("consumption_val_NoCollege", i , sep="")))*!!sym(paste("withincity_amenity_NoCollege", i, sep = "")))))
    
    #calculating fraction of workers in i conditional on CBSA
    equilibrium_objects[paste("wc_hhfr_NoCollege", i, sep = "")] <- ((exp(me_within[i]*equilibrium_objects[[paste("consumption_val_NoCollege", i , sep="")]])*
                                                                      equilibrium_objects[[paste("withincity_amenity_NoCollege", i, sep = "")]])/exp(equilibrium_objects[[paste("wc_Amenity_NoCollege", i, sep = "")]]))
    #should sum to 1 within MSAs...
    
    #Now, calculating the fraction of workers choosing CBSA c.Note, need to inversely weight by number of block groups in MSA since this data frame is not collapsed.
    equilibrium_objects[paste("ac_hhfr_NoCollege", i, sep = "")] <- (exp(me_across[i]*equilibrium_objects[[paste("wc_Amenity_NoCollege", i, sep = "")]])*equilibrium_objects[[paste("acrosscity_amenity_NoCollege", i, sep = "")]])/
                                                                  (sum(exp(me_across[i]*equilibrium_objects[[paste("wc_Amenity_NoCollege", i, sep = "")]])*equilibrium_objects[[paste("acrosscity_amenity_NoCollege", i, sep = "")]]/equilibrium_objects$MSAcount))
    
    #Should sum to 1 across cities, and it does.    
    
    #Lastly, updating total number of households in equilibrium_objects
    new_equilibrium_objects[paste("households_NoCollege", i, sep = "")] <- equilibrium_objects[[paste("ac_hhfr_NoCollege", i, sep = "")]]*
                                                                         equilibrium_objects[[paste("wc_hhfr_NoCollege", i, sep = "")]]*
                                                                         equilibrium_objects[paste("tot_households_NoCollege", i, sep = "")] #total number of households chosen given amenities and consumption indices
                                                                         #Should sum to original value of total households (very close up to 9 decimal machine precision)
  }

#Evaluating the distance between Old and New population distributions
  for (i in 1:7) {
    Max_vector_College[i] <-  max(abs(new_equilibrium_objects[[paste("households_College", i, sep = "")]] - equilibrium_objects[[paste("households_College", i, sep = "")]]))
    Max_vector_NoCollege[i] <-  max(abs(new_equilibrium_objects[[paste("households_NoCollege", i, sep = "")]] - equilibrium_objects[[paste("households_NoCollege", i, sep = "")]]))
  }

  #distance between old and new (max norm)
  Max_vector <- max(pmax(Max_vector_College, Max_vector_NoCollege))

  #UPDATING POPULATION DISTRIBUTION VALUES
  for (i in 1:7) {
    equilibrium_objects[paste("households_College", i, sep = "")] <- equilibrium_objects[paste("households_College", i, sep = "")] +  
                                                                      adjustment_speed*(new_equilibrium_objects[[paste("households_College", i, sep = "")]] - equilibrium_objects[paste("households_College", i, sep = "")])
    equilibrium_objects[paste("households_NoCollege", i, sep = "")] <- equilibrium_objects[paste("households_NoCollege", i, sep = "")] +  
                                                                      adjustment_speed*(new_equilibrium_objects[[paste("households_NoCollege", i, sep = "")]] - equilibrium_objects[paste("households_NoCollege", i, sep = "")])
  }


  
  #Printing info
  
  print(paste("The current iteration is", iter_no, sep = " "))
  print(paste("The current Cauchy distance is", Max_vector, sep = " "))  
  
  #Updating iteration number
  iter_no <- iter_no + 1
  
}#END WHILE LOOP

#Saving output of the counterfactuals for analysis with other programs.
#1) Main counterfactual equilibrium
#2) The importance of endogenous amenities
#3) The importance of the Hsieh Moretti mechanism.
#4) The importance of within-city sorting in its interaction with 2) and 3). 

#Baseline counterfactual
if (test_zero_equilibrium == 1 & Omega > 0) {
  save(equilibrium_objects, file = "Data/Counterfactuals/CounterfactualDataOutput/Counterfactual_Amenities.Rdata")
  
}

if (test_zero_equilibrium == 1 & Omega == 0) {
  save(equilibrium_objects, file = "Data/Counterfactuals/CounterfactualDataOutput/Counterfactual_NoAmenities.Rdata")
  
}

if (test_initial_equilibrium == 1) { #Saving initial equilibrium objects to calculate welfare
  save(equilibrium_objects, file = "Data/Counterfactuals/CounterfactualDataOutput/initial_equilibrium.Rdata")
}

rm(list=(ls()[ls()!="me_within" & ls()!="me_across"])) #removing all objects but these for use with other programs


