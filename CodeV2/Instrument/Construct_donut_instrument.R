#PACKAGES
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(ggplot2)

#Sf library
library(sf)


#Parrallel computing
library(doParallel) #Parallelize for loops

#date created: October 4th, 2022

#PARAMETERS

EXCLUSION_BUFFER <- c(200, 500, 750, 1000) 
INCLUSION_BUFFER <- c(1000, 1000, 1250, 1500) #vector of buffers you want to try
#Index of vectors above determine the specification-- i.e. Spec 1 is 200m exclusion, 1000m inclusion
#Each vector above should have the same length

#Load data
US_BLOCK <- st_read("DataV2/US_Data/Instrument/Slope_Extracted.shp") %>% select(State, County, Tract, BlockGroup, CBSA, Avg_Slope)

#Merging with other donut variables

#Destringing variables for merge
US_BLOCK["State"] <- as.double(US_BLOCK$State)
US_BLOCK["County"] <- as.double(US_BLOCK$County)
US_BLOCK["Tract"] <- as.double(US_BLOCK$Tract)
US_BLOCK["BlockGroup"] <- as.double(US_BLOCK$BlockGroup)
US_BLOCK["CBSA"] <- as.double(US_BLOCK$CBSA)


#Merging other variables from CoreLogic/ACS
OtherDonutVariables <- read_dta("DataV2/US_Data/Instrument/Donut_variables_nonTopographic.dta")
US_BLOCK <- left_join(US_BLOCK, OtherDonutVariables, by = c("State", "County", "Tract", "BlockGroup", "CBSA"))
rm(OtherDonutVariables)

#Renaming variables to shorter codes
US_BLOCK <- US_BLOCK %>% rename(sqf = buildingsquarefeet,
                                livsqf = livingsquarefeetallbuildings,
                                fpind = fireplaceindicator, #Note: fireplace and poolindicators don't discriminate on missing data.
                                poolind = poolindicator,
                                bedr = bedroomsallbuildings,
                                trooms = totalroomsallbuildings,
                                tbath = totalbathroomsallbuildings,
                                dm_hdens = demeaned_Housing_density,
                                dm_cshare = demeaned_car_share,
                                dm_ptshare = demeaned_public_transport_share,
                                dm_bussh = demeaned_bus_share,
                                dm_avgt = demeaned_avg_travel_time,
                                dm_ACSyrbuilt = demeaned_median_bage) #Note -- this is median building age from the ACS. 

#Replace living square feet with building square feet if missing
US_BLOCK$livsqf[is.na(US_BLOCK$yearbuilt)] <- US_BLOCK$sqf[is.na(US_BLOCK$yearbuilt)] #185k observations on square footage

#___________!INSTRUMENT LIST!__________________________
donut_instrument_list <- c("Avg_Slope", "yearbuilt", "livsqf", "dm_hdens", "dm_cshare", "dm_ptshare",
                           "dm_bussh", "dm_avgt", "dm_ACSyrbuilt") 

US_BLOCK <- US_BLOCK %>% select(State, County, Tract, BlockGroup, CBSA, starts_with(donut_instrument_list))

for (varname in donut_instrument_list) {
  for (s in 1:length(EXCLUSION_BUFFER)) {
  
    US_BLOCK[paste(paste("instrument", varname, sep = "_"), s, sep = "_spec_")] <- rep(NA, nrow(US_BLOCK))
  
  }
}

US_BLOCK_APPENDED <- data.frame() #final output (appended dataset)

#Check if data are in meters
st_crs(US_BLOCK)$units #'m'


#Loop over individual MSAs (start loop here)
for (loop_MSA in unique(US_BLOCK$CBSA)) {  
  
  US_BLOCK_MSA <- US_BLOCK[US_BLOCK$CBSA == loop_MSA,]
  
  US_BLOCK_MSA <- US_BLOCK_MSA %>% mutate(row_index = row_number())
  
  #Centroid geography
  US_BLOCK_MSA_CENTROID <- st_centroid(US_BLOCK_MSA)
  
  #Start specification loop here
  for (s in 1:length(EXCLUSION_BUFFER)) {
  
  #Included and excluded polygons
  Excluded_polygon <- st_buffer(US_BLOCK_MSA, dist = EXCLUSION_BUFFER[s]) #Buffer1 -- exclusion region
  Included_polygon <- st_buffer(US_BLOCK_MSA, dist = INCLUSION_BUFFER[s]) #Buffer2 -- inclusion region

  #Take all pairwise polygons whose centriods intersect the inclusion buffer but not the exclusion buffer. 
  Excluded <- st_intersects(Excluded_polygon, US_BLOCK_MSA_CENTROID)
  Included <- st_intersects(Included_polygon, US_BLOCK_MSA_CENTROID)
  
  #Loop over rows in the data 
  for (loop_row in 1:nrow(US_BLOCK_MSA)) {
    #Take list of intersecting rows 
      included_row <- Included[[loop_row]] 
      excluded_row <- Excluded[[loop_row]]
      
      included_row <- included_row[!included_row %in% excluded_row]#Delete intersection of included and excluded rows (including own row)
      
      #Take average of variables in list
      neighbors <- US_BLOCK_MSA_CENTROID[US_BLOCK_MSA_CENTROID$row_index %in% included_row,]
      excl_neighbors <- US_BLOCK_MSA_CENTROID[US_BLOCK_MSA_CENTROID$row_index %in% excluded_row,]
      
      for (varname in donut_instrument_list) {
        US_BLOCK_MSA[[paste(paste("instrument", varname, sep = "_"), s, sep = "_spec_")]][loop_row] <- mean(neighbors[[varname]], na.rm = TRUE)
        US_BLOCK_MSA[[paste(paste("control", varname, sep = "_"), s, sep = "_spec_")]][loop_row] <- mean(excl_neighbors[[varname]], na.rm = TRUE)
      } 
  } #NaNs that appear from this procedure must come from MSA's with no neighbors. 
}
  
  US_BLOCK_APPENDED <- rbind(US_BLOCK_APPENDED, US_BLOCK_MSA) #Appending to dataset
}

#Saving to stata format
US_BLOCK_APPENDED <- US_BLOCK_APPENDED %>% dplyr::select(starts_with(c("instrument", "control", "Avg", "State", "County",
                                                                       "Tract", "BlockGroup", "CBSA"))) %>% st_drop_geometry()

#outputting to STATA.
write_dta(US_BLOCK_APPENDED, "Datav2/US_Data/Instrument/Instrument_constructed.dta")
rm(list = ls())
