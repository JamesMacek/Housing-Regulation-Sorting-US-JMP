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

#This file calculates the slopes of block groups whose centroids lie 
#within the donut-- outside of the exclusion buffer and within the inclusion buffer.

#Registering doParallel
#ncores  <- min(detectCores() - 1, 6)
ncores  <- detectCores() - 1

#Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
registerDoParallel(ncores)

#date created: October 4th, 2022
#IMPORTING PARAMETERS
source("CodeV2/Instrument/Parameters/Instrument_Parameters.R")


EXCLUSION_BUFFER <- Exclusion_buffers_para 
INCLUSION_BUFFER <- Inclusion_buffers_para #vector of buffers you want to try
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

#___________!INSTRUMENT LIST!__________________________
donut_instrument_list <- c("Avg_Slope") 

US_BLOCK <- US_BLOCK %>% select(State, County, Tract, BlockGroup, CBSA, starts_with(donut_instrument_list))

#Check if data are in meters
st_crs(US_BLOCK)$units #'m'


#_____________________________________________
#Loop over individual MSAs (start loop here)
US_BLOCK_LIST <- foreach(loop_MSA = 1:length(unique(US_BLOCK$CBSA)), .packages = c("dplyr", "collapse", "doParallel", "sf")) %dopar% {  
  
  print(paste0("MSA start at ", Sys.time()))
  #Setting up MSA data frame (note this means only within MSA slopes included)
  US_BLOCK_MSA <- US_BLOCK[US_BLOCK$CBSA == unique(US_BLOCK$CBSA)[loop_MSA],]
  US_BLOCK_MSA <- US_BLOCK_MSA %>% arrange(State, County, Tract, BlockGroup) %>% mutate(row_index = row_number()) #Making sure dataset arranged properly
  
  #Start specification loop here
  for (s in 1:length(EXCLUSION_BUFFER)) {
    
    #Constructing included and excluded polygons
    #______________________________
    #Buffer 1-- exclusion region
    Excluded_polygon <- st_buffer(select(US_BLOCK_MSA, State, County, 
                                         Tract, BlockGroup, CBSA), dist = EXCLUSION_BUFFER[s])  #Buffer1 -- exclusion region 
    #Buffer 2-- inclusion region
    Included_polygon <- st_buffer(select(US_BLOCK_MSA, State, County, 
                                         Tract, BlockGroup, CBSA), dist = INCLUSION_BUFFER[s]) #Buffer2 -- inclusion region
    
    #Taking out excluded polygon from included
    Included_polygon <- foreach(row = 1:nrow(US_BLOCK_MSA), .combine = "rbind") %do% {
      
      return( st_difference( Included_polygon[row, ], select(Excluded_polygon, geometry)[row, ] )) 
      
    }
    
    
    #Taking out own block group slopes from excluded polygon, will be controlled for separately
    Excluded_polygon  <- foreach(row = 1:nrow(US_BLOCK_MSA), .combine = "rbind") %do% {
      
      return( st_difference( Excluded_polygon[row, ], select(US_BLOCK_MSA, geometry)[row, ] )) 
      
    }
    
    #____________________________________________________________________________________________
    #Taking spatial join for both included and excluded polygons
    
    #First, for included polygon
    Included <- st_intersection(Included_polygon, select(US_BLOCK_MSA, row_index, starts_with(donut_instrument_list))) #row_index is the slopes that need to be aggregated 
    
    #Spatially aggregating to block group level (not intersection level)
    Included["land_area"] <- as.numeric(st_area(Included))
    Included <- st_drop_geometry(Included)
    Included <- collap(Included, Avg_Slope ~ State + County + Tract + BlockGroup, w = Included$land_area) #collapsing data frame to block group level
    
    #Second, for excluded polygon
    Excluded <- st_intersection(Excluded_polygon, select(US_BLOCK_MSA, row_index, starts_with(donut_instrument_list))) #row_index is the slopes that need to be aggregated 
    
    #Spatially aggregating to block group level (not intersection level)
    Excluded["land_area"] <- as.numeric(st_area(Excluded))
    Excluded <- st_drop_geometry(Excluded)
    Excluded <- collap(Excluded, Avg_Slope ~ State + County + Tract + BlockGroup, w = Excluded$land_area) #collapsing data frame to block group level
    
    #Making sure dataset sorted
    Included <- Included %>% arrange(State, County, Tract, BlockGroup)
    Excluded <- Excluded %>% arrange(State, County, Tract, BlockGroup)
    
    
    #_Putting this all back in the MSA dataframe and returning! (note, needs to work for arbitrary variable list)
    for (varname in donut_instrument_list) {
      US_BLOCK_MSA[[paste(paste("instrument", varname, sep = "_"), s, sep = "_spec_")]] <- Included[[varname]]
      US_BLOCK_MSA[[paste(paste("control", varname, sep = "_"), s, sep = "_spec_")]] <- Excluded[[varname]]
    }
    
  } #End loop over specifications
  
  return(US_BLOCK_MSA) #Returning dataframe
  print(paste0("MSA complete at ", Sys.time()))
  
}#End loop over MSAs
stopImplicitCluster()

#Taking list, appending to data frame for output
US_BLOCK_APPENDED <- bind_rows(US_BLOCK_LIST)
rm(US_BLOCK_LIST)

#Saving to stata format
US_BLOCK_APPENDED <- US_BLOCK_APPENDED %>% dplyr::select(starts_with(c("instrument", "control", "Avg", "State", "County",
                                                                       "Tract", "BlockGroup", "CBSA"))) %>% st_drop_geometry()

#outputting to STATA.
write_dta(US_BLOCK_APPENDED, "Datav2/US_Data/Instrument/Instrument_constructed.dta")
rm(list = ls())