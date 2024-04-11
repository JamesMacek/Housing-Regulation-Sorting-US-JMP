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
ncores  <- min(detectCores() - 1, 6)

#Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
registerDoParallel(ncores)

#date created: October 4th, 2022
#IMPORTING PARAMETERS
source("CodeV2/Instrument/Parameters/Instrument_Parameters.R")


EXCLUSION_BUFFER <- Exclusion_buffers_para 
INCLUSION_BUFFER <- Inclusion_buffers_para #vector of buffers you want to try
#Index of vectors above determine the specification-- i.e. Spec 1 is 200m exclusion, 1000m inclusion
#Each vector above should have the same length

#Taking value of maximum buffer as an additional outer control
max_buffer <- max(INCLUSION_BUFFER)

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

for (varname in donut_instrument_list) {
  for (s in 1:length(EXCLUSION_BUFFER)) {
  
    US_BLOCK[paste(paste("instrument", varname, sep = "_"), s, sep = "_spec_")] <- rep(NA, nrow(US_BLOCK))
  
  }
}

US_BLOCK_APPENDED <- data.frame() #final output (appended dataset)

#Check if data are in meters
st_crs(US_BLOCK)$units #'m'


#Loop over individual MSAs (start loop here)
US_BLOCK_LIST <- foreach(loop_MSA = 1:length(unique(US_BLOCK$CBSA)), .packages = c("dplyr", "doParallel", "sf")) %dopar% {  
  
                      US_BLOCK_MSA <- US_BLOCK[US_BLOCK$CBSA == unique(US_BLOCK$CBSA)[loop_MSA],]
  
                      US_BLOCK_MSA <- US_BLOCK_MSA %>% mutate(row_index = row_number())
  
                      #Centroid geography
                      US_BLOCK_MSA_CENTROID <- st_centroid(US_BLOCK_MSA)
                      
                      #Constructing outer control buffer (as additional control) 
                      Outer_polygon <- st_buffer(US_BLOCK_MSA, dist = max_buffer)
                      Outer <- st_intersects(Outer_polygon, dist = US_BLOCK_MSA_CENTROID)
                      
                      #Start specification loop here for different donut constructions
                      for (s in 1:(length(EXCLUSION_BUFFER) - 1) ) { #looping over all specifications + final control specification. Note, we do up to -1 because last donut not identified
    
                          #Included and excluded polygons
                          Excluded_polygon <- st_buffer(US_BLOCK_MSA, dist = EXCLUSION_BUFFER[s]) #Buffer1 -- exclusion region
                          Included_polygon <- st_buffer(US_BLOCK_MSA, dist = INCLUSION_BUFFER[s]) #Buffer2 -- inclusion region
                  
                        
                          #Take all pairwise polygons whose centriods intersect the inclusion buffer but not the exclusion buffer
                          Excluded <- st_intersects(Excluded_polygon, US_BLOCK_MSA_CENTROID)
                          Included <- st_intersects(Included_polygon, US_BLOCK_MSA_CENTROID)
                          
  
                          #Loop over block groups in the data 
                          foreach(loop_row = 1:nrow(US_BLOCK_MSA)) %do% { #do not doParallel here, requires objects continuously updating in environment
                            
                            #Take list of intersecting rows at current block group
                            included_row <- Included[[loop_row]] 
                            excluded_row <- Excluded[[loop_row]]
                            outer_row <- Outer[[loop_row]]
                            
                            #Incides of outer control buffer (past included buffer of excluded instruments)
                            outer_row <- outer_row[!outer_row %in% included_row]
                            
                            #Indices of instruments past inner buffer
                            included_row <- included_row[!included_row %in% excluded_row] #Delete intersection of included and excluded rows (including own row)
                            
                         
                            
                            #Take average of variables in list
                            
                            #Extracting dataframes of relevant neighbors
                            neighbors <- US_BLOCK_MSA_CENTROID[US_BLOCK_MSA_CENTROID$row_index %in% included_row,]
                            excl_neighbors <- US_BLOCK_MSA_CENTROID[US_BLOCK_MSA_CENTROID$row_index %in% excluded_row,]
                            outer_neighbors <- US_BLOCK_MSA_CENTROID[US_BLOCK_MSA_CENTROID$row_index %in% outer_row,]
                            
                            for (varname in donut_instrument_list) {
                                US_BLOCK_MSA[[paste(paste("instrument", varname, sep = "_"), s, sep = "_spec_")]][loop_row] <- mean(neighbors[[varname]], na.rm = TRUE)
                                US_BLOCK_MSA[[paste(paste("control", varname, sep = "_"), s, sep = "_spec_")]][loop_row] <- mean(excl_neighbors[[varname]], na.rm = TRUE) #inner control buffer
                                US_BLOCK_MSA[[paste(paste("outer_control", varname, sep = "_"), s, sep = "_spec_")]][loop_row] <- mean(outer_neighbors[[varname]], na.rm = TRUE) #outer control buffer
                            } #also-- this constructs something like a population-weighted average, which might explain why a direct buffer is irrelevant as an instrument
                            
                            
                          } #NaNs that appear from this procedure must come from MSA's with no neighbors. End loop over rows
                        } #end loop over specifications
  
                      return(US_BLOCK_MSA)
}#End loop over MSAs
stopImplicitCluster()

#Taking list, appending to data frame for output
US_BLOCK_APPENDED <- bind_rows(US_BLOCK_LIST)
rm(US_BLOCK_LIST)

#Saving to stata format
US_BLOCK_APPENDED <- US_BLOCK_APPENDED %>% dplyr::select(starts_with(c("instrument", "control", "outer", "Avg", "Max", "State", "County",
                                                                       "Tract", "BlockGroup", "CBSA"))) %>% st_drop_geometry()

#outputting to STATA.
write_dta(US_BLOCK_APPENDED, "Datav2/US_Data/Instrument/Instrument_constructed.dta")
rm(list = ls())
