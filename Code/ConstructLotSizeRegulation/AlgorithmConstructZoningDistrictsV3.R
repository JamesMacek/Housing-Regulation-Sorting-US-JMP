#Date created: June 19th, 2022
#Date edited: June 20th, 2022

#Note: with Zillow Data we are skipping over clustering on actual zoning districts. 
#Requires output from ConstructBlockTractLotSizeAggregates.do 

#Constructs zoning districts by grouping/clustering close 2010 block groups on certain land use/lot size statistics, 

#Runs algorithm on metric of share of lots in certain types of development (single family, multifamily, etc.) + lot size distribution of 
#single + multifamily, where applicable. 

#Outputs "Data/ZillowData/Output/ConstructedZoningDistricts.dta" for use to detect minimum lot sizes. 

library(dplyr)
library(haven)
library(labelled)
library(sf)
library(spdep)
library(ClustGeo) #for spatially constrained clustering (parameterized by alpha). See CRAN documentation
library(cluster) #calculare silhouette score

#import 

#Reading stata (takes a bit to import large datasets)
BlockGroup <- read_dta("Data/ZillowData/Output/BlockGroupLandUseLotSizeStats.dta")
BlockGroup["temp"] <- rep(1, nrow(BlockGroup))

#Reading shapefiles for block groups (note, using sp objects for this exercise)
US_BLOCK_2010 <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp") 
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'STATEFP10'] <- 'State'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'COUNTYFP10'] <- 'County'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract/BlockGroup
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'BLKGRPCE10'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract/BlockGroup

BlockGroup <- full_join(US_BLOCK_2010, BlockGroup, by = c("State", "County", "Tract", "BlockGroup"))
rm(US_BLOCK_2010)
BlockGroup <- BlockGroup[!is.na(BlockGroup$temp),]
BlockGroup <- BlockGroup %>% select(-temp)

#Extracting centriods, they are only what are needed
BlockGroup <- st_centroid(BlockGroup)

#create new dataset to append
BlockGroupUpdated <- data.frame()

#List of variable names used for clustering on similarity
Vars <- c("SingleFamilyDummy", "DuFourDummy", "MultiFamilyDummy", "CommercialDummy",
          "VacantDummy", "SingleF_p10", "SingleF_p25", "SingleF_mode",
          "MultiF_p10", "MultiF_p25", "MultiF_mode")


#START LOOP OVER CBSAs HERE_________________________________________________________________________
start_time <- Sys.time()

choose_alpha <- 0 #Set to 1 if you want to choose alpha at every consideration
default_alpha <- 0.1 #0.1 appears to keep geographic cohesion high. defaults choice of alpha if choose_alpha == 0

minimum_no_cluster_factor <- 10 #i.e. want roughly 10 block groups per zoning district as a desired level of aggregation

for (i in unique(BlockGroup$CBSA)) {
#i <- 16980
  
  BlockGroupSubset <- BlockGroup[BlockGroup$CBSA == i,]
  rownames(BlockGroupSubset) <- NULL #removing rownames from old data

  #Extracting data
  Dat <- data.frame(BlockGroupSubset[, Vars]) #Extracting data
  Dat <- Dat %>% select(-geometry)

  #Rescaling data 
  Dat <- scale(Dat)
  Dat[is.nan(Dat)] <- 0 #occurs if variable is all 0, hence dividing by norm not defined in scale() function
  Dat <- data.frame(Dat)
  
  #Creating pairwise distances between centriods of polygons
  distGeo <- as.dist(st_distance(BlockGroupSubset)) #centriod distances
  
  #Pairwise distances based on clustering variables
  distSocial <- dist(Dat)

  #Begin choice of number of clusters
  max_NoCluster <- min(150, nrow(BlockGroupSubset) -1) #maximum number of clusters bounds to search over (this is fast so it doesn't matter)
  min_NoCluster <- floor(nrow(BlockGroupSubset)/minimum_no_cluster_factor) 
  
  #If minimum number of clusters is 1/0 (from porocedure), have to set to 2 for algorithm (for very small MSAs)
  if (min_NoCluster == 1 | min_NoCluster == 0) {
    min_NoCluster <- 2
  }
  
  #If maximum number of clusters is too large, set additional grid
  
  if (min_NoCluster >= max_NoCluster) { #shouldn't happen if max_NoCluster is set large. 
    max_NoCluster <- min_NoCluster + 150 #Set additional grid 
    
  }
  
  silhouette.store <- rep(-2, max_NoCluster) #storing silhouette coefficients for each choice of # of clusters
                      #note: -2 means that it wasn't calculated for that number of clusters
  
  if (i != 38540) { #SKIP LOOP IF IN SMALL CBSAs of 2 block groups -- i.e. Pocatello, Idaho 38540
  #START LOOP OVER NUMBER OF CLUSTERS HERE________________
  for(NoCluster in seq(min_NoCluster, max_NoCluster)) {
    
    clusterGroups <- seq(1, NoCluster) #indices for clusters
    
    if (choose_alpha == 1) { 
      #Choose geographic cohesion parameter
      ca <- choicealpha(D0 = distSocial, D1 = distGeo, 
                    range.alpha = seq(0, 1, 0.1), K = NoCluster) #choose alpha parameter, creates choicealpha object
      #Choose alpha maximizing the sum of explained inertia 
      ca.scores <- rowSums(ca$Qnorm)
      desired_alpha <- which(max(ca.scores) == ca.scores)
      desired_alpha <- ca$range.alpha[desired_alpha] #extract desired alpha from ca object given index
    
    }
    
    if (choose_alpha == 0){
      desired_alpha <- default_alpha
    }
    
    
    #Clustering
    Cluster <- cutree(hclustgeo(D0 = distSocial, D1 = distGeo, alpha = desired_alpha), NoCluster) #pretty fast! 
    
    silhouette.object <- silhouette(x = Cluster, dist = distSocial) #Cluster gives indices, distSocial gives distances
    silhouette.store[NoCluster] <- mean(silhouette.object[, 3]) #average silhouette score
      
  } #End loop over number of clusters  
  
  #extracting number of clusters associated with best match
  Desired_NoCluster <- which(max(silhouette.store) == silhouette.store)
  
  #Want minimum number of clusters proportional to number of block groups 
  
  
  #Setting desired cluster
  Cluster <- cutree(hclustgeo(D0 = distSocial, D1 = distGeo, alpha = desired_alpha), Desired_NoCluster)
  
  BlockGroupSubset["ZoningDistrictID"] <- Cluster #Storing clusters
  
  } #SKIP CLUSTERING FOR SMALL MSAs
  
  if (i == 38540) {
   BlockGroupSubset["ZoningDistrictID"] <- rep(1, nrow(BlockGroupSubset)) #If skipping MSAs, assign only one zoning district (for MSAs of size 2)
  }
  
  BlockGroupUpdated <- rbind(BlockGroupUpdated, BlockGroupSubset) #Updating new dataset
  
  print(i)  #printing CBSA in loop
} #END LOOP OVER CBSAs

end_time <- Sys.time()  #Crazy Fast-- approximately 7 seconds per MSA of size 2200. 
                        #Slow for very large MSAs--i.e. Los Angeles, Chicago, New York (curse of dimensionality) ~~ 5 minutes each

#Counting number of zoning districts
BlockGroupUpdated["uZoningDistrictID"] <- as.character(10000*as.numeric(BlockGroupUpdated$CBSA) + BlockGroupUpdated$ZoningDistrictID) #unique to county zoning district ID

#Selecting and saving
BlockGroupUpdated <- BlockGroupUpdated %>% select(uZoningDistrictID, ZoningDistrictID, 
                                                   CBSA, CBSA_NAME, State, County, Tract, BlockGroup)

#Saving in STATA format
write_dta(BlockGroupUpdated, "Data/ZillowData/Output/ConstructedZoningDistricts.dta")
