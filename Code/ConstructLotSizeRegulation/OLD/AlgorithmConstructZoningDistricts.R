#Date created: June 16th, 2022

#Note: with Zillow Data we are skipping over 
#Requires output from ConstructBlockTractLotSizeAggregates.do 

#Constructs zoning districts by grouping/clustering contiguous 2010 block groups on certain land use/lot size statistics, 
#using the algorithm of Campusano (2021)

#Runs algorithm on metric of share of lots in certain types of development (single family, multifamily, etc.) + lot size distribution of 
#single + multifamily, where applicable. 

library(dplyr)
library(haven)
library(labelled)
library(sf)
library(spdep) #to define zoning district
library(wordspace) #for row norms
library(Jmisc) #demean function

#import 

#Reading stata (takes a bit to import large datasets)
BlockGroupStats <- read_dta("Data/ZillowData/Output/BlockGroupLandUseLotSizeStats.dta")
BlockGroupStats["temp"] <- rep(1, nrow(BlockGroupStats))

#Reading shapefiles for block groups
US_BLOCK_2010 <- st_read("Data/US_Data/CensusBlockGroup2010/US_blck_grp_2010.shp") 
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'STATEFP10'] <- 'State'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'COUNTYFP10'] <- 'County'
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'TRACTCE10'] <- 'Tract' #Renaming these variables to join on State/County/Tract/BlockGroup
names(US_BLOCK_2010)[names(US_BLOCK_2010) == 'BLKGRPCE10'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract/BlockGroup

BlockGroupStats <- full_join(US_BLOCK_2010, BlockGroupStats, by = c("State", "County", "Tract", "BlockGroup"))
rm(US_BLOCK_2010)
BlockGroupStats <- BlockGroupStats[!is.na(BlockGroupStats$temp),]
BlockGroupStats <- BlockGroupStats %>% select(-temp)

#BlockGroupStats now a sf geometry. 

#START ALGORITHM -- CHOOSE ONE CBSA ONLY FOR TESTING
#Constructs new geometry that aggregates block groups up. 

#Set Delta in Campusano (2021)
i = 10180 #Constructing for Abeliene, Texas for testing

BlockGroupSubset <- BlockGroupStats[BlockGroupStats$CBSA == i,]
BlockGroupSubset <- BlockGroupSubset %>% mutate(ZoningDistrictID = row_number()) %>% group_by(ZoningDistrictID) #temporary zoning district IDs to initialize

BlockGroupMerged <- BlockGroupSubset %>% select(ZoningDistrictID, geometry) %>% group_by(ZoningDistrictID) #sf object to store geometry of merged zoning districts


#DELTA TOLERANCE
deltaTol <- 0.01 #set delta tolerance == 1

#Initialize Delta
delta <- 0

#_________start delta loop HERE___________

while (delta <= deltaTol) {
  

  #PART 1. Construct all contiguous neighborhoods in current iteration
  neighbors <- poly2nb(BlockGroupMerged) #row names doesn't work for poly2nb

  #PART 2. Calculate dissimilarity between all neighbors
  #initialize matrix to calculate disimilarity
  DisimilarityMat <- matrix(Inf, 
                          nrow = length(unique(BlockGroupSubset$ZoningDistrictID)),
                          ncol = length(unique(BlockGroupSubset$ZoningDistrictID)),
                          dimnames = list(unique(BlockGroupSubset$ZoningDistrictID), 
                                          unique(BlockGroupSubset$ZoningDistrictID))) #Inf means the are not neighbors after following function (to take minimum)
  CheckMat <- matrix(0, 
                     nrow = length(unique(BlockGroupSubset$ZoningDistrictID)),
                     ncol = length(unique(BlockGroupSubset$ZoningDistrictID)),
                     dimnames = list(unique(BlockGroupSubset$ZoningDistrictID), 
                                     unique(BlockGroupSubset$ZoningDistrictID))) #matrix that determines which have already been checked (to speed up code)
  
  for (kp in seq(1, nrow(BlockGroupMerged))) { #looping over all zoning districts in current iteration
      for (jp in neighbors[[kp]]) { #looping over all neighbors associated with district i 
                                #Can speed this up by appealing to symmetry
        
        #only do calculation for off diagonal, as it is symmetric
        if (CheckMat[kp, jp] == 0 & CheckMat[jp, kp] == 0) {
          
        
          #translating p subscript indices to incides of zoning districts
          k <- BlockGroupMerged$ZoningDistrictID[kp]
          j <- BlockGroupMerged$ZoningDistrictID[jp]
      
          #Calculating disimilarity for k, j alone, then for union of k and j
          dis_k <- BlockGroupSubset[BlockGroupSubset$ZoningDistrictID == k,] #subsetting polygons in k
          dis_k <- dis_k %>% select(SingleFamilyDummy, DuFourDummy, MultiFamilyDummy, CommercialDummy,
                                VacantDummy, SingleF_p10, SingleF_p25, SingleF_p50, SingleF_mean, SingleF_mode,
                                MultiF_p10, MultiF_p25, MultiF_p50, MultiF_mean, MultiF_mode) %>% st_drop_geometry()
      
          dis_k <- as.matrix(dis_k)
          dis_k <- as.matrix(demean(dis_k)) #Subtract column mean from zoning district K
          dis_k <- sum(rowNorms(dis_k, p = 2)^2)  #Take row-wise Euclidean norm and sum
      
          #Doing the same for j
          dis_j <- BlockGroupSubset[BlockGroupSubset$ZoningDistrictID == j,] #subsetting polygons in k
          dis_j <- dis_j %>% select(SingleFamilyDummy, DuFourDummy, MultiFamilyDummy, CommercialDummy,
                                VacantDummy, SingleF_p10, SingleF_p25, SingleF_p50, SingleF_mean, SingleF_mode,
                                MultiF_p10, MultiF_p25, MultiF_p50, MultiF_mean, MultiF_mode) %>% st_drop_geometry()
      
          dis_j <- as.matrix(dis_j)
          dis_j <- as.matrix(demean(dis_j)) #Subtract column mean from zoning district J
          dis_j <- sum(rowNorms(dis_j, p = 2)^2)  #Take row-wise sqaured Euclidean norm and sum
      
          #and the union of j and k
          dis_jk <- BlockGroupSubset[BlockGroupSubset$ZoningDistrictID == j | BlockGroupSubset$ZoningDistrictID == k,] #subsetting polygons in k
          dis_jk <- dis_jk %>% select(SingleFamilyDummy, DuFourDummy, MultiFamilyDummy, CommercialDummy,
                                VacantDummy, SingleF_p10, SingleF_p25, SingleF_p50, SingleF_mean, SingleF_mode,
                                MultiF_p10, MultiF_p25, MultiF_p50, MultiF_mean, MultiF_mode) %>% st_drop_geometry()
      
          dis_jk <- as.matrix(dis_jk)
          dis_jk <- as.matrix(demean(dis_jk)) #Subtract column mean from zoning district J
          dis_jk <- sum(rowNorms(dis_jk, p = 2)^2)  #Take row-wise sqaured Euclidean norm and sum
      
          #Calculating disimilarity on all variables we want to use
      
      
          DisimilarityMat[rownames(DisimilarityMat) %in% k, colnames(DisimilarityMat) %in% j] <- dis_jk - dis_j - dis_k
      
          #Updating CheckMat
          CheckMat[kp, jp] <- 1
        }
      } 
  
  }

  #Take (k, j) pair with minimum disimilarity
  ToMerge <- which(DisimilarityMat == min(DisimilarityMat), arr.ind = TRUE, useNames = TRUE) 
  ToMerge <- ToMerge[1, ] #Arbitrarily take first row (will contain same zoning district indices?)

  #PART 3. Merge neighborhoods that have minimum disimilarity. Update row names. 

  #Assign index to merged polygon as the minimum of the two merged 
  BlockGroupSubset$ZoningDistrictID[BlockGroupSubset$ZoningDistrictID == max(ToMerge)] <- min(ToMerge) 

  #Merge polygons in BlockGroupMerge object
  BlockGroupMerged$ZoningDistrictID[BlockGroupMerged$ZoningDistrictID == max(ToMerge)] <- min(ToMerge) 
  BlockGroupMerged <- BlockGroupMerged %>% group_by(ZoningDistrictID) %>% summarise(do_union = TRUE) #do_union merges polygons by group
  
  #Update delta
  delta <- min(DisimilarityMat)

}#end while loop!

#Try Heirarchial Clustering

