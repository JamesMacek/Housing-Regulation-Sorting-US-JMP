#Date created: June 16th, 2022
#Date Edited: June 19th, 2022

#Note: with Zillow Data we are skipping over 
#Requires output from ConstructBlockTractLotSizeAggregates.do 

#Constructs zoning districts by grouping/clustering contiguous 2010 block groups on certain land use/lot size statistics, 
#using a Ward type clustering algorithm

#Runs algorithm on metric of share of lots in certain types of development (single family, multifamily, etc.) + lot size distribution of 
#single + multifamily, where applicable. 

#Uses the SKATER package

library(dplyr)
library(haven)
library(labelled)
library(sf)
library(spdep)
library(rgdal)

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

#create new dataset to append
BlockGroupUpdated <- data.frame()

#List of variable names used for clustering on similarity
Vars <- c("SingleFamilyDummy", "DuFourDummy", "MultiFamilyDummy", "CommercialDummy",
          "VacantDummy", "SingleF_p10", "SingleF_p25", "SingleF_p50", "SingleF_mean", "SingleF_mode",
          "MultiF_p10", "MultiF_p25", "MultiF_p50", "MultiF_mean", "MultiF_mode")

#LOOP OVER CBSAs STARTS HERE_____________________________
#for (i in unique(BlockGroup$CBSA)) {
i <- 40140
  
BlockGroupSubset <- BlockGroup[BlockGroup$CBSA == i,]
rownames(BlockGroupSubset) <- NULL #removing rownames from old data


Dat <- data.frame(BlockGroupSubset[, Vars]) #Extracting data
Dat <- Dat %>% select(-geometry)
#Rescaling data
Dat <- scale(Dat)

#Converting BlockGroupSubset to sp object
BlockGroupSubset.neighbors <- poly2nb(BlockGroupSubset)

#Constructing costs
lcosts <- nbcosts(nb = BlockGroupSubset.neighbors, data = Dat) #compute bilateral costs (pairwise disimilarities)
BlockGroupSubset.w <- nb2listw(BlockGroupSubset.neighbors, lcosts, style = "B") #calculate spatial weighting matrix from costs

#Minimum spanning tree for use with SKATER algorithm spdep::skater
BlockGroupSubset.mst <- mstree(BlockGroupSubset.w)


#PARAMETERS FOR CUT LOOP_____________________________________________
max_NoCluster <- 20 #maximum number of clusters bounds to search over

silhouette_store <- rep(0, max_NoCluster) #storing silhouette coefficients for each choice of # of clusters

#LOOP OVER # of CUTS (Clusters) STARTS HERE
for(NoCluster in seq(1, max_NoCluster)) { #Start loop over number of clusters

#Indicies of cluster groups for use with program
clusterGroups <- seq(1, NoCluster)

Cluster <- skater(BlockGroupSubset.mst[, 1:2], Dat, NoCluster - 1, method = "euclidean") #calculating clusters with skater

#Initializing Silhouette Score vector
  
  silhouette = rep(0, nrow(BlockGroupSubset)) #silouette scores for each block group within NoCluster
  
  #Loop over each block group in BlockGroupSubset
  for (bg in seq(1, nrow(BlockGroupSubset))) {
    
    #Section 1: Compute within-cluster similarity
    bg_Cluster_Elements <- which(Cluster$groups %in% Cluster$groups[bg]) #Extracting indices in same cluster as bg
    
    #Calculating Euclidean distance between Dat[bg] and Dat[bg_Cluster_Elements]
    
    distance = 0 #initializing distance counter
    for (n in bg_Cluster_Elements) { 
      distance <- distance + sqrt(sum((Dat[bg,] - Dat[n,])^2)) #Euclidean norm
    }
    
    wClusterSim <- 0
    
    singleton <- 1 #assigns default 1 if cluster only has 1 observation 
    
    if (length(bg_Cluster_Elements) > 1) {
    wClusterSim <- (1/(length(bg_Cluster_Elements) - 1))*distance
    singleton <- 0 #assigns 0 if observation is not a singleton 
    }
    
    #Section 2: Compute cross-cluster similarity
    bClusterSim <- seq(0, NoCluster - 1)
    
    other_cluster_no <- 1 #initializing number of other clusters
    for (other_cluster in clusterGroups[!clusterGroups %in% Cluster$groups[bg]]) {
      
      bg_Cluster_Elements <- which(Cluster$groups %in% other_cluster) #Extracting block groups in particular other cluster
      
      #Calculating Euclidean distance between Dat[bg] and Dat[bg_Cluster_Elements]
      distance = 0 #initializing distance counter
      for (n in bg_Cluster_Elements) { 
        distance <- distance + sqrt(sum((Dat[bg,] - Dat[n,])^2))#Euclidean norm
      }
      
      bClusterSim[other_cluster_no] <- 1/(length(bg_Cluster_Elements))*distance
      other_cluster_no <- other_cluster_no + 1 #updating other cluster number
      
    } #end loop over other clusters
    
    bClusterSim <- min(bClusterSim) #extracting cluster with minimum dissimilarity. 
    
    #Section 3: Putting it all together
    silhouette[bg] <- 0
    
    if (singleton == 0) {
      silhouette[bg] <- (bClusterSim - wClusterSim)/(max(wClusterSim, bClusterSim))
    }
    
  }

#Adding silhouette score associated with NoCluster into vector
silhouette_store[NoCluster] <- mean(silhouette)

}

#extracting number of clusters associated with best match
Desired_NoCluster <- which(max(silhouette_store) == silhouette_store)

#Recomputing original cluster and assigning it to the data frame
Cluster <- skater(BlockGroupSubset.mst[, 1:2], Dat, Desired_NoCluster - 1, method = "euclidean")
BlockGroupSubset["ZoningDistrictID"] <- Cluster$groups

BlockGroupUpdated <- rbind(BlockGroupUpdated, BlockGroupSubset)

#} #End loop over MSAs