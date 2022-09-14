#Date created: June 16th, 2022
#Date edited: June 19th, 2022

#Note: with Zillow Data we are skipping over 
#Requires output from ConstructBlockTractLotSizeAggregates.do 

#Constructs zoning districts by grouping/clustering contiguous 2010 block groups on certain land use/lot size statistics, 
#using the SKATER clustering algorithm

#Runs algorithm on metric of share of lots in certain types of development (single family, multifamily, etc.) + lot size distribution of 
#single + multifamily, where applicable. 

#Uses the SKATER package

library(dplyr)
library(haven)
library(labelled)
library(sf)
library(spdep)
library(rgdal)
library(bigDM) #for random_partition function

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
start_time <- Sys.time()

#for (i in unique(BlockGroup$CBSA)) {
i <- 40140
  
BlockGroupSubset <- BlockGroup[BlockGroup$CBSA == i,]
rownames(BlockGroupSubset) <- NULL #removing rownames from old data

#Dividing Block Group Subset further into a random contiguous partition
#Ideally want ~ 150 block groups per partition. 

set.seed(1234) #set seed for random partition
BlockGroupSubset <- random_partition(carto = BlockGroupSubset, max.size = 350, min.size = 50, rows = 10, columns = 10) #algorithm will stall if size of MSA means no such thing exists) #125-180 observations, from bigDM package
                                                              #partition ID stored in ID.group


#Looping over all partitions in the random subset
for (part in unique(BlockGroupSubset$ID.group)) { #Start loop over MSA partitions

BlockGroupPSubset <-  BlockGroupSubset[BlockGroupSubset$ID.group == part,] #subsetting partition


#Taking neighbors of all block groups
BlockGroupSubset.neighbors <- poly2nb(BlockGroupPSubset)

#If some aren't neighbors, remove and set them as their own zoning district in master dataset.
#Otherwise, lcosts and others will fail (require no islands)

BlockGroupPSubset <- BlockGroupPSubset %>% mutate(temp = row_number()) #temporary row number index to remove islands

for (n in seq(1, nrow(BlockGroupPSubset))) {
  
  if (BlockGroupSubset.neighbors[[n]] == 0) {
    BlockGroupPSubset <- BlockGroupPSubset[BlockGroupPSubset$temp != n,] #remove n from data frame
  }
  
}

#Running neighbors again
BlockGroupSubset.neighbors <- poly2nb(BlockGroupPSubset)

#Constructing matrix of variables that we will cluster on 
Dat <- data.frame(BlockGroupPSubset[, Vars]) #Extracting data
Dat <- Dat %>% select(-geometry)
#Rescaling data
Dat <- scale(Dat)
#replacing NaN's with zeros
Dat[is.nan(Dat)] <- 0 #occurs if variable is all 0, hence dividing by norm not defined. 

#Constructing costs
lcosts <- nbcosts(nb = BlockGroupSubset.neighbors, data = Dat) #compute bilateral costs (pairwise disimilarities)
BlockGroupSubset.w <- nb2listw(BlockGroupSubset.neighbors, lcosts, style = "B") #calculate spatial weighting matrix from costs

#Minimum spanning tree for use with SKATER algorithm spdep::skater
BlockGroupSubset.mst <- mstree(BlockGroupSubset.w)


#PARAMETERS FOR CUT LOOP_____________________________________________
max_NoCluster <- 10 #maximum number of clusters bounds to search over

silhouette_store <- rep(-2, max_NoCluster) #storing silhouette coefficients for each choice of # of clusters, -2 because outside range of silhouette score

#LOOP OVER # of CUTS (Clusters) STARTS HERE
for(NoCluster in seq(1, max_NoCluster)) { #Start loop over number of clusters

#Indicies of cluster groups for use with program
clusterGroups <- seq(1, NoCluster)

if (NoCluster <= nrow(BlockGroupPSubset)) { #This should never happen but it does... unfortunatley. 
  
Cluster <- skater(BlockGroupSubset.mst[, 1:2], Dat, NoCluster - 1, method = "euclidean") #calculating clusters with skater

#Initializing Silhouette Score vector
  
  silhouette = rep(0, nrow(BlockGroupPSubset)) #silouette scores for each block group within NoCluster
  
  #Loop over each block group in BlockGroupSubset
  for (bg in seq(1, nrow(BlockGroupPSubset))) {
    
    #Section 1: Compute within-cluster similarity
    bg_Cluster_Elements <- which(Cluster$groups %in% Cluster$groups[bg]) #Extracting indices in same cluster as bg
    
    #Calculating Euclidean distance between Dat[bg] and Dat[bg_Cluster_Elements]
    
    distance = 0 #initializing distance counter
    for (n in bg_Cluster_Elements) { 
      distance <- distance + sqrt(sum((Dat[bg,] - Dat[n,])^2)) #Euclidean norm
    }
    
    wClusterSim <- 0
    
    singleton <- 1 #assigns default 0 
    
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
}#End if to check if size of data = no of clusters
}#End checking clusters

#extracting number of clusters associated with best match
Desired_NoCluster <- which(max(silhouette_store) == silhouette_store)

#Recomputing original cluster and assigning it to the data frame
#Should always work
Cluster <- skater(BlockGroupSubset.mst[, 1:2], Dat, Desired_NoCluster - 1, method = "euclidean")
BlockGroupPSubset["ZoningDistrictID"] <- Cluster$groups


BlockGroupUpdated <- rbind(BlockGroupUpdated, BlockGroupPSubset) # will have missing if 

} #end loop over within MSA partitions
#} #End loop over MSAs

end_time <- Sys.time()