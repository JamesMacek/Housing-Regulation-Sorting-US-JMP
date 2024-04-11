#Date Created: Dec 02, 2022

#This file clusters block groups into zoning districts.
#Use the output from collapse_stats_forClustering.do.

library(dplyr)
library(haven)
library(labelled)
library(sf)
library(spdep)
library(ClustGeo) #for spatially constrained clustering (parameterized by alpha). See CRAN documentation
library(cluster) #calculate silhouette score
library(doParallel) #for parallelization

#PART 0: Parallelization
#Checking number of cores on local PC
ncores  <- detectCores() - 1

#Setting up number of cores (we want to conserve ram as this program is reasonably ram intensive)
registerDoParallel(min(6, ncores))


#PART 1:______________________________PRELIMINARIES_____________________________
#Reading data to construct zoning districts
US_BLOCK <- read_dta("DataV2/CoreLogic/output/blkgrpstats_forClustering.dta")

#Reading shapefiles for block groups. Putting polygons in US_BLOCK.
US_BLOCK_shp <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") 
US_BLOCK_shp <- US_BLOCK_shp %>% mutate(State = as.numeric(STATEFP),
                                        County = as.numeric(COUNTYFP),
                                        Tract = as.numeric(TRACTCE),
                                        BlockGroup = as.numeric(BLKGRPCE)) %>%
                                 select(State, County, Tract, BlockGroup) #converting to numeric for potential merge

US_BLOCK["temp"] <- seq(1, nrow(US_BLOCK))
US_BLOCK <- left_join(US_BLOCK_shp, US_BLOCK, by = c("State", "County",
                                                     "Tract", "BlockGroup"))

US_BLOCK <- US_BLOCK[!is.na(US_BLOCK$temp),] %>% select(-temp)
rm(US_BLOCK_shp)

#Extracting centriods, they are only what are needed
US_BLOCK <- st_centroid(US_BLOCK)
US_BLOCK <- st_transform(US_BLOCK, 4269) #changing projection

#Joining with CBSA names to make stuff easier
US_CBSA_2010 <- st_read("DataV2/US_Data/Shapefiles/cb_2013_us_cbsa_500k.shp") #2013 
US_CBSA_2010 <- US_CBSA_2010[US_CBSA_2010$LSAD == "M1",]
US_CBSA_2010 <- US_CBSA_2010 %>% rename(CBSA = CBSAFP, CBSA_NAME = NAME) %>% 
                                 select(CBSA, CBSA_NAME) %>% st_drop_geometry() %>%
                                 mutate(CBSA = as.numeric(CBSA))

US_BLOCK <- left_join(US_BLOCK, US_CBSA_2010, by = c("CBSA"))

#create new dataset to append


#PART 2: __________________________

#Set of variable names to cluster on. Does not include zoning code dummies
#These will be created on each iteration to limit the amount of columns to be created.
#Do we want to use many variables? So called "curse of dimensionality" in clustering.
vars_toCluster <- c("keep_statmode",
                    "keep_statp10")  
                     #Note: keep includes mode across all regulated structure types (single family homes to quadriplexes)
                                     #In general, we want to cluster on as few objects as possible, else large municipalities will assign each block group to their own cluster (low silhouette score)
#data frame of these variables
clusterVars_df <- US_BLOCK %>% st_drop_geometry() %>% select(State, County, Tract, BlockGroup, all_of(vars_toCluster))

#PARAMETERS FOR THE CLUSTERING ALGORITHM
#start loop over desired level of clusters

alpha_grid <- c(0.5, 0.75, 0.95) #weights on geographic proximity (try a relatively sparse grid of them)
                                 #parameter weight on geographic proximity in the assignment of clusters. Choose many different clustering assignments to match these.
                                 #we desire spatial proximity, so only loop over parameters > 0.5 (50%)

minimum_no_cluster_factor_grid <- c(5, 15, 25, 100, 250) #grid to check what targeted maximum cluster size should be (in block groups). Choose large and small values

municipality_definition_grid <- c("Assigned_municipality", 
                                  "Assigned_geocoded_municipality") # start loop over municipality definitions


hyperparameter_loop <- foreach(municipalitydef_index = 1:length(municipality_definition_grid), .packages = c("ClustGeo", "cluster", 
                                                                                                             "dplyr", "haven", "sf",
                                                                                                             "spdep")) %:%
                       foreach(desired_alpha_index = 1:length(alpha_grid), .packages = c("ClustGeo", "cluster", 
                                                                                           "dplyr", "haven", "sf",
                                                                                           "spdep")) %dopar% { #loop over hyperparameters HERE
municipalitydef <- municipality_definition_grid[municipalitydef_index]
desired_alpha <- alpha_grid[desired_alpha_index]

US_BLOCK_toAPPEND <- rep(list(data.frame()), 
                         length(minimum_no_cluster_factor_grid)) #empty list for each minimum_no_cluster factor given municipality definition and desired alpha. 


start_time <- Sys.time()
start_time

#Start loop within MSAs, then loop across counties. 
#This means that clustering is never beyond the MSA-county level, and most of the time at the MSA-municipality level. 

#If file doesnt exist
if (municipalitydef == "Assigned_municipality") {
  municString <- "CORELOGIC"
} 

if (municipalitydef == "Assigned_geocoded_municipality") {
  municString <- "GEOCODED"
} 

loop_No <- 0 #printing loop number
for (j in unique(US_BLOCK$CBSA)) {

  US_BLOCK_MSA <- US_BLOCK %>% filter(CBSA == j)
  
  #Take loop across municipalities.
  
  for (i in unique(US_BLOCK_MSA[[municipalitydef]])) {
    
     US_BLOCK_MP <- US_BLOCK_MSA[US_BLOCK_MSA[[municipalitydef]] == i,] #looping over unique values of given municipality definition
    
     
     #Looping over assigned zoningcodes (if they exist). Assign zoning district to zoning code if so. 
     for (zone in unique(US_BLOCK_MP$Assigned_zoningcode)) {
        
       US_BLOCK_MP_ZN <- US_BLOCK_MP[US_BLOCK_MP$Assigned_zoningcode == zone,] #extracting zone.
       US_BLOCK_put_toAPPEND <- US_BLOCK_MP_ZN %>% select(State, County, Tract, 
                                                        BlockGroup, CBSA, CBSA_NAME, Assigned_municipality,
                                                        Assigned_geocoded_municipality, Assigned_zoningcode) %>%
                                                        st_drop_geometry() # dataset that will be appended to US_BLOCK_toAPPEND
       
       if (zone != "") { #if zone nonmissing...   
          US_BLOCK_put_toAPPEND["ZoningDistrictID"] <- which(zone == sort(unique(US_BLOCK_MP$Assigned_zoningcode))) #Assigning number for unique zoning code 
          
          for (minimum_no_cluster_factor in minimum_no_cluster_factor_grid) { #Assigning to each dataframe in append list to one cluster
            
            minimum_no_cluster_factor_index <- which(minimum_no_cluster_factor == minimum_no_cluster_factor_grid)  #Assigning each block group its own district.
            
            US_BLOCK_toAPPEND[[minimum_no_cluster_factor_index]] <- rbind(US_BLOCK_put_toAPPEND,
                                                                          US_BLOCK_toAPPEND[[minimum_no_cluster_factor_index]])
            
          }
     
       }
       
       if (zone == "") { #if zone missing, perform clustering algorithm
          
          #Extracting vars to cluster, putting this in a special dataframe to pass to ClustGeo, call it Dat. 
          Dat <- US_BLOCK_MP_ZN %>% st_drop_geometry() %>% select(all_of(vars_toCluster))
          # 01/22-- remove zoning codes as a clustering variable. Instead, use zoning information only!
        
          #Pairwise distances to enter into clustering algorithm
          DistGeo <- stats::as.dist(st_distance(US_BLOCK_MP_ZN))  #extracting distances from zoning codes 
     
          #Storing distances on "social" variables to cluster.
          Dat <- scale(Dat) #Clustering
          Dat[is.nan(Dat)] <- 0 #occurs if variable is all 0, hence dividing by norm not defined in scale() function
          Dat <- data.frame(Dat)
          DistSocial <- stats::dist(Dat) #take Euclidean distance as of 01/22
     
          #PARAMETERS: CHOICES BETWEEN APPROXIMATE MINIMUM AND APPROXIMATE MAXIMUM NUMBER OF CLUSTERS
          #            AS A FUNCTION OF nrow(US_BLOCK_MP). 
          max_NoCluster <- nrow(US_BLOCK_MP_ZN) - 1 #maximum number of clusters bounds to search over. We cap at roughly one block group per cluster

          silhouette.store <- rep(-2, max_NoCluster) #storing silhouette coefficients for each choice of # of clusters
          #note: -2 means that it wasn't calculated for that number of clusters. Silhouette scores are never below -1. 
     
     
          #Start loop over clusters if municipality has more than two block groups associated with it (so that min_NoCluster <= max_NoCluster)
          if (nrow(Dat) > 2 & nrow(unique(Dat)) > 1) { # the second condition checks if all block groups do not look the same
      
            #CREATE CLUSTERING OBJECT HERE
            ClusterGeoObject <- hclustgeo(D0 = DistSocial, D1 = DistGeo, 
                                   alpha = desired_alpha, scale = TRUE) #pretty fast! 
       
            for(NoCluster in seq(2, max_NoCluster)) { #Loop over clusters to choose one that maximizes silhouette score

              #CLUSTERING BEGINS HERE
              Cluster <- cutree(ClusterGeoObject, NoCluster) #Cut the tree at this number of clusters. Gives assignment indices.
         
              silhouette.object <- silhouette(x = Cluster, dist = DistSocial) #Cluster gives indices, distSocial gives distances (social distances)
              silhouette.store[NoCluster] <- mean(silhouette.object[, 3]) #average silhouette score
         
            }#Loop over clusters
     
            #Assigning clusters to each dataset in US_BLOCK_toAPPEND based on minimum cluster factor
            for (minimum_no_cluster_factor in minimum_no_cluster_factor_grid) {
         
              min_NoCluster <- max(floor(nrow(US_BLOCK_MP_ZN)/minimum_no_cluster_factor), 2) #minimum number of clusters to search over, provided >=2
              #Split number of block groups assigned to the municipality 
              #by the minimum number of clusters factor.
              #Example: if minimum_no_cluster_factor = 4, then we search over a minimum number of clusters that looks like nrow(Dat)/4
              #Algorithm requires obviously at least two clusters at each municipality iteration.  
         
        
              minimum_no_cluster_factor_index <- which(minimum_no_cluster_factor == minimum_no_cluster_factor_grid) #extracting index
         
              silhouette.store_minCluster <- c(rep(-2, min_NoCluster - 1), silhouette.store[min_NoCluster:max_NoCluster]) #setting all silhouette_stores to -2 when index below MinCluster
         
              Desired_NoCluster <- min(which(max(silhouette.store_minCluster) == 
                                        silhouette.store_minCluster)) #Take smallest number of clusters that maximizes sillhouette score
        
         
              #updating desired cluster number
              Cluster <- cutree(ClusterGeoObject, Desired_NoCluster)
     
              US_BLOCK_put_toAPPEND["ZoningDistrictID"] <- Cluster
         
        
              #Appending to list
              US_BLOCK_toAPPEND[[minimum_no_cluster_factor_index]] <- rbind(US_BLOCK_put_toAPPEND,
                                                                          US_BLOCK_toAPPEND[[minimum_no_cluster_factor_index]])
        
            }#End loop over minimum no_cluster_factors.
       
          } #Check if nrow(Dat) > 2
     
          if (nrow(Dat) <= 2 | nrow(unique(Dat)) <= 1) { #Check if municipality has less than two assigned block groups, or no variation in data
       

            US_BLOCK_put_toAPPEND["ZoningDistrictID"]  <- rep(1, nrow(US_BLOCK_MP_ZN)) #assign basic zoning districts to missing zoning-dist municipalities that have these many observations
       
       
            for (minimum_no_cluster_factor in minimum_no_cluster_factor_grid) { #Assigning to each dataframe in append list to one cluster (we cant cluster small subsets with nrow <= 2)
         
               minimum_no_cluster_factor_index <- which(minimum_no_cluster_factor == minimum_no_cluster_factor_grid)  #Assigning each block group its own district.
        
               US_BLOCK_toAPPEND[[minimum_no_cluster_factor_index]] <- rbind(US_BLOCK_put_toAPPEND,
                                                                         US_BLOCK_toAPPEND[[minimum_no_cluster_factor_index]])
         
            }
          }#Check all other types of municipalities
     
       }#Check if zone == ""
          
     
    }#end loop over zoning codes      
       
       
  loop_No <- loop_No + 1
  print(paste0("The current iteration over municipalities is ", loop_No))
  } #End loop over municipalities
} #End loop over MSA's

end_time <- Sys.time()
end_time


if (municipalitydef == "Assigned_municipality") {
  municString <- "CORELOGIC"
} 

if (municipalitydef == "Assigned_geocoded_municipality") {
  municString <- "GEOCODED"
} 

for (minimum_no_cluster_factor in minimum_no_cluster_factor_grid) { #Writing data for each minimum_no_cluster_factor within municipality definition and alpha_grid definition
  
  minimum_no_cluster_factor_index <- which(minimum_no_cluster_factor == minimum_no_cluster_factor_grid) #Index
  
  #Writing Constructing zoning districts as function of parameters
  filename_to_output <- paste0(paste0("ConstructedZoningDistricts_", 
                            paste(paste0("alpha_", desired_alpha), paste0("minClust", minimum_no_cluster_factor),
                                  sep = "_")), paste0("_", municString), ".dta")
  
  #Dataframe to output (with clustering variables)
  df_to_output <- left_join(US_BLOCK_toAPPEND[[minimum_no_cluster_factor_index]], clusterVars_df, by = c("State", "County", "Tract", "BlockGroup"))
  
  write_dta(df_to_output, paste0("DataV2/CoreLogic/output/", filename_to_output)) 
  
}

NULL #returning something for foreach.
}#End parallel loop over hyperparameters. 

gc()
rm(list = ls())