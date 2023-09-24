library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(readr)
library(collapse)
library(stringr)
library(labelled)
library(rlang)
library(Rfast)
library(ggplot2)
library(matrixStats)

#Date created: Dec 19th, 2022

#This file validates how accurate the regulation measurements perform on real data. Requires output from assign_Regulation.do and merge_stringency_forFacts.R

#PART 1: ___________Importing all clustering definitions_____________________________________________________________________
#importing data form Merge_stringency.R
load(file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")

#Part 2: Validation

#2.A: Terner Land Use Survey

TLUS <- read_dta("DataV2/DirectLotSize/TernerLandUseSurvey/TCRLUS_data_2018-11-12.dta")
TLUS["State"] <- rep(6, nrow(TLUS)) #TLUS is only for california states

#matching on city name-- will be from geocoded municipality.
#Extracting first word from Assigned_geocoded_municipality

US_BLOCK["city"] <- str_remove(US_BLOCK$Assigned_geocoded_municipality, pattern = " city")
US_BLOCK["city"] <- str_remove(US_BLOCK$city, pattern = " town")
TLUS_joined <- inner_join(TLUS, US_BLOCK, by = c("State", "city"))

#233 cities/towns joined (in 2013 definition CBSAs of California) in 23 MSAs (All MSAs)
#Missing: Madera, CA, (not a 2013 MSA)

#take land mass weighted average of each block group to arrive at city level measure of minimum lot sizes. 
TLUS_joined$zon_sfminlotsize[TLUS_joined$zon_sfminlotsize < 0] <- NA 
TLUS_joined$zon_mfminlotsize[TLUS_joined$zon_mfminlotsize < 0] <- NA #recorrecting missing codes

#Start loop over clustering definition
colnames <- colnames(TLUS_joined)
extract_colnames <- colnames[which(grepl("uDR", colnames, fixed = TRUE) == TRUE)] #all minimmum lot sizes
extract_MSE_TLUS <- rep(NA, length(extract_colnames))

for (column in extract_colnames) {
  
  
  TLUS_selected <- TLUS_joined %>% select(city, State, CBSA, CBSA_NAME, ZoningDistrictID, Assigned_municipality, 
                                          contains(column), contains(sub( "uDR", "singlefamily_mode", column)),
                                          contains(sub( "uDR", "duplex_mode", column)),
                                          contains(sub( "uDR", "triplex_mode", column)),
                                          contains(sub( "uDR", "quadriplex_mode", column)),
                                          contains("minlotsize")) 
  
  #Setting minimum lot sizes to be acres from official TLUS
  TLUS_selected$zon_sfminlotsize <- (TLUS_joined$zon_sfminlotsize)/43560
  TLUS_selected$zon_mfminlotsize <- (TLUS_joined$zon_mfminlotsize)/43560
  
  #Re-multiplying uDR by implied unit density restriction for comparison to single and multifamily lots
  structureType <- as.vector(apply(as.matrix(data.frame(TLUS_selected[sub( "uDR", "singlefamily_mode", column)],
                           TLUS_selected[sub( "uDR", "duplex_mode", column)],
                           TLUS_selected[sub( "uDR", "triplex_mode", column)],
                           TLUS_selected[sub( "uDR", "quadriplex_mode", column)])), MARGIN = 1, FUN = which.min)) #finding which structure yields minimum. structureType[[row]] gives unit density restriction
  structureType <- lapply(structureType, function(x) if(length(x) == 0) NA else x)
  structureType <- unlist(structureType) #vector of implied unit density restrictions
  TLUS_selected["uDR"] <- TLUS_selected[[column]]*structureType #coverting modes back to lot area (rather than implied unit density restriction)
  
  
  #calculating mode over all observations by city
  collap_formula <- as.formula(paste0("uDR", " ~ city + CBSA_NAME + zon_sfminlotsize + zon_mfminlotsize"))
  TLUS_selected <- collap(TLUS_selected, collap_formula, FUN = fmode) #note: using mode doesn't matter a whole lot
  
  #Calculating singlefamily or multifamily structure that minimizes distance with implied uDR
  TLUS_selected["sf_deviation"] <- abs(TLUS_selected$uDR - TLUS_selected$zon_sfminlotsize)
  TLUS_selected["mf_deviation"] <- abs(TLUS_selected$uDR - TLUS_selected$zon_mfminlotsize)
  TLUS_selected <- TLUS_selected %>% rowwise() %>% mutate(min_deviation = min(sf_deviation, mf_deviation))
  
  #Calculating median error rates 
  extract_MSE_TLUS[which(column == extract_colnames)] <- median(TLUS_selected$min_deviation/abs(TLUS_selected$zon_sfminlotsize), na.rm = TRUE)
  
}

print(paste0("The best clustering algorithm on TLUS is ", extract_colnames[which(extract_MSE_TLUS == min(extract_MSE_TLUS))]))

#Clearing memory
itemList <- ls()
itemList <- itemList[!(itemList %in% "US_BLOCK") & 
                     !(itemList %in% "extract_MSE_TLUS") &
                     !(itemList %in%  "extract_colnames")]
rm(list = itemList)
rm(itemList)

#Part 2: Performing validation at microgeographic level for various US cities.
#This is done by overlaying block group data on the official zoning maps of these cities. 

#importing geography
#Loading block group geometry
blkgeo <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") %>% 
  mutate(State = as.numeric(STATEFP), County = as.numeric(COUNTYFP), Tract = as.numeric(TRACTCE),
         BlockGroup = as.numeric(BLKGRPCE)) %>%
  select(State, County, Tract, BlockGroup)
US_BLOCK <- left_join(blkgeo, US_BLOCK, by = c("State", "County", "Tract", "BlockGroup"))
US_BLOCK <- US_BLOCK %>% filter(!is.na(CBSA)) #blocks that appear in sample geography
rm(blkgeo)
US_BLOCK <- st_transform(US_BLOCK, crs = 4269)
sf_use_s2(FALSE) #switching spherical geometry off-- this causes issues with st_intersects. No big deal given small locations. 

#Listing all folders in Zoning_Maps
cityList <- list.files("DataV2/DirectLotSize/Zoning_Maps")

#Initializing data frame to append all observations across different cities.
total_appended_df <- data.frame()

#vector to store MSE
extract_MSE_citymap <- rep(NA, length(extract_colnames))

for (city in cityList) {
  
  zipfile <- list.files(paste0("DataV2/DirectLotSize/Zoning_Maps/", city), 
                         pattern = "*.zip") #should be length 1
  unzip(paste0("DataV2/DirectLotSize/Zoning_Maps/", city, "/", zipfile), 
        exdir = paste0("DataV2/DirectLotSize/Zoning_Maps/", city))
  
  shapefiles <- list.files(paste0("DataV2/DirectLotSize/Zoning_Maps/", city), 
                           pattern = "*.shp")
  shapefiles <- shapefiles[!(str_detect(shapefiles, ".xml"))] #removing .xml files
  
  shpfile_appended <- data.frame() #appending all shpfiles
  shpfile_list <- list() #list to store all shapefiles
  
  for (file in shapefiles) {
   shpfile_list[[which(file == shapefiles)]] <- st_read(paste0("DataV2/DirectLotSize/Zoning_Maps/", city, "/", file)) #importing all shapefiles in folder.
  }
  
  
  for (file in 1:length(shapefiles)) {
    shpfile_list[[file]] <- st_cast(shpfile_list[[file]], "MULTIPOLYGON")
    shpfile_appended <- plyr::rbind.fill(shpfile_appended, shpfile_list[[file]])
  }
  
  #coercing to sf, setting common crs
  shpfile_appended <- st_as_sf(shpfile_appended)
  shpfile_appended <- st_transform(shpfile_appended, crs = 4269)
  shpfile_appended <- st_zm(shpfile_appended, drop = TRUE) #dropping z coordinates from some shapefiles
  
  #START MERGING ZONING DISTRICTS WITH US BLOCK DATA
  #spatial join
  shpfile_appended <- st_join(shpfile_appended, US_BLOCK, join = st_intersects) #checking which block groups intersect with zoning polygon
  
  shpfile_appended["Official_uDR"] <- rep(NA, nrow(shpfile_appended)) #initializing column of official zoning restrictions.
  
  #Start city by city zoning definitions
  if (city == "Albany GA") {
    
    #See table in corresponding folder
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "R-1"] <- 12800/43560 #R-1: single family, ~0.3 acres
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "R-2"] <- 8400/43560  
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "R-3"] <-  (6400/43560)/2 #(plus an ADU 0.2 acre lots)
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "R-E"] <- 10 #Estate districts
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "R-G"] <- 2 #Estate districts
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "AG"] <- 2 #Ag district (residential property allowed if it looks exactly like RG)
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "R-MHS"] <- 10000/43560 #mobile home allowed
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "R-MHP"] <- 3200/43560 #mobile home allowed
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "C-R"] <- 2200/43560 #mobile home allowed
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "C-1"] <- 1600/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "C-5"] <- 1600/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "C-6"] <- 2500/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "C-7"] <- 1600/43560 #multifamily allowed in commercial districts
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "C-2"] <- 1600/43560 #multifamily allowed in commercial districts
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "C-3"] <- 2500/43560 #multifamily allowed in commercial districts
    shpfile_appended$Official_uDR[shpfile_appended$ZONE == "C-8"] <- 8400/43560 #multifamily allowed in commercial district
  }
  
  if (city == "Atlanta") {
    
    #(see municode links in shapefiles)
    
    #Commercial
    shpfile_appended$Official_uDR[shpfile_appended$ZONECLASS == "C-1"] <- 5000/43560 #C-1 district single family homes (some multifamily)
    
    #Residential 
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-1")] <- 2 #2 acre lots in R-1 districts, but most lots predate zoning restrictions.
    shpfile_appended$Official_uDR[shpfile_appended$ZONECLASS == "R-2"] <- 1
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-2-")] <- 1
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-2A")] <- 30000/43560 
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-2B")] <- 28000/43560 
    shpfile_appended$Official_uDR[shpfile_appended$ZONECLASS == "R-3"] <- 18000/43560 
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-3-")] <- 18000/43560 
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-3A")] <- 13000/43560 
    shpfile_appended$Official_uDR[shpfile_appended$ZONECLASS == "R-4"] <- 9000/43560
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-4A")] <- 7500/43560
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-4B")] <- 2300/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONECLASS == "R-5"] <- (7500/43560)/2 #(duplexes allowed)
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "R-5-")] <- (7500/43560)/2
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONECLASS, "RG")] <- (1000/43560)/2 #All RG districts under one municode
    
    #All the rest of the many zoning districts are special-- generally not for residences. 
    
  }
  
  if (city == "Berkeley") {
    
    #Residential
    "https://berkeley.municipal.codes/BMC/23.202.080"
    
    #EXCEPTIONS WITH MINIMUM LOT SIZES
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "R-1"] <- 5000/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "R-1A"] <- (4500/43560)/2
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "R-1H"] <- (4500/43560)/2
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "ES-R"] <- (25000/43560)
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "R-2"] <- (5000/43560)/2 #R-1 with duplexes
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "R-2A"] <- (1650/43560)
    
  }
  
  if (city == "Cleveland") {
    "https://codelibrary.amlegal.com/codes/cleveland/latest/cleveland_oh/0-0-0-15793#JD_Chapter355" 
    
    #Residential
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_DIST == "AA"] <- 7200/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_DIST == "A"] <- 4800/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_DIST == "B"] <- 2400/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_DIST == "C"] <- 2400/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_DIST %in% c("D", "E", "F", "G", "H", "J", "K")] <- 2100/43560 #Ignoring residential buildings
    
  }
  
  if (city == "Columbus") {
    
    #Residential districts
    #https://library.municode.com/oh/columbus/codes/code_of_ordinances?nodeId=TIT33ZOCO_CH3332REDI_3332.11ARDIRE
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "R1"] <- 7200/43560
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA %in% c("R2", "R3")] <- 5000/43560
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "RURAL"] <- 5
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "LRR"] <- 1
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "RRR"] <- 20000/43560
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "RR"] <- 10000/43560
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "SR"] <- 7200/43560
    
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "MHD"] <- 7200/43560
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "R-2F"] <- 3000/43560
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA == "R-4"] <- 2500/43560
    
    
    #Apartment districts ()
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA %in% c("AR-1", "AR-4")] <- 1200/43560
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA %in% c("AR-2")] <- 800/43560
    shpfile_appended$Official_uDR[shpfile_appended$CLASSIFICA %in% c("ARLD")] <- 2500/43560
    
    #No clear minimum lot sizes for residential properties in non residential districts (if allowed). leaving these blank. 
    
  }
  
  if (city == "Hayward") {
    #https://library.municode.com/ca/hayward/codes/municipal_code?nodeId=HAYWARD_MUNICIPAL_CODE_CH10PLZOSU_ART1ZOOR_S10-1.500HIDEREDIRH
    
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RH"] <- 1250/43560 #may allow for apartments, but this is the unit density restriction for all building types
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RM"] <- 2500/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RMB3.5"] <- 3500/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RMB4"] <- 4000/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RS"] <- 5000/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RSB10"] <- 10000/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RSB4"] <- 4000/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RSB6"] <- 4000/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RSB8"] <- 8000/43560
    
    #Commercial (mixed use)
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "CN"] <- 2500/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "CN-R"] <- 1743/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "CG"] <- 2500/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "CO"] <- 2500/43560
    shpfile_appended$Official_uDR[shpfile_appended$ZONING_ == "RO"] <- 2500/43560
  }
  
  if (city == "Mesa") {
    #https://library.municode.com/az/mesa/codes/code_of_ordinances?nodeId=COOR_TIT11ZOOR_ART2BAZO_CH4AGDI_11-4-3DEST
    
    #Agricultural district
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "AG - Agricultural"] <- 10 
    
    #Residential
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "RS-6 Single Residence 6"] <- 6000/43560
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "RS-7 Single Residence 7"] <- 7000/43560
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "RS-9 Single Residence 9"] <- 9000/43560
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "RS-15 Single Residence 15"] <- 15000/43560
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "RS-35 Single Residence 35"] <- 35000/43560
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "RS-43 Single Residence 43"] <- 1
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "RS-90 Single Residence 90"] <- 90000/43560
    
    #Downtown residential
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "DR-1 - Downtown Residential 1"] <- 6000/43560
    shpfile_appended$Official_uDR[shpfile_appended$dscr == "DR-2 - Downtown Residential 2"] <- (18000/43560)/4 #Quadriplexes allowed
    
    
  }
  
  if (city == "Miami") {
    #https://codehub.gridics.com/us/fl/miami#/cc66de11-c6ed-4007-a7b8-3a10fbb320c6/dddf8a5e-0f63-4002-a203-27b4857b183a
    
    #Suburban zones
    shpfile_appended$Official_uDR[shpfile_appended$M21_ZONE == "T3-R"] <- 5000/43560 #
    shpfile_appended$Official_uDR[shpfile_appended$M21_ZONE == "T3-L"] <- 5000/43560 #
    shpfile_appended$Official_uDR[shpfile_appended$M21_ZONE == "T3-O"] <- (5000/43560)/2 # Duplexes allowed
    
    #Urban zones
    shpfile_appended$Official_uDR[shpfile_appended$Map_Code == "T4"] <- 1/36 # 36 units per acre density restriction (essentially zero minimum lot size!)
    shpfile_appended$Official_uDR[shpfile_appended$Map_Code == "T5"] <- 1/65
    
    #Urban core zones
    shpfile_appended$Official_uDR[shpfile_appended$Map_Code == "T6-8"] <- 1/150 #essentially 0 minimum lot size
    shpfile_appended$Official_uDR[shpfile_appended$Map_Code == "T6-12"] <- 1/150
    shpfile_appended$Official_uDR[shpfile_appended$Map_Code == "T6-24"] <- 1/150#essentially 0 minimum lot size (many exceptions go even lower for 6-48, etc)
    shpfile_appended$Official_uDR[shpfile_appended$Map_Code == "T6-48"] <- 1/150
    shpfile_appended$Official_uDR[shpfile_appended$Map_Code == "T6-80"] <- 1/150
    
    #Other zones
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$Map_Code, "C")] <- 1/150 #civic zones
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$Map_Code, "D")] <- 1/36 #district zones
    
    
  }
  
  if (city == "Minneapolis") {
    #https://library.municode.com/mn/minneapolis/codes/code_of_ordinances?nodeId=MICOOR_TIT20ZOCO_CH546REDI_ARTIIR1MUMIDI
    
    #Residential
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_CODE == "R1"] <- (6000/43560)/3 #triplexes allowed on 6000sqft lots
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_CODE == "R1A"] <- (5000/43560)/3
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_CODE == "R2"] <- (6000/43560)/3
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_CODE == "R2B"] <- (5000/43560)/3
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_CODE == "R3"] <- (1500/43560) #unit density restriction
    shpfile_appended$Official_uDR[shpfile_appended$ZONE_CODE == "R4"] <- (1250/43560) #unit density restriction
    
    #Mixed use commercial
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONE_CODE, "C")] <- (5000/43560)/3 #all mixed use commercial min lot sizes
  }
  
  if (city == "New Orleans") {
    
    #https://czo.nola.gov/article-13/
    
    #Residential
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-RS"] <- 6000/43560 
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-RD"] <- 2000/43560 
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-LRS1"] <- 2500/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-LRS2"] <- 6700/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-LRS3"] <- 5000/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-LRD1"] <- 3250/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-LRD2"] <- 2500/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-RM1"] <- 1250/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-RM2"] <- 1200/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-LRM1"] <- 1200/43560
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "S-LRM2"] <- 1000/43560
    
    #Historical residential 
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "HU-RS"] <- 5000/43560   
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "HU-RD1"] <- 2000/43560 
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "HU-RD2"] <- 5000/43560 
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "HU-RM1"] <- 1250/43560 
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "HU-RM2"] <- 800/43560 
    
    #Commercial 
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONE_CODE, "C-")] <- 1000/43560   
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONE_CODE, "MU-")] <- 1000/43560 
    
    #Historical mixed use
    shpfile_appended$Official_uDR[str_detect(shpfile_appended$ZONE_CODE, "HU-B1")] <- 1000/43560 
    shpfile_appended$Official_uDR[shpfile_appended$zoneclass == "HU-MU"] <- 1000/43560 
    
    #There are more, but it gets complicated. leave these blank.
    
  }
  
  if (city == "Oakland") {
    
    #https://library.municode.com/ca/oakland/codes/planning_code
    
    #Residential
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RD-1"] <- 5000/43560
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RD-2"] <- (6000/43560)/2
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RM-1"] <- 5000/43560
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RM-2"] <- (4000/43560)/2
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RM-3"] <- (4000/43560)/2
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RM-4"] <- (1100/43560)
    
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RU-1"] <- (1100/43560) #Residential urban
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RU-2"] <- (800/43560)
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RU-3"] <- (450/43560)
    
    #Hillside zones
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RH-1"] <- 1
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RH-2"] <- 25000/43560
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RH-3"] <- 12000/43560
    shpfile_appended$Official_uDR[shpfile_appended$basezone == "RH-4"] <- 6500/43560
    
    
  }
  
  if (city == "Scottsdale") {
    #https://library.municode.com/az/scottsdale/codes/code_of_ordinances?nodeId=VOLII_APXBBAZOOR_ARTVDIRE_S5.804PRDEST
    
    #Residential
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R1-190"] <- 190000/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R1-70"] <- 70000/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R1-130"] <- 130000/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R1-35"] <- 35000/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R1-18"] <- 18000/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R1-10"] <- 10000/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R1-7"] <- 7000/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R1-5"] <- 5000/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R-2"] <- (8000/43560)/2
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R-3"] <- 3370/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "R-4"] <- 5240/43560
    shpfile_appended$Official_uDR[shpfile_appended$full_zonin == "S-R"] <- 1/12
    
  }
  
  
  #Start appending data here that can be used to calculate accuracy measures. 
  shpfile_appended <- shpfile_appended %>% select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME,
                                                  Assigned_municipality, Assigned_geocoded_municipality, 
                                                  ZoningDistrictID,
                                                  Official_uDR, city, contains("uDR"), contains("mode"))
  #Appending this data frame to total_appended_df
  total_appended_df <- rbind(total_appended_df, shpfile_appended)
  
} #End loop over cities appending all data
  
#Calculating weights to weigh block groups evenly
total_appended_df <- total_appended_df %>% group_by(State, County, Tract, BlockGroup) %>% mutate(blkgrp_weight = 1/n())
  
#Calculating polygon_area weighted median of error, matching uDR to closest unit density restriction under the modes for single family to quadriplex 
#(this makes sense-- sometimes, on paper restrictions don't match up with the types of structures.)
for (column in extract_colnames) {
    
    total_appended_df["p_error_sf"] <- abs(total_appended_df$Official_uDR - total_appended_df[[sub( "uDR", "singlefamily_mode", column)]])/abs(total_appended_df$Official_uDR) #deviation using sf mode
    total_appended_df["p_error_d"] <- abs(total_appended_df$Official_uDR - total_appended_df[[sub( "uDR", "duplex_mode", column)]])/abs(total_appended_df$Official_uDR) #deviation using duplex mode
    total_appended_df["p_error_t"] <- abs(total_appended_df$Official_uDR - total_appended_df[[sub( "uDR", "triplex_mode", column)]])/abs(total_appended_df$Official_uDR) #deviation using triplex mode
    total_appended_df["p_error_q"] <- abs(total_appended_df$Official_uDR - total_appended_df[[sub( "uDR", "quadriplex_mode", column)]])/abs(total_appended_df$Official_uDR) #deviation using quadriplex mode
    
    #taking minium deviation from each structure type
    total_appended_df["min_deviation"] <- pmin(total_appended_df$p_error_sf,
                                               total_appended_df$p_error_d,
                                               total_appended_df$p_error_t,
                                               total_appended_df$p_error_q, na.rm = TRUE) #finding which structure yields minimum. structureType[[row]] gives unit density restriction

    
    #Use weighting scheme that weighs block groups evenly
    extract_MSE_citymap[which(column == extract_colnames)] <- matrixStats::weightedMedian(total_appended_df$min_deviation, w = total_appended_df$blkgrp_weight, na.rm = TRUE)
}

extract_MSE_citymap
print(paste0("The best clustering algorithm on CityMaps is ", extract_colnames[which(extract_MSE_citymap == min(extract_MSE_citymap))])) #22% median error.

#Taking observation that minimizes weighted average of both
print(paste0("The best clustering algorithm on a equal weight of both is ", extract_colnames[which(0.5*extract_MSE_citymap + 0.5*extract_MSE_TLUS == min(0.5*extract_MSE_citymap + 0.5*extract_MSE_TLUS))]))

