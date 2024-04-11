#Date Created: Nov 29, 2022

#This file constructs a series of appendable .dta format files from the current assessments
#by reading the delimited files one chunk at a time, subsetting columns we need, plus some other things.
#Uses only high-compression variables to keep the resulting .dta files as small as possible. 

#MAKE SURE CURRENT ASSESSMENT DELIMITED FILE IS UNZIPPED BEFORE RUNNING PROGRAM.
#RUN ALL CODE FROM DOWNLOADS BEFORE RUNNING THIS PROGRAM AS WELL. 

#________FOLDERS_____________________

data_filepath <- "DataV2/CoreLogic"


#Packages imported
library(vroom) #fast reading of delimited files
library(collapse)
library(dplyr)
library(haven) #to save in .dta format
library(sf)


#need to turn of sf_use_s2 (spherical geometry) because of spatial joining issues
sf::sf_use_s2(FALSE) 

#________PART 1: IMPORTING GEOGRAPHY___________________________________________________________________
US_CBSA_2010 <- st_read("DataV2/US_Data/Shapefiles/cb_2013_us_cbsa_500k.shp") #2013 MSAs
US_CBSA_2010 <- US_CBSA_2010[US_CBSA_2010$LSAD == "M1",]
US_CBSA_2010 <- US_CBSA_2010 %>% rename(CBSA = CBSAFP)


US_BLOCK <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") #From IPUMS NHGIS
US_BLOCK_CENTRIOD <- st_centroid(US_BLOCK)
US_BLOCK_CENTRIOD <- st_transform(US_BLOCK_CENTRIOD, crs = 4269)


#Matching block group centroids to CBSA polygons are sufficient for accuracy.  
US_BLOCK["Temp"] <- as.integer(st_intersects(US_BLOCK_CENTRIOD, US_CBSA_2010)) #Centroid matched to polygon by observation in US_CBSA_2010
US_CBSA_2010 <- mutate(US_CBSA_2010, Temp = row_number()) #Temp used to join MSA names with matched centroids

#Matching CBSA FIPS Code to US_TRACT_2010_CENTRIOD data.
#Data frame containing names of CBSA, unique CBSA code, etc. 
Temp_tojoin <- US_CBSA_2010 %>%
  dplyr::select(Temp, NAME, CBSA)
Temp_tojoin <- as.data.frame(Temp_tojoin) #converting for left_join
Temp_tojoin <- Temp_tojoin[, !colnames(Temp_tojoin) %in% "geometry"] #deleting geometry data from sf object to join 
US_BLOCK <- left_join(US_BLOCK, Temp_tojoin, by = "Temp")
rm(Temp_tojoin, US_BLOCK_CENTRIOD)

#Renaming
US_BLOCK <- US_BLOCK[, !colnames(US_BLOCK) %in% "Temp"] #removing variable used solely for matching
names(US_BLOCK)[names(US_BLOCK) == 'NAME'] <- 'CBSA_NAME'
names(US_BLOCK)[names(US_BLOCK) == 'STATEFP'] <- 'State'
names(US_BLOCK)[names(US_BLOCK) == 'COUNTYFP'] <- 'County'
names(US_BLOCK)[names(US_BLOCK) == 'TRACTCE'] <- 'Tract' #Renaming these variables to join on State/County/Tract
names(US_BLOCK)[names(US_BLOCK) == 'BLKGRPCE'] <- 'BlockGroup' #Renaming these variables to join on State/County/Tract

#Dropping tracts not in used CBSA
US_BLOCK<- US_BLOCK[!is.na(US_BLOCK$CBSA),] #200,670 block groups remain.

US_BLOCK <- US_BLOCK[!(US_BLOCK$State == 15 | US_BLOCK$State == 72 | US_BLOCK$State == '02'),] #removing VI, Alaska, Hawaii

#Covering land mass to acres (currently it appears to be square meters) https://www.census.gov/quickfacts/fact/note/US/LND110210
US_BLOCK$ALAND <- US_BLOCK$ALAND/4046.86

US_BLOCK <- US_BLOCK %>% dplyr::select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, ALAND) #197,062 block groups remain. 

#Setting each as numeric
for (destring in c("State", "County", "Tract", "BlockGroup", "CBSA")) {
  US_BLOCK[destring] <- as.numeric(US_BLOCK[[destring]]) 
}

#Saving in STATA format for use with other programs
US_BLOCK_2DTA <- US_BLOCK %>% st_drop_geometry()
write_dta(US_BLOCK_2DTA, "DataV2/US_Data/Output/SampleGeography.dta")
rm(US_BLOCK_2DTA)

#Removing CBSA_NAME to save data
US_BLOCK <- US_BLOCK %>% select(-CBSA_NAME)

#US BLOCK is our dataset of in-sample block groups within our 2013-definition MSAs.
#transforming US_BLOCK to lat/lon coordinates
US_BLOCK <- st_transform(US_BLOCK, 4269)

#Importing definitions of cities from shapefile (requires output from Downloads folder)
US_PLACE <- st_read("DataV2/US_Data/Shapefiles/2020Places.shp") %>% filter(LSAD == 25 | LSAD == 43 | LSAD == 37) %>%
                                                                    select(NAMELSAD) %>% rename(Geocoded_municipality = NAMELSAD)
#	city (suffix)	Consolidated City, County or Equivalent Feature, County Subdivision, Economic Census Place, Incorporated Place
#This is the official definition of LSAD code 25. These appear to correspond to municipalities in the assessments often.
#Also ad LSAD == 37 and LSAD == 43 (municipalities and towns). See official census documents
#Correspond to roughly the 14,000 municipalities recorded in the assessments.
#____________________________________________________________________________________________________________

#PART 2: setting up for loop over chunks

#File name for current assessments
mainFileName <- "/university_of_toronto_property_basic_res2_01403803_20221115_080220"

#creating directories for temporary files that are outputs of this program
if (dir.exists(paste0(data_filepath, "/temp/dta/currentAssessments")) == FALSE) {
  
  dir.create(paste0(data_filepath, "/temp/dta/currentAssessments"))
}

#importing cnts file (from metadata) to get filesize (number of rows)
if (file.exists(paste0(data_filepath, 
                       paste0(mainFileName, "_cnts.txt"))) == FALSE) {
  
  unzip(zipfile = paste0(data_filepath, paste0(mainFileName, "_meta.zip")), 
        files = c(paste0(mainFileName, "_cnts.txt")),
        exdir = data_filepath)
  
}

#Getting TotalObs
cnts <- vroom(paste0(data_filepath, paste0(mainFileName, "_cnts.txt")))
TotalObs <- as.numeric(cnts[cnts$fips_code == "Grand Total",]$counts) 

cnts_state <- collap(cnts, counts ~ state, FUN = fsum) #collapsing by state, using this vector as state labels (harmonized over datasets)
stateList <- cnts_state[!is.na(cnts_state$state) & !cnts_state$state == "NULL",]$state #vector of State abbreviations to loop over
rm(cnts, cnts_state)

#Importing sample records for variable names, coercing to stata compliant names
unzip(zipfile = paste0(data_filepath, paste0(mainFileName, "_meta.zip")), 
      files = c("SampleRecords.txt"),
      exdir = data_filepath)

SampleRecords <- as.matrix(vroom(paste0(data_filepath, "/SampleRecords.txt"), 
                                 col_names = FALSE, n_max = 1))

#transposing to rows, extracting vector
SampleRecords <- data.frame(t(SampleRecords))
#Removing all special characters to get at STATA compliant names
SampleRecords["StataVariableName"] <- gsub("[[:punct:]]", "", SampleRecords$t.SampleRecords.) 
#Removing spaces
SampleRecords$StataVariableName <- gsub(" ", "", SampleRecords$StataVariableName)
#setting to lowercase
SampleRecords$StataVariableName <- tolower(SampleRecords$StataVariableName)
#deleting numbers 
SampleRecords$StataVariableName <- gsub("1", "one", SampleRecords$StataVariableName)
SampleRecords$StataVariableName <- gsub("2", "two", SampleRecords$StataVariableName)
SampleRecords$StataVariableName <- gsub("3", "three", SampleRecords$StataVariableName)
#subsetting first 32 columns
SampleRecords$StataVariableName <- substr(SampleRecords$StataVariableName, 1, 32)
colNames_to_read <- SampleRecords$StataVariableName #reading the variable names we want to save
rm(SampleRecords)

#COLUMN NAMES TO KEEP (BY NAME!):
colNames_to_keep <- c("clip", "municipalityname",
                      "jurisdictioncountycode", "towncode",
                      "landusecode", "mobilehomeindicator",
                      "zoningcode", "propertyindicatorcode", "numberofbuildings",
                      "viewcode", "locationinfluencecode", "blocklevellatitude", "blocklevellongitude", 
                      "parcellevellatitude", "parcellevellongitude", "situsdeliverypointvalidationvaca",
                      "landvaluecalculated", "improvementvaluecalculated", "assessedyear", "frontfootage", 
                      "depthfootage", "acres", "landsquarefootage", "yearbuilt", "effectiveyearbuilt", "bedroomsallbuildings",
                      "totalroomsallbuildings", "totalbathroomsallbuildings", "numberofbathrooms", 
                      "fullbathsallbuildings", "halfbathsallbuildings", "numberofbathfixtures", 
                      "totalnumberofbathfixturesallbuil",
                      "airconditioningcode", "basementfinishcode", "basementtypecode", "buildingcode",
                      "buildingimprovementcode", "buildingimprovementconditioncode", "constructiontypecode",
                      "exteriorwallcode", "fireplaceindicator", "numberoffireplaces", "fireplacetypecode",
                      "foundationtypecode", "floortypecode", "framecode", "garagecode", "heatingtypecode",
                      "numberofparkingspaces", "parkingtypecode", "poolindicator", "poolcode", 
                      "buildingqualitycode", "roofcovercode", "rooftypecode", "storiescode", "storiesnumber",
                      "buildingstylecode", "numberofunits", "buildingsquarefeet", "livingsquarefeetallbuildings", 
                      "fuelcode", "electricitywiringcode", "sewercode", "utilitiescode","watercode")




#PART 3: LOOP OVER CHUNKS
#The idea (from here) is to match each current assessment with the currect block group using the polygons. 
loop_row = 5000000 #5 million per chunk
no_of_loops = ceiling(TotalObs/loop_row) #calculating how many loops required to cover dataset. Take maximum 100m (roughly half of the transactions data) 

#start loop over chunks here
gc() #garbage collecting

for (chunk in seq(1, no_of_loops)) {   
  # lines to skip in delimit read under current iteration
  loop_skip <- (chunk - 1)*loop_row + 1 
  
  #create dataset if it already does not exist
  if (file.exists(paste0(data_filepath, paste0("/temp/dta/currentAssessments/", 
                                               paste0(chunk, "currentAssess_tmpchunk.dta")))) == FALSE) {
 
  
  #Importing chunk
  Assessment_chunk <- vroom(paste0(data_filepath, paste0(mainFileName, "_data.txt")),
                            skip = loop_skip, n_max = loop_row, #Tells us which batch to import on this loop
                            col_names =  FALSE, skip_empty_rows = FALSE, quote = "")


  colnames(Assessment_chunk) <- colNames_to_read #assigning column names
  Assessment_chunk <- Assessment_chunk %>% dplyr::select(all_of(colNames_to_keep))
  
  #setting geocodes up
  #Replacing parcel coordinates with block coordinates if one parcel coordinate is missing.
  Assessment_chunk$parcellevellatitude[is.na(Assessment_chunk$parcellevellatitude)] <- 
                                        #Replacing missing values with corresponding block level geo information
                                        Assessment_chunk$blocklevellatitude[which(is.na(Assessment_chunk$parcellevellatitude))]
  
  Assessment_chunk$parcellevellongitude[is.na(Assessment_chunk$parcellevellatitude)] <- 
                                            Assessment_chunk$blocklevellongitude[which(is.na(Assessment_chunk$parcellevellatitude))]
  
  Assessment_chunk$parcellevellatitude[is.na(Assessment_chunk$parcellevellongitude)] <- 
                            #Replacing missing values with corresponding block level geo information
                                                  Assessment_chunk$blocklevellatitude[which(is.na(Assessment_chunk$parcellevellongitude))]
  
  Assessment_chunk$parcellevellongitude[is.na(Assessment_chunk$parcellevellongitude)] <- 
                                                 Assessment_chunk$blocklevellongitude[which(is.na(Assessment_chunk$parcellevellongitude))]
  
  #Keeping only assessments with parcel level information
  Assessment_chunk <- Assessment_chunk[!is.na(Assessment_chunk$parcellevellatitude) & 
                                       !is.na(Assessment_chunk$parcellevellongitude),]
  
  #Setting up sf object
  Assessment_chunk["parcellevellongitude_temp"] <- Assessment_chunk$parcellevellongitude #temporary lat/lon to store for SF
  Assessment_chunk["parcellevellatitude_temp"] <- Assessment_chunk$parcellevellatitude
  Assessment_chunk <- st_as_sf(Assessment_chunk, 
                               coords = c("parcellevellongitude_temp", "parcellevellatitude_temp"))
  Assessment_chunk <- st_set_crs(Assessment_chunk, 4269) #setting CRS to be in line with US BLOCK geometry
  
  #Dropping block geography info
  Assessment_chunk <- Assessment_chunk %>% select(-blocklevellatitude, - blocklevellongitude)
  
  #Spatially joining our columns to 2020 definition block groups in sample
  Assessment_chunk <- st_join(Assessment_chunk, US_BLOCK, join = st_intersects) #assumes planar coordinates despite lat/lon. Fine approximation if we are thinking about small geographical regions.
  
 
  
  #removing all parcels not in sample, dropping geometry
  Assessment_chunk <- Assessment_chunk %>% dplyr::filter(!is.na(CBSA))  #Unmatched will have missing in any variable from US_BLOCK
                                                   #Probably about 80% of assessments survive this process.
  #Spatially joining to geocoded definitions of US cities, towns and municipalities (using US Places shapefiles.)
  Assessment_chunk <- st_join(Assessment_chunk, US_PLACE, join = st_intersects) %>% st_drop_geometry()
  
  #FURTHER CLEANING ON VARIABLE BY VARIABLE BASIS
  Assessment_chunk <- Assessment_chunk %>% filter(!is.na(assessedyear) &
                                                  assessedyear >= 2015)    #1.dropping if assessed year before 2015 or NA
  
  Assessment_chunk <- Assessment_chunk %>% mutate(mobilehomeindicator = 
                                                   recode(Assessment_chunk$mobilehomeindicator, `Y` = 1, 
                                                         .missing = 0)) %>%
                                           mutate(fireplaceindicator = 
                                                  recode(Assessment_chunk$fireplaceindicator, `Y` = 1, 
                                                  .missing = 0)) %>% 
                                           mutate(poolindicator = 
                                                              recode(Assessment_chunk$poolindicator, `Y` = 1, 
                                                              .missing = 0))
  
  
  #Writing data as chunked .dta file
  
  write_dta(Assessment_chunk, 
                path = paste0(data_filepath, paste0("/temp/dta/currentAssessments/", 
                                                    paste0(chunk, "currentAssess_tmpchunk.dta"))))
  #with stata compression, this brings things down to approx 250mb per file. This is close. 29GB file, 32GB ram. 
  #Could do better with value labels, etc. 
  
  rm(Assessment_chunk)
  gc()

  } 
} #End loop over chunks

#_ END PROGRAM
rm(list = ls())
gc()