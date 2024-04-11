
#This file processes the 2010-2020 block group crosswalk for geographic harmonization. 
library(sf)
library(dplyr)
library(haven)
library(stringr)


#Read in 2010-2020 crosswalk (note: 2012 definition, not 2010 definition but the changes aren't that large between these two)
crosswalk <- read.csv("DataV2/US_Data/Shapefiles/nhgis_bg2010_bg2020.csv") %>% select(bg2010gj, bg2020gj, wt_hh) #wt_hh--fraction of 2010 blk grp in 2020 blk grp

#creating correct crosswalk format
crosswalk["state_tmp"] <- substr(crosswalk$bg2010gj, 2, 3)
crosswalk["county_tmp"] <- substr(crosswalk$bg2010gj, 5, 7)
crosswalk["tract_tmp"] <- substr(crosswalk$bg2010gj, 9, 14)
crosswalk["blockgrp_tmp"] <- substr(crosswalk$bg2010gj, 15, 15)
crosswalk["GEOID_2012"] <- paste0(crosswalk$state_tmp, crosswalk$county_tmp, crosswalk$tract_tmp, crosswalk$blockgrp_tmp)

crosswalk["state_tmp"] <- substr(crosswalk$bg2020gj, 2, 3)
crosswalk["county_tmp"] <- substr(crosswalk$bg2020gj, 5, 7)
crosswalk["tract_tmp"] <- substr(crosswalk$bg2020gj, 9, 14)
crosswalk["blockgrp_tmp"] <- substr(crosswalk$bg2020gj, 15, 15)
crosswalk["GEOID_2020"] <- paste0(crosswalk$state_tmp, crosswalk$county_tmp, crosswalk$tract_tmp, crosswalk$blockgrp_tmp)
                                                                                                                            
                                                                                                                                                                                                                                                      

crosswalk <- crosswalk %>% select(GEOID_2012, GEOID_2020, wt_hh)




write_dta(crosswalk, "DataV2/US_Data/Shapefiles/blkgrp_2010_2020_crosswalk.dta")
