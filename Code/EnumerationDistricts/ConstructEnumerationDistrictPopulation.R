#Construct household density of each enumeration district for 52 cities.

#Use Brown's Urban Transition GIS Project
#Historical GIS files for enumeration districts, and IPUMS extract to assign average rent/house values for each city. 

#Date Created: May 25th 2022
#Date edited: June 21, 2022

library(tidyverse)
library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(collapse)
library(ggplot2)
library(stringr)

ED_HH <- read_dta("Data/US_Data/Historical")

#Lists all cities for which we have data from project
CityDta <- list.files(path = "Data/US_Data/Historical/CityDta/1930", pattern = "*.csv")

#Cities for which we actually have shapefiles--our sample 
CityShp <- list.files("Data/US_Data/Historical/CityShp", pattern = "*.zip")


#Creating master data to be appended over loop. 
Master <- data.frame()


#Loop over cities in CityShp, open household data from CityDta and collapse by ward/enumeration district 

for (i in CityShp) {
  
fName  <- i

#Parsing name of City. 
fNameSplit <- str_split(fName, pattern = "3")
fNameSplit <- fNameSplit[[1]]
fNameSplit <- fNameSplit[1]

fName30 <- paste(fNameSplit, "30", sep = "")

#Importing CSV with data
Dta <- read_csv(paste(paste("Data/US_Data/Historical/CityDta/1930/", fNameSplit, sep = ""), ".csv", sep = ""))

#Importing corresponding Shapefile
#Unzipping
unzip(zipfile = paste("Data/US_Data/Historical/CityShp/", fName, sep = ""), exdir = "Data/US_Data/Historical/CityShp")

Shp <- st_read(paste(paste("Data/US_Data/Historical/CityShp/", fName30, sep = ""), 
                     paste(paste("/", fNameSplit, sep = ""), "_ed30_aggr.shp", sep = ""), sep = ""))

#Removing null geometry from this
Shp <- Shp[!is.na(Shp$ed) & Shp$ed != "." & Shp$ed != "0" & Shp$totalpop > 0,]

#Constructing Data by collapsing. 
#Removing duplicate observations in each household (B_hh)
Dta <- distinct(Dta, B_hh, .keep_all = TRUE)

#Counting households by enumeration district
Dta <- Dta %>% group_by(B_ed) %>% mutate(housing_count = n())
Dta <- collap(Dta, housing_count ~ B_ed, fun = "fmean")
Dta <- Dta %>% rename(ed = B_ed)
Shp["ed"] <- as.numeric(Shp$ed) #destringing

#merging with Shpfile
Shp <- left_join(Shp, Dta, by = "ed")

#Constructing housing unit density
Shp["housing_unit_density"] <- 1000*Shp$housing_count/Shp$Shape_Area
Shp["population_density"] <- 1000*Shp$totalpop/Shp$Shape_Area


#Constructing City names
Shp["City"] <- rep(fNameSplit, nrow(Shp))

#Deleting geometry 
Shp <- Shp %>% st_drop_geometry() 
Shp <- Shp %>% select(ed, population_density, housing_unit_density, City, Shape_Area)

#Appending data
Master <- rbind(Shp, Master)

#END LOOP
}

#Saving data
write_dta(Master, "Data/US_Data/Historical/Enumeration_Dist_HH.dta")


