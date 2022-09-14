#Constructs population unit density distributions for France, to compare with the US as a robustness check

#Date Created: April 27th 2022

library(sf)
library(haven) #Reading stata.dta files 
library(dplyr)
library(collapse)
library(ggplot2)

#France Functional Urban Area Commuting zones Boundaries

FRA_urb <- st_read("Data/EU_Data/FunctionalUrbanAreas/FRA_core_commuting.shp")
FRA_urb <- st_transform(FRA_urb, 4326) #creating row number index for matching, adjusting projection
#Need to express in GEODETIC CRS

#Reading in GEOSTAT 1km/1km population grid data for entire EU
GEOSTAT_grid <- st_read("Data/EU_Data/2018/JRC_POPULATION_2018.shp") %>% st_transform(4326)

#Extracting centriods to match 
GEOSTAT_grid <- st_centroid(GEOSTAT_grid) #extracting centriods

#Joining to french FUA's, keeping polygons we want
GEOSTAT_grid <- st_join(GEOSTAT_grid, FRA_urb, join = st_within)
GEOSTAT_grid <- GEOSTAT_grid[!is.na(GEOSTAT_grid$fuacode),]


#Generating population density by FUA
GEOSTAT_grid <- GEOSTAT_grid %>% group_by(fuacode) %>%
                                 mutate(demeaned_popdens = TOT_P_2018/mean(TOT_P_2018, na.rm = TRUE)) #Note: all observations are 1km x 1km, no need to adjust by tract land
#Ordering grids within fua's
GEOSTAT_grid <- GEOSTAT_grid %>% group_by(fuacode) %>% mutate(rank_density_FUA = 
                                                                order(order(TOT_P_2018, decreasing = FALSE))/(max(order(order(TOT_P_2018, decreasing = FALSE))) + 1))


#Reading + Joining land values data to FRA_urb
landval <- read.csv("Data/EU_Data/FrenchLandValues2018/full.csv")
landval <- landval[!is.na(landval$latitude) & !is.na(landval$longitude), ]
landval <- landval %>% select(longitude, latitude, valeur_fonciere, surface_terrain) %>% 
                                rename(land_value = valeur_fonciere)

landval <- st_as_sf(landval, coords = c("longitude", "latitude"))
landval <- st_set_crs(landval, 4326)

landval <- st_join(landval, FRA_urb, join = st_within)
landval <- landval[!is.na(landval$fuacode), ] #keeping matched properties

#Generating land values in each area and collapsing.
landval <- landval %>% st_drop_geometry()
landval <-  collap(landval, land_value ~ fuacode + fuaname, FUN = c("fmedian"))  #taking median to get over outliers, have for 85 FUAs
FRA_urb <- left_join(FRA_urb, landval, by = c("fuacode", "fuaname"))
rm(landval)

FRA_urb <- FRA_urb[!is.na(FRA_urb$land_value), ] #Deleting cities without land value statistics 

#putting land values in GEOSTAT dataframe
temp <- FRA_urb %>% st_drop_geometry()
GEOSTAT_grid <- left_join(GEOSTAT_grid, temp, by = c("fuacode", "fuaname"))
rm(temp)

#Creating quantiles
quantile_FUA_landval <- quantile(FRA_urb$land_value, probs = seq(0, 1, 0.25), na.rm = TRUE)


#CONSTRUCTING GRAPHS FOR URBAN DENSITY PLOT

ggplot() + 
  geom_smooth(method = 'loess', span= 0.5, data = GEOSTAT_grid[GEOSTAT_grid$land_value > as.numeric(quantile_FUA_landval["75%"]) &
                                                               !is.na(GEOSTAT_grid$land_value),],
              aes(x=rank_density_FUA, y=demeaned_popdens, colour = 'Top 25%')) +
  geom_smooth(method = 'loess', span = 0.5, data = GEOSTAT_grid[GEOSTAT_grid$land_value < as.numeric(quantile_FUA_landval["25%"]) &
                                                                !is.na(GEOSTAT_grid$land_value),] ,
              aes(x=rank_density_FUA, y=demeaned_popdens, colour = 'Bottom 25%')) +
  scale_colour_manual(name="Sample", values = c("red", "blue")) +
  xlab("Ranked by Population Density (1km x 1km square)") +
  ylab("Population Density (French FUA Average = 1)") +
  ggsave("Data/EU_Data/Output/tractdens_dist.png", width = 20, height = 12, units = "cm") 

#THEY LOOK EXACTLY EQUAL!
#What cities are contained in each sample?
FUA_urb_bot25 <- FRA_urb[FRA_urb$land_value < as.numeric(quantile_FUA_landval["25%"]),]
FUA_urb_top25 <- FRA_urb[FRA_urb$land_value > as.numeric(quantile_FUA_landval["75%"]),]

