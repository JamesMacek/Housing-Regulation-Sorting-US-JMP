#This file outputs a map that plots the first stage residual in San Francisco.

#Packages
library(dplyr)
library(haven)
library(labelled)
library(readr)
library(rlang)
library(ggplot2)
library(forcats)
library(sf)
library(haven) #Reading stata.dta files 
sf_use_s2(FALSE) #Switching off spherical geometry
library(here)
library(purrr)
library(collapse)
library(viridis) #for colors
library(patchwork) #combining plots

source("CodeV2/Counterfactual/Functions/OpenStreetMapping_Functions.R")
source("CodeV2/Facts/Parameters/Facts_parameters.R")
source("CodeV2/Facts/Functions/Facts_functions.R")

#importing data
load(file = "DataV2/US_Data/Output/CBSA_med_house_price.Rdata")
load(file = "DataV2/US_Data/Output/Constructed_Block_V2.Rdata")

#Converting density to square miles...
US_BLOCK$Housing_density <- US_BLOCK$Housing_density*640


#Quantiles of city distributions on various statistics for robustness
for (qtile in c("", "_dens", "_pop", "_wage")) {
  load(file = paste0("DataV2/US_Data/Output/CBSA_quantiles", qtile, ".Rdata"))
}

#Reading IV estimates
first_stage <- read_dta("DataV2/US_Data/Instrument/all_IV.dta") %>% 
               select(State, County, Tract, BlockGroup, CBSA, CBSA_NAME, log_Average_Income, baseline_firststage_income, rank_density_CBSA, rank_inv_D2CBD,
                      log_Amenity, log_Amenity_ltype, log_Amenity_mtype, log_Amenity_htype, PooledWage,
                      consumption_Val_l, consumption_Val_m, consumption_Val_h)
#take average amenity levels + amenities for high relative to low skill households

#Plot amenities for high, low and medium skill as function of city productivity
CityCollapse <- collap(first_stage, log_Amenity + log_Amenity_ltype + log_Amenity_mtype + log_Amenity_htype + consumption_Val_l +
                         consumption_Val_m + consumption_Val_h + PooledWage ~ CBSA + CBSA_NAME)

#Very odd result...
ggplot() + geom_smooth(data = CityCollapse, aes(x=rank(PooledWage)/max(rank(PooledWage)), y=log_Amenity_ltype))
ggplot() + geom_smooth(data = CityCollapse, aes(x=rank(PooledWage)/max(rank(PooledWage)), y=log_Amenity_mtype))
ggplot() + geom_smooth(data = CityCollapse, aes(x=rank(PooledWage)/max(rank(PooledWage)), y=log_Amenity_htype)) #only thing that makes sense

#This makes somewhat some sense. 
ggplot() + geom_smooth(data = CityCollapse, aes(x=rank(PooledWage)/max(rank(PooledWage)), y= consumption_Val_l))
ggplot() + geom_smooth(data = CityCollapse, aes(x=rank(PooledWage)/max(rank(PooledWage)), y= consumption_Val_m))
ggplot() + geom_smooth(data = CityCollapse, aes(x=rank(PooledWage)/max(rank(PooledWage)), y= consumption_Val_h))


excluded_controls_income_formula <- as.formula('demeaned_log_Income ~ s(rank_density_CBSA, k = 1, bs = "cr") + rank_inv_D2CBD')


baselineSampleNames =  list("Superstar" = "Top 25% \n Productivity",
                            "nonSuperstar" = "All other cities") #Passes to flexibleEstimation function 

#Baseline regression of income on density, 
Income.plot <- flexibleEstimation(Dataframe_list = list("Superstar" = top,
                                                        "nonSuperstar" = bot),
                                  formula = excluded_controls_income_formula,
                                  SampleNames = baselineSampleNames  ) +
                    xlab("Density quantiles (Block Group level)") +
                    ylab("Log Average Income (demeaned by MSA)") +  
                    theme_gray(base_size = 15) & theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))








#Reading shapefiles
blkgeo <- st_read("DataV2/US_Data/Shapefiles/US_blck_grp_2020.shp") %>% 
  mutate(State = as.numeric(STATEFP), County = as.numeric(COUNTYFP), Tract = as.numeric(TRACTCE),
         BlockGroup = as.numeric(BLKGRPCE)) %>%
  select(State, County, Tract, BlockGroup)


first_stage <- inner_join(blkgeo, first_stage, by = c("State", "County", "Tract", "BlockGroup"))
rm(blkgeo)
first_stage <- st_transform(first_stage, 4326) #to lat/lon coordinates system
SanFran_bbox <- st_bbox(first_stage) #getting bounding box for SF for plot


log_Amenity_sd <- sd(first_stage$log_Amenity, na.rm = TRUE)


#Getting arguments for map
args <- createMappingArgs(boundingBox = SanFran_bbox,
                          additionalZoom = 2)

#Creating map  of average amenities
plot <- ggplot() + coord_sf() +
  pmap(args, annotation_raster, interpolate = TRUE) + 
  geom_sf(data = first_stage, #keep only non-missing
          mapping = aes(fill = rank(log_Amenity)/max(rank(log_Amenity))),  #use quantiles of amenity distribution
          alpha = 0.75, lwd = 0) +
  coord_sf(expand = FALSE) + 
  scale_fill_gradientn(colours=c("orange", "royalblue4"), name = "") +    
  theme_gray(base_size = 20) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
ggsave("DataV2/US_Data/Instrument/log_Amenity_plots.png", plot = plot, 
       width = 35, height = 21.875, units = "cm", type = "cairo") 


#Define relative difference in amenities for high and low skill

first_stage$relative_amenity <- first_stage$log_Amenity_htype - first_stage$log_Amenity_ltype

#Map of relative amenities High - low
plot <- ggplot() + coord_sf() +
  pmap(args, annotation_raster, interpolate = TRUE) + 
  geom_sf(data = first_stage,
          mapping = aes(fill = rank(relative_amenity)/max(rank(first_stage$relative_amenity))), 
          alpha = 0.75, lwd = 0) +
  coord_sf(expand = FALSE) + 
  scale_fill_gradientn(colours=c("blue", "red"), name = "") +    
  theme_gray(base_size = 20) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_text(size = 8),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5)) 
ggsave("DataV2/US_Data/Instrument/relative_log_amenity.png", plot = plot, 
       width = 35, height = 21.875, units = "cm", type = "cairo") 


