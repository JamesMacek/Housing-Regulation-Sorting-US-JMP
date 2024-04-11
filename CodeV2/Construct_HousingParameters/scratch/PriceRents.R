#Construct a price-to-rent ratio 
#dataset using Zillow official housing statistics. This is to translate land value stringency into yearly rental equivalent
#By city. 

#Date created: June 29th 2023

library(dplyr)
library(haven)
library(labelled)
library(sf)
library(collapse)
library(fuzzyjoin)
library(stringr)
library(doParallel)

#Importing rent and housing price (smoothed, quality adjusted via repeat sales index)

#Note: incomplete rent coverage across MSAs.
Rent <- read.csv("DataV2/US_Data/ZillowPriceRents/Metro_zori_sm_month.csv") %>% select(RegionName, RegionType, starts_with("X2020"), starts_with("X2019"),
                                                                                       starts_with("X2018"), starts_with("X2017"), starts_with("X2016")) %>%
                                                                                rename(CBSA_NAME = RegionName)#extract jan30th, 2020 price rents
Price <- read.csv("DataV2/US_Data/ZillowPriceRents/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv") %>% select(RegionName, RegionType, starts_with("X2020"), starts_with("X2019"),
                                                                                                                      starts_with("X2018"), starts_with("X2017"), starts_with("X2016")) %>% #extract jan30th, 2020 price rents
                                                                                                               rename(CBSA_NAME = RegionName)
#Average price and rent data over 4 year period. Taking price-to-rent means we partial out inflation, assuming roughly balanced sample

#TAKING AVERAGE ACROSS EACH MONTH IN 2020
Rent["Rent"] <- rowMeans(as.matrix(select(Rent, starts_with("X20"))), na.rm = TRUE)
Rent <- Rent %>% select(-starts_with("X20")) %>% mutate(Rent = replace(Rent, is.nan(Rent), NA))
Price["House_value"] <- rowMeans(as.matrix(select(Price, starts_with("X20"))), na.rm = TRUE)
Price <- Price %>% select(-starts_with("X20"))

Price <- full_join(Price, Rent)

#US Average price to rent
US_price_to_rent <- (Price[Price$CBSA_NAME == "United States",]$House_value)/(Rent[Rent$CBSA_NAME == "United States",]$Rent*12) #12.4

#Some matches are due to names of municipalities being the same (despite being in different states). Extracting states
Price_split <- str_split(Price$CBSA_NAME, pattern = " ")
#taking last element
Price["State"] <- foreach(i = 1:nrow(Price), .combine = c) %do% {
  return(Price_split[[i]][length(Price_split[[i]])])
  
}
rm(Price_split)

Price$CBSA_NAME <- str_replace(Price$CBSA_NAME, "," , "")

#Importing main empirical dataset for list of CBSAs in sample 
MSA <- read_dta("DataV2/US_Data/Output/Constructed_Block_V2.dta") %>% select(CBSA, CBSA_NAME) 
MSA <- MSA[!duplicated(MSA),]

#We need to match these data on strings.

#Parsing out all major municipalities in MSA
MSA_split <- str_split(MSA$CBSA_NAME, pattern = ",") #TAKING OUT STATES
MSA["name_noState"] <- foreach(i = 1:nrow(MSA), .combine = c) %do% {
                        return(MSA_split[[i]][1])
  
}
MSA["State"] <- foreach(i = 1:nrow(MSA), .combine = c) %do% {
  return(MSA_split[[i]][2])
  
}

MSA_split <- str_split(MSA$name_noState, pattern = "-")
#Checking max length
max <- 0
for (i in 1:length(MSA_split)) {
  if (max < length(MSA_split[[i]])) {
      max <- length(MSA_split[[i]])
  }
}
#Getting list in dataframe
for (col in 1:max) {
  MSA[paste0("CBSA_NAME_", col)] <- foreach(i = 1:nrow(MSA), .combine = c, .errorhandling = "pass") %do% {
                                        if (!is.na(MSA_split[[i]][col])) {
                                          if (MSA_split[[i]][col] != "") {
                                            return(MSA_split[[i]][col])
                                          }else{
                                            return("")
                                          }
                                        }else{
                                          return("")
                                        }
                                    }

}

MSA$CBSA_NAME <- ""
for (col in 1:max) {
  
  if (col == 1) {
    MSA["CBSA_NAME"] <- paste0(MSA$CBSA_NAME, MSA[[paste0("CBSA_NAME_", col)]])
  }
  
  if (col != 1) {
    MSA["CBSA_NAME"] <- paste0(MSA$CBSA_NAME, " ",  MSA[[paste0("CBSA_NAME_", col)]])
  }
}
rm(MSA_split)

#Fuzzy matching MSAs to zillow regions
Joined <- stringdist_join(MSA, Price, 
                          by= paste0("CBSA_NAME"),
                          mode = "full",
                          max_dist = 0.5 ,
                          method = "jw",
                          distance_col = "dist")
Joined <- Joined %>% group_by(CBSA) 

#Now, matching on state because different cities share the same name from different states

#Splitting up all states from MSA sample
State_split <- str_split(Joined$State.x, pattern = "-")
max <- 0 #finding max number of states
for (i in 1:length(State_split)) {
  if (max < length(State_split[[i]])) {
    max <- length(State_split[[i]])
  }
}

for (col in 1:max) {
  Joined[paste0("State.x_", col)] <- foreach(i = 1:nrow(Joined), .combine = c, .errorhandling = "pass") %do% {
    if (!is.na(State_split[[i]][col])) {
      if (State_split[[i]][col] != "") {
        return(State_split[[i]][col])
      }else{
        return("")
      }
    }else{
      return("")
    }
  }
  
}

#Removing spaces from first column
Joined$State.x_1 <- str_replace(Joined$State.x_1, " ", "")

#Checking to see if at least one state from State.y matches up with one state in State.x_`col`
Joined["State_match"] <- 0 #running match variable
for (col in 1:max) {
  
  Joined$State_match <- Joined$State_match + ifelse(Joined[[paste0("State.x_", col)]] == Joined$State.y, 1, 0)
  
}
Joined <- Joined %>% filter(State_match == 1) 

#Now, matching on minimum distance
Joined <- Joined %>% group_by(CBSA) %>% mutate(match = ifelse(dist == min(dist), 1, 0)) %>% filter(match == 1)
#All metros matched on this


Joined <- select(Joined, CBSA, name_noState, State.x, House_value, Rent)

Joined["Price_to_rent"] <- Joined$House_value/(Joined$Rent*12) #in yearly rent
print(paste0("We have price to rent data for ", length(Joined[!is.na(Joined$Price_to_rent),]$Price_to_rent), " metros"))

#Imputing the rest with US average price to rent
Joined["impute_price_to_rent"] <- ifelse(is.na(Joined$Price_to_rent), 1, 0)
Joined$Price_to_rent[Joined$impute_price_to_rent == 1] <- US_price_to_rent

print(head(arrange(Joined, Price_to_rent)))
print(head(arrange(Joined, desc(Price_to_rent))))

#Saving
write_dta(select(Joined, CBSA, Price_to_rent, impute_price_to_rent), "DataV2/US_Data/Output/price_to_rent_by_city.dta")
 
