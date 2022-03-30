#ZAsmt_PROBE DATA

#Probing data for Montana's historical assessment data as a sample. 
library(readr)
library(dplyr)


setwd("Z:/Dropbox/SchoolFolder/Projects/Zoning/ZillowData/ZAsmt_Montana")

Files = c("Building", "BuildingAreas", "Garage", "LotSiteAppeal",
          "Main", "Value") #different delimited text files, See Camilo Acosta's stuff.

#Importing + Codebooks
#___________________________________________________________
for (i in Files) {
  
AssignVal <- read_delim(paste(i, ".txt", sep = ""), delim = "|", col_names = FALSE) #Data frame
colnames(AssignVal) <- pull(read_csv(paste(i, "_colname.csv", sep = ""), col_names = FALSE), X1) #Pull converts to vector (not data frame)
assign(i, AssignVal)  #to new variable
}

rm(AssignVal) #from memory

#___________________________________________________________