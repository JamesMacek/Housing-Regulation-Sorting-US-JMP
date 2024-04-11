#Date Created: Dec 9, 2022

#This file constructs a series of appendable .dta format files from the transactions
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

#OVERWRITE_FILES
overwrite_files <- TRUE #Set to false if you don't want to overrwrite files

#need to turn of sf_use_s2 (spherical geometry) because of spatial joining issues. Some approximation error using flat geometry
sf::sf_use_s2(FALSE) 

#_________PART 1: setting up for loop over chunks_____________________________________________________

#File name for current assessments
mainFileName <- "/university_of_toronto_ownertransfer_v3_01403805_20221115_080140"

#creating directories for temporary files that are outputs of this program
if (dir.exists(paste0(data_filepath, "/temp/dta/transactions")) == FALSE) {
  
  dir.create(paste0(data_filepath, "/temp/dta/transactions"))
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

#
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

#
colNames_to_keep <- c("clip","fipscode", "ownertransfercompositetransactio", "multiorsplitparcelcode",
                      "primarycategorycode", "deedcategorytypecode", "saletypecode", "saleamount", "salederiveddate",
                      "residentialindicator", "cashpurchaseindicator", "ownershiptransferpercentage",
                      "mortgagepurchaseindicator", "investorpurchaseindicator",
                      "resaleindicator","newconstructionindicator", "foreclosurereoindicator")

#_____________PART 2: LOOP OVER CHUNKS___________________________________________________________________________
#The idea (from here) is to match each current assessment with the currect block group using the polygons. 
loop_row = 10000000 #10 million per chunk
no_of_loops = ceiling(TotalObs/loop_row) 

#start loop over chunks here
gc() #garbage collecting

for (chunk in seq(1, no_of_loops)) {   
 
  # lines to skip in delimit read under current iteration
  loop_skip <- (chunk - 1)*loop_row + 1 
  
  #create dataset if it already does not exist
  if ( file.exists(paste0(data_filepath, paste0("/temp/dta/transactions/", 
                                               paste0(chunk, "transactions_tmpchunk.dta")))) == FALSE |
       overwrite_files == TRUE ) {
    
    
    #Importing chunk
    transactions_chunk <- vroom(paste0(data_filepath, paste0(mainFileName, "_data.txt")),
                              skip = loop_skip, n_max = loop_row, #Tells us which batch to import on this loop
                              col_names =  FALSE, skip_empty_rows = FALSE, quote = "")
    
  
    
    colnames(transactions_chunk) <- colNames_to_read #assigning column names
    transactions_chunk <- transactions_chunk %>% dplyr::select(all_of(colNames_to_keep))
    
    #Recoding code variables to string
    for (j in 1:length(colNames_to_keep)) {
      if (grepl("code", colNames_to_keep[j], fixed = TRUE) == TRUE) {
        
        transactions_chunk[[colNames_to_keep[j]]] <- as.character(transactions_chunk[[colNames_to_keep[j]]])
        
      }
    }
    
    #Keeping arms length transactions with nonmissing sales amounts, clips.
    transactions_chunk <- transactions_chunk %>% filter(!is.na(primarycategorycode) & primarycategorycode == "A" &
                                                        !is.na(saleamount) & !is.na(clip))
    
    #Extracting YearSold, MonthSold, DaySold, keeping observations
    transactions_chunk["yearSold"] <- as.numeric(substr(transactions_chunk$salederiveddate, 1, 4))
    transactions_chunk["monthSold"] <- as.numeric(substr(transactions_chunk$salederiveddate, 5, 6))
    transactions_chunk["daySold"] <- as.numeric(substr(transactions_chunk$salederiveddate, 7, 8))
    transactions_chunk <- transactions_chunk %>% select(-salederiveddate)
    
    #FURTHER CLEANING ON VARIABLE BY VARIABLE BASIS
    transactions_chunk <- transactions_chunk %>% mutate(residentialindicator = 
                                                          recode(transactions_chunk$residentialindicator, `Y` = 1, 
                                                                 .missing = 0)) #recoding residentialindicator to numeric.
    #relabeling fips code
    transactions_chunk <- transactions_chunk %>% rename(fipscode_fromSale = fipscode)    #Saving in .dta format
    
    
    #Saving two datasets, one for historical 2008-2012 transactions and one for >=2016 transactions
    transactions_chunk_out <- transactions_chunk %>% filter(yearSold >= 2016 & !is.na(yearSold)) #Filtering transactions for construction of hedonic indices centered in 2020/2010
    
    write_dta(transactions_chunk_out,
                path = paste0(data_filepath, paste0("/temp/dta/transactions/", 
                                                    paste0(chunk, "transactions_tmpchunk.dta"))))
    
    transactions_chunk_out <- transactions_chunk %>% filter((yearSold >= 2008 & yearSold <= 2012) & !is.na(yearSold)) #historical indices
    write_dta(transactions_chunk_out,
              path = paste0(data_filepath, paste0("/temp/dta/transactions/", 
                                                  paste0(chunk, "transactions_tmpchunk_hist.dta"))))
    
    rm(transactions_chunk, transactions_chunk_out)
    
  }
   
  gc()
}  #end loops over chunks

gc()
rm(list = ls())
