#' Cleaning script for the fish length to biomass conversion csv file. 
#' There are a few columns that aren't necessary and can be removed for 
#' data management and storage in the repository. 
#' 
#' Steps
#' 
#' 1. Read in data
#' 2. Remove unnecessary columns
#' 3. Save as new csv


# 1. Read in data/libraries
library(dplyr)

fishLW <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A981cfae4-a5bf-4546-bb66-35a429b16843"))

# 2. Remove unncessary columns

fishLW_clean <- fishLW %>%
  dplyr::select(-c(LmaxCompare, EsQ, Entered, C_Code))

# 3. Save as a new csv

# write.csv(fishLW_clean, "Data/fish_length_weight_conversions_cleaned.csv", row.names = FALSE)
