#' Cleaning script for the fish length to biomass conversion csv file. 
#' There are a few columns that aren't necessary and can be removed for 
#' data management and storage in the repository. 
#' 
#' Steps
#' 
#' 1. Read in data
#' 2. Remove unnecessary columns
#' 3. Add in rows of species caught no in LW 
#' 4. Save as new csv


# 1. Read in data/libraries
library(dplyr)

fishLW <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A981cfae4-a5bf-4546-bb66-35a429b16843"), 
                   header = TRUE)

# 2. Remove unncessary columns

fishLW_clean <- fishLW %>%
  dplyr::select(-c(LmaxCompare, EsQ, Entered, C_Code))

#3. Add in new rows
# Remove list of column names
LWnames <- colnames(fishLW_clean)
# add in Slender eelblenny
df <- data.frame("Slender eelblenny", "Lumpenus fabricii", "SLENEEL", "NA", "23.0", "TL", "NA", 
                 "Male/unsexed", "NA", "0.00389", "3.12", "NA", "NA", "NA", "NA", "NA", "0.00180",
                 "0.00842", "2.94", "3.30", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df2 <- data.frame("Roughback sculpin", "Chitonotus pugetensis", "SCULRGH", "NA", "36.5", "TL", "NA", 
                 "Male/unsexed", "NA", "0.00646", "3.16", "NA", "NA", "NA", "NA", "NA", "0.00284",
                 "0.01468", "2.96", "3.36", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df3 <- data.frame("Capeline", "Mallotus villosus", "CAPELIN", "NA", "20.0", "TL", "NA", 
                  "Male/unsexed", "NA", "0.00372", "3.19", "NA", "NA", "NA", "NA", "NA", "0.00270",
                  "0.00510", "3.10", "3.28", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df4 <- data.frame("Bay Goby", "Lepidogobius lepidus", "GOBYBAY", "NA", "10.0", "TL", "NA", 
                  "Male/unsexed", "NA", "0.00851", "3.08", "NA", "NA", "NA", "NA", "NA", "0.00459",
                  "0.01580", "2.92", "3.24", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df5 <- data.frame("Blackeye Goby", "Rhinogobiops nicholsii", "GOBYBE", "NA", "15.0", "TL", "NA", 
                  "Male/unsexed", "NA", "0.00871", "3.08", "NA", "NA", "NA", "NA", "NA", "0.00414",
                  "0.01834", "2.90", "3.26", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df6 <- data.frame("Painted greenling", "Oxylebius pictus", "GREENPAI", "NA", "25.0", "TL", "NA", 
                  "Male/unsexed", "NA", "0.01122", "3.04", "NA", "NA", "NA", "NA", "NA", "0.00514",
                  "0.02450", "2.87", "3.21", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df7 <- data.frame("Rock greenling", "Hexagrammos lagocephalus", "GREENROC", "NA", "61.0", "TL", "NA", 
                  "Male/unsexed", "NA", "0.00776", "3.14", "NA", "NA", "NA", "NA", "NA", "0.00438",
                  "0.01377", "2.98", "3.30", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df8 <- data.frame("Kelp perch", "Brachyistius frenatus", "PERCHKE", "NA", "22.0", "TL", "NA", 
                  "Male/unsexed", "NA", "0.01318", "3.05", "NA", "NA", "NA", "NA", "NA", "0.00542",
                  "0.03207", "2.84", "3.26", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df9 <- data.frame("Northern spearnose poacher", "Agonopsis vulsa", "POASPEAR", "NA", "20.0", "TL", "NA", 
                  "Male/unsexed", "NA", "0.00389", "3.12", "NA", "NA", "NA", "NA", "NA", "0.00180",
                  "0.00842", "2.94", "3.30", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")
names(df) <- LWnames
names(df2) <- LWnames
names(df3) <- LWnames
names(df4) <- LWnames
names(df5) <- LWnames
names(df6) <- LWnames
names(df7) <- LWnames
names(df8) <- LWnames
names(df9) <- LWnames

names(df) <- c("species_common", "species_scientific", "sp_code", "LengthMin_cm","LengthMax_cm",
               "Type", "Number", "Sex", "a_cm.g", "aTL_cm.g", "b_cm.g", "CoeffDetermination", 
               "SEa", "SEb", "SDa", "SDb", "a.CI.lwr", "a.CI.upr", "b.CI.lwr", "b.CI.upr",
               "Method", "Locality", "DataRef", "Comment")

# merge together
dat <- bind_rows(df, df2, df3, df4, df5, df6, df7, df8, df9)
fishLW_clean2 <- rbind(fishLW_clean, dat)
# 3. Save as a new csv

write.csv(fishLW_clean2, "Data/fish_length_weight_conversions_cleaned.csv", row.names = FALSE)
