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

# 3. Fix species code for Brown Irish Lord
fishLW_clean$sp_code <- as.factor(fishLW_clean$sp_code)
levels(fishLW_clean$sp_code)[levels(fishLW_clean$sp_code)=="UNLORD"] <- "LORDBI"
fishLW_clean$sp_code <- as.character(fishLW_clean$sp_code)
unique(fishLW_clean$sp_code)

# 4 . Add in new rows
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

df10 <- data.frame("Ribbon snailfish", "Liparis cyclopus", "SNAILFR", "NA", "11.4", "TL", "NA", 
                   "Male/unsexed", "NA", "0.00457", "3.17", "NA", "NA", "NA", "NA", "NA", "0.00217",
                   "0.00962", "2.98", "3.36", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df11 <- data.frame("Tidepool snailfish", "Liparis florae", "SNAILFTI", "NA", "18.3", "TL", "NA", 
                   "Male/unsexed", "NA", "0.00457", "3.17", "NA", "NA", "NA", "NA", "NA", "0.00217",
                   "0.00962", "2.98", "3.36", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df12 <- data.frame("Showy snailfish", "Liparis pulchellus", "SNAILFSH", "NA", "25.0", "TL", "NA", 
                   "Male/unsexed", "NA", "0.00457", "3.17", "NA", "NA", "NA", "NA", "NA", "0.00217",
                   "0.00962", "2.98", "3.36", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df13 <- data.frame("Ringtail snailfish", "Liparis rutteri", "SNAILFRT", "NA", "7.0", "SL", "NA", 
                   "Male/unsexed", "NA", "0.00457", "3.17", "NA", "NA", "NA", "NA", "NA", "0.00217",
                   "0.00962", "2.98", "3.36", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df14 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "16.0", "40.0", "TL", "4019", 
                   "Mixed", "0.03200", "NA","2.737", "0.997", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "Peter the Great Bay, Sea of Japan", "NA", "NA")

df15 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "9.0", "47.0", "TL", "3037", 
                   "Mixed", "0.1186", "NA","2.997", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "Eastern Bering Sea, USA", "NA", "NA")

df16 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "11.0", "43.0", "TL", "2890", 
                   "Mixed", "0.01221", "NA","3.010", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "Terpeniya Bay, SE Sakhalin Russia", "NA", "NA")

df17 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "NA", "NA", "TL", "NA", 
                   "male", "0.00988", "NA","3.043", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "Alaska", "NA", "NA")

df18 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "NA", "NA", "FL", "340", 
                   "unsexed", "0.01170", "NA","3.043", "0.960", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "Olyutorsky-Navarin region, Russia", "NA", "NA")

df19 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "NA", "NA", "TL", "NA", 
                   "female", "0.00768", "NA","3.123", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "Eastern Bering Sea, Alaska", "NA", "NA")

df20 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "4.0", "40.0", "TL", "875", 
                   "unsexed", "0.00760", "NA","3.153", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "Sakhalin Island, Russia", "NA", "NA")

df21 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "9.0", "45.0", "TL", "2975", 
                   "Mixed", "0.00455", "NA","3.266", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "North Tatar Strait, Russia", "NA", "NA")

df22 <- data.frame("Yellowfin Sole", "Limanda aspera", "SOLEYEL", "NA", "NA", "FL", "150", 
                   "unsexed", "0.00410", "NA","3.333", "0.922", "NA", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "type I linear regression", "Olyutorsky-Navarin region, Russia", "NA", "NA")

df23 <- data.frame("Northern ronquil", "Ronquilus jordani", "RONQLNOR", "NA", "NA", "TL", "NA",
                   "unsexed", "0.00389", "NA", "3.12", "NA", "NA", "NA", "NA", "NA", "0.00180", "0.00842",
                   "2.94", "3.30", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df24 <- data.frame("Sockeye salmon", "Oncorhynchus nerka", "SALSOCK", "65.0", "65.0", "TL", "NA", 
                   "unsexed", "0.01047", "NA", "3.03", "NA", "NA", "NA", "NA", "NA", "0.00484", "0.02265",
                   "2.86", "3.20", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df25 <- data.frame("Red Irish lord", "Hemilepidotus hemilepidotus", "LORDRI", "NA", "NA", "TL", "NA",
                   "unsexed", "0.00724", "NA", "3.13", "NA", "NA", "NA", "NA", "NA", "0.00309", "0.01700",
                   "2.92", "3.34", "FishBase - Bayesian Ref. 93245", "NA", "93245", "NA")

df26 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "FL", "NA", 
                   "mixed", "0.01920", "NA", "2.821", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "south Saskatchewan River system, Canada", "NA", "NA")

df27 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "SL", "241", 
                   "unsexed", "0.02900", "NA", "2.825", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "Logan River, Utah, USA", "NA", "NA")

df28 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "FL", "NA", 
                   "unsexed", "0.1570", "NA", "2.827", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "Oregon, streams, USA", "NA", "NA")

df29 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "FL", "348", 
                   "mixed", "0.00995", "NA", "2.930", "0.940", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "Bear Lake, Utah-Idaho, USA", "NA", "NA")

df30 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "FL", "NA", 
                   "unsexed", "0.01290", "NA", "2.948", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "Oregon, streams, USA", "NA", "NA")

df31 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "SL", "351", 
                   "unsexed", "0.02080", "NA", "2.953", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "Henry's Lake, Idaho, USA", "NA", "NA")

df32 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "TL", "NA", 
                   "unsexed", "0.00865", "NA", "3.000", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "Yellowstone Lake, Amica C., Wyoming, USA", "NA", "NA")

df33 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "TL", "426", 
                   "juvenile", "0.00649", "NA", "3.000", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "Trapper's Lake, Colorado, USA", "NA", "NA")

df34 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "FL", "NA", 
                   "unsexed", "0.00652", "NA", "3.086", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "California, USA", "NA", "NA")

df35 <- data.frame("Cutthroat trout", "Oncorhynchus clarkii", "CUTTHRT", "NA", "NA", "NA", "NA", 
                   "unsexed", "0.00650", "NA", "3.086", "NA", "NA", "NA", "NA", "NA", "NA", "NA",
                   "NA", "NA", "type I linear regression", "Blue Lake California, USA", "NA", "NA")

names(df) <- LWnames
names(df2) <- LWnames
names(df3) <- LWnames
names(df4) <- LWnames
names(df5) <- LWnames
names(df6) <- LWnames
names(df7) <- LWnames
names(df8) <- LWnames
names(df9) <- LWnames
names(df10) <- LWnames
names(df11) <- LWnames
names(df12) <- LWnames
names(df13) <- LWnames
names(df14) <- LWnames
names(df15) <- LWnames
names(df16) <- LWnames
names(df17) <- LWnames
names(df18) <- LWnames
names(df19) <- LWnames
names(df20) <- LWnames
names(df21) <- LWnames
names(df22) <- LWnames
names(df23) <- LWnames
names(df24) <- LWnames
names(df25) <- LWnames
names(df26) <- LWnames
names(df27) <- LWnames
names(df28) <- LWnames
names(df29) <- LWnames
names(df30) <- LWnames
names(df31) <- LWnames
names(df32) <- LWnames
names(df33) <- LWnames
names(df34) <- LWnames
names(df35) <- LWnames

names(df) <- c("species_common", "species_scientific", "sp_code", "LengthMin_cm","LengthMax_cm",
               "Type", "Number", "Sex", "a_cm.g", "aTL_cm.g", "b_cm.g", "CoeffDetermination", 
               "SEa", "SEb", "SDa", "SDb", "a.CI.lwr", "a.CI.upr", "b.CI.lwr", "b.CI.upr",
               "Method", "Locality", "DataRef", "Comment")

# merge together
dat <- bind_rows(df, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, 
                 df15, df16, df17, df18, df19, df20, df21, df22, df23, df24, df25, df26, df27,
                 df28, df29, df30, df31, df32, df33, df34, df35)
fishLW_clean2 <- rbind(fishLW_clean, dat)
# 3. Save as a new csv

#write.csv(fishLW_clean2, "Data/fish_length_weight_conversions_cleaned_2021.csv", row.names = FALSE)
