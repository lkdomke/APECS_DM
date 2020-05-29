# Data cleaning for beach seine data

#' As apart of a project studying eelgrass ecosystems in southern Southeast Alaska, beach seines 
#' were done along the outer coast of Prince of Wales Island. A 37-m variable mesh net and a 
#' 16ft skiff was used to set a 'round haul' beach seine through eelgrass meadows. Fish were
#' identified to the finest taxonomic level, enumerated, and measured either total 
#' length for most fish species or to fork length if the caudal fin was forked 
#' (e.g. salmon, shiner perch).
#' After 30 measured fish, fish were no longer 
#' measured and lengths will be extrapolated from the 30 measured fish. 
#' 
#' This script is to clean the *dataset from 2017* of the eelgrass-associated fish community. The script will:
#' 1. Classify invertebrates and vertebrates so that only fish can be filtered
#' 2. Convert unmeasured fish to *length measurements*
#' 3. Convert length data to *biomass*
#' 4. Change site_code to new naming scheme that includes two column codes
#' 5. Convert date to julian day and add year column

# Libraries
library(plyr)
library(dplyr)
library(lubridate)
library(readxl)

# Data input
seine <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/knb.92404.1"),
                  stringsAsFactors = FALSE, header = TRUE)
sp_names <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3Abc823c8e-7be3-444b-a872-2e450ea3e85b"),
                     stringsAsFactors = FALSE, header = TRUE)
site_names <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3Ac9c99ce9-fbdd-4879-a2c9-c90448cdba7b"))
fishLW <- read.csv("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A42b3eee9-f6a3-4169-99ef-ed388f46d172", stringsAsFactors = FALSE, header = TRUE)
  
######## Step 1. Classify taxons ######## 
unique(seine$species_common)
# 74 different species, need to remove non-fish species
# are all species on the sp_names csv?
anti_join(seine, sp_names, by = c("sp_code" = "SpCode")) # yes

 # Based on sp_code combine with sp_names--includes taxa
fish.tax <- seine %>%
  group_by(sp_code) %>% 
  left_join(sp_names, by = c("sp_code" = "SpCode")) %>% 
  select(-c(Sp_CommonName, Sp_ScientificName, Taxa))

# remove non-fish species
fish <- fish.tax %>% 
  filter(taxon == "Vertebrata")
 
####### Step 2. Unmeasured -> measured ######## 
# This code comes from WR original script (Eelgrass_biometrics_data_analysis, L 325)

# From the raw data this script will export a file summarized by site ready for analysis. 
#' The goal will be to summaries this data at the site level. Summaries will occur in a few different
#' ways to prep the data for different types on analysis. There's 1 file generated here. 

#' All we need is the beach seine data and the fish length-mass conversions. 

# make sure columns have appropriate character type
glimpse(fish)
fish$unmeasured <- as.numeric(fish$unmeasured)

## Separate measured and unmeasured fish

fish.m <- fish %>%
  #filter(taxon == "Vertebrata") %>% done above
  filter(fork_length != "NA")


fish.um <- fish %>%
  # filter(taxon == "Vertebrata") %>% done above
  filter(unmeasured != "estimate") %>% # assume that infield estimates are accurate
  filter(is.na(fork_length)) # have 96 observations that need to be assigned lenghts


## Assign lengths to unmeasured fish
#' When beach seining we only measured the first 30 individuals of a species, and counted the rest. 
#' We can use the measured fishes to create a distribution from which we can assign 
#' lengths to the unmeasured fishes.

#' Assign lengths to unmeasured fish based on sampled distribution.
#'  This assignment should happen at the EventID level. i.e. use the distribution of fishes at a 
#'  EventID to assign unmeasured fishes at that EventID. 

#' We will assume the least and just use the sampled proportions to assign lenghts to unmeasured fish. 
# Exclued fishes that do not have a measured counterpart. These were insantces when we 
# tossed a fish without knowing what it was other than it was a sculpin thing


# figure out which species at sites that there is not a measured conterpart 
x <- fish.um %>%
  group_by(site, sp_code) %>%
  anti_join(fish.m, by = c("site", "sp_code"))

# want to remove these unmeasured fish from unmeasured df
fish.um.redu <- fish.um %>%
  group_by(site, sp_code) %>%
  anti_join(x, by = c("site", "sp_code"))

# in addition want to drop UNFISH (row 89) because even though it has a measured equivalent
# (only 1) it could be any fish and not the same species
fish.um.redu <- fish.um.redu[-89,]
# Now calculate a fork_length/total_length for each unmeasured fish based on the 
# distribution of the existing fish BY site (one site per seine). THIS IS FOR 2017 ONLY

d <- data.frame() # empty dataframe to fill with for loop

for(s in unique(fish.um.redu$site)){ # cycle through sites
  dat.m <- fish.m %>% # subset measured data
    filter(site == s)
  dat.um <- fish.um.redu %>% #subset unmeasured data
    filter(site == s)
  for(i in unique(dat.um$sp_code)){ #cycle through species that are in UNMEASURED data
      samp <- dat.m %>% # create sample from which to make distrubution
        filter(sp_code == i)
      unmeas <- dat.um %>% # isolate unmeasured fish
        filter(sp_code == i)
      unmeas <- as.numeric(unmeas$unmeasured) # save unmeasured value
      dat <- data.frame(size = as.character(samp$fork_length))
      dat2 <- dat %>% 
        group_by(size) %>% 
        dplyr::summarise(count = n())
      dat2$porb <- (dat2$count/sum(dat2$count))
      dat2$x <- as.numeric(paste(dat2$size))
      fx <- function(n){ # function derived from limits and probabilities of above
        sample(x = min(dat2$x):max(dat2$x), n, replace = TRUE, prob = dat2$prob)
      }
      dat3 <- data.frame(site = s, sp_code = i, fork_length = fx(unmeas))
      d <- rbind(d, dat3)
    }
} 
# this is returning an error in regards to prob, but i am not sure what its talking about

#' Append assigned lengths to master data with all site level data
## Extract site and sp data ##
fish.site <- unique(fish[,c("site", "date", "YYYYMMDD", "start_time", "end_time", "slope", "tide_height", 
                          "tide_time")])
fish.sp <- unique(fish[,c("species_common", "species_scientific", "sp_code", "taxon")])

## Merge with loop output, the lengths of unmeasured fishes ##
d.info <- left_join(d, fish.site, by = "site")

## Add species detail ##
d.info <- merge(d.info, fish.sp, by = "sp_code")

test <- anti_join(d.info, d, by = "sp_code")

## Merge with original measured fishes ##
fish.all <- bind_rows(fish.m, d.info)


# Do some basic checks to make sure the the conversion from unmeasured to measured worked correctly
unique(as.factor(fish.all$site)) # 21 sites, correct
unique(as.factor(fish.all$sp_code)) # 56 levels of fish species
unique(as.factor(fish$sp_code)) # 56 levels of fish species in the original dataframe, correct! 7 unknown sp. 

# count total number of fish in original dataframe
fish$abundance <- as.numeric(ifelse(is.na(fish$fork_length), paste(fish$unmeasured), 1)) 

sum_fish <- fish %>%
  group_by(site) %>%
  summarise(abundance = sum(abundance))

sum_fish %>%
  summarise(total = sum(abundance)) #29744

# does this match the fish.all dataframe?
fish.all$abundance <- as.numeric(ifelse(is.na(fish.all$fork_length), paste(fish.all$unmeasured), 1)) 

fish.all.sum <- fish.all %>%
  group_by(site) %>%
  summarise(abundance = sum(abundance))

fish.all.sum %>%
  summarise(total = sum(abundance)) # 29986, there is a 242 fish discrepancy... less than a 1% difference. 



####### Step 3. Convert length to biomass ######## 
## Calculate Biomass
#' Using the length-weight conversion values individual lengths will be converted to biomass. 
#' Coefficients are in cm*g so fork lengths will need to be converted to cm from mm.

## Calculate Biomass
#' Using the length-weight conversion values individual lengths will be converted to biomass. 
#' Coefficients are in cm*g so fork lengths will need to be converted to cm from mm.

#' First the L-W conversion data will need to be prepped for use. 
#' This will include defineing one a value (collapse the a TL column), 
#' define sp_code for new species, and filter out estimates that were made from 
#' SL (standard length) and fish calssified as UNFISH or unidentified fish, 
#' and then summarise by taking the average a and b value for each species.


## put all a values in one column ##
fishLW$a_cm.g <- ifelse(is.na(fishLW$a_cm.g), fishLW$aTL_cm.g, fishLW$a_cm.g)

## define sp_code for new species ##
# Shorthorn sculpin #
fishLW$sp_code <- ifelse(fishLW$species_common == "Shorthorn sculpin", paste("SCULSHRN"), fishLW$sp_code)

# Longhorn sculpin #
fishLW$sp_code <- ifelse(fishLW$species_common == "Longhorn sculpin", paste("SCULLHRN"), fishLW$sp_code)

# Rockfish general #
fishLW$sp_code <- ifelse(fishLW$species_common == "Rockfish", paste("ROCKSEB"), fishLW$sp_code)

## Filter and summarise ##
LW <- fishLW %>%
  filter(Type != "SL") %>% 
  filter(sp_code != "UNFISH") %>%
  group_by(sp_code) %>% 
  dplyr::summarise(a = mean(a_cm.g),
            b = mean(b_cm.g))

#' We know that we do not have estimates for a and b estiamtes for all the species so we need to apply other values to other species. First identify what species we need to have stand ins for
#' 1. "UNSCUL" - Unidentified sculpins will use the average a and b values for all sculpins species.
#' 2. "UNFLAT" - Unidentified flatfish will use the average a and b values for all flatfish species.
#' 3. "UNGREEN" - Unidentified greenling will use the average a and b values for all greenling speices.
#' 4. "UNMYOXO" - Unidentified Myoxocephalus sp. will use the the average of a and b values of sampled members of the Myoxocephalus genus.
#' 5. "UNGUNN" - Unidentified gunnel will use average a and b values from the sampled gunnel species
#' 6. "UNARTE" - Unidentified Aretedius sp. will use the average a nad b values of sampled members of the Artedeius genus.


## Unique species in master data ##
sp <- data.frame(sp_code = unique(fish.all$sp_code))

## Species that need stand ins ##
nomatch <- anti_join(sp, LW, by = "sp_code")

## Summarising stand ins ##
# Sculpins
scul <- data.frame(sp_code = "UNSCUL", fishLW %>% 
                     filter(sp_code == "SCULBUF" | sp_code == "SCULGRT" | sp_code == "SCULGRU" |sp_code == "SCULLHRN" | sp_code == "SCULMAN" |sp_code == "SCULPAD" | sp_code == "SCULPSTG" | sp_code == "SCULSAIL" | sp_code == "SCULSHRN" | sp_code =="SCULSILV" | sp_code == "SCULSMO" | sp_code == "SCULTIDE") %>% 
                     summarise(a = mean(a_cm.g),
                               b = mean(b_cm.g)))

# Flats
flat <- data.frame(sp_code = "UNFLAT", fishLW %>% 
                     filter(sp_code == "SDABPAC" | sp_code == "SDABSPKL" | sp_code == "SOLEBUT" |sp_code == "SOLECO" | sp_code == "SOLEENG" |sp_code == "SOLEROC" | sp_code == "FLOUNST") %>% 
                     summarise(a = mean(a_cm.g),
                               b = mean(b_cm.g)))

# Greenlings
grns <- data.frame(sp_code = "UNGREEN", fishLW %>% 
                     filter(sp_code == "GREENKEL" | sp_code == "GREENMAS" | sp_code == "GREENWHI") %>% 
                     summarise(a = mean(a_cm.g),
                               b = mean(b_cm.g)))

# Myoxocephalus
myoxo <- data.frame(sp_code = "UNMYOXO", fishLW %>% 
                      filter(sp_code == "SCULGRT" | sp_code == "SCULLHRN" | sp_code == "SCULSHRN") %>% 
                      summarise(a = mean(a_cm.g),
                                b = mean(b_cm.g)))

# Gunnels
gunn <- data.frame(sp_code = "UNGUNN", fishLW %>% 
                     filter(sp_code == "GUNNCRE" | sp_code == "GUNNPEN") %>% 
                     summarise(a = mean(a_cm.g),
                               b = mean(b_cm.g)))

arte <- data.frame(sp_code = "UNARTE", fishLW %>% 
                     filter(sp_code == "SCULPAD" | sp_code == "SCULSMO") %>% 
                     summarise(a = mean(a_cm.g),
                               b = mean(b_cm.g)))

## Master a b lsit ##
LW.master <- rbind.data.frame(LW, scul, flat, grns, myoxo, gunn, arte)

## Filter out UNFISH from fish.all ##
fish.all <- fish.all %>% 
  filter(sp_code != "UNFISH")

## Check that everything is cool ##
anti_join(fish.all, LW.master, by = "sp_code") # its all cool

# Finally join the a and b values from above with the master data and calcualte biomass.
## Merge a and b values ##
fish.all.m <- merge(fish.all, LW.master, by = "sp_code")

## Convert mm to cm ##
fish.all.m$fork_length_cm <- fish.all.m$fork_length / 10

## Calculate mass in g ##
fish.all.m$mass_g <- (fish.all.m$a * fish.all.m$fork_length_cm^fish.all.m$b)

## some plots, cuz ya know ##
hist(fish.all.m$mass_g)
hist(log(fish.all.m$mass_g))
range(fish.all.m$mass_g)

## Clean up ##
fish.all.m$taxon[is.na(fish.all.m$taxon)] <- "Vertebrata"

# remove unused rows and readd in tide height
# extra tide height from original df
tide_h <- fish %>%
  group_by(site) %>%
  dplyr::select(site, tide_height) %>%
  distinct(tide_height)

fish.all.m <- fish.all.m %>%
  left_join(tide_h, by = "site") %>%
  dplyr::select(-c(sex, abundance, tide_height.y)) %>%
  rename(tide_height = tide_height.x)

## Checking ##
fish.site <- fish.all.m %>% 
  group_by(site) %>% 
  summarise(mass_g = sum(mass_g))

fish.site$mass_kg <- fish.site$mass_g / 1000

boxplot(fish.site$mass_g)

m02 <- fish.all.m[fish.all.m$site == "2017_M_02",]


######## Step 4. Convert date to JD and add year column ######## 

fish.all.m <- fish.all.m %>% 
  mutate(date = mdy(date)) %>% # convert to date format
  mutate(julian = yday(date)) %>% # make the date a julian day
  mutate(year = year(date)) # make a new column with just year


######## Step 5. Convert site names to 2-column code system ######## 

fish.all.m <- fish.all.m %>% 
  left_join(site_names, by = c("site" = "site_2017")) %>% # this needs to be changed for e/a YEAR. 
  select(-c(site_2018, freshwater, general_description, sediment_description, siteID_NOAA, study, a, b, fork_length_cm, 
            site_2019, site, sex, unmeasured, abundance, YYYYMMDD))

names(fish.all.m)
# place name gives the physical location, bay_code and bay_sample together give you each
# unique site that was sampled. 

## Export
# Export of full mass derived csv

#write.csv(fish.all.m, "Data/fish_mass_2017_derived.csv", row.names = FALSE)



