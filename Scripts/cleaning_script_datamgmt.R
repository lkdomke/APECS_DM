# Data cleaning for beach seine data

#' As apart of a project studying eelgrass ecosystems in southern Southeast Alaska, beach seines 
#' were done along the outer coast of Prince of Wales Island. A 37-m variable mesh net and a 
#' 16ft skiff was used to set a 'round haul' beach seine through eelgrass meadows. Fish were
#' identified to the finest taxonomic level, enumerated, and measured either total length for most fish species 
#' or to fork length if the caudal fin was forked (e.g. salmon, shiner perch).
#' After 30 measured fish, fish were no longer 
#' measured and lengths will be extrapolated from the 30 measured fish. 
#' 
#' This script is to clean the dataset from 2017 of the eelgrass-associated fish community. The script will:
#' 1. Classify invertebrates and vertebrates so that only fish can be filtered
#' 2. Convert date to julian day and add year column
#' 4. Convert unmeasured fish to *length measurements*
#' 5. Convert length data to *biomass*
#' 3. Change site_code to new naming scheme that includes two column codes

# Libraries
library(plyr)
library(dplyr)
library(lubridate)
library(readxl)

# Data input
## Include a better version of this csv... 
seine <- read.csv("../ALL_DATA/seagrass_beach_seine_data_2017_RAW.csv")
sp_names <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A91a429e4-33f0-4530-8ae9-4a40686e0a21"))
site_meta <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3A8e560dce-1ad2-44a8-9cdd-84771fdc7798"))

fishLW <- read.csv("../ALL_DATA/Lia_fish/fish_length_weight_conversion.csv", 
                   stringsAsFactors = FALSE, header = TRUE)
  
######## Step 1. Classify taxons ######## 
levels(as.factor(seine$species_common)) 
# 68 different species, need to remove non-fish species

# Based on sp_code combine with sp_names--includes taxa
fish.tax <- seine %>%
  group_by(sp_code) %>% 
  left_join(sp_names, by = c("sp_code" = "SpCode")) %>% 
  select(-c(Sp_CommonName, Sp_ScientificName, Taxa))

# remove non-fish species
fish <- fish.tax %>% 
  filter(taxon == "Vertebrata")
 
####### Step 4. Unmeasured -> measured ######## 

st.er <- function(x, na.rm = TRUE) {
  stopifnot(is.numeric(x))
  return(sd(x, na.rm = na.rm)/sqrt(length(x)))
}


# From the raw data this script will export a file summarized by site ready for analysis. 
#' The goal will be to summaries this data at the site level. Summaries will occur in a few different
#' ways to prep the data for different types on analysis. There are X files generated here. 
#' Simple summaries...(fill in more here as appropriate)

## Data import
#' All we need is the beach seine data and the fish length-mass conversions. 
#' This can be merged with other data that already has been summarized in other scripts. 
#' The merging can either happen here or in other scripts depending on the purpose.


## Separate measured and unmeasured fish
# The first step is to separate the measured fish from the unmeasured fish.

fish.m <- fish %>%
  #filter(taxon == "Vertebrata") %>% done above
  filter(fork_length != "NA")


fish.um <- fish %>%
  # filter(taxon == "Vertebrata") %>% 
  filter(unmeasured != "estimate") %>% # assume that infield estimates are accurate
  filter(is.na(fork_length))


## Assign lengths to unmeasured fish
#' When beach seining we only measured the first 30 individuals of a species, and counted the rest. 
#' We can use the measured fishes to create a distribution from which we can assign 
#' lengths to the unmeasured fishes.

#' Assign lengths to unmeasured fish based on sampled distribution.
#'  This assignment should happen at the EventID level. i.e. use the distribution of fishes at a 
#'  EventID to assign unmeasured fishes at that EventID. 

#' We will assume the least and just use the sampled proportions to assign lenghts to unmeasured fish. 


q <- data.frame() # empty dataframe to fill with for loop

for(s in unique(fish.um$site)){ # cycle through uniqie sites (only one seine per site)
  m <- fish.m %>% # subset measured data by iteration EventID
    filter(site == s)
  u <- fish.um %>% # subset unmeasured data by iteration EventID
    filter(site == s)
  for(i in unique(u$sp_code)){ # cycle through species that are in UNMEASURED data
    samp <- m %>% # create sample of measured fish from which to make distrubution
      filter(sp_code == i)
    unmeas <- u %>% # isolate unmeasured fish
      filter(sp_code == i)
    unmeas <- as.numeric(unmeas$unmeasured) # save unmeasured value
    dat.temp1 <- data.frame(size = as.character(samp$fork_length))
    dat.temp2 <- dat.temp1 %>% 
      group_by(size) %>% 
      summarise(count = n())
    dat.temp2$porb <- (dat.temp2$count/sum(dat.temp2$count))
    dat.temp2$x <- as.numeric(paste(dat.temp2$size))
    fx <- function(n){ # function derived from limits and probabilities of above
      sample(x = min(dat.temp2$x):max(dat.temp2$x), n, replace = TRUE, prob = dat.temp2$prob)
    }
    dat.temp3 <- data.frame(site = s, sp_code = i, fork_length = fx(unmeas))
    q <- rbind(q, dat.temp3) # append iteration to full data
  }
} # this is returning an error in regards to prob, but i am not sure what its talking about

##### Append assigned lengths to master data with all site level data
## Merge with original measured fishes ##
## Extract site and sp data ##
fish.site <- unique(fish[,1:9])

## Merge with loop output, the lengths of unmeasured fishes ##
d.info <- merge(q, fish.site, by = "site")

## Merge with original measured fishes ##
fish.m$fork_length <- as.numeric(fish.m$fork_length)
fish.m$date <- as.Date(fish.m$date)
fish.all <- bind_rows(fish.m, d.info)

## Fill in other data species data ##
sp <- data.frame(unique(fish.m[, 10:13]))
fish.all$species_common <- sp$species_common[match(fish.all$sp_code, sp$sp_code)]
fish.all$species_scientific <- sp$species_scientific[match(fish.all$sp_code, sp$sp_code)]

# Do some basic checks to make sure the the conversion from unmeasured to measured worked correctly
unique(as.factor(fish.all$site)) # 21 sites, correct
unique(as.factor(fish.all$sp_code)) # 56 levels of fish species
unique(as.factor(fish$sp_code)) # 56 levels of fish species in the original dataframe, correct! 7 unknown sp. 

# count total number of fish in original dataframe
fish$fork_length <- as.numeric(fish$fork_length)
fish$unmeasured <- as.numeric(fish$unmeasured)
um.fish.sum <- fish %>%
  dplyr::summarize(total = sum(unmeasured, na.rm = TRUE)) %>%
  dplyr::summarize(fish_total = sum(total)) 
(um.fish.sum) # 8040 unmeasured fish 

m.fish.sum <- fish %>%
  count(fork_length, na.rm = TRUE) %>%
  dplyr::summarize(sp_sum = sum(n)) %>%
  dplyr::summarize(total_sum = sum(sp_sum))
(m.fish.sum) # 4692 measured fish 

# total number of fish
(fish.sum <- um.fish.sum + m.fish.sum) # 12,732

# does this match the fish.all dataframe?


## Append Taxonomy
#' To help with later data analysis we will append family and order information so grouping 
#' by higher taxonomic values can be done.


## Append Taxonomy ##
fish.all <- merge(fish.all, unique(fishLW[, 1:5]), by.x = "species_scientific", by.y = "species_scientific", all.x = TRUE)
fish.all$Family <- ifelse(fish.all$sp_code == "UNLORD", "Cottidae", fish.all$Family)
fish.all$Family <- ifelse(fish.all$sp_code == "UNARTE", "Cottidae", fish.all$Family)
fish.all$Family <- ifelse(fish.all$sp_code == "UNGUNN", "Pholidae", fish.all$Family)

fish.all$Order <- ifelse(fish.all$sp_code == "UNLORD", "Scorpaeniformes", fish.all$Order)
fish.all$Order <- ifelse(fish.all$sp_code == "UNARTE", "Scorpaeniformes", fish.all$Order)
fish.all$Order <- ifelse(fish.all$sp_code == "UNGUNN", "Perciformes", fish.all$Order)

####### Step 5. Convert length to biomass ######## 
## Calculate Biomass
#' Using the length-weight conversion values individual lengths will be converted to biomass. 
#' Coefficients are in cm*g so fork lengths will need to be converted to cm from mm.

# First we need to do some prep work to account for fish that do not have specific a and b values.
## Unique species in master data ##
sci <- data.frame(SpCommon = unique(fish.all$species_common))

## Species that need stand ins ##
nomatch <- anti_join(sci, fishLW, by = "SpCommon")

## Species that don't have a or b balues ##
nodat <- subset(fishLW, is.na(fishLW$a)) 
nodat$species_common

# Calculate mean a and b values by Family #
fams <- fishLW %>% 
  group_by(Family) %>% 
  summarise(n = n(),
            mean_a = mean(a, na.rm = TRUE),
            mean_b = mean(b, na.rm = TRUE))

## Calculate mean a and b values by Species with appended Family ##
sps <- fishLW %>% 
  group_by(SpCommon) %>%
  summarise(Family = unique(Family),
            n = n(),
            mean_a = mean(a, na.rm = TRUE),
            mean_b = mean(b, na.rm = TRUE)) 


#' Now we can calculate mass with appropriate a nad b values. 
#' First priority will go to average values of that Species, then average valeus for that Family. 
#' Then we can calculate mass of each fish

## Append a and b ##
fish.all$a <- sps$mean_a[match(fish.all$SpCommon, sps$SpCommon)]
fish.all$b <- sps$mean_b[match(fish.all$SpCommon, sps$SpCommon)]

fish.all$a <- ifelse(fish.all$sp_code == "UNLORD", fams$mean_a[fams$Family == "Cottidae"], fish.all$a)
fish.all$a <- ifelse(fish.all$sp_code == "UNARTE", fams$mean_a[fams$Family == "Cottidae"], fish.all$a)
fish.all$a <- ifelse(fish.all$sp_code == "UNGUNN", fams$mean_a[fams$Family == "Pholidae"], fish.all$a)

fish.all$b <- ifelse(fish.all$sp_code == "UNLORD", fams$mean_b[fams$Family == "Cottidae"], fish.all$b)
fish.all$b <- ifelse(fish.all$sp_code == "UNARTE", fams$mean_b[fams$Family == "Cottidae"], fish.all$b)
fish.all$b <- ifelse(fish.all$sp_code == "UNGUNN", fams$mean_b[fams$Family == "Pholidae"], fish.all$b)

## Calculate mass per fish ##
fish.all$fork_length_cm <- fish.all$fork_length / 10 # Convert mm to cm
fish.all$mass_g <- (fish.all$a * fish.all$fork_length_cm^fish.all$b)

range(fish.all$mass_g, na.rm = TRUE)
```

## Group: Mass by site, species, total
Summarise data so that there is mass by site for each species and the total for that site.

## Summarise and spread ##
dat <- fish.all %>% 
  group_by(site, sp_code) %>% 
  summarise(mass_g = sum(mass_g)) %>% 
  spread(key = sp_code, value = mass_g)

## clean up and calculate ##
dat <- dat[, -29] # remove NA column
dat[is.na(dat)] <- 0
dat$total_fish_mass <- rowSums(dat[, 2:30])
```
######## Step 2. Convert date to JD and add year column ######## 

# do this at the end of the script... 
#fish.all <- fish.all %>% 
#  mutate(date = ymd(date)) %>% # convert to date format
#  mutate(julian = yday(date)) %>% # make the date a julian day
#  mutate(year = year(date)) # make a new column with just year


######## Step 3. Convert site names to 2-column code system ######## 

# add this step to the end of the script. 
#fish2 <- fish1 %>% 
#  left_join(site_meta, by = c("site" = "site_2017")) %>% # this needs to be changed for e/a YEAR. 
#  select(-c(site_2018, freshwater, general_description, sediment_description, siteID_NOAA))

# place name gives the physical location, bay_code and bay_sample together give you each
# unique site that was sampled. 



## Export
# Export of raw form and site summary form

write.csv(fish.all, "../ALL_DATA/Fish_mass_full_2018_derived.csv", row.names = FALSE)

write.csv(dat, "../ALL_DATA/Fish_mass_site_2018_derived.csv", row.names = FALSE)



