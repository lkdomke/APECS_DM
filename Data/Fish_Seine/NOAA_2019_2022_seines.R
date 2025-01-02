## Quick script to clean fish data from 2019 and 2021 for Jessica

fish <- read.csv("Data/Fish_Seine/combined_seine_derived_2017-2022.csv", stringsAsFactors = F, header = T)
sites <- read.csv("Data/Site_universalnames_allyears_updated2022.csv", stringsAsFactors = F, header = T)

library(tidyverse)

# do we have all the sites related data included
names(fish)

# missing the siteID - NOAA
head(sites) # can add it in using this one
sites <- sites %>% # usually I dont recommend overwriting the dataframe with the same name
  unite(bay_id, bay_code:bay_sample) %>%
  dplyr::select(c(bay_id, siteID_NOAA))

# filter by just 2019 and 2021
fish2 <- fish %>%
  filter(year == "2019" | year == "2021") %>% # filter by 2019 OR 2021
  unite(bay_id, bay_code:bay_sample) %>%
  left_join(sites)


#write.csv(fish2, "fish_seine_2019_2021_noaa_sites.csv")
