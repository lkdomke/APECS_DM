# pits and sediment cleaning script after the data has been combined from 2017 - 2021
# script created 5-16-21
# Lia Domke

#' pits/sediment were collected during 2017 - 2021 (+ more years potentially), csv was put together 
#' to include data across all years sampled with the same columns
#' bay_code/bay_sample were added to associate the same sites across years
#'

# read in data
pits <- read.csv("Data/Pits_and_Clams_Combined_2017-2021.csv", stringsAsFactors = FALSE, header = TRUE)
pits19 <- read.csv("Data/Eelgrass_pits_sediment/seagrass_pit_sediment_2019_RAW.csv")
site.names <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3Ae371e407-4886-4a29-a1f9-cec8ec375c67"), 
                       stringsAsFactors = FALSE, header = TRUE)

library(tidyr)
library(dplyr)

# clean up site info - just the lat/long and bay_id
sites <- site.names %>%
  unite(bay_id, bay_code:bay_sample) %>%
  dplyr::select(c(bay_id, latitude, longitude))

# take a look at the df - what needs to be cleaned?
glimpse(pits) # drop the so_region column
unique(pits$trans_loc) # need to make this column all lower and drop trailing ws
unique(pits$sed1_type) # need to make all lower and drop trailing whitespaces
unique(pits$sed2_type) # same here
unique(pits$place_name)
unique(pits$pit_count_m2)

# trim white spaces
pits$trans_loc <- trimws(pits$trans_loc)
pits$sed1_type <- trimws(pits$sed1_type)
pits$sed2_type <- trimws(pits$sed2_type)

# there's an issue where all the 2019 data was copied over incorrectly. 
# first create df of what we want to remove
pits.remove <- pits %>%
  filter(bin_loc_transect_m == "Shells collected along entire transect") %>%
  unite(bay_id, bay_code:bay_sample)

pits$place_name <- trimws(pits$place_name)
pits19$site_name <- trimws(pits19$site_name)
pits19$trans_loc <- trimws(pits19$trans_loc)

# then keep the bayid info so we can add it back
site.19 <- pits %>%
  unite(bay_id, bay_code:bay_sample) %>%
  filter(bin_loc_transect_m == "Shells collected along entire transect") %>%
  dplyr::select(bay_id, site_code, place_name, year, latitude_N, longitude_E) %>%
  distinct()

# subset the full df by the ones we want gone
pits.sub <- anti_join(pits, pits.remove) %>%
  unite(notes, notes:X.5, na.rm = TRUE)

# add back in the 2019 data with the correct info
pits19.2 <- pits19 %>%
  dplyr::rename(place_name = site_name, date_MMDDYY = date_mm.dd.yyyy, date_YYYYMMDD = date_yyyy.mm.dd,
                bin_loc_transect_m = bin_no, pit_count_m2 = pit_density_m2) %>%
  left_join(site.19)

test <- pits19.2%>%
  dplyr::select(bay_id, site_code, place_name, year) %>%
  distinct()
# a couple sites didn't join - just do that manually
pits19.3 <- pits19.2 %>%
  mutate(bay_id = ifelse(place_name == "Baker Island" & year == 2019, "BAKE_A", bay_id),
         bay_id = ifelse(place_name == "Hecta Bay - eel" & year == 2019, "HECA_A", bay_id),
         place_name = ifelse(bay_id == "HECA_A", "Heceta Island", place_name),
         bay_id = ifelse(place_name == "Naukati" & year == 2019, "NAUK_A", bay_id),
         bay_id = ifelse(site_code == "2017_H_02" & year == 2019, "NOSK_A", bay_id),
         place_name = ifelse(site_code == "2017_H_02" & year == 2019, "Nossuk Bay", place_name)) %>%
  mutate(bin_loc_transect_m = ifelse(bin_loc_transect_m == 1, 10, bin_loc_transect_m),
         bin_loc_transect_m = ifelse(bin_loc_transect_m == 2, 20, bin_loc_transect_m),
         bin_loc_transect_m = ifelse(bin_loc_transect_m == 3, 30, bin_loc_transect_m),
         bin_loc_transect_m = ifelse(bin_loc_transect_m == 4, 40, bin_loc_transect_m),
         bin_loc_transect_m = ifelse(bin_loc_transect_m == 5, 50, bin_loc_transect_m)) %>%
  separate(bay_id, into = c("bay_code", "bay_sample"))


pits.all <- rbind(pits.sub, pits19.3)


# remove unnecessary columns and add in lat/long
pits_clean <- pits.all %>%
  unite(bay_id, bay_code:bay_sample) %>%
  left_join(sites) %>%
  mutate(trans_loc = tolower(trans_loc),
         sed1_type = tolower(sed2_type),
         sed2_type = tolower(sed2_type)) %>%
  mutate(bay_id = ifelse(bay_id == "NAUK_A" & year == "2018", "NAUK_B", bay_id)) # change bay code for nauk site in 2018 should be nauk_b not a. 
  
glimpse(pits_clean)
unique(pits_clean$notes)

# remove the _ in notes
pits_clean$notes <- sub("__", "", pits_clean$notes)
unique(pits_clean$notes)
pits_clean$notes <- sub("_", "", pits_clean$notes)
unique(pits_clean$notes)

# calculate the distance between the two points - so the lat/long originally listed in the combined df and the one in the universal site names df
library(geosphere)
loc <- pits_clean %>%
  dplyr::select(bay_id, year, longitude_E, latitude_N, longitude, latitude) %>%
  distinct() %>%
  unite(bay_year, bay_id:year) %>%
  filter(longitude_E != "NA")

d <- data.frame() # empty dataframe

for(s in unique(loc$bay_year)){
  loc1 <- loc %>%
    filter(bay_year == s) %>%
    dplyr::select(c(longitude_E, latitude_N))
  loc2 <- loc %>%
    filter(bay_year == s) %>%
    dplyr::select(c(longitude, latitude))
  dist.m <- distm(loc1, loc2, fun = distGeo) # value in meters
  df <- data.frame(bay_year = s, distance_m = dist.m)
  d <- rbind(d, df)
}

options(scipen = 999)
d.large <- d %>%
  filter(distance_m != 0) %>%
  filter(distance_m > 100) %>%
  arrange(desc(distance_m))

mismatch.sites <- subset(loc, bay_year %in% d.large$bay_year)

# lets use the universal lat/long for all the sites aside from Sala_a_2019 because we did pits in an easier accessible site (not so muddy)
sala_lat <- pits_clean %>%
  filter(bay_id == "SALA_A" & year == "2019") %>%
  dplyr::select(bay_id, latitude_N, longitude_E, year) %>%
  distinct()

pits2 <- pits_clean %>%
  dplyr::select(-c(longitude_E, latitude_N)) %>%
  mutate(latitude = ifelse(bay_id == "SALA_A" & year == 2019, "55.67565", latitude),
         longitude = ifelse(bay_id == "SALA_A" & year == 2019, "-133.3735", longitude))

unique(pits2$notes)

# change how large of an area was surveyed 
clean_pits2 <- pits2 %>%
  mutate(bin_area_m2 = ifelse(notes == "clam shells collected for 50m x 1 (half of normal transect)", 10, bin_area_m2)) %>%
  mutate(bin_area_m2 = ifelse(notes == "clam shells collected for 50m x 1 (half of normal transect)live cockles on transect", 10,
                               bin_area_m2)) %>%
  group_by(bay_id, year, trans_loc, bin_loc_transect_m) %>%
  mutate(sed_avg_no = (sed1_no + sed2_no)/2,
         pit_count_m2 = pit_count_bin/bin_area_m2) %>%
  dplyr::select(-so_region)

glimpse(clean_pits2)
unique(clean_pits2$bay_id)

#write.csv(clean_pits2, "Data/Eelgrass_pits_sediment/pits_sediment_2017-2021_cleaned_5-16-22.csv")
