# pits and sediment cleaning script after the data has been combined from 2017 - 2021
# script created 5-16-21
# Lia Domke

#' pits/sediment were collected during 2017 - 2021 (+ more years potentially), csv was put together 
#' to include data across all years sampled with the same columns
#' bay_code/bay_sample were added to associate the same sites across years
#'

# read in data
pits <- read.csv("Data/Eelgrass_pits_sediment/Pits_and_Clams_Combined_2017-2021.csv", stringsAsFactors = FALSE, header = TRUE)
site.names <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3Ae371e407-4886-4a29-a1f9-cec8ec375c67"), 
                       stringsAsFactors = FALSE, header = TRUE)

library(tidyr)
library(dplyr)
library(lubridate)

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

###### lets add in the csv files from 2017 - 2019 
pit19 <- read.csv("Data/Eelgrass_pits_sediment/seagrass_pit_sediment_2019_RAW.csv", stringsAsFactors = FALSE, header = TRUE)
pit17 <- read.csv("Data/Eelgrass_pits_sediment/seagrass_pits_sediment_2017_derived.csv", stringsAsFactors = FALSE, header = TRUE)
pit18 <- read.csv("Data/Eelgrass_pits_sediment/pits_sediment_2018_RAW.csv", stringsAsFactors = FALSE, header = TRUE)
pit20 <- read.csv("Data/Eelgrass_pits_sediment/seaotter_pit_sediment_2020_checked.csv", stringsAsFactors = FALSE, header = TRUE)
pit21 <- read.csv("Data/Eelgrass_pits_sediment/seagrass_pits_sediment_2021.csv", stringsAsFactors = FALSE, header = TRUE)
pit22 <- read.csv("Data/Eelgrass_pits_sediment/seagrass_pits_sediment_2022_RAW.csv", stringsAsFactors = FALSE, header = TRUE)

head(pit17) # missing bin_area_m2
head(pit18) # missing bay_codes, missing bin_area_m2
head(pit19) # missing bay_codes, missing correct calc for pit_density_m2
head(pit20) # missing lat/long
head(pit21) # missing bin_area_m2
head(pit22) # missing lat/long and bin_area_m2
names(pits)

### clean 2017
pit17_clean <- pit17 %>%
  dplyr::rename(site_code = site, bin_loc_transect_m = trans_m) %>%
  mutate(date_YYYYMMDD = ymd(date), year = year(date), trans_loc = tolower(trans_type),
         sed1_type = tolower(sed1_type), sed2_type = tolower(sed2_type),
         bin_area_m2 = 10, pit_count_bin = pit_bin, notes = "",
         sed1_no = ifelse(sed1_type == "sandy mud", 2, sed1_no),
         sed2_no = ifelse(sed2_type == "sand", 4, sed2_no),
         sed2_no = ifelse(sed2_type == "pebble", 6, sed2_no)) %>%
  group_by(place_name, trans_loc, trans_bin) %>%
  dplyr::mutate(sed_avg_no = ((sed1_no + sed2_no)/2), pit_count_m2 = pit_count_bin/bin_area_m2) %>%
  ungroup() %>%
  dplyr::select(bay_code, bay_sample, site_code, place_name, latitude, longitude, date_YYYYMMDD, year, trans_loc, bin_loc_transect_m,
                trans_bin, bin_area_m2, pit_count_bin, pit_count_m2, sed1_type, sed2_type, sed1_no, sed2_no, sed_avg_no,
                field_collector, notes)
  
pit17_clean %>% # check to make sure the sed types match up
  dplyr::select(sed2_type, sed2_no) %>%
  distinct()

###### clean 2018 
head(pit18) # missing bay_codes, missing bin_area_m2

# lets pull the site - bay code correlation from the combined csv
pits %>%
  filter(year == 2018) %>%
  dplyr::select(bay_code, bay_sample, site_code, place_name, latitude_N, longitude_E) %>%
  distinct()

unique(pit18$site)

pit18 %>%
  dplyr::select

pit18 %>% # check to make sure the sed types match up
  dplyr::select(sed2_type, sed2_no) %>%
  distinct()
############# above is where I left off on 2018 - in sed2 type there was an instance that the second sediment type was
######### listed as gravel and cobble and then the number is a date (43319) instead of a sed type - should check
# the original datasheets for this. 

##### clean 2019
head(pit19) # missing bay_codes, missing correct calc for pit_density_m2
unique(pit19$site_code)
pit19.named <- pit19 %>%
  mutate(site_name = ifelse(site_name == "Baker Island", "Baker Island - eel", site_name),
         site_name = ifelse(site_name == "Hecta Bay - eel", "Heceta", site_name),
         site_name = ifelse(site_name == "Klawock Airport", "Klawock airport", site_name),
         site_name = ifelse(site_name == "Shakan Bay  - eel", "Shakan - eel", site_name),
         site_name = ifelse(site_name == "Shakan Bay - kelp", "Shakan - kelp", site_name),
         site_code = ifelse(site_code == "2017_L_01", "2017_L_03", site_code))

names(site.names) 
sites19 <- site.names %>%
  dplyr::select(site_2019, site_2017, place_name, bay_code, bay_sample, latitude, longitude)

sites19.1 <- left_join(pit19.named, sites19, by = c("site_name" = "site_2019")) %>%
  dplyr::select(site_name, bay_code, bay_sample, site_code, latitude, longitude) %>%
  filter(!is.na(bay_code)) %>%
  distinct()

sites19.2  <- left_join(pit19.named, sites19, by = c("site_code" = "site_2017")) %>%
  dplyr::select(site_name, bay_code, bay_sample, site_code, latitude, longitude) %>%
  filter(site_code != "") %>%
  distinct()

id.19 <- rbind(sites19.1, sites19.2) %>%
  unite(bay_id, bay_code:bay_sample) %>%
  filter(bay_id != "NATZ_A") %>% # drop the NATZ A because we actually sampled NATZ C
  distinct()

# Okay now we use this id.19 to add in the correct lat/long and bay codes and cal pit/m2 and avg sed
pit19.named$sed1_type <- trimws(pit19.named$sed1_type, which = "both")
pit19.named$sed2_type <- trimws(pit19.named$sed2_type, which = "both")

pit19_clean <- pit19.named %>%
  left_join(id.19) %>%
  separate(bay_id, into = c("bay_code", "bay_sample")) %>%
  dplyr::rename(trans_bin = bin_no, place_name = site_name) %>%
  mutate(date_YYYYMMDD = mdy(date_mm.dd.yyyy), trans_loc = tolower(trans_loc),
         sed2_type = tolower(sed2_type), sed1_type = tolower(sed1_type),
         bin_loc_transect_m = "",
         bin_loc_transect_m = ifelse(trans_bin == 1, "10", bin_loc_transect_m),
         bin_loc_transect_m = ifelse(trans_bin == 2, "20", bin_loc_transect_m),
         bin_loc_transect_m = ifelse(trans_bin == 3, "30", bin_loc_transect_m),
         bin_loc_transect_m = ifelse(trans_bin == 4, "40", bin_loc_transect_m),
         bin_loc_transect_m = ifelse(trans_bin == 5, "50", bin_loc_transect_m),
         bin_area_m2 = as.numeric(bin_area_m2),
         sed1_no = as.numeric(sed1_no),
         sed2_no = as.numeric(sed2_no)) %>%
  group_by(place_name, trans_loc, trans_bin) %>%
  mutate(pit_count_m2 = pit_count_bin/bin_area_m2,
         sed_avg_no = ((sed1_no + sed2_no)/2)) %>%
  ungroup() %>%
  dplyr::select(bay_code, bay_sample, site_code, place_name, latitude, longitude, date_YYYYMMDD, year,
                trans_loc, bin_loc_transect_m, trans_bin, bin_area_m2, pit_count_bin, pit_count_m2, 
                sed1_type, sed2_type, sed1_no, sed2_no, sed_avg_no, field_collector, notes)

pit19_clean2 %>% # check to make sure the sed types match up
  dplyr::select(sed2_type, sed2_no) %>%
  distinct()

# okay there are some issues with the sediment types
pit19_clean2 <- pit19_clean %>%
  mutate(sed1_no = ifelse(sed1_type == "sand", 4, sed1_no),
         sed1_no = ifelse(sed1_type == "mud", 1, sed1_no),
         sed1_no = ifelse(sed1_type == "sandy mud", 2, sed1_no),
         sed2_type = ifelse(sed2_type == "mudddy sand", "muddy sand", sed2_type),
         sed2_no = ifelse(sed2_type == "sand", 4, sed2_no))


pit17_and_19_clean <- rbind(pit17_clean, pit19_clean2) # okay good check the df are compatible. 


########## clean 2020 
head(pit20) # missing lat/long

pit20$sed1_type <- trimws(pit20$sed1_type, which = "both")
pit20$sed2_type <- trimws(pit20$sed2_type, which = "both")

pit20_clean <- pit20 %>%
  unite(bay_id, bay_code:bay_sample) %>%
  left_join(sites, by = "bay_id") %>%
  separate(bay_id, into = c("bay_code", "bay_sample")) %>%
  dplyr::rename(place_name = site, bin_loc_transect_m = trans_m, pit_count_bin = pit_bin) %>%
  mutate(site_code = NA, date_YYYYMMDD = mdy(date), year = year(date_YYYYMMDD),
         trans_loc = tolower(trans_type), bin_area_m2 = as.numeric(20)) %>%
  group_by(place_name, trans_loc, bin_loc_transect_m) %>%
  mutate(pit_count_m2 = pit_count_bin/bin_area_m2,
         sed_avg_no = ((sed1_no + sed2_no)/2)) %>%
  ungroup() %>%
  dplyr::select(bay_code, bay_sample, site_code, place_name, latitude, longitude, date_YYYYMMDD, year,
                trans_loc, bin_loc_transect_m, trans_bin, bin_area_m2, pit_count_bin, pit_count_m2,
                sed1_type, sed2_type, sed1_no, sed2_no, sed_avg_no, field_collector, notes)

# drop the empty rows
pit20_clean <- pit20_clean[-c(91:94),]

pit20_clean %>% # check to make sure the sed types match up
  dplyr::select(sed1_type, sed1_no) %>%
  distinct()

pit17.19.20_clean <- rbind(pit17_and_19_clean, pit20_clean) # add combines together all good

####### cleaning 2021
head(pit21) # missing bin_area_m2

pit21$sed1_type <- trimws(pit21$sed1_type, which = "both")
pit21$sed2_type <- trimws(pit21$sed2_type, which = "both")


pit21_clean <- pit21 %>%
  unite(bay_id, bay_code:bay_sample) %>%
  left_join(sites, by = "bay_id") %>%
  separate(bay_id, into = c("bay_code", "bay_sample")) %>%
  dplyr::rename(place_name = site, pit_count_bin = pit_bin) %>%
  mutate(site_code = NA, date_YYYYMMDD = mdy(date), year = year(date_YYYYMMDD), trans_loc = tolower(trans_type),
         bin_area_m2 = as.numeric(20), sed1_type = tolower(sed1_type), sed2_type = tolower(sed2_type)) %>%
  mutate(bin_loc_transect_m = as.character(trans_m)) %>%
  group_by(place_name, trans_loc, bin_loc_transect_m) %>%
  mutate(pit_count_m2 = pit_count_bin/bin_area_m2,
         sed_avg_no = ((sed1_no + sed2_no)/2)) %>%
  ungroup() %>%
  dplyr::select(bay_code, bay_sample, site_code, place_name, latitude, longitude, date_YYYYMMDD,
                year, trans_loc, bin_loc_transect_m, trans_bin, bin_area_m2, pit_count_bin, pit_count_m2, sed1_type, sed2_type, sed1_no,
                sed2_no, sed_avg_no, field_collector, notes)

pit21_clean %>% # check to make sure the sed types match up
  dplyr::select(sed1_type, sed1_no) %>%
  distinct() # looks good

pit17.19.20.21_clean <- rbind(pit21_clean, pit17.19.20_clean)


####### cleaning 2022
head(pit22) # missing lat/long and bin_area_m2

pit22_clean <- pit22 %>%
  unite(bay_id, bay_code:bay_sample) %>%
  left_join(sites, by = c("bay_id")) %>%
  separate(bay_id, into = c("bay_code", "bay_sample")) %>%
  mutate(trans_loc = tolower(trans_type), sed1_type = tolower(sed1_type), sed2_type = tolower(sed2_type)) %>%
  mutate(bin_area_m2 = as.numeric(20)) %>% # theres no bin area included, it was 10 m x 2 m 
  mutate(site_code = "") %>%
  mutate(date_YYYYMMDD = mdy(date), year = year(date_YYYYMMDD)) %>%
  rename(place_name = site, pit_count_bin = pit_bin,
         bin_loc_transect_m = trans_m) %>%
  group_by(place_name, trans_loc, bin_loc_transect_m) %>%
  mutate(pit_count_m2 = pit_count_bin/bin_area_m2,
         sed_avg_no = ((sed1_no + sed2_no)/2)) %>%
  ungroup() %>%
  dplyr::select(bay_code, bay_sample, site_code, place_name, latitude, longitude, date_YYYYMMDD,
                year, trans_loc, bin_loc_transect_m, trans_bin, bin_area_m2, pit_count_bin, pit_count_m2, sed1_type, sed2_type, sed1_no,
                sed2_no, sed_avg_no, field_collector, notes)

pit22_clean %>% # check to make sure the sed types match up
  dplyr::select(sed2_type, sed2_no) %>%
  distinct() # looks good


pit17.19.20.21.22_clean <- rbind(pit22_clean, pit17.19.20.21_clean)

#write.csv(pit17.19.20.21.22_clean, "Data/Eelgrass_pits_sediment/pits_sediment_2017.2019-2022_cleaned_8-29-22.csv")



###################### everything below is just code I want to keep track of cause it was cool #######################
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



