#' data cleaning/management for shell litter after data combined from 2017 - 2021
#' script created 5-12-2022
#' Lia Domke
#' 
#' shell litter were collected during 2017 - 2021 (+ 2022), csv was put together 
#' to include data across all years sampled with the same columns
#' bay_code/bay_sample 
#' 

# read in data

death <- read.csv("Data/seagrass_shelldeath_2017-2021_cleaned5-12-22.csv", stringsAsFactors = FALSE, header = TRUE)
site.names <- read.csv(url("https://knb.ecoinformatics.org/knb/d1/mn/v2/object/urn%3Auuid%3Ae371e407-4886-4a29-a1f9-cec8ec375c67"), 
                       stringsAsFactors = FALSE, header = TRUE)
  
library(tidyr)
library(dplyr)
library(stringr)

unique(death$place_name)
unique(death$year)
unique(death$species_code)
unique(death$species_scientific)

# remove any trailing whitespaces
death$species_scientific <- trimws(death$species_scientific, which = c("both"))
death$species_common <- trimws(death$species_common, which = c("both"))
  
death.sites <- death %>%
  unite(bay_id, bay_code:bay_sample) %>%
  dplyr::select(bay_id) %>%
  distinct()

# use 'r mapvalues' to create a from list and to list to convert the species_codes to the consistent species codes. 
# create site bay id
site <- site.names %>%
  unite(bay_id, bay_code:bay_sample) %>%
  dplyr::select(c(bay_id, latitude, longitude))


# update the species code based on what scientific species it is
death_clean <- death %>% 
  unite(bay_id, bay_code:bay_sample) %>%
  mutate(death, species_common = tolower(species_common)) %>%
  mutate(species_scientific = ifelse(species_common == "macoma spp" & species_scientific != "Macoma spp", "Macoma spp", species_scientific)) %>%
  mutate(species_code = ifelse(species_scientific == "Clinocardium nutallii" & species_code != "CLAMCK", "CLAMCK", species_code)) %>%
  mutate(species_code = ifelse(species_scientific == "Protothaca staminea" & species_code != "CLAMST", "CLAMST", species_code)) %>%
  mutate(species_code = ifelse(species_scientific == "Saxidomus gigantea" & species_code != "CLAMBU", "CLAMBU", species_code)) %>%
  mutate(species_code = ifelse(species_scientific == "Macoma nasuta" & species_code != "CLAMMB", "CLAMMB", species_code)) %>%
  mutate(species_code = ifelse(species_scientific == "Macoma inquinata" & species_code != "CLMPOI", "CLMPOI", species_code)) %>%
  mutate(species_code = ifelse(species_scientific == "Macoma spp" & species_code != "CLMMAC", "CLMMAC", species_code)) %>%
  mutate(species_scientific = ifelse(species_scientific == "Mya arenicola", "Mya arenaria", species_scientific)) %>%
  left_join(site, by = c("bay_id", "latitude", "longitude")) %>%
  dplyr::select(c(bay_code, bay_sample, bay_id, place_name, year, date, YYYYMMDD, trans_loc, trans_area_m2, species_common, 
                  species_scientific, species_code, death_estimate, width_mm, notes, measurers, latitude, longitude))

# what species to be the same 
macoma <- c("macoma sp.", "macoma spp", "macoma spp.", "macoma sp")
death_clean$species_common <- sapply(death_clean["species_common"], function(x) replace(x, x %in% macoma, "macoma spp"))
death_clean$species_common <- death_clean$species_common[,1]


# fix the species that are incorrect
# capax should be Tresus capax for scientific name
death_clean2 <- death_clean %>%
  mutate(species_scientific = ifelse(species_common == "capax", "Tresus capax", species_scientific)) %>%
  mutate(species_code = ifelse(species_common == "capax", "CLAMCX", species_code)) %>%
  mutate(species_common = ifelse(species_scientific == "Mya truncata", "truncate softshell", species_common)) %>%
  mutate(species_code = ifelse(species_scientific == "Mya arenaria", "CLAMSS", species_code)) %>%
  mutate(species_scientific = ifelse(species_common == "baltic macoma", "Macoma balthica", species_scientific)) %>%
  mutate(species_scientific = ifelse(species_common == "macoma spp", "Macoma spp", species_scientific)) %>%
  mutate(species_code = ifelse(species_common == "macoma spp", "CLMMAC", species_code))


# create sp list with clam sp_code, species scientific and common name to check to make sure they are correct
sp_names <- death_clean2 %>%
  dplyr::select(species_common:species_code) %>%
  distinct()
# the nas that show up in sp_names are sites were no shells were collected along the transect. 

death_clean2 %>%
  group_by(species_common, species_code, species_scientific) %>%
  dplyr::summarise(number = n())


#write.csv(death_clean2, "Data/seagrass_shelldeath_2017-2021_cleaned5-16-22.csv")
#write.csv(sp_names, "../../../../clam_species_list_2021.csv")

## after we finished the above code - we also sampled in 2022 so lets read in that data and make sure that it matches what we have already done

death22 <- read.csv("Data/Shell_death/seagrass_shelldeath_2022_RAW_8-12-22.csv", header = TRUE, stringsAsFactors = FALSE)

glimpse(death22)
unique(death22$IF.Function)
unique(death22$death_estimate)

death22.1 <- death22 %>%
  dplyr::select(-c(IF.Function, Width_checked)) %>%
  mutate(species_common = tolower(species_common))


death22.1 %>%
  dplyr::select(species_common:species_code) %>%
  distinct()
# looks good all the names look correct

death22.1 %>%
  group_by(species_common, species_code, species_scientific) %>%
  dplyr::summarise(number = n()) # 989 meausred shell litter

unique(death22.1$place_name)
unite(death22.1, bay_id, bay_code:bay_sample) %>%
  dplyr::select(bay_id, place_name) %>%
  distinct()

# okay great lets add it in with the shelldeath from 2017 - 2021
names(death_clean2)
names(death22.1)

# only thing missing is the united bay id and the lat long
death22.2 <- death22.1 %>%
  unite(bay_id, bay_code:bay_sample) %>%
  left_join(site, by = c("bay_id")) %>%
  separate(bay_id, into = c("bay_code", "bay_sample"))

# lets join them together
death17.22 <- death_clean2 %>%
  dplyr::select(-bay_id) %>%
  rbind(death22.2)


#write.csv(death17.22, "Data/Shell_death/seagrass_shelldeath_2017-2022_cleaned8-12-22.csv")
