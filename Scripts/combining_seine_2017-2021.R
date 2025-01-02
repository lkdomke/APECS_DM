# Script to combine derived seine data from 2017 - 2021

# created June 9th, 2022
# updated August 30 2022
# Lia Domke

# read in data
seine17 <- read.csv("Data/Fish_Seine/fish_mass_2017_derived.csv")
seine18 <- read.csv("Data/Fish_Seine/fish_mass_2018_derived.csv")
seine19 <- read.csv("Data/Fish_Seine/fish_mass_2019_derived.csv")
seine20 <- read.csv("Data/Fish_Seine/fish_mass_2020_derived.csv")
seine21 <- read.csv("Data/Fish_Seine/fish_mass_2021_derived.csv")
seine22 <- read.csv("Data/Fish_Seine/fish_mass_2022_derived.csv")
# load libraries needed
library(tidyverse)

# check to see if column names match up
names(seine17)
names(seine18)
names(seine19)
names(seine20)
names(seine21) # need to drop siteID NOAA
names(seine22)

# drop the abundance column in seine21(not needed cause every line is an individual sampled)
seine21 <- select(seine21, -c(abundance, siteID_NOAA)) %>%
  separate(bay_ID, into = c("bay_code", "bay_sample"))

seine22 <- select(seine22, -c(abundance)) %>%
  separate(bay_id, into = c("bay_code", "bay_sample"))

names(seine20)
names(seine21)
names(seine22)

df1 <- rbind(seine17, seine18)
df2 <- rbind(df1, seine19)
df3 <- rbind(df2, seine20)
df4 <- rbind(df3, seine21)
df5 <- rbind(df4, seine22)

# df4 should have all the sites to date
unique(df5$year)
unique(df5$bay_code)
unique(df5$date)
unique(df4$sp_code)

df5$abundance <- 1
# how many sticklebacks did we catch? 
df5 %>%
  filter(sp_code == "STICK3") %>%
  dplyr::summarise(counts = sum(abundance))

# which species of greatest abundance?
plot <- df5 %>%
  unite(bay_ID, bay_code:bay_sample) %>%
  group_by(sp_code, year, bay_ID) %>%
  dplyr::summarise(counts = sum(abundance)) %>%
  filter(counts > 1) %>%
  ggplot() +
  geom_col(aes(y = counts, x = reorder(year, -counts))) 

plot + facet_wrap(~sp_code)

df5 <- dplyr::select(df5, -abundance)

#write.csv(df5, "Data/Fish_Seine/combined_seine_derived_2017-2022.csv")
