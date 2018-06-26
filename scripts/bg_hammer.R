library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(tmap)
library(tmaptools)
library(sf)
library(magrittr)
library(purrr)
library(ggplot2)
library(readxl)


# read in county-level number of housing units, 1940-2010
# calculated in h_units.R
h3 <- read_csv("csv/h_units_national.csv")

# read in housing units by 'year structure built' from 2011-15 ACS, downloaded from NHGIS
# metadata: data/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp_codebook.txt
USbg <- read_csv("data/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp.csv")
#for scott's computer:
#USbg <- read.csv("C:/Users/scott/Dropbox/hafley/urbanization/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp.csv")

# clean tibble
USbg <- USbg %>%
  select(-c(3,4,9,10,13:37)) %>%
  mutate(COUNTYJ = str_sub(GISJOIN, 1, 8))

# read block group area
# area unit: square miles of each block group.
# area calculated in ArcGIS; area calculated in equal area projection
bg_area <- read_csv("data/areas.txt")

# remove unecessary columns
bg_area <- bg_area %>%
  select(-c(7:9))

# join area to 'Year Structure Built' tibble
USbg2 <- USbg %>%
  left_join(x = USbg, y = bg_area, by = "GISJOIN") %>%
  rename(geoid = GEOID)



# join county housing units to block group tibble
US_bg3 <- left_join(USbg2,h3, by = 'COUNTYJ')


# jXX = total number of housing units in county during time t, based on year structure built in ACS
# iXX = total number of housing units in block group during time t, based on year structure built in ACS
# adjXX = adjusted number of housing units in block group, hammer method output
# additional reference: https://www.nature.com/articles/nclimate2961#methods

US_bg3 <- US_bg3 %>%
  group_by(COUNTYJ) %>%
  mutate(
    j70 = sum(ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j80 = sum(ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j90 = sum(ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j00 = sum(ADQSE005,ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j10 = sum(ADQSE004,ADQSE005,ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    i70 = (ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i80 = (ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i90 = (ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i00 = (ADQSE005 + ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i10 = (ADQSE004 + ADQSE005 + ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    adj40 = (hu40/sum(ADQSE011)*ADQSE011),
    adj70 = hu70/j70 * i70,
    adj80 = hu80/j80 * i80,
    adj90 = hu90/j90 * i90,
    adj00 = hu00/j00 * i00,
    adj10 = hu10/j10 * i10
  ) %>%
  ungroup(US_bg2) %>%
  mutate(COUNTYJ = as_factor(COUNTYJ)
  )

# huXX_sqmi = housing units/square mile
US_bg3 <- US_bg3 %>%
  group_by(COUNTYJ) %>%
  mutate(
    hu40_sqmi = adj40/area,
    hu70_sqmi = adj70/area,
    hu80_sqmi = adj80/area,
    hu90_sqmi = adj90/area,
    hu00_sqmi = adj00/area,
    hu10_sqmi = adj10/area
  )

# write_csv(US_bg3, "csv/blk_grp_complete.csv")