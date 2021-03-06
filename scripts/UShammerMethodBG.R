#*********************************************************************************
# original created: March 30, 2017                                               #
# by: Taylor Hafley                                                              #
# last modified: 6.25.18                                                         #
# local wd ("Google Drive/school/R/urban/data)                                   #
# (dropbox/Projects/InProgress/urban                                             #
# Git: ("/data/UShammerMethodBG.R)                                               #
# data source(s): tidycensus, tigris, NHGIS, and M. Hauer                        #
#                                                                                #
#                                                                                #
#*********************************************************************************

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
#for scott's computer:
#USarea <- read.csv("C:/Users/scott/Dropbox/hafley/urbanization/areas.txt")

# remove unecessary columns
bg_area <- bg_area %>%
  select(-c(7:9))

# join area to 'Year Structure Built' tibble
USbg2 <- USbg %>%
  left_join(x = USbg, y = bg_area, by = "GISJOIN") %>%
  rename(geoid = GEOID)

#write_csv(USbg2, "data/USbg2.csv")

#read in housing unit by year Census
hu1940 <- read_csv("data/nhgis0022_csv/nhgis0022_ds78_1940_county.csv") %>%
  select(-c(3:7,9)) %>%
  rename(COUNTYJ = GISJOIN,
         h_units = BXR001)
#for scott's computer:
#hu1940 <- read.csv("C:/Users/scott/Dropbox/hafley/urbanization/nhgis0022_csv/nhgis0022_ds78_1940_county.csv")

#for scott's computer (shoulda done setwd sooner....):
#setwd("C:/Users/scott"))

hu1950_60 <- read_xlsx("data/hammer_50_60.xlsx") %>%
  gather(YEAR,"h_units","h1950",5:6) %>%       # 1950 and 1960 housing units in one colum
  mutate(COUNTYJ = str_c("G", FIPS, "0"),        # initial step to match NHGIS data
         YEAR = as.integer(substring(YEAR, 2))) %>%
  select(COUNTYJ, YEAR, h_units)

# add '0' to match NHGIS data
# there has to be a better way to add this '0'  
str_sub(hu1950_60$COUNTYJ, 4,3 ) <- '0'

hu1970 <- read_csv("data/nhgis0025_csv/nhgis0025_ds94_1970_county.csv") %>%
  select(-c(3:7)) %>%
  rename(COUNTYJ = GISJOIN, 
         h_units = CBV001)

hu1980 <- read_csv("data/nhgis0025_csv/nhgis0025_ds104_1980_county.csv") %>%
  select(-c(3:10))%>%
  rename(COUNTYJ = GISJOIN,
         h_units = C8Y001)

hu1990 <- read_csv("../../../../dropbox/hafley/urbanization/nhgis0025_csv/nhgis0025_ds120_1990_county.csv")%>%
  select(-c(3:16))%>%
  rename(COUNTYJ = GISJOIN,
         h_units = ESA001)

hu2000 <- read_csv("../../../../dropbox/hafley/urbanization/nhgis0025_csv/nhgis0025_ds146_2000_county.csv")%>%
  select(-c(3:12))%>%
  rename(COUNTYJ = GISJOIN,
         h_units = FKI001)

hu2010 <- read_csv("data/nhgis0026_csv/nhgis0026_ds172_2010_county.csv") %>%
  select(-c(3:39))%>%
  rename(COUNTYJ = GISJOIN,
         h_units = IFC001)


# bind separate decades to one tibble ('long' format)
h2 <- bind_rows(hu1940,hu1950_60,hu1970,hu1980,hu1990,hu2000,hu2010)

# transpose to 'wide' format
h3 <- h2 %>%
  spread(YEAR,h_units) %>%
  rename(hu40 = `1940`,
         hu50 = `1950`,
         hu60 = `1960`,
         hu70 = `1970`,
         hu80 = `1980`,
         hu90 = `1990`,
         hu00 = `2000`,
         hu10 = `2010`)

#write_csv(h3, "csv/h_units_national.csv")

# clean environment
rm(hu1940, hu1950_60, hu1970, hu1980, hu1990, hu2000, hu2010)


# join county housing units to block group tibble
US_bg3 <- left_join(USbg2,h3, by = 'COUNTYJ')


# jXX = total number of housing units in county during time t, based on year structure built in ACS
# iXX = total number of housing units in block group during time t, based on year structure built in ACS
# adjXX = adjusted number of housing units in block group, hammer method output
# additional reference: https://www.nature.com/articles/nclimate2961#methods

US_bg3 <- US_bg3 %>%
  group_by(COUNTYJ) %>%
  mutate(
    j50 = sum(ADQSE010, ADQSE011),
    j60 = sum(ADQSE009, ADQSE010, ADQSE011),
    j70 = sum(ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j80 = sum(ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j90 = sum(ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j00 = sum(ADQSE005,ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    j10 = sum(ADQSE004,ADQSE005,ADQSE006,ADQSE007,ADQSE008, ADQSE009, ADQSE010, ADQSE011),
    i50 = (ADQSE010 + ADQSE011),
    i60 = (ADQSE009 + ADQSE010 + ADQSE011),
    i70 = (ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i80 = (ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i90 = (ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i00 = (ADQSE005 + ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    i10 = (ADQSE004 + ADQSE005 + ADQSE006 + ADQSE007 + ADQSE008 + ADQSE009 + ADQSE010 + ADQSE011),
    adj40 = (hu40/sum(ADQSE011)*ADQSE011),
    adj50 = hu50/j50 * i50,
    adj60 = hu60/j60 * i60,
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
    hu50_sqmi = adj50/area,
    hu60_sqmi = adj60/area,
    hu70_sqmi = adj70/area,
    hu80_sqmi = adj80/area,
    hu90_sqmi = adj90/area,
    hu00_sqmi = adj00/area,
    hu10_sqmi = adj10/area
  )

write_csv(US_bg3, "csv/blk_grp_complete.csv")

# US_bg3 is a complete dataset. It includes adjusted housing units, and housing units
# per square mile for each block group. I create urban classifications in 'suburbanBG.R'



labels <- c(G1300670 = 'Cobb', G1301210 = 'Fulton', G1300890 = 'DeKalb',
            G1301350 = 'Gwinnett', G1302190 = 'Oconee', G1300590 = 'Clarke')

US_bg2 %>% 
  filter(STATEA == 13 & COUNTYJ %in% c('G1300670','G1301210','G1300890','G1301350','G1302190',
                                       'G1300590') & hu90_sqmi >10 & hu90_sqmi < 800) %>%
  ggplot((mapping = aes(x = hu90_sqmi)))+
  geom_histogram(bins = 10) +
  labs(title = "1990 Housing Units", x = "hu/sq mi") +
  facet_wrap('COUNTYJ', labeller = labeller(COUNTYJ = labels))


US_bg2 %>% 
  filter(STATEA == 13 & COUNTYJ %in% c('G1300670','G1301210','G1300890','G1301350','G1302190',
                                       'G1300590')) %>%
  ggplot(mapping = aes(x=COUNTYJ, y = hu40_sqmi)) +
  geom_boxplot()

# write_csv(test1c,"Dropbox/hafley/urbanization/test1c.csv") #If you want to save as a file




# example
US_bg2 %>%
  group_by(COUNTYJ) %>%
  summarize(
    sum(ADQSE011)
)

# skip for now, commented out
#US_bg2$urb40 <- ''
#US_bg2[,'urb40'] <- sapply(US_bg2[,'hu40_sqmi'],function(x)ifelse(x > 200,1,0))
#US_bg2$urb40 <- as.factor(US_bg2$urb40)

# set up urban classification
#US_bg2$urb50 <- ''
#US_bg2$urb60 <- ''
#US_bg2$urb70 <- ''
#US_bg2$urb80 <- ''
#US_bg2$urb90 <- ''
#US_bg2$urb00 <- ''
#US_bg2$urb10 <- ''

# determine if area is urban. If huXX_sqmi > 200 or if previous decade is
# classified as urban, then bg is urban, according to this approach
#US_bg2[,'urb50'] <- ifelse((US_bg2$hu50_sqmi > 200 | US_bg2$urb40 == 1),1,0)
#US_bg2[,'urb60'] <- ifelse((US_bg2$hu60_sqmi > 200 | US_bg2$urb50 == 1),1,0)
#US_bg2[,'urb70'] <- ifelse((US_bg2$hu70_sqmi > 200 | US_bg2$urb40 == 1),1,0)
#US_bg2[,'urb80'] <- ifelse((US_bg2$hu80_sqmi > 200 | US_bg2$urb70 == 1),1,0)
#US_bg2[,'urb90'] <- ifelse((US_bg2$hu90_sqmi > 200 | US_bg2$urb80 == 1),1,0)
#US_bg2[,'urb00'] <- ifelse((US_bg2$hu00_sqmi > 200 | US_bg2$urb90 == 1),1,0)
#US_bg2[,'urb10'] <- ifelse((US_bg2$hu10_sqmi > 200 | US_bg2$urb00 == 1),1,0)


# convert to factors
#US_bg2$urb40 <- as.factor(US_bg2$urb40)
#US_bg2$urb50 <- as.factor(US_bg2$urb50)
#US_bg2$urb60 <- as.factor(US_bg2$urb60)
#US_bg2$urb70 <- as.factor(US_bg2$urb70)
#US_bg2$urb80 <- as.factor(US_bg2$urb80)
#US_bg2$urb90 <- as.factor(US_bg2$urb90)
#US_bg2$urb00 <- as.factor(US_bg2$urb00)
#US_bg2$urb10 <- as.factor(US_bg2$urb10)