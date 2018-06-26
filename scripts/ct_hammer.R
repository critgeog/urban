
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
h_units_national <- read_csv("csv/h_units_national.csv")

# read in housing units by 'year structure built' from 2011-15 ACS, downloaded from NHGIS
# metadata: data/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp_codebook.txt
USct <- read_csv("data/nhgis0054_csv/nhgis0054_ds215_20155_2015_tract.csv")
#for scott's computer:
#USbg <- read.csv("C:/Users/scott/Dropbox/hafley/urbanization/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp.csv")

# clean tibble
USct <- USct %>%
  select(-c(3,4,9,10,12:37)) %>%
  mutate(COUNTYJ = str_sub(GISJOIN, 1, 8))

# read block group area
# area unit: square miles of each block group.
# area calculated in ArcGIS; re-projected and calculated using equal area projection
ct_area <- read_csv("data/areasct.txt")

# remove unecessary columns
ct_area <- ct_area %>%
  select(-c(7:9))

# join area to 'Year Structure Built' tibble
USct <- USct %>%
  left_join(x = USct, y = ct_area, by = "GISJOIN") %>%
  rename(geoid = GEOID)

# join county housing units to block group tibble
US_ct2 <- left_join(USct,h_units_national, by = 'COUNTYJ')


# jXX = total number of housing units in county during time t, based on year structure built in ACS
# iXX = total number of housing units in block group during time t, based on year structure built in ACS
# adjXX = adjusted number of housing units in block group, hammer method output
# additional reference: https://www.nature.com/articles/nclimate2961#methods

US_ct2 <- US_ct2 %>%
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
US_ct2 <- US_ct2 %>%
  group_by(COUNTYJ) %>%
  mutate(
    hu40_sqmi = adj40/areaCT,
    hu50_sqmi = adj50/areaCT,
    hu60_sqmi = adj60/areaCT,
    hu70_sqmi = adj70/areaCT,
    hu80_sqmi = adj80/areaCT,
    hu90_sqmi = adj90/areaCT,
    hu00_sqmi = adj00/areaCT,
    hu10_sqmi = adj10/areaCT
  )

# write_csv(US_bg2, "csv/blk_grp_complete.csv")










######################################################################
# visualizations
#******************

#labels <- c(G1300670 = 'Cobb', G1301210 = 'Fulton', G1300890 = 'DeKalb',
#            G1301350 = 'Gwinnett', G1302190 = 'Oconee', G1300590 = 'Clarke')

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






# skip for now, commented out
US_bg2$urb40 <- ''
US_bg2[,'urb40'] <- sapply(US_bg2[,'hu40_sqmi'],function(x)ifelse(x > 200,1,0))
US_bg2$urb40 <- as.factor(US_bg2$urb40)

# set up urban classification
US_bg2$urb50 <- ''
US_bg2$urb60 <- ''
US_bg2$urb70 <- ''
US_bg2$urb80 <- ''
US_bg2$urb90 <- ''
US_bg2$urb00 <- ''
US_bg2$urb10 <- ''

# determine if area is urban. If huXX_sqmi > 200 or if previous decade is
# classified as urban, then bg is urban, according to this approach
US_bg2[,'urb50'] <- ifelse((US_bg2$hu50_sqmi > 200 | US_bg2$urb40 == 1),1,0)
US_bg2[,'urb60'] <- ifelse((US_bg2$hu60_sqmi > 200 | US_bg2$urb50 == 1),1,0)
US_bg2[,'urb70'] <- ifelse((US_bg2$hu70_sqmi > 200 | US_bg2$urb40 == 1),1,0)
US_bg2[,'urb80'] <- ifelse((US_bg2$hu80_sqmi > 200 | US_bg2$urb70 == 1),1,0)
US_bg2[,'urb90'] <- ifelse((US_bg2$hu90_sqmi > 200 | US_bg2$urb80 == 1),1,0)
US_bg2[,'urb00'] <- ifelse((US_bg2$hu00_sqmi > 200 | US_bg2$urb90 == 1),1,0)
US_bg2[,'urb10'] <- ifelse((US_bg2$hu10_sqmi > 200 | US_bg2$urb00 == 1),1,0)


# convert to factors
US_bg2$urb40 <- as.factor(US_bg2$urb40)
US_bg2$urb50 <- as.factor(US_bg2$urb50)
US_bg2$urb60 <- as.factor(US_bg2$urb60)
US_bg2$urb70 <- as.factor(US_bg2$urb70)
US_bg2$urb80 <- as.factor(US_bg2$urb80)
US_bg2$urb90 <- as.factor(US_bg2$urb90)
US_bg2$urb00 <- as.factor(US_bg2$urb00)
US_bg2$urb10 <- as.factor(US_bg2$urb10)