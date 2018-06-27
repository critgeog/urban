# #*******************************************************************************
# project: urban.Rproj                                                           #
# by: Taylor Hafley                                                              #
# last modified: 6.27.18                                                         #
# local wd ("Google Drive/school/R/urban/data)                                   #
# (dropbox/Projects/InProgress/urban                                             #
# Git: ("/data/UShammerMethodBG.R)                                               #
# data source(s): tidycensus, tigris, NHGIS, and M. Hauer                        #
#                                                                                #
#*********************************************************************************

library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
library(tmap)
library(tmaptools)
#library(magrittr)
library(purrr)
library(ggplot2) # need new or development version of ggplot2
library(readxl)

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
