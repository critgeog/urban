#*********************************************************************************
# copied from hammerMethod.R from desktop/tempNHGIS/current projects/urban       #
# original created: March 30, 2017                                               #
# file create: November 10, 2017                                                 #
# last modified: 2.1.18                                                         #
# saved: ("NHGIS/current projects/urban/UShammerMethod.R)                        #
# data from NHGIS; and expansion on hammerMethod.R, which is only ATL            #
# read in location: ("school/tempNHGIS/current projects/urban)                   #
# Author: Taylor Hafley                                                          #
#*********************************************************************************

library(tidyverse)
library(tidycensus)
library(magrittr)
library(stringr)
library(tigris)
library(viridis)
library(sf)
library(sp)
library(rgdal)
library(tmap)
library(tmaptools)
library(ggplot2)
library(janitor)
library(maptools)
library(htmlwidgets)
library(leaflet)
options(tigris_use_cache = TRUE)

th_api_acs <- '45544f0d114cfaa037a5566745d18bb8d4778cfa'

census_api_key('45544f0d114cfaa037a5566745d18bb8d4778cfa', install = TRUE)

?census_api_key
Sys.getenv("CENSUS_API_KEY")


load_variables(year = 2015, dataset = "acs5") %>% View
#load_variables(year = 2010, dataset = "acs5") %>% View

USbg <- read.csv("Dropbox/hafley/urbanization/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp.csv")
#for scott's computer:
#USbg <- read.csv("C:/Users/scott/Dropbox/hafley/urbanization/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp.csv")
USbg <- as_tibble(USbg)
head(USbg)
USbg <- USbg[,-(c(3,4,9,10,13:37))]
USbg$COUNTYJ <- str_sub(USbg$GISJOIN, 1, 8)

# USarea is the square miles of each block group
USarea <- read.csv("Dropbox/hafley/urbanization/areas.txt")
#for scott's computer:
#USarea <- read.csv("C:/Users/scott/Dropbox/hafley/urbanization/areas.txt")
USarea <- as_tibble(USarea)
head(USarea)
USarea <- USarea[,-(c(7:9))]

# join area earlier
USbg2 <- left_join(x = USbg, y = USarea, by = "GISJOIN")

USbg2 %>%
  rename(geoid = GEOID)

USbg2$GEOID

#read in housing unit by year Census
hu1940 <- read.csv("Dropbox/hafley/urbanization/nhgis0022_csv/nhgis0022_ds78_1940_county.csv")
#for scott's computer:
#hu1940 <- read.csv("C:/Users/scott/Dropbox/hafley/urbanization/nhgis0022_csv/nhgis0022_ds78_1940_county.csv")
hu1940 <- as_tibble(hu1940)
hu1940$COUNTYA <- as.integer(hu1940$COUNTYA/10)
str(hu1940)

#for scott's computer (shoulda done setwd sooner....):
#setwd("C:/Users/scott")
hu1970 <- read_csv("Dropbox/hafley/urbanization/nhgis0025_csv/nhgis0025_ds94_1970_county.csv")
hu1980 <- read_csv("Dropbox/hafley/urbanization/nhgis0025_csv/nhgis0025_ds104_1980_county.csv")
hu1990 <- read_csv("Dropbox/hafley/urbanization/nhgis0025_csv/nhgis0025_ds120_1990_county.csv")
hu2000 <- read_csv("Dropbox/hafley/urbanization/nhgis0025_csv/nhgis0025_ds146_2000_county.csv")
hu2010 <- read.csv("Dropbox/hafley/urbanization/nhgis0026_csv/nhgis0026_ds172_2010_county.csv")
hu2010 <- as_tibble(hu2010)
str(hu2010)


# modify by Year Census
hu1940 <- hu1940[,-(c(3:7,9))]
hu1970 <- hu1970[,-(3:7)]
hu1980 <- hu1980[,-(c(3:10))]
hu1990 <- hu1990[,-(c(3:16))]
hu2000 <- hu2000[,-(c(3:12))]
hu2010 <- hu2010[,-(c(3:39))]

hu1940 %<>%
 rename(COUNTYJ = GISJOIN)
hu1970 %<>%
  rename(COUNTYJ = GISJOIN)
hu1980 %<>%
  rename(COUNTYJ = GISJOIN)
hu1990 %<>%
  rename(COUNTYJ = GISJOIN)
hu2000 %<>%
  rename(COUNTYJ = GISJOIN)
hu2010 %<>%
  rename(COUNTYJ = GISJOIN)

#str(hu2010$COUNTYJ)
#hu1940$COUNTYJ <- as.factor(hu1940$COUNTYJ)
#str(hu1940)

test1 <- left_join(USbg2,hu1940, by = ("COUNTYJ"))
test1 <- left_join(test1,hu1970, by = "COUNTYJ")
test1 <- left_join(test1,hu1980, by = "COUNTYJ")
test1 <- left_join(test1,hu1990, by = "COUNTYJ")
test1 <- left_join(test1,hu2000, by = "COUNTYJ")
test1 <- left_join(test1,hu2010, by = "COUNTYJ")

test1 <- test1 %>%
  rename(hu40 = BXR001, hu70 = CBV001, hu80 = C8Y001, hu90 = ESA001, hu00 =FKI001, hu10 = IFC001)

test1a <- test1 %>%
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
    adj40a = (hu40/sum(ADQSE011)*ADQSE011),
    adj70a = hu70/j70 * i70,
    adj80a = hu80/j80 * i80,
    adj90a = hu90/j90 * i90,
    adj00a = hu00/j00 * i00,
    adj10a = hu10/j10 * i10
  )

#test1a %>%
#  filter(i10 < i00)

test1a$COUNTYJ <- as.factor(test1a$COUNTYJ)

# example
test1a %>%
  group_by(COUNTYJ) %>%
  summarize(
    sum(ADQSE011)
  )

test1c$ham40

# ham = housing units/square mile
test1c <- test1a %>%
  group_by(COUNTYJ) %>%
  mutate(
    ham40 = adj40a/area,
    ham70 = adj70a/area,
    ham80 = adj80a/area,
    ham90 = adj90a/area,
    ham00 = adj00a/area,
    ham10 = adj10a/area
  )

test1c <- ungroup(test1c)

# skip for now, commented out
#test1c$urb40 <- ''
#test1c[,'urb40'] <- sapply(test1c[,'ham40'],function(x)ifelse(x > 200,1,0))
#test1c$urb40 <- as.factor(test1c$urb40)

# set up urban classification
#test1c$urb70 <- ''
#test1c$urb80 <- ''
#test1c$urb90 <- ''
#test1c$urb00 <- ''
#test1c$urb10 <- ''

# determine if area is urban. If ham > 200, then bg is urban
#test1c[,'urb70'] <- ifelse((test1c$ham70 > 200 | test1c$urb40 == 1),1,0)
#test1c[,'urb80'] <- ifelse((test1c$ham80 > 200 | test1c$urb70 == 1),1,0)
#test1c[,'urb90'] <- ifelse((test1c$ham90 > 200 | test1c$urb80 == 1),1,0)
#test1c[,'urb00'] <- ifelse((test1c$ham00 > 200 | test1c$urb90 == 1),1,0)
#test1c[,'urb10'] <- ifelse((test1c$ham10 > 200 | test1c$urb00 == 1),1,0)

#test1c$urb70 <- as.factor(test1c$urb70)
#test1c$urb80 <- as.factor(test1c$urb80)
#test1c$urb90 <- as.factor(test1c$urb90)
#test1c$urb00 <- as.factor(test1c$urb00)
#test1c$urb10 <- as.factor(test1c$urb10)

#write.csv(test1c,"desktop/tempNHGIS/current projects/urban/usbgham.csv")

# compare number of urban block groups in 1940 w/ 2010
#ggplot(test1c, mapping = aes(x = urb40)) +
#  geom_histogram(stat = 'count')

#ggplot(test1c, mapping = aes(x = urb10)) +
#  geom_histogram(stat = 'count')

#test1c %>%
#  group_by(COUNTYJ) %>%
#  ggplot(mapping = aes(x = urb40)) +
#  geom_histogram(stat = 'count')


####################################
##### MESSAGE TO SCOTT
##### Date: January 17, 2018

# test1c represents a complete dataset. Includes adjusted housing units, hammer hu, and urban classification

# i'm just messing around in lines after 180; none of this produces anything particularly meaningful
# I have pretty good visualizations for the ATLANTA only region in a separate file. Will add to the dropbox soon


# slow and ineffective visually
test1c %>% 
  filter(STATEA == 13 & ham90 >10 & ham90 < 800) %>%
  ggplot((mapping = aes(x = ham90)))+
  geom_histogram() +
  facet_wrap('COUNTYJ')

test1c %>% 
  filter(STATEA == 13 & COUNTYA < 25) %>%
  ggplot((mapping = aes(x=COUNTYA, y = ham40, group = COUNTYA)))+
  geom_boxplot()

write_csv(test1c,"Dropbox/hafley/urbanization/test1c.csv") #If you want to save as a file

# this is the 'end'

# data on # of homeowners in Fulton County
df_acs15 <- get_acs(
  geography = "block group",
  county = c("Fulton","DeKalb","Gwinnett","Cherokee",
             "Cobb","Douglas","Fayette","Clayton",
             "Henry","Rockdale"),
  state = "GA",
  variables = c('B25003_001','B25003_002','B25003_003'), 
  year = 2015,
  key = th_api_acs,
  geometry = TRUE,
  output = 'wide'
) %>% clean_names() %>%
  rename(B25003_001_15 = b25003_001e, B25003_002_15 = b25003_002e, B25003_003_15 = b25003_003e) %>%
  mutate(hopct15 = B25003_002_15/B25003_001_15*100)
class(df_acs15)

# data on # of poor people in ARC
df_acs15 <- get_acs(
  geography = "tract",
  county = c("Fulton","DeKalb","Gwinnett","Cherokee",
             "Cobb","Douglas","Fayette","Clayton",
             "Henry","Rockdale"),
  state = "GA",
  variables = c('B17001_002E','B17001_002M','B01003_001E', 'B01003_001M'), 
  year = 2015,
  key = th_api_acs,
  geometry = TRUE,
  output = 'wide'
) %>% clean_names() %>%
    mutate(pov_pct = b17001_002e/b01003_001e*100,
           pov_moe = b17001_002m/b01003_001m*100)
class(df_acs15)


# data on # of homeowners in Fulton County
df_acs15 <- get_acs(
  geography = "block group",
  county = c("Clarke"), 
             state = "OH",
  variables = c(), 
  year = 2015,
  key = th_api_acs,
  geometry = TRUE,
  output = 'wide'
) %>% clean_names() %>%
  rename(B25003_001_15 = b25003_001e, B25003_002_15 = b25003_002e, B25003_003_15 = b25003_003e) %>%
  mutate(hopct15 = B25003_002_15/B25003_001_15*100)
class(df_acs15)

# data on # of poor people in ARC
df_acs15 <- get_acs(
  geography = "block group",
  county = c("Clark"), 
  state = "OH",
  variables = c('B17001_002E','B17001_002M','B01003_001E', 'B01003_001M','B25003_001','B25003_002','B25003_003'), 
  year = 2015,
  key = th_api_acs,
  geometry = TRUE,
  output = 'wide'
) %>% clean_names() %>%
  rename(B25003_001_15 = b25003_001e, B25003_002_15 = b25003_002e, B25003_003_15 = b25003_003e) %>%
  mutate(hopct15 = B25003_002_15/B25003_001_15*100,
         pov_pct = b17001_002e/b01003_001e*100,
         pov_moe = b17001_002m/b01003_001m*100)
class(df_acs15)

test1c$area %>% b2 
# now, Join 2010 "tbl_df" to 2015 "sf"

#gaham <- test1c %>%
#  filter(STATEA == 13)

#acsjoin <- merge(x = df_acs15, y = gaham, by.x = 'geoid', by.y = 'GEOID')
#acsjoin <- merge(x = df_acs15, y = test1c, by.x = 'geoid', by.y = 'GEOID')

sprfldj <- merge(x = df_acs15, y = test1c, by.x = 'geoid', by.y = 'GEOID')

# df_acs15 is poverty rates for Census Tracts in ARC
sprfldj %>% View


# ham = housing units/square mile
sprfldj <- sprfldj %>%
  group_by(COUNTYJ) %>%
  mutate(
    ham40 = adj40a/area,
    ham70 = adj70a/area,
    ham80 = adj80a/area,
    ham90 = adj90a/area,
    ham00 = adj00a/area,
    ham10 = adj10a/area
  )

test1c <- ungroup(test1c)

#acs1015 %<>%
#  mutate(chg10_15 = (hopct15 - hopct10)) %>%
#  mutate(tdiff10_15 = B25003_002_15 - B25003_002_10)

# map the data
sprfldj %>% 
  ggplot()+
  geom_sf(aes(fill = hopct15))+
  scale_fill_viridis("urban classification")+
  coord_sf(datum = NA)+
  theme_void(base_family =  "mono")+
  theme(legend.position = c(.15, .15))+
  labs(title = "Atlanta 2010 urbanized \nboundaryby block group\n",
       subtitle = "ACS 2015, via tidycensus",
       caption = "taylor.hafley@uga.edu", 
       x = NULL, y = NULL)

summary(df_acs15$pov_pct)

sprfldj %>% 
  ggplot()+
  geom_sf(aes(fill = urb40))+
  scale_fill_viridis_d("urban classification")+
  coord_sf(datum = NA)+
  theme_void(base_family =  "mono")+
  theme(legend.position = c(.15, .15))+
  labs(title = "Atlanta 1940 urbanized \nboundaryby block group\n",
       subtitle = "ACS 2015, via tidycensus",
       caption = "taylor.hafley@uga.edu", 
       x = NULL, y = NULL)

# tmap

tm_shape(df_acs15) +
  tm_polygons("pov_pct")

tm_shape(df_acs15) +
  tm_polygons("b17001_002e")

tmap_mode("view")

tm_shape(df_acs15) +
  tm_polygons(c("pov_pct", "b17001_002e"), 
              style=c("pretty", "pretty"),
              palette=list("RdYlGn", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("Percent", "Total")) +
  tm_style_grey()

#tmap_mode("plot")

tm_shape(df_acs15) +
  tm_bubbles(size=c("pov_pct", "b17001_002e"), title.size="Homeownership") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("1940", "2010"))

tm_shape(df_acs15) +
  tm_bubbles(size=c("pov_pct"), title.size="Percent Poverty") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("1940"))





# tmap

tm_shape(sprfldj) +
  tm_polygons("hopct15")

tm_shape(sprfldj) +
  tm_polygons("adj10a")

tmap_mode("view")

tm_shape(df_acs15) +
  tm_polygons(c("pov_pct", "b17001_002e"), 
              style=c("pretty", "pretty"),
              palette=list("RdYlGn", "Purples"),
              auto.palette.mapping=FALSE,
              title=c("Percent", "Total")) +
  tm_style_grey()

#tmap_mode("plot")

tm_shape(df_acs15) +
  tm_bubbles(size=c("pov_pct", "b17001_002e"), title.size="Homeownership") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("1940", "2010"))

tm_shape(df_acs15) +
  tm_bubbles(size=c("pov_pct"), title.size="Percent Poverty") +
  tm_facets(free.scales=FALSE) +
  tm_layout(panel.labels=c("1940"))






# these don't work for now (1/19/18)
# stupid example, but this is how you put dot density map

acsjoin %>% View()

tm_shape(acsjoin) +
  tm_borders() +
  tm_bubbles("ham10", "blue", border.col = "black", border.lwd=1, 
             size.lim = c(0, 44000), sizes.legend = c(100, 500, 1000, 4000), 
             title.size="Metropolitan Population") +
  tm_layout()

summary(acsjoin$ham10)

tm_shape(acsjoin) +
  tm_polygons("hopct10", title = "H.O difference") +
  tm_facets("COUNTYJ", free.coords = FALSE) +
  tm_style_grey()

tm_shape(acsjoin) +
  tm_polygons("hopct10", title = "H.O difference") +
  tm_facets("COUNTYJ") +
  tm_style_grey()