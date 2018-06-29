# #*******************************************************************************
# project: urban.Rproj                                                           #
# by: Taylor Hafley                                                              #
# last modified: 6.27.18                                                         #
# local wd ("Google Drive/school/R/urban/data)                                   #
# (dropbox/Projects/InProgress/urban                                             #
# Git: ("/data/UShammerMethodBG.R)                                               #
# data source(s): tidycensus, tigris, NHGIS, and M. Hauer                        #
#                                                                                #
# Script: calculate housing units per square mile in block groups from 1940-2010 #
#         map Atlanta region at end of script
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
library(ggplot2)
library(readxl)

# read in number of housing units (county), 1940-2010
# calculated in h_units.R
h_units_national <- read_csv("csv/h_units_national.csv")

# read in housing units: 'Year structure built' (block group, 2011-15 ACS, 
# downloaded from NHGIS)
# metadata: data/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp_codebook.txt
USbg <- read_csv("data/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp.csv")
#for scott's computer:
#USbg <- read.csv("C:/Users/scott/Dropbox/hafley/urbanization/nhgis0051_csv/nhgis0051_ds215_20155_2015_blck_grp.csv")

# clean block group ACS
USbg <- USbg %>%
  select(-c(3,4,9,10,13:37)) %>%
  mutate(COUNTYJ = str_sub(GISJOIN, 1, 8))

# read block group area
# unit: square miles
# area calculated in ArcGIS; re-projected data, and calculated area 
# using equal area projection
bg_area <- read_csv("data/areas.txt")

# remove unecessary columns
bg_area <- bg_area %>%
  select(-c(7:9))

# join area to block group ACS
USbg <- USbg %>%
  left_join(x = USbg, y = bg_area, by = "GISJOIN") %>%
  rename(geoid = GEOID)

# join housing units (county, 1940-2010) to block group ACS
US_bg2 <- left_join(USbg,h_units_national, by = 'COUNTYJ')


# jXX = total number of housing units in county during time t, 
           #     based on year structure built in ACS
# iXX = total number of housing units in block group during time t, 
           # based on year structure built in ACS
# adjXX = adjusted number of housing units in block group, hammer method output
# reference: https://www.nature.com/articles/nclimate2961#methods

US_bg2 <- US_bg2 %>%
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
US_bg2 <- US_bg2 %>%
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

# write_csv(US_bg2, "csv/blk_grp_complete.csv")










#optional: categorize block group as 'urban' if housing units/sq. mil > 200
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






######################
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



# use tigris package to enable mapping; generate tigris file of atlanta, ga 
# metropolitan block groups. Join to US_bg (atlanta parcels) and map.

# GA block groups
gaBGs <- block_groups("GA", cb = TRUE)

ggplot(gaBGs) +
  geom_sf()

# Atlanta Metro
cb <- core_based_statistical_areas(cb = TRUE)
atl <- filter(cb, grepl("Atlanta", NAME))

ggplot(atl) + 
  geom_sf()

# select bg's inside atl metro
w1 <- st_within(gaBGs, atl)

print(length(w1))
print(w1[1:5])

w2 <- map_lgl(w1, function(x) {
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
})

p2 <- gaBGs[w2,]

ggplot() + 
  geom_sf(data = p2) + 
  geom_sf(data = atl, fill = NA, color = "red")

atl_hammer <- left_join(x = p2, y = US_bg2, by = c('GEOID' = 'geoid'))
atl_hammer <- atl_hammer %>%
  st_transform(crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 
               +lon_0=-84 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 
               +units=m +no_defs")

write_sf(atl_hammer, "data/atl2.geojson")

# the following scripts create the maps located in the 'maps' folder.

leg_col <- c("#E1EFFA","#065AA0")
lbl <- c("< 200 units/sq mi", "> 200 units/sq mi")

rd <- primary_roads()

atl40 <- tm_shape(atl_hammer) + 
  tm_fill('hu40_sqmi', breaks = c(0, 200, 20000), 
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE,
          title = '1940') +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "1940") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

atl40
  
atl50 <- tm_shape(atl_hammer) + 
  tm_fill('hu50_sqmi', breaks = c(0, 200, 20000), 
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "1950") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

atl60 <- tm_shape(atl_hammer) + 
  tm_fill('hu60_sqmi', breaks = c(0, 200, 20000),
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "1960") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

atl70 <- tm_shape(atl_hammer) + 
  tm_fill('hu70_sqmi', breaks = c(0, 200, 20000),
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "1970") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

atl80 <- tm_shape(atl_hammer) + 
  tm_fill('hu80_sqmi', breaks = c(0, 200, 20000),
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "1980") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)
  
atl90 <- tm_shape(atl_hammer) + 
  tm_fill('hu90_sqmi', breaks = c(0, 200, 20000),
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "1990") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

atl00 <- tm_shape(atl_hammer) + 
  tm_fill('hu00_sqmi', breaks = c(0, 200, 20000),
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "2000") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)

atl10 <- tm_shape(atl_hammer) + 
  tm_fill('hu10_sqmi', breaks = c(0, 200, 20000),
          palette = leg_col, auto.palette.mapping = FALSE,
          legend.show = FALSE) +
  tm_add_legend(type = c("fill"), labels = lbl, col = leg_col, 
                title = "2010") +
  tm_shape(rd) +
  tm_lines(col = 'black', alpha = 0.6) +
  tm_compass(type = "arrow", size = 4, position = c(0.82, 0.08)) +
  tm_legend(position = c(0.025, 0.05),
            bg.color = "white",
            frame = TRUE,
            legend.text.size = .7,
            legend.title.size = 1.3) + 
  tm_layout(frame = FALSE, 
            outer.margins=c(0,0,0,0), 
            inner.margins=c(0,0,0,0), asp=0)


library(magick)
list.files(path = "maps/.", pattern = "*.png", full.names = T) %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("maps/atl.gif") # write to current dir



# animation of maps made in qgis
list.files(path = "maps/Q/.", pattern = "*.png", full.names = T) %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=2) %>% # animates, can opt for number of loops
  image_write("maps/Q/atlq.gif") # write to current dir
